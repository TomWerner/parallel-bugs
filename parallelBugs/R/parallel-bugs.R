list.of.packages <- c("snow", "snowfall", "R2OpenBUGS", "coda", "rlecuyer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}

library(snow)
library(snowfall)
library(R2OpenBUGS)
library(coda)
library(lattice)
library(rlecuyer)
library(stats)


#' Builds a list of available CPUs for sfInit.
#' @param host.list list of ip addresses, or a single ip address
#' @param cpu.counts list of corresponding available cpus, or cpu count for all machines, or cpu count for single machine
#' @return list of available CPUs
#'
#' build.cluster.list('localhost', 3)   # c('localhost', 'localhost', 'localhost')
#' build.cluster.list(c('machine1', 'machine2', 'machine3'), 1)  # c('machine1', 'machine2', 'machine3')
#' build.cluster.list(c('machine1', 'machine2'), c(2, 1))   # c('machine1', 'machine1', 'machine2')
#'
build.cluster.list = function(host.list, cpu.counts) {
  if (!is.vector(host.list) && is.character(host.list)) {
    host.list = c(host.list)
  }
  if (is.vector(host.list) && (is.numeric(cpu.counts) && length(cpu.counts) == 1)) {
    cpu.counts = rep(cpu.counts, length(host.list))
  }
  if (is.vector(host.list) && is.vector(cpu.counts)) {
    if (length(host.list) != length(cpu.counts)) {
      stop("Mismatch between cpu list and host list lengths")
      return()
    }
  }
  cluster.hosts = NULL
  for (i in 1:length(host.list)) {
    cluster.hosts = c(cluster.hosts, rep(host.list[i], cpu.counts[i]))
  }
  return(cluster.hosts)
}


#' Worker function, runs on each CPU
#' @param chain number the chain is (1, 2, 3, ...)
#' @param data the R2OpenBUGS::bugs data
#' @param model.str the OpenBugs model string
#' @param inits the inits list from the main function
#' @param params the params to record
#' @param n.iter number of iterations
#' @param n.burnin number of burnin iterations
#' @param OpenBUGS.exe the openbugs executable location
#' @param bugs.debug run R2OpenBUGS with debug flag
#' @return coda mcmc data
bugs.worker <- function(chain, data, model.str, inits, params, n.iter, n.burnin, OpenBUGS.exe, bugs.debug=FALSE)
{
  inits = inits[paste('chain', chain, sep='')]
  folder = paste(getwd(), '/chain', chain, sep='')
  unlink(folder, recursive=TRUE)
  dir.create(folder)
  sink(paste(folder, "/model.txt", sep=""))
  cat(model.str)
  sink()

  # Make OpenBUGS call
  R2OpenBUGS::bugs(data=data, inits=inits, parameters.to.save=params,
       n.iter=n.iter, n.chains=1, n.burnin=n.burnin, n.thin=1,
       model.file="model.txt", debug=bugs.debug, codaPkg=TRUE,
       OpenBUGS.pgm=OpenBUGS.exe,
       working.directory=folder,bugs.seed=as.integer(stats::runif(1, 1, 14)))
  R2OpenBUGS::read.bugs(c(paste(folder, "/CODAchain1.txt", sep="")))[[1]]
}


#' Main function of parallelBugs. Pass parameters that would be passed to R2OpenBUGS::bugs, as well as
#' cpu/host information, to run the model with multiple chains in parallel.
#'
#' @param host.list A single ip as a string ('localhost', 'server.org') or a vector of string ips c('machine1', 'machine2', 'machine3')
#' @param cpu.counts A single integer if each machine has the same number of cores to utilize, or a vector corresponding to the number of cores to utilize per machine
#' @param model.str the OpenBUGS model as a string
#' @param bugs.data data to pass to R2OpenBUGS::bugs - it should be a list
#' @param n.chains The number of chains to run - it should equal the sum of cpu.counts
#' @param init.list list of inits for each chain, structured as list(chain1=list(...), chain2=(list(...)), ...)
#' @param parameters.to.save params to pass to R2OpenBUGS - should be a list
#' @param OpenBUGS.exe OpenBUGS executable location, to be passed to OpenBUGS.pgm
#' @param n.iter number of iterations
#' @param n.burnin number of burnin iterations
#' @param bugs.debug if R2OpenBUGS will run bugs() call with debug true
#' @return coda mcmc.list object
#' @examples
#'model.str = "model
#'{
#'  tau ~ dgamma(0.0001, 0.0001)
#'  mu ~ dflat()
#'  sigma <- 1 / sqrt(tau)
#'
#'  for(i in 1:N)
#'  {
#'    x.data[i]~dnorm(mu, tau)
#'  }
#'}"
#'bugs.data = list(x.data=rnorm(1000, 12, 5), N=1000)
#'
#'# Initial values of parameters. Note that you can use
#'# R random number functions here
#'init.list = list(
#'  chain1=list(mu=runif(1, -50, 50), tau=1),
#'  chain2=list(mu=-50, tau=10),
#'  chain3=list(mu=50, tau=.1)
#')
#'
#'# Parameters to collect statistics on
#'params <- c("mu", "sigma")
#'
#'OpenBUGS.exe="C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
#'
#'\dontrun{
#'parallel.bugs('localhost', 3, model.str,
#'              bugs.data, 3, init.list, params,
#'              OpenBUGS.exe, n.iter=1000)
#'}
parallel.bugs = function(host.list,
                         cpu.counts,
                         model.str,
                         bugs.data,
                         n.chains,
                         init.list,
                         parameters.to.save,
                         OpenBUGS.exe,
                         n.iter=5000,
                         n.burnin=1000,
                         bugs.debug=FALSE
) {
  if (length(init.list) != n.chains) {
    stop("Number of chains must match length of init list")
  }

  # Initialize the cluster using the specified hosts and cpu counts. In order to have
  # a specific number of cpu's per host, we repeat the host cpu times.
  snowfall::sfInit(parallel=TRUE, socketHosts = build.cluster.list(host.list, cpu.counts))

  # Assign the R2OpenBUGS library to each CPU
  snowfall::sfLibrary(package='R2OpenBUGS')
  snowfall::sfClusterSetupRNG()

  result = coda::mcmc.list(snowfall::sfLapply(1:n.chains, fun=bugs.worker,
                              data=bugs.data, inits=init.list,
                              params=parameters.to.save,
                              model.str=model.str, n.iter=n.iter,
                              n.burnin=n.burnin, OpenBUGS.exe=OpenBUGS.exe,
                              bugs.debug=bugs.debug))
  snowfall::sfStop()
  return(result)
}
