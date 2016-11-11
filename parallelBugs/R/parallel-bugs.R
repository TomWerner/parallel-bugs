library(snow)
library(snowfall)
library(R2OpenBUGS)
library(coda)

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
  bugs(data=data, inits=inits, parameters.to.save=params,
       n.iter=n.iter, n.chains=1, n.burnin=n.burnin, n.thin=1,
       model.file="model.txt", debug=bugs.debug, codaPkg=TRUE,
       OpenBUGS.pgm=OpenBUGS.exe,
       working.directory=folder)
  read.bugs(c(paste(folder, "/CODAchain1.txt", sep="")))[[1]]
}

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
  sfInit(parallel=TRUE, socketHosts = build.cluster.list(host.list, cpu.counts))
  
  # Assign the R2OpenBUGS library to each CPU
  sfLibrary(R2OpenBUGS)
  sfClusterSetupRNG()
  
  result = mcmc.list(sfLapply(1:n.chains, fun=bugs.worker, 
                              data=bugs.data, inits=init.list, 
                              params=parameters.to.save, 
                              model.str=model.str, n.iter=n.iter, 
                              n.burnin=n.burnin, OpenBUGS.exe=OpenBUGS.exe,
                              bugs.debug=bugs.debug))
  sfStop()
  return(result)
}
