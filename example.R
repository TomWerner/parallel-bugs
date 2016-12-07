install.packages('devtools')
library(devtools)
install_github("tomwerner/parallel-bugs", subdir='parallelBugs')

library(R2OpenBUGS)
library(coda)
library(parallelBugs)
library(snow)
library(snowfall)

#OpenBUGs model
model.str = "model
{
  tau ~ dgamma(0.0001, 0.0001)
  mu ~ dflat()
  sigma <- 1 / sqrt(tau)

  for(i in 1:N)
  {
    x.data[i]~dnorm(mu, tau)
  }
}"
bugs.data = list(x.data=rnorm(1000, 12, 5), N=1000)
  
# Initial values of parameters. Note that you can use
# R random number functions here
init.list = list(
  chain1=list(mu=runif(1, -50, 50), tau=1),
  chain2=list(mu=-50, tau=10),
  chain3=list(mu=50, tau=.1)
)

# Parameters to collect statistics on
params <- c("mu", "sigma")

OpenBUGS.exe="C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"


folder="chain1"
system.time(bugs(data=bugs.data, inits=init.list, 
                 parameters.to.save=params, n.iter=100000, 
                 n.chains=3, n.burnin=1000, n.thin=1,
                 model.file="model.txt", debug=FALSE, 
                 codaPkg=TRUE, OpenBUGS.pgm=OpenBUGS.exe,
                 working.directory=folder,
                 bugs.seed=as.integer(runif(1, 1, 14))))

system.time(parallel.bugs('localhost', 3, model.str, 
                          bugs.data, 3, init.list, params, 
                          OpenBUGS.exe, n.iter=100000))


