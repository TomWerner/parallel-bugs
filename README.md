# parallel-bugs
R Package for running OpenBUGS chains on a cluster or in parallel. Cross platform compatible.

## Getting OpenBUGS
http://www.openbugs.net/w/Downloads

## Example
```R
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

# Simulated normal dataset
bugs.data = list(x.data=rnorm(1000, 12, 5), N=1000)

# Initial values of parameters. Note that you can use
# R random number functions here
init.list = list(
  chain1=list(mu=runif(-50, 50), tau=1),
  chain2=list(mu=-50, tau=10),
  chain3=list(mu=50, tau=.1)
)

# Parameters to collect statistics on
params <- c("mu", "sigma")

# Windows, for linux it would be different
OpenBUGS.exe="C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

# Runs 3 chain in parallel on a local machine.
# Returns a coda mcmc list object
result = parallel.bugs('localhost', 3, model.str, bugs.data, 3, init.list, params, OpenBUGS.exe, n.iter=10000)

# Assumes 4 cores per machine
# result = parallel.bugs(c('localhost', 'other.machine'), 4, ...)

# Specify number of cores per machine
# result = parallel.bugs(c('localhost', 'other.machine'), c(4, 8), ...)
```
