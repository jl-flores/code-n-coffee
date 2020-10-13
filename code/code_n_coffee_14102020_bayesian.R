# Presenter: Kevin Jutras

##############
# Lab, Jan 08 2019
##############
# This code installs the necessary packages for the class and
# does some simple tests to make sure it's working correctly.
# The main goal is to have the `rethinking` package installed, and
# its two main functions, map() and map2stan() running.
##############

# make sure you're using at least R 3.4
# (if not, re-install R)
version 

# install rstan
install.packages("rstan", repos = "https://cloud.r-project.org/")

# install necessary prerequisites for `rethinking` package
install.packages(c("coda","mvtnorm","devtools","loo"))
s
# install McElreath's 'rethinking' package
# from https://github.com/rmcelreath/rethinking
library(devtools)
devtools::install_github("rmcelreath/rethinking")

# load rethinking package
library(rethinking)
# the following two commands should usually be run after loading `rethinking`
options(mc.cores = parallel::detectCores()) # make sure you're using all your computer's cores
rstan_options(auto_write = TRUE) # minimizie the amount of recompiling you need to do

##############
# That should be it for installing and loading the package.
# Now we will test the dens(), map(), and map2stan() functions
##############

# draw 10,000 random normal samples and look at them using dens()
obs <- rnorm(10e3,mean = 2,sd = 1) 
dens(obs)
dens(obs,norm.comp = TRUE)


# test map() function:
# make a simple model
f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 20 ),
  sigma ~ dcauchy( 0 , 3 )
)
# fit it using maximum a posteriori (MAP)
fit_map <- map( 
  f , 
  data = list(y=obs) ,
  start=list(mu=0,sigma=1)
)
precis(fit_map)


# and finally, test the map2stan() function
# (this should tak a while to run, and will
# probably throw some warnings and errors.
# Don't worry about these for now)
# This fits the same model as above, using MCMC instead of MAP
fit_stan <- map2stan( 
  f , 
  data = list(y=obs)
)
precis(fit_stan)


# precis(fit_map) and precis(fit_stan) should give almost identical results
precis(fit_map)
precis(fit_stan)
