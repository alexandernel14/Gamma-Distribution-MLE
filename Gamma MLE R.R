library(stats)

# Set a seed so that the code can be repeated to be able to reproduce results
set.seed(33)
# Create simulated data for 1000 observations of a Gamma Distributed Random Variable
sampleData <- rgamma(n = 1000,shape = 2, rate =0.2)

# Let x_bar be the sample mean from the simulated data
x_bar  <- mean(sampleData) # alpha/lambda is the mean
x_bar

# Take the natural logarithm of each of the values in the sampleData
lnSample <- log(sampleData)
head(lnSample)

# Get the mean of the natural log sample data
meanlnSample <- mean(lnSample)
meanlnSample

# Write a function that corresponds to the partial derivative that we derived of the log-likelihood function
# with respect to alpha.

f <- function(x){
  
  # In this functions case we take the value x which will be the value of alpha that we are solving for.
  log(x) - digamma(x) - log(x_bar) + meanlnSample
}

# Plot curves of the derivative of the log-likelihood function with respect to alpha.

curve(f,from = 0, to = 5)
abline(h = 0)

curve(f,from = 1, to = 3)
abline(h=0)

curve(f,from = 2, to = 2.5)
abline(h = 0)


# Use the uniroot function to find where the derivative is equal to zero so that we can find
# the root (x value or alpha hat) at which it occurs.
a <- uniroot(f,lower = 2.0, upper = 2.2)
a
# Set alpha_hat equal to the root of the function f on the interval (2.0,2.2)
alpha_hat <- a$root
alpha_hat
abline(v = alpha_hat)

# Recall that the Maximum Likelihood Estimate of Lambda was found to be alpha_hat over x_bar.
lambda_hat <- alpha_hat/x_bar
lambda_hat

