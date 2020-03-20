blb4lm
=========
---


# Description #
  The package blb4lm takes a dataset as input, using the method of BLB (bag of little bootstrap) to obtain the coefficients and standard deviation of a given model.
  
  The package first divides the data into an assigned number of subsamples, then performs bootstrap on each subsample to calculate their coefficients, and eventually combines all coefficients to obtain the model for the entire data set. It can also provide predicted values for a new data set using the obtained model.


# Installation #

Using the 'devtools' package:

    > install.packages("devtools")
    > library(devtools)
    > install_github('blb4lm')
    
    
## CRAN ##

To install the most recent package from CRAN type:

    > install.packages("blb4lm")
    > library(blb4lm)

# Usage #

The packages contains four functions:

    > blb4lm()
    > coef.blb4lm()
    > sigma.blb4lm()
    > predict.blb4lm()
    
See the vignette for detailed applications for the functions.
