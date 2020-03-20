# blb4lm (Bag of Little Bootstrp for Linear Regresion) #

# Description #

This package is the final project of STA141C course at UC Davis WQ2020 by Yifeng Jiang, Zhihao Chen, Shenglun Wu, and Qiushuang Xu. It may not be regularly maintained in the future.

blb4lm takes a dataset as input, using the method of BLB (bag of little bootstrap) to obtain the coefficients and standard deviation of a given model.
  
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

# Contributors #

Yifeng was incharge of the development of the source code; Zhihao documented the code; Shenglun and Qiushuang wrote the vignette and Readme file.

# Reference #

The concept of bag of little bootstrap comes from Ariel Kleiner and the paper A Scalable Bootstrap for Massive Data: https://arxiv.org/pdf/1112.5016.pdf

Professor Randy Lai also gave valuable consultation on the architecture of this package.
