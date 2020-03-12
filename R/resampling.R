# parameters
# r -- number of resamples
# n -- sample size
# subs -- subsamples returned from subsampling function

blb4lm <- function(formula, s, r, data){
  if(is.na(data) == TRUE){
    message("NA are present in the data and it's removed")
    data <- data[!is.na(data)]
    n <- nrow(non_missing)
  }
  subsamples <- subsampling(data, s)
  estimates <- map(subsamples, ~resampleBuild(r, n, ., formula))
}

subsampling <- function(data, s) {
  index <- sample.int(s, nrow(data), replace = TRUE)
  data %>% split(index)
}

resampleBuild <- function(r, n, sub, formula){
  models <- seq_len(r) %>% map(~singleBoots(., n, sub, formula))
  return(models)
}

singleBoots <- function(i, n, sub, formula){
  freqs <- rmultinom(1, n, rep(1, nrow(sub)))
  each_model <- lm_helper(formula = y~x, weights = freqs, data = sub)
  return(each_model)
}

lm_helper <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blb_coef(fit), sigma = blb_sigma(fit, freqs))
}

blb_coef <- function(object){
  return(as.numeric(object$coef))
}

blb_sigma <- function(object, freqs){
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y

}

# test
library(tidyverse)
n <- 1e6
resampleBuild(40, n, sub = part1, form=y ~ x)

