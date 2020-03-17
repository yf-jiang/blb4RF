#' @import stats
#' @import purrr
#' @import furrr
#' @import future
#' @import utils

#' @title blb4lm: Implement bag of little bootstrap on Linear regression
#' @aliases blb4lm-package
#' @details
#' The package contains function: blb4lm(), coef.blb4lm(), sigma.blb4lm(), predict.blb4lm().
#'     These functions can help users quickly apply bag of little bootstrap on linear regression
#' to get the estimates of coefficients and their standard deviation. Also, users can predict
#' new data with the new model.
"_PACKAGE"

#' @name blb4lm
#' @title Bag of Little Bootstrap for Linear Regression
#' @param formula The formula for linear regression
#' @param s Number of subsamples
#' @param r Times of resampling
#' @param data A data frame
#' @param parallel Whether to use parallel (TRUR/FALSE)
#' @param num_cores How many cores to use in parallel
#' @return A list of list of coefficients and sigma
#' @details
#' Split the training data into s parts and resampling r times; then fit lm model for each resamples.
#' @export

blb4lm <- function(formula, s, r, data, parallel = FALSE, num_cores = NULL){
  n <- nrow(data)
  subsamples <- subsampling(data, s)

  if (parallel == TRUE){
    plan(multiprocess, workers = num_cores)
    model_est <- future_map(subsamples, ~resampleBuild(r, n, ., formula))
  }
  else if(parallel == FALSE){
    model_est <- map(subsamples, ~resampleBuild(r, n, ., formula))
  }
  else{
    warning("parallel parameter has to be logical")
  }

  res <- list(estimates = model_est, formula = formula)
  class(res) <- "blb4lm"
  invisible(res)
}

# split the input data into s subsamples
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
  each_model <- lm_helper(formula, data = sub, weights = freqs)
  return(each_model)
}

lm_helper <- function(formula, data, weights){
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = weights)
  list(coef = blb_coef(fit), sigma = blb_sigma(fit, weights))
}

blb_coef <- function(object){
  return(object$coef)
}

blb_sigma <- function(object, freqs){
  p <- object$rank
  e_square <- (object$residuals)^2
  return(sum(e_square*freqs) / (sum(freqs) - p + 1))
}
