# parameters
# r -- number of resamples
# n -- sample size
# subs -- subsamples returned from subsampling function

blb4lm <- function(formula, s, r, data){
  # if(is.na(data) == TRUE){
  #   message("NA are present in the data and it's removed")
  #   data <- data[!is.na(data)]
  #   n <- nrow(non_missing)
  # }

  n <- nrow(data)
  subsamples <- subsampling(data, s)
  model_est <- map(subsamples, ~resampleBuild(r, n, ., formula))

  res <- list(estimates = model_est, formula = formula)
  class(res) <- "blb4lm"
  invisible(res)
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

print.blb4lm <- function(object, ...) {
  print(object)
}

coef.blb4lm <- function(object, confidence, level){
  coef_eachSub <- object$estimate %>% map(get_coef)
  coef_result <- coef_eachSub %>% map(coef_res) %>% reduce(`+`) / length(coef_eachSub)

  if(confidence == TRUE){
    quantile_eachSub <- coef_eachSub %>% map(., ~coef_quant(., level))
    coef_confint <- quantile_eachSub %>% reduce(`+`) / length(quantile_eachSub)
    res <- list(model_coefficients = coef_result, coefficients_confidence_interval = coef_confint)
    return(res)
  }
  else{
    return(coef_result)
  }
}

get_coef <- function(l){
  map(l, function(x){x$coef}) %>% reduce(cbind)
}

coef_quant <- function(u, level){
  q1 <- (1 - level)/2
  q2 <- (1 + level)/2
  apply(u, 1, function(i){quantile(i, c(q1, q2))})
}

coef_res <- function(l){
  apply(l, 1, function(i){mean(i)})
}


# test
library(tidyverse)
n <- 1e6

test <- blb4lm(y~x, 100, 10, part1)
t = seq_len(length(test$estimates)) %>% map(function(i) {test$estimates[[i]]})
t1 = seq_len(length(t))

func_conf <- function(l){
  map(l, function(x){x$coef}) %>% reduce(cbind)
}

func_sigma <- function(l){
  map_dbl(l, function(x){x$sigma}) %>% reduce(`+`)/length(l)
}

c = test$estimates %>% map(func_conf)
conf <- function(u){
  apply(u, 1, function(i){quantile(i, c(0.025, 0.975))})
}
c %>% map(conf)
sig = test$estimates %>% map(func_sigma)
