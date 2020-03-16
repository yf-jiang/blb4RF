# parameters
# formula -- the regression formula
# s -- number of subsamples
# r -- number of bootstrap resamples
# data -- "training" data
# parallel -- the option to make parallization using furrr



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

print.blb4lm <- function(object) {
  print(object)
}

# export
# function to calculate model coefficients
coef.blb4lm <- function(object, confidence, level){
  # only coefficients
  coef_eachSub <- object$estimate %>% map(get_coef)
  coef_result <- coef_eachSub %>% map(mean_coef) %>% reduce(`+`) / length(coef_eachSub)

  # with confidence interval
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

# export
# function to calculate sigma
sigma.blb4lm <- function(object, confidence, level){
  sigma_eachSub <- object$estimate %>% map(get_sigma)
  sigma_result <- sigma_eachSub %>% map(mean) %>% reduce(`+`) / length(sigma_eachSub)

  if(confidence == TRUE){
    q1 <- (1 - level)/2
    q2 <- (1 + level)/2
    sigma_confint <- sigma_eachSub %>% map(., ~quantile(., c(q1, q2))) %>% reduce(`+`) / length(sigma_eachSub)
    res <- list(Sigma = sigma_result, Sigma_Confidence_Interval = sigma_confint)
    return(res)
  }
  else{
    return(sigma_result)
  }
}

# export
# to predict y given inputs
predict.blb4lm <- function(object, new_data, confidence, level){
  model_coefs <- coef.blb4lm(object, confidence = FALSE)
  temp_predict <- apply(new_data, 1, function(d){d*model_coefs[-1]})
  y_predict <- model_coefs[1] + apply(temp_predict, 2, sum)

  if (confidence == TRUE){
    coef_confint <- coef.blb4lm(object, confidence = TRUE, level = level)$coefficients_confidence_interval
    int_intercept <- coef_confint[1,]
    int_x <- apply(coef_confint[,-1], 1, function(i){
      apply(new_data, 1, function(d){sum(d*i)})
      })
    int_y <- apply(int_x, 1, function(int){int + int_intercept})

    res <- list(response = y_predict, response_interval = int_y)
    return(res)
  }
  else{
    return(y_predict)
  }
}

# helper functions: invisible to users
get_coef <- function(l){
  map(l, function(x){x$coef}) %>% reduce(cbind)
}

coef_quant <- function(u, level){
  q1 <- (1 - level)/2
  q2 <- (1 + level)/2
  apply(u, 1, function(i){quantile(i, c(q1, q2))})
}

mean_coef <- function(l){
  apply(l, 1, function(i){mean(i)})
}

get_sigma <- function(l){
  map_dbl(l, function(x){x$sigma})
}

predic_int <- function(l, intercept_int){
  apply(l, 1, function(x){x + intercept_int})
}
# test code
library(tidyverse)
n <- 1e6

test <- blb4lm(mpg~hp+wt, 3, 100, mtcars)

test_c = coef.blb4lm(test, confidence = TRUE, level = 0.95)

sig = test$estimates %>% map(get_sigma)


new_data <- as.matrix(mtcars[c(1:5),])
new_data = new_data[,c(4, 6)]
pred <- test_c$coefficients_confidence_interval
# predict.blb4lm
temp <- test_c$coefficients_confidence_interval[,-1] %>% map(., ~.*new_data) %>% reduce(cbind)
func_predict <- function(m){



}
