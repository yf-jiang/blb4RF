#' @name print.blb4lm
#' @title Print the formula of blb4lm
#' @param x The output from the function blb4lm
#' @param ... Additional arguments to be passed to other functions
#' @return The formula of linear regression used in blb4lm
#' @export

# function to print basic information of a blb4lm object
print.blb4lm <- function(x, ...) {
  if (class(x) == "blb4lm"){
    s <- length(x$estimates)
    r <- length(x$estimates[[1]])
    cat("class: blb4lm", sep = "\n")
    cat("regression formula: ")
    print(x$formula)
    cat("number of subsamples:", s, "\n")
    cat("number of bootstrap times for each subsample:", r, "\n")
  }else{
    warning("The object wasn't built by blb4lm")
  }

}

utils::globalVariables(".")

#' @name coef.blb4lm
#' @title Calculating coefficients from blb4lm
#' @param object The output from the function blb4lm
#' @param confidence Whether the user needs a confidence interval (TRUE/FALSE). Default is 'FALSE'.
#' @param level The level of significance. Default is 0.95.
#' @param ... Additional arguments to be passed to other functions
#' @return The coeffiecients, or the confidence intervals of the coefficients
#' @export

# function to calculate model coefficients
coef.blb4lm <- function(object, confidence = FALSE, level = 0.95, ...){
  if (is.logical(confidence) != TRUE) {
    warning("the type of parameter confidence has to be logical")
  }

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

#' @name sigma.blb4lm
#' @title Calculating sigma from blb4lm
#' @param object The output from the function blb4lm
#' @param confidence Whether the user needs a confidence interval (TRUE/FALSE). Default is 'FALSE'.
#' @param level The level of significance. Default is 0.95.
#' @param ... Additional arguments to be passed to other functions
#' @return sigma, or the confidence intervals of the sigma
#' @export

# function to calculate sigma
sigma.blb4lm <- function(object, confidence = FALSE, level = 0.95,...){

  if (is.logical(confidence) != TRUE) {
    warning("the type of parameter confidence has to be logical")
  }

  sigma_eachSub <- object$estimate %>% map(get_sigma)
  sigma_result <- sigma_eachSub %>% map(mean) %>% reduce(`+`) / length(sigma_eachSub)

  if(confidence == TRUE){
    q1 <- (1 - level)/2
    q2 <- (1 + level)/2
    sigma_confint <- sigma_eachSub %>% map(., ~quantile(., c(q1, q2))) %>% reduce(`+`) /
      length(sigma_eachSub)
    res <- list(Sigma = sigma_result, Sigma_Confidence_Interval = sigma_confint)
    return(res)
  }
  else{
    return(sigma_result)
  }
}

#' @name predict.blb4lm
#' @title Predict for new data after doing blb4lm
#' @param object The output from the function blb4lm
#' @param new_data A data frame
#' @param confidence Whether the user needs a confidence interval (TRUE/FALSE). Default is 'FALSE'.
#' @param level The level of significance. Default is 0.95.
#' @param ... Additional arguments to be passed to other functions
#' @return sigma, or the confidence intervals of the sigma
#' @export

# function to predict y given inputs
predict.blb4lm <- function(object, new_data, confidence = FALSE, level = 0.95, ...){
  # model.matrix for future calculation
  new_data_matrix <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)

  if (confidence == TRUE){
    temp <- coef.blb4lm(object, confidence = TRUE, level = level)
    model_coefs <- temp$model_coefficients
    coef_confint <- temp$coefficients_confidence_interval

    y_predict <- apply(new_data_matrix, 1, function(d){sum(d*model_coefs)})

    int_y <- apply(coef_confint, 1, function(i){
      apply(new_data_matrix, 1, function(d){sum(d*i)})
    })

    res <- list(response = y_predict, response_interval = int_y)
    return(res)
  }
  else if (confidence == FALSE){
    model_coefs <- coef.blb4lm(object, confidence = FALSE)
    y_predict <- apply(new_data_matrix, 1, function(d){sum(d*model_coefs)})
    return(y_predict)
  }
  else{
    warning("The parameter confidence can only take logical value")
  }
}

# helper functions called in user functions above: invisible to users
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
