library(ranger)
set.seed(8)
data = data[c(1:10000),]
ind <- sample(nrow(data), 1000, replace = FALSE)
part_data <- data[ind,]
test_data <- data[-ind,]
set.seed(24)
freqs_int <- rmultinom(1, nrow(data), rep(1, nrow(part_data)))
freqs_frac <- freqs_int/nrow(part_data)

blb_model <- ranger(y ~x, part_data, case.weights = freqs_frac, keep.inbag=TRUE)
pred_blb <- predict(blb_model, test_data[c(1:100),], type = "se")
pred_blb$predictions
pred_blb$se
pred_result_blb <- cbind("prediction" = pred_blb$predictions, "se" = pred_blb$se)

resampled = part_data[rep(seq_len(nrow(part_data)), times = freqs_int), ]
trad_model <- ranger(y ~ x, resampled, keep.inbag = TRUE)
pred_trad <- predict(trad_model, test_data[c(1:100),], type = "se")
pred_trad$predictions
pred_trad$se
pred_result_trad <- cbind("prediction" = pred_trad$predictions, "se" = pred_trad$se)

cbind(pred_trad$predictions - pred_trad$se, pred_trad$predictions + pred_trad$se)
cbind(pred_blb$predictions - pred_blb$se, pred_blb$predictions + pred_blb$se)

library(randomForestSRC)
blb <- rfsrc(y ~x, part_data, case.wt = freqs_frac)
src_blb <- predict(blb, test_data[1,])
trad <- rfsrc(type ~ ., test, probability = TRUE)
src_trad <- predict(trad, test_spam[1,])
