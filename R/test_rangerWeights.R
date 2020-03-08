library(ranger)
library(kernlab)  # for the data spam
data(spam)
set.seed(8)
ind <- sample(nrow(spam), 1000, replace = FALSE)
part_spam <- spam[ind,]
test_spam <- spam[-ind,]
set.seed(24)
freqs_int <- rmultinom(1, nrow(spam), rep(1, nrow(part_spam)))
freqs_frac <- freqs_int/nrow(part_spam)

blb_model <- ranger(type ~ ., part_spam, probability = TRUE, case.weights = freqs_frac)
pred_blb <- predict(blb_model, test_spam[1,])
pred_blb$predictions

#test = cbind(part_spam, freqs_int)
test = part_spam[rep(seq_len(nrow(part_spam)), times = freqs_int), ]
traditional_model <- ranger(type ~ ., test, probability = TRUE)
pred_trad <- predict(traditional_model, test_spam[1,])
pred_trad$predictions

# library(randomForestSRC)
# blb <- rfsrc(type ~ ., part_spam, probability = TRUE, case.wt = freqs_frac)
# src_blb <- predict(blb, test_spam[1,])
# trad <- rfsrc(type ~ ., test, probability = TRUE)
# src_trad <- predict(trad, test_spam[1,])
