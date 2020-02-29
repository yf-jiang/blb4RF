# parameters

# r -- number of resamples
# s -- number of subsamples
# b == n^gamma -- size of each subsample
# data -- should data be an argument?

non_missing <- data[!is.na(data)]
n <- length(non_missing)
s <- n/b

# subsampling
sub_seqential <- function(s, b, non_missing){
  n_non_missing <- nrow(non_missing)
  index <- sample(n_non_missing, n_non_missing, replace = FALSE)
  cat(index)
  # subsamples <- list()
  # for(i in seq_len(s)){
  #   subsamples[[i]] <- non_missing[index[1:b],]
  #   index <- index[-c(1:b)]
  # }
  # *** needs to be rewritten in functional programming due to the inefficiency of using OOP

  subsamples <- seq_len(s) %>% map(function(i){
    non_missing[index[1:b],]
    index <- index[-c(1:b)]
  })
  subsamples
}

resampling <- function(r, b, n, subsamples){
  freqs <- rmultinom(1, n, rep(1, length(non_missing_data)))
  # implement RF
}

sub_parallel <- function(cl, n, data){
  library(parallel)
  non_missing <- data[!is.na(data)]
  clusterExport(cl, data)

  invisible(clusterEvalQ(cl, {
    library(tidyverse)
    NULL
  }))

  sub_sample <- parLapplyLB(cl, data, function(i){
    freqs <- rmultinom(1, n, rep(1, length(non_missing_data)))

    # based on freqs, how to extract the rows selected? Rcpp?
    resample <- matrix()
    for(i in seq_len(length(non_missing))){
      for(j in freqs[i,]){
        rbind(resampled, non_missing[i,])
      }

    }


    resample <- non_missing %>% map(function(i){

    })
    data[freqs, ]
    # implement RF
  })
}
