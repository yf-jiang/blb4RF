# parameters
# r --
# b --
# data -- should data be an argument?



# subsample
sub_seqential <- function(n, data){
  non_missing <- data[!is.na(data)]
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
