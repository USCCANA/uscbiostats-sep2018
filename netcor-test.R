gennet <- function(n = 4, d = .5, .diag = FALSE) {
  
  ans <- matrix(
    as.integer(runif(n*n) < d), 
    ncol = n
  )
  
  if (!.diag)
    diag(ans) <- 0L
  
  ans
  
}

statistic <- function(...) {
  
  l <- list(...)
  
  # Is this a single element list?
  if (length(l) == 1 && is.list(l[[1]]))
    l <- unlist(l[1], recursive = FALSE)
  
  l <- lapply(l, `diag<-`, NA)
  
  n <- length(l)
  m <- n*(n-1)/2
  
  v <- NULL
  for (i in 1:n)
    for (j in i:n) {
      if(i == j)
        next
      
      # The hamming distance
      # v <- c(v, n*(n-1) - sum(l[[i]] == l[[j]], na.rm = TRUE))
      v <- c(v, n*(n-1) - sum(l[[i]] != l[[j]], na.rm = TRUE))
    }
  
  # Computing scores and returning
  # max_ecount <- max(sapply(l, sum))
  
  list(
    score = mean(v)/n/(n-1),
    vals  = v
  )
  
}
