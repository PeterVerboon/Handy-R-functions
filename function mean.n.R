

## function similar to SPSS mean function

mean.n   <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)
}