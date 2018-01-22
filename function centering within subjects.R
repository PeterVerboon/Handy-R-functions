

## Function to center variables (varnames) within groups (subjnr) 
## Within subject means can be removed (drop=TRUE)
## Varnames is a character vector with the variable names to be centered


subj.center <- function(data, varnames, subjnr, rmMEAN = FALSE ) {
  
  personmeans <- aggregate(data[,varnames], by=list(data[,subjnr]), mean, na.rm=TRUE) 
  
  names(personmeans)<-c(subjnr, paste0(varnames, ".mean"))
  
  data <- merge(data, personmeans, by=subjnr) 
  
  data[,paste0(varnames, ".cnt")] <- data[,varnames] - data[,paste0(varnames, ".mean")]  
  
  if (rmMEAN) data[,paste0(varnames, ".mean")]  <- NULL
  
  return(data)
  
}   # END FUNCTION
 




## test

subnum <- rep((1:3), each=4)
x1 <- c(1:12)
x2 <- rep((1:4), each=3)
x3 <- c(1:5,1:7)
dat <- data.frame(cbind(subnum,x1,x2,x3))

vnames <- c("x1","x2","x3")

subj.center(dat, vnames, subjnr = "subnum", rmMEAN = TRUE)




