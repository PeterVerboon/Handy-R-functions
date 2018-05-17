
options(digits = 2)
require(lavaan);
require(userfriendlyscience);
require(MASS);
require(dplyr)


dat1 <- simDataSet(300,varNames=c("y", "x","m1","m2","m3","c1","c2"))
dat1$x1 <- (dat1$x - 1) * 3.0
dat1$y1 <- (dat1$y * 2.0) + 1

##### 
##### Function mediationSem() is build to test Mediation for one predictor and one dependent variable (as in PROCESS, see Hayes(2013))
##### Based on LAVAAN (Rosseel, 2012)
##### In funcion buildModel() the model for input in LAVAAN is constructed
#####


buildModel <- function (xvar, mvars , yvar , cmvars = NULL, cyvars = NULL) {
 
    nm <- length(mvars)
    ncm <- length(cmvars)
    ncy <- length(cyvars)
    
    a1 <- 1:length(xvar)
    a2 <- 1:nm 
    b2 <- 1:length(yvar)
    b1 <- a2
    c1 <- 1:length(cmvars)
    c2 <- 1:length(cyvars)
    h <- as.vector(outer(1:nm, 1:ncm, paste0))
    
    
    # naming the parameters
    
    a <- paste0("a",a1,a2)        # path from x to m
    b <- paste0("b",b1,b2)        # path from m to y
    c <- paste0("c",a1,b2)        # path from x to y
    d <- paste0("d",h)            # path from c1 to m
    f <- paste0("f",c2,b2)        # path from c2 to y
  
    # construct covariances between covariates for M 
    
    model_cov1 <- " "; 
    if (length(cmvars) > 1) {
    ha <- expand.grid(cmvars,cmvars)
    hb <- matrix(data=c(1:(ncm**2)),nrow=ncm,ncol=ncm)
    s <- as.vector(lower.tri(hb))
    ha <- ha[s,]
    model_cov1 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ")
    }
    
    # construct covariances between mediators and covariates for Y
    
    model_cov2 <- " ";
    vars <- c(mvars, cyvars)
    if (length(vars) > 1) {
    nmy <- nm + ncy
    ha <- expand.grid(vars,vars)
    hb <- matrix(data=c(1:(nmy**2)),nrow=nmy,ncol=nmy)
    s <- as.vector(lower.tri(hb))
    ha <- ha[s,]
    model_cov2 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ")
    }
     
   # indirect effects 
    
    ind <- paste0("ind", a2 )
    modeld <- " "          # initialize path from c1 to m    
    modelf <- " "          # initialize path from c2 to y
    
    modela <- paste0( mvars, " ~ " ,a,"*",xvar ,  collapse = " \n ")
    modelb <- paste0( yvar, " ~ " ,b,"*",mvars , collapse = " \n ")
    modelc <- paste0( yvar, " ~ " ,c,"*",xvar , collapse = " \n ")
    if (length(cmvars)) { modeld <- paste0( rep(mvars,ncm), " ~ " ,d,"*",rep(cmvars, each=nm) , collapse = " \n ") }
    if (length(cyvars)) { modelf <- paste0( yvar, " ~ " ,f,"*",cyvars , collapse = " \n ") }
    modeli <- paste0(ind , " := " , a, " * ", b, collapse = " \n ")
    modelt <- paste0("total"," := " , (paste0(ind,  collapse = " + ")))
    model <- paste0(modela," \n ",modelb," \n ", modelc, " \n ", modeld, " \n ", modelf, " \n ", model_cov1," \n ", model_cov2, " \n ", modeli, " \n ", modelt)
  
    return(model)
}







mediationSem <- function (dat = NULL, xvar, mvars, yvar , cmvars = NULL, cyvars = NULL, nboot=1000) {

  options(digits = 2)
  require(lavaan);
  require(dplyr)
  
  model <- buildModel(xvar=xvar, mvars= mvars, yvar = yvar, cmvars = cmvars, cyvars = cyvars)
  
  result <- sem(model, data=dat, fixed.x = FALSE, std.lv = TRUE, se="bootstrap",bootstrap=nboot);
  
  a2 <- 1:length(mvars) 
  ind <- paste0("ind", a2 )
  
  out <- inspect(result, "r2")
  cat("----------------------", "\n\n")
  cat( "R square", "\n\n")
  print(out) 
 
  out <- parameterestimates(result)
  cat("\n","----------------------", "\n\n")
  cat( "Direct effect", "\n\n")
  print(filter(out, lhs %in% yvar & rhs %in% xvar)[,-c(1:3)])
  
  
  cat("\n","----------------------", "\n\n")
  cat( "Indirect effects", "\n\n")
  print(filter(out, lhs %in% c(ind, "total"))[,-c(1:3)])
  
  
  cat("\n","----------------------", "\n\n")
  cat( "Standardized indirect effects", "\n\n")
  print(lavInspect(result, "std")$beta[yvar,mvars] * lavInspect(result, "std")$beta[mvars, xvar])
 
  cat("\n","----------------------", "\n\n")
  
   
  
  return(result)
  
}






###### test



res <- mediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", cmvars = c("c1","c2") , cyvars = c("c1","c2"), nboot=50)

summary(res, fit.measures=TRUE, rsq=TRUE, standardized = TRUE)



