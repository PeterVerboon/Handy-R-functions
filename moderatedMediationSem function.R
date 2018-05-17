
options(digits = 2)
require(lavaan);
require(userfriendlyscience);
require(MASS);
require(dplyr)

## construct test data 

dat1 <- simDataSet(300,varNames=c("y", "x","m1","m2","m3","c1","c2","mod1","mod2"))
dat1$bimod1 <- factor((dat1$y > .1 & dat1$x > .1)*1)
dat1$bimod2 <- dat1$bimod1; levels(dat1$bimod2) <- c("Male", "Female")
dat1$x1 <- (dat1$x - 1) * 3.0
dat1$m1 <- .4*dat1$m1  + .4*dat1$x + .2*rnorm(300, mean=0, sd=1)
dat1$y1 <- .4*(dat1$m1 * dat1$mod2) + .4*dat1$m1+ .2*rnorm(300, mean=0, sd=1)

##### 
##### Function mediationSem() is build to test Mediation for one predictor and one dependent variable (as in PROCESS, see Hayes(2013))
##### Based on LAVAAN (Rosseel, 2012)
##### In funcion buildModel2() the model for input in LAVAAN is constructed
#####


buildModel2 <- function (xvar, mvars , yvar , xmmod = NULL, mymod = NULL, cmvars = NULL, cyvars = NULL) {
 
    nm <- length(mvars)
    ncm <- length(cmvars)
    ncy <- length(cyvars)
    
    a1 <- 1:length(xvar)                                               # first index predictor in x - m path (= 1 because only one predictor)
    a2 <- 1:nm                                                         # second indices mediators for x - m paths
    b2 <- 1:length(yvar)                                               # second index predictor in m - y path (= 1 because only one dependent)
    b1 <- a2                                                           # first indices mediators for m - y paths
    c1 <- 1:length(cmvars)
    c2 <- 1:length(cyvars)
    h <- as.vector(outer(1:nm, 1:ncm, paste0))
    
    
    # naming the parameters
    
    a <- paste0("a",a1,a2)                             # path from x to m
    if(length(xmmod)) {w1 <- paste0("w",a2); w2 <- paste0("im",a2);   }      # path from moderator*x to m
    b <- paste0("b",b1,b2)                             # path from m to y
    if(length(mymod)) {v1 <- paste0("v",1); v2 <- paste0("iy",a2); }      # path from moderator*m to y
    c <- paste0("c",a1,b2)                            # path from x to y
    d <- paste0("d",h)                                # path from c1 to m
    f <- paste0("f",c2,b2)                            # path from c2 to y
  
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
    modmedx <- paste0("modmedx", a2 )
    modmedm <- paste0("modmedm", a2 )
    
    modela2 <- " "          # initialize path from mod on x - m path   
    modelb2 <- " "          # initialize path from mod on m - y path
    
    modelw <- " "          # initialize path from mod on  m path   
    modelw2 <- " "          # initialize path from int on m path
    
    modelv <- " "          # initialize path from mod on  m path   
    modelv2 <- " "          # initialize path from int on m path
    
    modeli1 <- " "           # initialize indirect effects with mod x on  m path   
    modeli2 <- " "          # initialize indirect effects with mod m on y path
    
    modeld <- " "          # initialize path from c1 to m    
    modelf <- " "          # initialize path from c2 to y
    
   # construct interaction terms
    
    if(length(xmmod)) { xmint <- paste0("xmInteraction",c(1:nm)) }  
    if(length(mymod)) { myint <- paste0("myInteraction",c(1:nm)) }  
    
    
    modela1 <- paste0( mvars, " ~ " ,a,"*",xvar ,  collapse = " \n ")
    if(length(xmmod)) { modelw <- paste0( mvars, " ~ " ,w1,"*",xmmod ,  collapse = " \n ") ;
                        modelw2 <- paste0( mvars, " ~ " ,w2,"*",xmint ,  collapse = " \n ") }
    
    modelb1 <- paste0( yvar, " ~ " ,b,"*",mvars , collapse = " \n ")

    if(length(mymod)) { modelv <- paste0( yvar, " ~ " ,v1,"*",mymod , collapse = " \n ") ;
                        modelv2 <- paste0( yvar, " ~ " ,v2,"*",myint , collapse = " \n ") }
    
    modelc <- paste0( yvar, " ~ " ,c,"*",xvar , collapse = " \n ")

    if (length(cmvars)) { modeld <- paste0( rep(mvars,ncm), " ~ " ,d,"*",rep(cmvars, each=nm) , collapse = " \n ") }
    if (length(cyvars)) { modelf <- paste0( yvar, " ~ " ,f,"*",cyvars , collapse = " \n ") }

    modeli <- paste0(ind , " := " , a, " * ", b, collapse = " \n ")
    if(length(xmmod)) modeli1 <- paste0(modmedx , " := " , w2, " * ", b, collapse = " \n ")
    if(length(mymod)) modeli2 <- paste0(modmedm , " := " , v2, " * ", a, collapse = " \n ")
    
    modelt <- paste0("total"," := " , (paste0(ind,  collapse = " + ")))
    model <- paste0(modela1," \n ",modela2," \n ",
                    modelb1," \n ", modelb2," \n ",
                    modelc, " \n ", modeld, " \n ", 
                    modelw, " \n ", modelw2, " \n ",
                    modelv, " \n ", modelv2, " \n ",
                    modelf, " \n ", 
                    model_cov1," \n ", model_cov2, " \n ", 
                    modeli, " \n ", modeli1, " \n ", modeli2, " \n ",
                    modelt)
  
    return(model)
}







moderatedMediationSem <- function (dat = NULL, xvar, mvars, yvar , xmmod = NULL, mymod = NULL, cmvars = NULL, cyvars = NULL, plot=FALSE, nboot=1000) {

  options(digits = 2)
  require(lavaan);
  require(dplyr)
  
  nm <- length(mvars)

  ## check if there is a moderator for the x - m path 
  
  xdichotomous <- FALSE
  if (length(xmmod)) {
    if (is.factor(dat[,xmmod])) {  
      if (length(levels(dat[,xmmod])) > 2) { 
        stop("Moderator is factor with more than two levels")}
      else {
        xdichotomous <- TRUE
        dat[,"xmodOriginal"] <- dat[,xmmod] 
        dat[,xmmod] <- as.numeric(dat[,xmmod]) - 1} 
    }
    
      xmint <- paste0("xmInteraction",c(1:nm))
      xmterms <- paste0(paste0("dat$",xmmod,"*","dat$",mvars))  
    
      for (i in 1:nm) {
         dat[,xmint[i]] <- eval(parse(text = xmterms[i] )) }
      
  }
  
  
  ## check if there is a moderator for the m - y path 
  
  ydichotomous <- FALSE
  if (length(mymod)) {
    if (is.factor(dat[,mymod])) {  
      if (length(levels(dat[,mymod])) > 2) { 
        stop("Moderator is factor with more than two levels")}
      else {
        ydichotomous <- TRUE
        dat[,"ymodOriginal"] <- dat[,mymod] 
        dat[,mymod] <- as.numeric(dat[,mymod]) - 1} 
    }
    
    myint <- paste0("myInteraction",c(1:nm))
    myterms <- paste0(paste0("dat$",mymod,"*","dat$",mvars))  
    
    for (i in 1:nm) {
      dat[,myint[i]] <- eval(parse(text = myterms[i] ))
    }
  }
  
  model <- buildModel2(xvar=xvar, mvars= mvars, yvar = yvar, xmmod = xmmod, mymod = mymod, cmvars = cmvars, cyvars = cyvars)
  
  result <- sem(model, data=dat, fixed.x = FALSE, std.lv = TRUE, se="bootstrap",bootstrap=nboot);
  
  a2 <- 1:nm 
  ind <- paste0("ind", a2 )
  indinter <- paste0("indinter", a2 )
  
  
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
  print(filter(out, lhs %in% c(ind,indinter, "total"))[,-c(1:3)])
  
  
  cat("\n","----------------------", "\n\n")
  cat( "Standardized indirect effects", "\n\n")
  print(lavInspect(result, "std")$beta[yvar,mvars] * lavInspect(result, "std")$beta[mvars, xvar])
 
  cat("\n","----------------------", "\n\n")
  
 
  
  if (plot) {
    if (xdichotomous) {dat[,xmmod] <- dat[,"xmodOriginal"]  }
    if (ydichotomous) {dat[,mymod] <- dat[,"ymodOriginal"]  }
    plotSimSlopes(dat = dat, out=out, xvar=xvar, mvars= mvars, yvar = yvar, xmmod = xmmod, mymod = mymod, cmvars = cmvars, cyvars = cyvars);
    }
  
  
  
  return(result)
  
}

    





###### test


model <- buildModel2(xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod1")

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "mod1", plot = TRUE, nboot=50)
res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod1", plot = TRUE, nboot=50)
res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod2", plot = TRUE, nboot=50)
res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", mymod = "mod1", plot = TRUE, nboot=50)
res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", mymod = "bimod1", plot = TRUE, nboot=50)
res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", mymod = "bimod2", plot = TRUE, nboot=50)

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1",  plot = TRUE, nboot=50)

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", cmvars =c("c1","c2"), cyvars =c("c1","c2"), 
                             plot = TRUE, nboot=50)   ## equals mediationSem()

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "mod1", cmvars =c("c1","c2"), plot = TRUE, nboot=50)

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod2", cmvars =c("c1","c2"), cyvars =c("c1","c2"), 
                             plot = TRUE, nboot=50)

res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod2", mymod= "mod1", 
                             cmvars =c("c1","c2"), cyvars =c("c1","c2"), 
                             plot = TRUE, nboot=50)


summary(res, fit.measures=TRUE, rsq=TRUE, standardized = TRUE)

out <- parameterestimates(res)

