################################################################################
# effect.size (x)
# 
# Description: Compute effect sizes eta^2, partial eta^2 and omega^2 for aov 
#              and lm models
#
# Input: 
#    1) x: object of type lm or aov
#
# Output: a dataframe with the ANOVA table containing the usual suspects 
#         + eta^2, partial eta^2 and omega^2
#
# Gjalt-Jorn Peters, 2016
#
################################################################################

effect.size <- function (x) {
  if (class(x)[1]=="aov") {
    s=summary(x)
    ss=s[[1]]$`Sum Sq`
    df=s[[1]]$Df
    ms=s[[1]]$`Mean Sq`
    tab=s[[1]]
  }
  else if (class(x)[1]=="lm") {
    s=anova(x)
    ss=s$`Sum Sq`
    df=s$Df
    ms=s$`Mean Sq`
    tab=s
  }
  eta2=ss/sum(ss)
  partial_eta2=ss/(ss+ss[length(ss)])
  omega2=(ss-df*ms[length(ms)])/(sum(ss)+ms[length(ms)]) 
  new.table=cbind(tab, eta2, partial_eta2, omega2)
  return(new.table)
}