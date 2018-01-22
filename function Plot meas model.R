
## Function to plot measurement model 
## Gjalt-Jorn Peters, 2016


require(userfriendlyscience)
require(semPlot)

plotSimpleMeasurementModel <- function(constructName, items,
                                       residuals = FALSE,
                                       cardinal = TRUE,
                                       rotation = 4,
                                       edge.color = "#000000",
                                       sizeMan = 14, sizeMan2 = 8,
                                       sizeLat = 18, sizeLat2 = 12,
                                       esize = 4,
                                       asize = 6,
                                       border.width = 4,
                                       label.prop = .8,
                                       mar = c(3,5,3,8),
                                       ...) {
  semPlotModelObject <-
    semPlotModel(paste('depVar =~',
                       paste0(paste0('v', 1:length(items)),
                              collapse = " + ")));
  return(semPaths(semPlotModelObject,
                  residuals=residuals,
                  cardinal = cardinal,
                  rotation = rotation,
                  edge.color = edge.color,
                  sizeMan = sizeMan, sizeMan2 = sizeMan2,
                  sizeLat = sizeLat, sizeLat2 = sizeLat2,
                  esize = esize, asize = asize,
                  border.width = border.width,
                  label.prop = label.prop,
                  mar = mar,
                  nodeLabels = c(items, constructName)));
}

# esize: the lines
# asize: the arrows
# border.width: the boxes
# sizeMan: length of boxes
# sizeMan2: width of boxes
# sizeLat: length of oval
# sizeLat2: width of oval
# label.prop: proportion of of space of label in box/oval


plotSimpleMeasurementModel(constructName = 'Statistiekangst',
                           items = c("ik ben bang voor \nhet vak statistiek   ",
                                     "cursussen statistiek \nvermijd ik graag",
                                     "statistiektentamens \nfrustreren mij",
                                     "ik word onzeker bij \nstatistische problemen",
                                     "ik ben niet graag \nbezig met statistiek "
                                     ),sizeLat=18, sizeLat2=12, sizeMan=22, sizeMan2=8, esize=2, asize=4, border.width = 2,label.prop = .7);


