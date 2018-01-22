###########################################################
###########################################################
###
### Function to generate a graph where a variables is
### plotted 
####  Directory thuis 
setwd("~/Documents/Open Universiteit/Onderzoek/R stuff/Multilevel_R");

# Directory werk
setwd("M:/Mijn documenten/Onderzoek/Methodologie/Multilevel")

getwd();

### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

workingPath <- "~/Documents/Open Universiteit/Onderzoek/R stuff/Multilevel_R" ;

###########################################################
### Function definitions
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.

safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

###########################################################
### Load packages
###########################################################

### To read SPSS datafiles
safeRequire('foreign');
### To make plots
safeRequire('ggplot2');

###########################################################
### Start
###########################################################

### Note: see also
###   http://www.ats.ucla.edu/stat/r/faq/smooths.htm
### And of course the ggplot2 manual, e.g.
###   http://docs.ggplot2.org/current/stat_summary.html

### Set working directory
setwd(workingPath);

### Load data
dat <- read.spss('test growth analysis.sav', to.data.frame = TRUE);

### Make clear that Groep is a factor
dat$Groep <- factor(dat$Groep);

### Run regression model
dat.model <- lm(EW ~ poly(dag, 2) + Groep + Groep:poly(dag, 2), data=dat);
dat.model <- lme(EW ~ meting+dag, data=dat, random= ~ 1|respondent, method = "ML")

### Note that R automatically dummy codes for Groep
### (first group is reference point)
summary(dat.model);

### Store predicted values
dat$predictions <- dat.model$fitted.values;

### Generate basic plot with the data (we define width at .4
### for the errorbars). Use the black and white theme.
plot <- ggplot(dat, aes(x = dag, y = EW, predictions <- predictions,
                        group = Groep, colour=Groep, width=.2)) +
  theme_bw();

### Add dots for each datapoint, half transparent
plot <- plot + geom_point(alpha=.5);

### Add bigger point for means
plot <- plot + stat_summary(fun.y="mean", geom="point", size=4);

### Add error bars
plot <- plot + stat_summary(fun.data="mean_cl_boot", geom="errorbar", size=1);

### Add lines between means
plot <- plot + stat_summary(fun.y="mean", geom="line", size=1, linetype="dashed", alpha=.5);

### Add regression lines;
### first one uses 'manual' predictions that were
### computer and stored above;
### second and third ones let ggplot2 do it. Setting the alpha only
### sets the alpha (transparency) of the confidence intervals, so
### the second layer here draws thicker lines over the confidence
### interval ribbons.
#plot <- plot + stat_summary(aes(y = predictions), fun.y="mean", geom="line", size=1);
plot <- plot + stat_smooth(method="lm", formula=y ~ poly(x, 2), aes(fill=Groep), alpha=.2);
plot <- plot + stat_smooth(method="lm", formula=y ~ poly(x, 2), aes(fill=Groep), alpha=0, size=1);

### Adding a vertical line
plot <- plot + geom_vline(data=data.frame(x = 2.2), aes(xintercept=x), linetype="dashed");

### Adding some text to the vertical line
plot <- plot + geom_text(data=data.frame(x=2.3, y=6.2, label="text here", Groep=NA),
                         mapping=aes(x = x, y = y, label = label),
                         hjust=0, colour="black");

### Show plot
plot;


