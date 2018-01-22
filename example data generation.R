### For simDataSet and massConvertToNumeric
require('userfriendlyscience');

### If local version of simDataSet is used, load these two packages;
### MASS for generating multivariately normal data
#safeRequire('MASS');
### And sclaes for rescaling data
#safeRequire('scales');

### For exporting to SPSS's native format, .sav
safeRequire('rio');

### Directory to write datafiles to
workingPath <- 'B:/Data/teaching/OU/Courses/Inleiding Data Analyse (PB0202)/data';

#####################################################################
### Studie 0
#####################################################################

### (op basis van de echte datafile)

#####################################################################
### Studie 1
#####################################################################

studie1.dat <-
  simDataSet(274, varNames=c('age', 'sex',
                             'idaInvestment', 'educationLevel',
                             'needForCognition',
                             'statisticalSelfEfficacy'),
             means=c(40, 0, 10, 0,  40, 2),
             sds=c(10, 1,  3, 1, 15, 1),
             specifiedCorrelations=list(c('sex', 'age', 0.03),
                                        c('sex', 'educationLevel', -.04),
                                        c('age', 'educationLevel', -.04),
                                        c('age', 'needForCognition', 0.02),
                                        c('idaInvestment', 'educationLevel', -.46),
                                        c('idaInvestment', 'needForCognition', .52),
                                        c('idaInvestment', 'statisticalSelfEfficacy', .33),
                                        c('needForCognition', 'statisticalSelfEfficacy', .61)),
             ranges = list(age = c(18, 60), idaInvestment = c(3,30),
                           needForCognition = c(1*15, 5*15), statisticalSelfEfficacy = c(1, 5)),
             factors=c("sex", "educationLevel"),
             cuts=list(c(0), c(-.5, .5)),
             labels=list(c('female', 'male'), c('lower', 'middle', 'higher')),
             silent=FALSE);

### Adding participant id
studie1.dat <- cbind(data.frame(id = 1:nrow(studie1.dat)), studie1.dat);

### Round age
studie1.dat$age <- round(studie1.dat$age);

### Round study investment (specified in hours)
studie1.dat$idaInvestment <- round(studie1.dat$idaInvestment);

### Convert need for cognition to a mean on the basis of
### the rounded sum to get plausible values
studie1.dat$needForCognition <- round(studie1.dat$needForCognition) / 15;

### Inspect final dataset
summary(studie1.dat);
round(cor(massConvertToNumeric(studie1.dat)), 2);
 
### Export native SPSS .sav file
export(studie1.dat, file=file.path(workingPath, "studie1.sav"));

### Export comma separated values file plus syntax for importing into SPSS
exportToSPSS(studie1.dat,
             datafile=file.path(workingPath, "studie1.csv"),
             codefile=file.path(workingPath, "studie1.sps"));

#####################################################################
### Studie 2
#####################################################################



#####################################################################
### Studie 3
#####################################################################

studie3.dat <- simDataSet(494, varNames=c('age',
                                          'sex',
                                          'education',
                                          'lifeEvents',
                                          'problembasedCoping',
                                          'emotionalCoping',
                                          'resilience',
                                          'depressionSymptoms'),
                          means = c(40,
                                    0,
                                    0,
                                    5,
                                    3.5,
                                    3.5,
                                    3.5,
                                    3.5),
                          sds = c(10,
                                  1,
                                  1,
                                  1.5,
                                  1.5,
                                  1.5,
                                  1.5,
                                  1.5),
                          specifiedCorrelations =
                            list(c('problembasedCoping', 'emotionalCoping', -.5),
                                 c('problembasedCoping', 'resilience', .5),
                                 c('problembasedCoping', 'depressionSymptoms', -.4),
                                 c('depressionSymptoms', 'emotionalCoping', .6),
                                 c('depressionSymptoms', 'resilience', -.3)),
                          ranges = list(age = c(18, 54),
                                        lifeEvents = c(0,8),
                                        problembasedCoping = c(1, 7),
                                        emotionalCoping = c(1, 7),
                                        resilience = c(1, 7),
                                        depressionSymptoms = c(1, 7)),
                          factors=c("sex", "education"),
                          cuts=list(c(0),
                                    c(-.5, .5)),
                          labels=list(c('female', 'male'),
                                      c('lower', 'middle', 'higher')),
                          silent=FALSE);

### Export native SPSS .sav file
export(studie3.dat, file=file.path(workingPath, "studie3.sav"));

### Export comma separated values file plus syntax for importing into SPSS
exportToSPSS(studie3.dat,
             datafile=file.path(workingPath, "studie3.csv"),
             codefile=file.path(workingPath, "studie3.sps"));


#####################################################################
### Studie 4
#####################################################################
