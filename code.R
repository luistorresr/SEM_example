###############################################################################################
################################# SEM Example #################################################
###############################################################################################

# R version 4.0 and above 

# Packages 

library(lavaan) # SEM analysis 

library(semTools) # SEM graphs and reliability 
library(MVN) # normality checks 
library(faoutlier) # outlier checks 
library(car) # multicolineality and errors check 


# Load data




## Inspect data




############################ Running SEM ######################################

# STEP 1 - Measurement models
    ## Variable 1 (x1): CSR 
    ## Variable 2 (x2): Overall justice 
    ## Variable 3 (y1:y4): Gender strategies


## Variable 1 (x1): CSR 

### model specification 

mm.CSR <- 'self_protecting =~ CSR1rev + CSR2rev + CSR3rev + CSR4rev
            compliance =~ CSR5 + CSR6 + CSR7 + CSR8 + CSR9 + CSR10
            capability =~  CSR11 + CSR12 + CSR13 + CSR14 + CSR15 + CSR16
            caring =~ CSR17 + CSR18 + CSR19 + CSR20 + CSR21
            strategizing =~ CSR22 + CSR23 + CSR24 + CSR25 + CSR26
            transforming =~ CSR27 + CSR28 + CSR29 + CSR30 
            CSR =~ self_protecting + compliance + capability + caring + strategizing + transforming'

### Basic checks before fitting the model

uniNorm(data.frame(Surveydata$CSR1rev, Surveydata$CSR2rev, Surveydata$CSR3rev, Surveydata$CSR4rev, Surveydata$CSR5, Surveydata$CSR6, 
                   Surveydata$CSR7, Surveydata$CSR8, Surveydata$CSR9, Surveydata$CSR10, Surveydata$CSR11, Surveydata$CSR12, 
                   Surveydata$CSR13, Surveydata$CSR14, Surveydata$CSR15, Surveydata$CSR16, Surveydata$CSR17, Surveydata$CSR18, 
                   Surveydata$CSR19, Surveydata$CSR20, Surveydata$CSR21, Surveydata$CSR22, Surveydata$CSR23, Surveydata$CSR24, 
                   Surveydata$CSR25, Surveydata$CSR26, Surveydata$CSR27, Surveydata$CSR28, Surveydata$CSR29, Surveydata$CSR30), 
        type = c("Lillie"), desc = TRUE) # Lillie for Kolgomorov-Smirnov - univariate normality 

mardiaTest(data.frame(Surveydata$CSR1rev, Surveydata$CSR2rev, Surveydata$CSR3rev, Surveydata$CSR4rev, Surveydata$CSR5, Surveydata$CSR6, 
                      Surveydata$CSR7, Surveydata$CSR8, Surveydata$CSR9, Surveydata$CSR10, Surveydata$CSR11, Surveydata$CSR12, 
                      Surveydata$CSR13, Surveydata$CSR14, Surveydata$CSR15, Surveydata$CSR16, Surveydata$CSR17, Surveydata$CSR18, 
                      Surveydata$CSR19, Surveydata$CSR20, Surveydata$CSR21, Surveydata$CSR22, Surveydata$CSR23, Surveydata$CSR24, 
                      Surveydata$CSR25, Surveydata$CSR26, Surveydata$CSR27, Surveydata$CSR28, Surveydata$CSR29, Surveydata$CSR30)) #### multivariate normality

gCD(Surveydata, mm.CSR) # check for model influential outliers


### Fitting the model - CFA for ordinal data with no normal distribution 

mm.CSR.fit <- cfa(mm.CSR, data = Surveydata, 
                  ordered = c("CSR1rev", "CSR2rev", "CSR3rev", "CSR4rev", "CSR5", "CSR6", "CSR7", "CSR8", "CSR9", "CSR10", 
                              "CSR11", "CSR12", "CSR13", "CSR14", "CSR15", "CSR16", "CSR17", "CSR18", "CSR19", "CSR20", 
                              "CSR21", "CSR22", "CSR23", "CSR24", "CSR25", "CSR26", "CSR27", "CSR28", "CSR29", "CSR30"), 
                  estimator = "ULSMV", mimic = "Mplus")

### Model fit 

summary(mm.CSR.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

reliability(mm.CSR.fit)


## Variable 2 (x2): Overall justice 

### Model specification 

mm.justice <- 'justice =~ Justice1 + Justice2 + Justice3'


### Basic checks before fitting the model

uniNorm(data.frame(Surveydata$Justice1, Surveydata$Justice2, Surveydata$Justice3), 
        type = c("Lillie"), desc = TRUE) # Lillie for Kolgomorov-Smirnov for univariate normality 

mardiaTest(data.frame(Surveydata$Justice1, Surveydata$Justice2, Surveydata$Justice3)) # multivariate normality 

gCD(Surveydata, mmi.justice) # Outliers

### Fitting the model - CFA for ordinal data with no normal distribution 

mm.justice.fit <- cfa(mm.justice, data = Surveydata, 
                      ordered = c("Justice1", "Justice2", "Justice3"), 
                      estimator = "ULSMV", mimic = "Mplus")

### Model fit

summary(mm.justice.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(mm.justice.fit) # reliability analysis 


## Variable 3 (y1:y4): Gender strategies

### model specification 

mm.gender <- ' training =~ Gender1 + Gender2 + Gender3
                feminine =~ Gender4 + Gender5 + Gender6
                opportunitites =~ Gender7 + Gender8 + Gender9 + Gender10 + Gender11 + Gender12
                infraestructure =~ Gender13 + Gender14 + Gender15 + Gender16 + Gender17 + Gender18'

### Basic checks before fitting the model

uniNorm(data.frame(Surveydata$Gender1, Surveydata$Gender2, Surveydata$Gender3, Surveydata$Gender4, Surveydata$Gender5, 
                   Surveydata$Gender6, Surveydata$Gender7, Surveydata$Gender8, Surveydata$Gender9, Surveydata$Gender10, 
                   Surveydata$Gender11, Surveydata$Gender12, Surveydata$Gender13, Surveydata$Gender14, Surveydata$Gender15, 
                   Surveydata$Gender16, Surveydata$Gender17, Surveydata$Gender18), type = c("Lillie"), desc = TRUE) # Lillie for Kolgomorov-Smirnov - univariate

mardiaTest(data.frame(Surveydata$Gender1, Surveydata$Gender2, Surveydata$Gender3, Surveydata$Gender4, Surveydata$Gender5, 
                      Surveydata$Gender6, Surveydata$Gender7, Surveydata$Gender8, Surveydata$Gender9, Surveydata$Gender10, 
                      Surveydata$Gender11, Surveydata$Gender12, Surveydata$Gender13, Surveydata$Gender14, Surveydata$Gender15, 
                      Surveydata$Gender16, Surveydata$Gender17, Surveydata$Gender18)) # multivariate 

gCD(Surveydata, mmi.gender) # Outliers


### Fitting the model - CFA for ordinal data with no normal distribution 

mm.gender.fit <- cfa(mm.gender, data = Surveydata, 
                     ordered = c("Gender1", "Gender2", "Gender3", "Gender4", "Gender5", "Gender6", "Gender7", "Gender8", "Gender9", "Gender10", 
                                 "Gender11", "Gender12", "Gender13", "Gender14", "Gender15", "Gender16", "Gender17", "Gender18"), 
                     estimator = "ULSMV", group = NULL, mimic = "Mplus")

### Model fit

summary(mmi.gender.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(mmi.gender.fit)
reliability(mmi.gender.fit)


########## End step 1 #############


# STEP 2 - Structural model (or path analysis)

## extract factor scores from our fitted models

lavPredict(mm.CSR.fit, type = "lv", newdata = NULL, method = "EBM", se.fit = FALSE, label = TRUE, optim.method = "nlminb")
lavPredict(mm.gender.fit, type = "lv", newdata = NULL, method = "EBM", se.fit = FALSE, label = TRUE, optim.method = "nlminb")
lavPredict(mm.justice.fit, type = "lv", newdata = NULL, method = "EBM", se.fit = FALSE, label = TRUE, optim.method = "nlminb")


## Model specification 

fullmodel <- '

  # regressions for direct effect 

      training ~ CSRdevelopment
      feminine ~ CSRdevelopment
      opportunities ~ CSRdevelopment
      infraestructure ~ CSRdevelopment

  # regressions for the indirect effects

      training ~ justice
      feminine ~ justice
      opportunities ~ justice
      infraestructure ~ justice
      justice ~ CSRdevelopment
'

## Check for assumptions 

### Normality 

uniNorm(data.frame(factorscores$CSRdevelopment, factorscores$training, factorscores$feminine, 
                   factorscores$opportunities, factorscores$infraestructure, factorscores$justice),
        type = c("Lillie"), desc = TRUE) # Lillie for Kolgomorov-Smirnov

mardiaTest(data.frame(factorscores$CSRdevelopment, factorscores$training, factorscores$feminine, 
                      factorscores$opportunities, factorscores$infraestructure, factorscores$justice))

### Outliers

gCD(factorscores, fullmodel)


### Multicolinearity 

#### Inpact the correlations matrix
sm.scores.corr <- lavCor(data.frame(factorscores$CSRdevelopment, factorscores$training, factorscores$feminine, 
                                    factorscores$opportunities, factorscores$infraestructure, factorscores$justice), 
                         zero.keep.margins = TRUE, group = NULL, 
                         missing = "listwise", ov.names.x = NULL, se = "none", 
                         estimator = "two.step", output = "cor")

rcorr(as.matrix(data.frame(factorscores$CSRdevelopment, factorscores$training, factorscores$feminine, 
                           factorscores$opportunities, factorscores$infraestructure, factorscores$justice)), 
      type="pearson") # significance level


#### VIF (variance inflation factors)

vif(lm(training ~ CSRdevelopment + justice, data = factorscores))
vif(lm(feminine ~ CSRdevelopment + justice, data = factorscores))
vif(lm(opportunities ~ CSRdevelopment + justice, data = factorscores))
vif(lm(infraestructure ~ CSRdevelopment + justice, data = factorscores))

### Test for Autocorrelated Errors

durbinWatsonTest(lm(training ~ CSRdevelopment + neosexism + justice, data = factorscores)) 
durbinWatsonTest(lm(feminine ~ CSRdevelopment + neosexism + justice, data = factorscores)) 
durbinWatsonTest(lm(opportunities ~ CSRdevelopment + neosexism + justice, data = factorscores)) 
durbinWatsonTest(lm(infraestructure ~ CSRdevelopment + neosexism + justice, data = factorscores)) 

## fit the model

fullmodel.fit <- sem(fullmodel, data = factorscores, estimator = "MLR", group = NULL)

## model fit

summary(fullmodel.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(fullmodel.fit, "srmr")



