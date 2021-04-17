###############################################################################################
################################# SEM Example #################################################
###############################################################################################

# R version 4.0 and above 

# Packages 

if(!require(lavaan)) install.packages("lavaan", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(semTools)) install.packages("semTools", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(MVN)) install.packages("MVN", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(faoutlier)) install.packages("faoutlier", dependencies = TRUE, repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", dependencies = TRUE, repos = "http://cran.us.r-project.org")

library(lavaan) # SEM analysis - https://lavaan.ugent.be/
library(semTools) # SEM graphs and reliability 

library(haven) # importing data
library(tidyverse) # data manipulation 

library(MVN) # normality checks 
library(faoutlier) # outlier checks 
library(car) # multicolineality and errors check 


# Load data

url_data <- "https://github.com/luistorresr/SEM_example/raw/main/data_example_spss.sav"

data <- read_sav(url_data)

rm(url_data)

# inspect the data

dim(data) # number of rows and columns 
names(data) # variable name
head(data, n=10) # summary of the first ten rows 


############################ Running SEM ######################################

# STEP 1 - Measurement models
    ## Variable 1 (x1): CSR 
    ## Variable 2 (x2): Overall justice 
    ## Variable 3 (y1:y4): Gender strategies


## Variable 1 (x1): CSR 

csrdata <- data %>% select(CSR1rev:CSR30)

### Model specification 

mm.CSR <- 'prot =~ CSR2rev + CSR3rev + CSR4rev
              comp =~ CSR6 + CSR7 + CSR8 + CSR9 + CSR10 + CSR16 + CSR14 
              cap =~  CSR11 + CSR21 + CSR17
              emb =~  CSR18 + CSR19 + CSR20 
              stra =~ CSR22 + CSR23 + CSR24 + CSR25 + CSR26 + CSR15 + CSR13
              tran =~  CSR27 + CSR28 + CSR29 + CSR30
              CSR =~ prot + comp + cap + emb + stra + tran'


### Basic checks before fitting the model

csrdata %>% lavCor(ordered = names(csrdata), 
                  missing = "listwise", ov.names.x = NULL, se = "none", 
                  estimator = "two.step", output = "cor") # Polychoric correlation

gcd <- csrdata %>% gCD(mm.CSR) # check for model influential outliers
plot(gcd, y = NULL, main = "Generalized Cook Distance", type = c("p", "h"), ylab = "gCD")

csrdata <- csrdata[-c(33, 4),] # we may decide to remove these two cases, but we will not


### Fitting the model - CFA for ordinal data with no normal distribution 

mm.CSR.fit <- lavaan::cfa(mm.CSR, data = csrdata, 
                  ordered = names(csrdata), 
                  estimator = "ULSMV", mimic = "Mplus")

### Model fit 

summary(mm.CSR.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(mm.CSR.fit)
options(digits = 2)

modindices(mm.CSR.fit) %>% arrange(desc(mi))


## Variable 2 (x2): Overall justice 

jdata <- data %>% select(Justice1, Justice2, Justice3)

### Model specification 

mm.justice <- 'justice =~ Justice1 + Justice2 + Justice3'


### Basic checks before fitting the model

jdata %>% lavCor(ordered = names(jdata), 
                   missing = "listwise", ov.names.x = NULL, se = "none", 
                   estimator = "two.step", output = "cor") # Polychoric correlation

gcd2 <- jdata %>% gCD(mm.justice) # check for model influential outliers
plot(gcd2, y = NULL, main = "Generalized Cook Distance", type = c("p", "h"), ylab = "gCD")


### Fitting the model - CFA for ordinal data with no normal distribution 

mm.justice.fit <- lavaan::cfa(mm.justice, data = jdata, 
                      ordered = c("Justice1", "Justice2", "Justice3"), 
                      estimator = "ULSMV", mimic = "Mplus")

### Model fit

summary(mm.justice.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(mm.justice.fit) # reliability analysis 


## Variable 3 (y1:y4): Gender strategies

gdata <- data %>% select(Gender1:Gender20)


### model specification 

mm.gender <- ' train =~ Gender1 + Gender2 + Gender3
                div =~ Gender4 + Gender5 + Gender6
                opp =~ Gender7 + Gender8 + Gender9 + Gender10 + Gender11 + Gender12
                inf =~ Gender13 + Gender14 + Gender15 + Gender16 + Gender17 + Gender18'

### Basic checks before fitting the model

gdata %>% lavCor(ordered = names(gdata), 
                 missing = "listwise", ov.names.x = NULL, se = "none", 
                 estimator = "two.step", output = "cor") # Polychoric correlation

gcd3 <- gdata %>% gCD(mm.gender) # check for model influential outliers
plot(gcd3, y = NULL, main = "Generalized Cook Distance", type = c("p", "h"), ylab = "gCD")



### Fitting the model - CFA for ordinal data with no normal distribution 

mm.gender.fit <- lavaan::cfa(mm.gender, data = gdata, 
                     ordered = names(gdata), 
                     estimator = "ULSMV", group = NULL, mimic = "Mplus")

### Model fit

summary(mm.gender.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

reliability(mm.gender.fit)

modindices(mm.gender.fit) %>%  arrange(desc(mi))


########## End step 1 ############


# STEP 2 - Structural model (or path analysis)

## extract factor scores from our fitted models

csrpred <- lavPredict(mm.CSR.fit, type = "lv", newdata = NULL, method = "EBM", label = TRUE, optim.method = "nlminb")
gpred <- lavPredict(mm.gender.fit, type = "lv", newdata = NULL, method = "EBM", label = TRUE, optim.method = "nlminb")
jpred <- lavPredict(mm.justice.fit, type = "lv", newdata = NULL, method = "EBM", label = TRUE, optim.method = "nlminb")

pred <- cbind(csrpred, gpred, jpred) %>% as_tibble(.)

pred <- pred %>% select(CSR, justice, train, div, opp, inf)


## Model specification 

fullmodel <- '

  # regressions for direct effect 

      train ~ CSR
      div ~ CSR
      opp ~ CSR
      inf ~ CSR

  # regressions for the indirect effects

      train ~ justice
      div ~ justice
      opp ~ justice
      inf ~ justice
      justice ~ CSR
'

## Check for assumptions 

pred %>% lavCor(missing = "listwise", ov.names.x = NULL, se = "none", 
                estimator = "two.step", output = "cor") # Pearson correlation

gcd3 <- pred %>% gCD(fullmodel) # check for model influential outliers
plot(gcd3, y = NULL, main = "Generalized Cook Distance", type = c("p", "h"), ylab = "gCD")


#### VIF (variance inflation factors)

vif(lm(train ~ CSR + justice, data = pred))
vif(lm(div ~ CSR + justice, data = pred))
vif(lm(opp ~ CSR + justice, data = pred))
vif(lm(inf ~ CSR + justice, data = pred))

### Test for Autocorrelated Errors

durbinWatsonTest(lm(train ~ CSR + justice, data = pred)) 
durbinWatsonTest(lm(div ~ CSR + justice, data = pred)) 
durbinWatsonTest(lm(opp ~ CSR + justice, data = pred)) 
durbinWatsonTest(lm(inf ~ CSR + justice, data = pred)) 

## fit the model

fullmodel.fit <- lavaan::sem(fullmodel, data = pred, estimator = "MLR", group = NULL, meanstructure=TRUE)

## model fit

summary(fullmodel.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
standardizedSolution(fullmodel.fit, type="std.all")


