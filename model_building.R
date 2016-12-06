###########################################
###                                     ###
###             Model Building          ###
###                                     ###
###########################################

### Removing influential Obs
donor <- subset(donor, TARGDOL <= 100)

### Taking Test and Training Data set
training <- donor[donor$ID %% 2 == 0 ,]
test <- donor[donor$ID %% 2 != 0 , ]

# Creating a fit
fit = glm(DONATED ~ MONTHS12 + MONTHS6, family=binomial, data = donor)
summary(fit)
fit = glm(DONATED ~ CNCOD1 , family=binomial, data = donor) # The more recent a person has donated, the more likely to donate
fit = glm(DONATED ~ CNTMLIF , family=binomial, data = donor) # The more times donated in lifetime, the more likely to donate
fit = glm(DONATED ~ CNTRLIF , family=binomial, data = donor) # The more they have dontated, the more likely to donate
fit = glm(DONATED ~ CNMONL , family=binomial, data = donor) # The longer they have been a donor, the more likely to donate
fit = glm(DONATED ~ SEX , family=binomial, data = donor) # Married couples most likely to donate
fit = glm(DONATED ~ MONTHS12 + MONTHS6 + CNDAT1 + CNTRLIF + CONLARG + CNMON1 + CNTMLIF + cadence1 + CNMONF, family=binomial, data = training)

summary(fit)

### Testing a few cases on the model
testcaselow = data.frame(MONTHS12=0, MONTHS6=0, CNCOD1=5, CNTMLIF=1, CNTRLIF=5, CNMONF=180)
predict(fit,newdata=testcaselow,type="response") # Calculated probability that this person will donate

testcasemedium = data.frame(MONTHS12=1, MONTHS6=0, CNCOD1=10, CNTMLIF=3, CNTRLIF=30, CNMONF=36)
predict(fit,newdata=testcasemedium,type="response")

testcasehigh = data.frame(MONTHS12=0, MONTHS6=1, CNCOD1=5, CNTMLIF=6, CNTRLIF=30, CNMONF=36)
predict(fit,newdata=testcasehigh,type="response")

# Truth Tables
tab=table(fit$y, fit$fitted.values>.50) # Set the probability threshold to classify donors
tab # Classification Matrix
CCR=sum(diag(tab))/sum(tab)
CCR # Correct Classification Rate

# Specificity Accuracy
# See what contribution codes are most successful

hist(donor$CNMONF)
hist(donor$cadence1)

############# Best Case Fit ##############

fit = glm(DONATED ~ 
            CNDOL1 +
            SLOPE1 +
            CNTRLIF + 
            MONTH6 +
            MONTH12 +
            CNTMLIF +
            SEX$B +
            CNMON1
          ,family=binomial, data = training)

summary(fit)