###########################################
###                                     ###
###   Logistic  Model Building          ###
###                                     ###
###########################################

#### Run the variable creation first!!!!

### Removing influential Obs
donor <- subset(donor, TARGDOL <= 250)

### Taking Test and Training Data set
seq <- seq(3, nrow(donor), by=3)
test <- donor[seq,]
training <- donor[-seq,]
rm(seq)


############# Best Case Fit ##############

glm.fit = glm(DONATED ~ 
            CNDOL1 +
            SLOPE1 +
            MONTHS6 +
            CNTMLIF +
            STATE +
            GENDER +
            CNMON1 +
            CNCODEBIN,
            family=binomial, data = training)

summary(glm.fit)



# Truth Tables  
tab=table(glm.fit$y, glm.fit$fitted.values>.50) # Set the probability threshold to classify donors
tab # Classification Matrix
CCR=sum(diag(tab))/sum(tab)
CCR # Correct Classification Rate
rm(tab)
rm(CCR)

### Testing a few cases on the model
testcasehigh = data.frame(CNDOL1=5, SLOPE1=0.1, MONTHS6=1, CNTMLIF=5, STATE=1, GENDER=1, CNMON1=1, CNCODEBIN=1)
predict(glm.fit,newdata=testcasehigh,type="response") # Calculated probability that this person will donate
rm(testcasehigh)
