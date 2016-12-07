###########################################
###                                     ###
###   Muliple Reg Model Building        ###
###                                     ###
###########################################

#### Run the Logistic Model First!!!!

############# Best Case Fit ##############

training <- subset(training, training$DONATED == 1)

reg.fit <- lm(TARGDOL ~ 
                STATE.LM 
                + GENDER
                + CNTMLIF 
                + CONLARG
                + CONTRFST # Not Significant
                + CNDOL1  
                + SLOPE1 
                + TREND # Gives contradicting values
              , data=training)

#### Julia's Best
# reg.fit <- lm(TARGDOL ~ CNDOL1 + CNTRLIF + SEX  + CNDOL1:SEX + CNDOL2 + CNDOL3 + CNMON1 + CNTMLIF, data=training)

summary(reg.fit)

### Calculating Expected Value ###
pred.glm <- predict(glm.fit, newdata=test, type = 'response')
pred.reg <- predict(reg.fit, newdata=test)
E <- pred.glm*pred.reg
sort.E <- sort(E, decreasing=TRUE)
sum(head(sort.E,1000)) #################### This is our pay off
rm(pred.glm,pred.reg,E,sort.E)
