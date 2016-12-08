###########################################
###                                     ###
###   Muliple Reg Model Building        ###
###                                     ###
###########################################

#### Run the Logistic Model First!!!!

library(leaps)

training <- subset(training, training$DONATED == 1)

############# Best Case Fit ##############
best <- leaps(training[,c('STATE.LM','CNTRLIF','CONLARG','CNDOL1','CNMON1')],
              training[,'TARGDOL'],
              method = 'Cp',
              nbest = 2,
              names = c('STATE.LM','CNTRLIF','CONLARG','CNDOL1','CNMON1')
              )
data.frame(size = best$size, Cp = best$Cp, best$which)

reg.fit <- lm(TARGDOL ~ 
                #+ STATCODE #Grouped into STATE.LM 
                #+ SEX #Grouped into GENDER
                CNTRLIF 
                #+ CNTMLIF #The number of times is correllated with total amount. Total amount better for this case.
                + CONLARG
                #+ CONTRFST # Not Significant
                + CNDOL1
                #+ CNDOL2 # Captured in SLOPE1
                #+ CNDOL3 # Captured in SLOPE2
                + CNMON1
                #+ CNMON2 # Captured in cadence1
                #+ CNMON3 # Captured in cadence2
                #+ CNCOD1TYPE
                + cadence1
                #+ cadence2
                #+ cadence_final
                #+ cadence_due
                + SLOPE1 
                #+ SLOPE2
                #+ TREND # Gives contradicting values
                #+ GENDER # Not Significant
                + STATE.LM
                #+ CNCODEBIN
              , data=training)

summary(reg.fit)

#### Jeff's Best
reg.fit <- lm(TARGDOL ~ CNTRLIF + CONLARG + CNDOL1 + CNMON1 + cadence1 + SLOPE1 + STATE.LM, data=training)
summary(reg.fit)

#### Julia's Best
training$CNDOL1.1 <- training$CNDOL1
training$CNDOL1.1[is.na(training$CNDOL1.1)] <- 0 
training$CNDOL2.1 <- training$CNDOL2
training$CNDOL2.1[is.na(training$CNDOL2.1)] <- 0 
training$CNDOL3.1 <- training$CNDOL3
training$CNDOL3.1[is.na(training$CNDOL3.1)] <- 0 
reg.fit <- lm(TARGDOL ~ CNDOL1.1 + CNTRLIF + SEX  + CNDOL1:SEX + CNDOL2.1 + CNDOL3.1 + CNMON1 + CNTMLIF, data=training)

### Calculating Expected Value ###
pred.glm <- predict(glm.fit, newdata=test, type = 'response')
pred.reg <- predict(reg.fit, newdata=test)
E <- pred.glm*pred.reg
sort.E <- sort(E, decreasing=TRUE)
sum(head(sort.E,1000)) #################### This is our pay off
rm(pred.glm,pred.reg,E,sort.E)
