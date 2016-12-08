library(car)
library(data.table)
library(MASS)
donor <- read.csv("donation data.csv")
codes <- read.csv("dmef1code.csv")
donor$DONATED <- as.numeric(donor$TARGDOL > 0)
donor <- donor[,c('ID','DONATED','TARGDOL','STATCODE','SEX','CNTRLIF','CNTMLIF','CONLARG','CONTRFST','CNDOL1','CNDOL2','CNDOL3','CNCOD1','CNCOD2','CNCOD3','CNDAT1','CNDAT2','CNDAT3','CNMON1','CNMON2','CNMON3','CNMONF','CNMONL')]

# Calculating months between latest and 2nd latest
donor$cadence1 <- donor$CNDAT1 - donor$CNDAT2
donor$cadence2 <- donor$CNDAT2 - donor$CNDAT3
donor$cadence_final <- ifelse(donor$cadence1 - donor$cadence2 == 0, donor$cadence1, NA)

#Calculating donations forthcoming based on cadence
donor$cadence_due <-  (192 - donor$cadence_final == donor$CNDAT1 |
                         191 - donor$cadence_final == donor$CNDAT1 |
                         190 - donor$cadence_final == donor$CNDAT1 )

# Calculating if the Cadence was correct
donor$cadence_correct <- as.numeric(donor$cadence_due) == donor$DONATED

# Creating donors that have dontated three months from our test period. NOTE that we 
# have 0 donations for our 3 month time frame, because we are 4 months from our test period
donor$MONTHS3 <- as.numeric((donor$CNDAT1 == 189 | donor$CNDAT1 == 188 | donor$CNDAT1 == 187) & donor$cadence_final == 3)
donor$MONTHS6 <- as.numeric((donor$CNDAT1 == 186 | donor$CNDAT1 == 185 | donor$CNDAT1 == 184) & donor$cadence_final == 6)
donor$MONTHS12 <- as.numeric((donor$CNDAT1 == 180 | donor$CNDAT1 == 179 | donor$CNDAT1 == 178) & donor$cadence_final == 12)
donor$MONTHS3[is.na(donor$MONTHS3)] <- 0
donor$MONTHS6[is.na(donor$MONTHS6)] <- 0
donor$MONTHS12[is.na(donor$MONTHS12)] <- 0

#### Merging Codes
donor <- merge(donor, codes, by.x="CNCOD1", by.y="CODE", all.x=TRUE) #Merge code types for CNCOD1
names(donor)[names(donor) == 'CODETYPE'] <- 'CNCOD1TYPE'
donor <- merge(donor, codes, by.x="CNCOD2", by.y="CODE", all.x =TRUE) #Merge code types for CNCOD2
names(donor)[names(donor) == 'CODETYPE'] <- 'CNCOD2TYPE'
donor <- merge(donor, codes, by.x="CNCOD3", by.y="CODE", all.x =TRUE) #Merge code types for CNCOD3
names(donor)[names(donor) == 'CODETYPE'] <- 'CNCOD3TYPE'
donor$CNCOD3 <- NULL
donor$CNCOD2 <- NULL
donor$CNCOD1 <- NULL
rm(codes)

### Slope
donor$SLOPE1 <- (donor$CNDOL1 - donor$CNDOL2) / donor$CNDOL1
donor$SLOPE2 <- (donor$CNDOL2 - donor$CNDOL3) / donor$CNDOL2
donor$slope1_pos <- as.numeric(donor$SLOPE1 > 0)
donor$slope1_neut <- as.numeric(donor$SLOPE1 == 0)
donor$slope1_neg <- as.numeric(donor$SLOPE1 < 0)
donor$slope2_pos <- as.numeric(donor$SLOPE2 > 0)
donor$slope2_neut <- as.numeric(donor$SLOPE2 == 0)
donor$slope2_neg <- as.numeric(donor$SLOPE2 < 0)
donor$TREND <- ifelse(donor$slope1_pos == 1, 'Positive',
                      ifelse(donor$slope1_neut == 1 , 'Steady',
                             ifelse(donor$slope1_neg == 1 ,'Negative', NA)))
donor$SLOPE1[is.na(donor$SLOPE1)] <- 0
donor$SLOPE2[is.na(donor$SLOPE2)] <- 0
### Maybe for simplicicties sake, just look at the slope1

###Variables for Logistic
donor$GENDER <- as.numeric(donor$SEX == 'B')
donor$STATE <- as.numeric(donor$STATCODE == 'GU'|
                            donor$STATCODE == 'SD'|
                            donor$STATCODE == 'ND'|
                            donor$STATCODE == 'WI'|
                            donor$STATCODE == 'PA'|
                            donor$STATCODE == 'WY'|
                            donor$STATCODE == 'OH'|
                            donor$STATCODE == 'IA'|
                            donor$STATCODE == 'PR'|
                            donor$STATCODE == 'MN'|
                            donor$STATCODE == 'IN'|
                            donor$STATCODE == 'AK'|
                            donor$STATCODE == 'MI'|
                            donor$STATCODE == 'AZ'|
                            donor$STATCODE == 'NE'|
                            donor$STATCODE == 'MA'|
                            donor$STATCODE == 'MO'|
                            donor$STATCODE == 'NJ'|
                            donor$STATCODE == 'CT'|
                            donor$STATCODE == 'NY')
#Bar chart Contribution Code


#Generate training and testing
donor <- subset(donor, TARGDOL <= 250)
donor$logTARG = log(donor$TARGDOL)
merged = donor
mergedTraining1 = merged[seq(1, 99200, 3),]
mergedTraining2 = merged[seq(2, 99200, 3),]
mergedTraining = rbind(mergedTraining1, mergedTraining2)
mergedTesting = merged[seq(3, 99200, 3),]
logitTraining = mergedTraining
logitTraining$DONATED = logitTraining$TARGDOL >0
logitTraining$TARGDOL = NULL
logitTesting = mergedTesting
logitTesting$DONATED = logitTesting$TARGDOL >0
logitTesting$TARGDOL = NULL

linearDatasetTraining = mergedTraining[mergedTraining$TARGDOL > 0,]
linearDatasetTesting = mergedTesting[mergedTesting$TARGDOL >0,]

testingIDs = linearDatasetTesting$ID
trainingIDs = linearDatasetTraining$ID

logit1 = glm(formula = DONATED~CNDOL1 + MONTHS6 + CNTMLIF  + CNMON1 + CNCOD1TYPE, family = "binomial", data = logitTraining)
logitTest1 = predict(logit1, newdata = logitTesting)
logitTrain1 = predict(logit1, newdata = logitTraining)

priorDonation = sum(logitTraining$DONATED>0)/66134
logitTestOdds1 = exp(logitTest1)/(1+exp(logitTest1))

logitTrainOdds1 = exp(logitTrain1)/(1+exp(logitTrain1))
linear1 = lm(formula = TARGDOL ~ CNDOL1 +CONLARG + CNMON1 + CNTRLIF + CNTMLIF + CNCOD1TYPE, data = linearDatasetTraining)
qqPlot(linear1, main = "QQPlot of linear regression")
predictedDonation1 = predict(linear1, newdata = mergedTesting)

predictedValue1 = predictedDonation1*logitTestOdds1
sortedPrediction = sort(predictedValue1)
topPrediction = sortedPrediction[length(sortedPrediction):1]
topPredictions = topPrediction[1:1000]
predictedDonors= names(topPredictions)
donorData = (merged[predictedDonors,])
totalIncome = sum(donorData$TARGDOL)


#Bar chart Contribution code
code1 = merged[merged$CNCOD1TYPE == "*",]
code2 = merged[merged$CNCOD1TYPE == "A",]
code3 = merged[merged$CNCOD1TYPE == "B",]
code4 = merged[merged$CNCOD1TYPE == "C",]
code5 = merged[merged$CNCOD1TYPE == "D",]
code6 = merged[merged$CNCOD1TYPE == "M",]

code1Odds = sum(code1$DONATED)/nrow(code1)
codeA = sum(code2$DONATED)/nrow(code2)
codeB = sum(code3$DONATED)/nrow(code3)
codeC = sum(code4$DONATED)/nrow(code4)
codeD = sum(code5$DONATED)/nrow(code5)
codeM = sum(code6$DONATED)/nrow(code6)

codeOdds = cbind(codeA,codeB,codeC,codeD,codeM)
barplot(codeOdds, main = "Code Effect on Contribution", xlab = "Contribution Code")

codeAValue = sum(code2$TARGDOL)/nrow(code2[code2$DONATED == 1,])
codeBValue = sum(code3$TARGDOL)/nrow(code3[code3$DONATED == 1,])
codeCValue = sum(code4$TARGDOL)/nrow(code4[code4$DONATED == 1,])
codeDValue = sum(code5$TARGDOL)/nrow(code5[code5$DONATED == 1,])
codeMValue = sum(code6$TARGDOL)/nrow(code6[code6$DONATED == 1,])

codeVal = cbind(codeAValue,codeBValue,codeCValue,codeDValue,codeMValue)
barplot(codeVal, main = "Code Effect on Contribution Amount", xlab = "Contribution Code")


