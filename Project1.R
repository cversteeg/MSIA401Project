library(car)
library(data.table)

data <- read.csv("donation data.csv")
codes <- read.csv("dmef1code.csv")

############ Merge code types with code ID ############

#Merge code types for CNCOD1
merged <- merge(data, codes, by.x="CNCOD1", by.y="CODE", all.x=TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'CNCOD1TYPE'

#Merge code types for CNCOD2
merged <- merge(merged, codes, by.x="CNCOD2", by.y="CODE", all.x =TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'CNCOD2TYPE'

#Merge code types for CNCOD3
merged <- merge(merged, codes, by.x="CNCOD3", by.y="CODE", all.x =TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'CNCOD3TYPE'

#Merge code types for SLCOD1
merged <- merge(merged, codes, by.x="SLCOD1", by.y="CODE", all.x =TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'SLCOC1TYPE'

#Merge code types for SLCOD2
merged <- merge(merged, codes, by.x="SLCOD2", by.y="CODE", all.x =TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'SLCOC2TYPE'

#Merge code types for SLCOD3
merged <- merge(merged, codes, by.x="SLCOD3", by.y="CODE", all.x =TRUE)
names(merged)[names(merged) == 'CODETYPE'] <- 'SLCOC3TYPE'

############ Set N/A values to 0 in four code type columns ############

na.col <- c("CNCOD2TYPE", "CNCOD3TYPE", "SLCOC2TYPE", "SLCOC3TYPE")

merged[,na.col] <-
  apply(merged[,na.col], 2, function(x){replace(x, is.na(x), 0)})

#Check that for every 0 value in SLCOC2TYPE, SLCOC3TPE is also 0
a <- merged$SLCOC2TYPE[which(merged$SLCOC2TYPE==0)]
b <- merged$SLCOC3TYPE[which(merged$SLCOC2TYPE==0)]
identical(length(a),sum(b==0))

#Check that for every 0 value in CNCOD2TYPE, CNCOD3TPE is also 0
c <- merged$CNCOD2TYPE[which(merged$CNCOD2TYPE==0)]
d <- merged$CNCOD3TYPE[which(merged$CNCOD2TYPE==0)]
identical(length(c),sum(d==0))

############ Create new feature "Region" that groups states ############
merged$REGION <- merged$STATCODE

merged$REGION <- recode(merged$REGION, "c('AK', 'CA', 'HI', 'OR', 'WA')='PAC';
                        c('ID', 'MT', 'WY')='NWM';
                        c('AZ', 'NM', 'CO', 'NV', 'UT')='SWM';
                        c('IL', 'IN', 'MI', 'OH', 'WI')='ENC';
                        c('IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD')='WNC';
                        c('MA', 'ME', 'NH', 'VT', 'RI', 'CT')='NE';
                        c('DE', 'MD', 'NJ', 'DC', 'PA', 'NY', 'VA', 'WV')='MATL';
                        c('FL', 'GA', 'NC', 'SC')='SATL';
                        c('AL', 'KY', 'MS', 'TN')='ESC';
                        c('AR', 'LA', 'OK', 'TX')='WSC';
                        else='NonUS'")

dt <- data.table(merged)

#Print mean TARGDOL by Region
dt[,list(mean=mean(TARGDOL)),by=REGION]

#Print mean TARGDOL by state
dt[,list(mean=mean(TARGDOL)),by=STATCODE]

#Set code types as factors
merged$SLCOC2TYPE <- as.factor(merged$SLCOC2TYPE)
merged$SLCOC3TYPE <- as.factor(merged$SLCOC3TYPE)
merged$CNCOD2TYPE <- as.factor(merged$CNCOD2TYPE)
merged$CNCOD3TYPE <- as.factor(merged$CNCOD3TYPE)
#Adding Trend line
merged$trend = sign(merged$CNDOL1 - merged$CNDOL2)
merged$trend[is.na(merged$trend)] = 0


#Generate training and testing
mergedTraining1 = merged[seq(1, 99200, 3),]
mergedTraining2 = merged[seq(2, 99200, 3),]
mergedTraining = rbind(mergedTraining1, mergedTraining2)
mergedTesting = merged[seq(3, 99200, 3),]
logitTraining = mergedTraining
logitTraining$donated = logitTraining$TARGDOL >0
logitTraining$TARGDOL = NULL
logitTesting = mergedTesting
logitTesting$donated = logitTesting$TARGDOL >0
logitTesting$TARGDOL = NULL



logit1 = glm(formula = donated~ CONLARG + CNTMLIF + REGION + SEX +CNMON1, family = "binomial", data = logitTraining)
logitTest1 = predict(logit1, newdata = logitTesting)
logitTrain1 = predict(logit1, newdata = logitTraining)

priorDonation = sum(logitTraining$donated>0)/66134

logitTestOdds1 = exp(logitTest1)/(1+exp(logitTest1))
logitTestPosterior1 = priorDonation*logitTestOdds1/(priorDonation*logitTestOdds1 + (1-priorDonation)*(1-logitTestOdds1))

logitTrainOdds1 = exp(logitTrain1)/(1+exp(logitTrain1))
logitTrainPosterior1 = priorDonation*logitTrainOdds1/(priorDonation*logitTrainOdds1 + (1-priorDonation)*(1-logitTrainOdds1))

linear1 = lm(formula = TARGDOL~CONLARG +CNTMLIF +REGION + SEX + CNMON1, data = mergedTraining)
predictedDonation1 = predict(linear1, newdata = mergedTesting)

predictedValue1 = predictedDonation1*logitTestPosterior1
sortedPrediction = sort(predictedValue1)
topPrediction = sortedPrediction[length(sortedPrediction):-1:1]
topPredictions = topPrediction[1:1000]
totalIncome = sum(topPredictions)
