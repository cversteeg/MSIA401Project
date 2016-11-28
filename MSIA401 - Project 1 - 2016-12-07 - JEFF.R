### Install Packages


### Read Data

donor <- read.csv("C:/Users/Jeff/Downloads/donation data.csv")
codes <- read.csv("C:/Users/Jeff/Downloads/dmef1code.csv")

### Reordering Variables
donor$DONATED <- as.numeric(donor$TARGDOL > 0)
donor <- donor[,c('ID','DONATED','TARGDOL','STATCODE','SEX','CNTRLIF','CNTMLIF','CONLARG','CONTRFST','CNDOL1','CNDOL2','CNDOL3','CNCOD1','CNCOD2','CNCOD3','CNDAT1','CNDAT2','CNDAT3','CNMON1','CNMON2','CNMON3','CNMONF','CNMONL')]

# Calculating months between latest and 2nd latest
donor$cadence1 <- donor$CNDAT1 - donor$CNDAT2
donor$cadence2 <- donor$CNDAT2 - donor$CNDAT3
donor$cadence_final <- ifelse(donor$cadence1 - donor$cadence2 == 0, donor$cadence1, NA)
frequency_table <- as.data.frame(table(donor$cadence_final))#  Frequency Table
frequency_table
plot(x = frequency_table$Var1, y = frequency_table$Freq / sum(frequency_table$Freq), labels = TRUE)
# Note that if Cadence == 0, then the donor donated 3 times in the same month


# In the write-up about the cadence, we should mention that cadence takes at least 3 
# donations to establish, because we need to look at the time difference between the donations
# If a donor has only donated once, and it 17 months ago, it would be foolish to think
# They have a cadence of 17 months. If they donated twice, and both times were 6 months apart
# now we have a cadence

# Also based on the cadences, as expected 3, 6, 12, and 24 are the most popular cadences
# We could consider using these as predictors as well

#Calculating donations forthcoming based on cadence
donor$cadence_due <-  (192 - donor$cadence_final == donor$CNDAT1 |
                         191 - donor$cadence_final == donor$CNDAT1 |
                         190 - donor$cadence_final == donor$CNDAT1 )
table1 <- table(donor$DONATED,donor$cadence_due)
sum(diag(table1))/sum(table1) # Overall correct rate = 60%
table1[1,1]/sum(table1[1,]) # Correctly identified those that DID NOT donated = 80%
table1[2,2]/sum(table1[2,]) # Correctly identified those that DID donate = 30%

donor$cadence_correct <- as.numeric(donor$cadence_due) == donor$DONATED
table2 <- table(donor$cadence_correct, donor$cadence_final)
prop.table(table2,2)
# Summary: I was only able to find cadences for 2,423 donors which is only 2%
# of the population. However, cadence was only able to get 60% correct.
# Also, there aren't many cadences that are better than others. 12 months and 6 months is ok
# Ultimately, I bet cadence will not be statistically significant


# Creating donors that have dontated three months from our test period. NOTE that we 
# have 0 donations for our 3 month time frame, because we are 4 months from our test period
donor$MONTHS3 <- as.numeric(donor$CNDAT1 == 189 | donor$CNDAT1 == 188 | donor$CNDAT1 == 187)
donor$MONTHS6 <- as.numeric(donor$CNDAT1 == 186 | donor$CNDAT1 == 185 | donor$CNDAT1 == 184)
donor$MONTHS12 <- as.numeric(donor$CNDAT1 == 180 | donor$CNDAT1 == 179 | donor$CNDAT1 == 178)

### Taking Test and Training Data set
training <- donor[-(seq(3, nrow(donor), 3)) ,]
test <- donor[seq(3, nrow(donor), 3),]


# Creating a fit
fit = glm(DONATED ~ MONTHS12 + MONTHS6, family=binomial, data = donor)
summary(fit)
fit = glm(DONATED ~ CNCOD1 , family=binomial, data = donor) # The more recent a person has donated, the more likely to donate
fit = glm(DONATED ~ CNTMLIF , family=binomial, data = donor) # The more times donated in lifetime, the more likely to donate
fit = glm(DONATED ~ CNTRLIF , family=binomial, data = donor) # The more they have dontated, the more likely to donate
fit = glm(DONATED ~ CNMONL , family=binomial, data = donor) # The longer they have been a donor, the more likely to donate
fit = glm(DONATED ~ SEX , family=binomial, data = donor) # Married couples most likely to donate
fit = glm(DONATED ~ MONTHS12 + MONTHS6 + CNCOD1 + CNTRLIF + CONLARG + CNMON1 + CNTMLIF + cadence1 + CNMONF, family=binomial, data = training)

summary(fit)

### Testing a few cases on the model
testcaselow = data.frame(MONTHS12=0, MONTHS6=0, CNCOD1=5, CNTMLIF=1, CNTRLIF=5, CNMONF=180)
predict(fit,newdata=testcaselow,type="response") # Calculated probability that this person will donate

testcasemedium = data.frame(MONTHS12=1, MONTHS6=0, CNCOD1=10, CNTMLIF=3, CNTRLIF=30, CNMONF=36)
predict(fit,newdata=testcasemedium,type="response")

testcasehigh = data.frame(MONTHS12=0, MONTHS6=1, CNCOD1=5, CNTMLIF=6, CNTRLIF=30, CNMONF=36)
predict(fit,newdata=testcasehigh,type="response")

# Truth Tables
tab=table(fit$y, fit$fitted.values>.20) # Set the probability threshold to classify donors
tab # Classification Matrix
CCR=sum(diag(tab2))/sum(tab2)
CCR # Correct Classification Rate

# Specificity Accuracy
# See what contribution codes are most successful

hist(donor$CNMONF)
hist(donor$cadence1)
