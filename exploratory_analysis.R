###########################################
###                                     ###
###       Exploratory Analysis          ###
###                                     ###
###########################################

library(car)
library(data.table)
library("ggplot2")
library(plyr)
library(dplyr)
library(pROC)

#### Cadence ####

frequency_table <- as.data.frame(table(donor$cadence_final))#  Frequency Table
frequency_table
plot(x = frequency_table$Var1, y = (frequency_table$Freq / sum(frequency_table$Freq))*100, xlab = "Months of Cadence", ylab = "Percent of Cadences", main = "Percent of Cadences")
# Note that if Cadence == 0, then the donor donated 3 times in the same monthN
as.data.frame(frequency_table$Freq / sum(frequency_table$Freq)*100)

# In the write-up about the cadence, we should mention that cadence takes at least 3 
# donations to establish, because we need to look at the time difference between the donations
# If a donor has only donated once, and it 17 months ago, it would be foolish to think
# They have a cadence of 17 months. If they donated twice, and both times were 6 months apart
# now we have a cadence

# Also based on the cadences, as expected 3, 6, 12, and 24 are the most popular cadences
# We could consider using these as predictors as well

table1 <- table(donor$DONATED,donor$cadence_due)
sum(diag(table1))/sum(table1) # Overall correct rate = 60%
table1[1,1]/sum(table1[1,]) # Correctly identified those that DID NOT donated = 80%
table1[2,2]/sum(table1[2,]) # Correctly identified those that DID donate = 30%

# Calculating if the cadence was correct
table2 <- table(donor$cadence_correct, donor$cadence_final)
barplot(prop.table(table2,2)[2,1:24], ylab = "% Correctly Predicted", xlab = "Months of Cadence", main = "Accuracy of Cadence")

# Summary: I was only able to find cadences for 2,423 donors which is only 2%
# of the population. However, cadence was only able to get 60% correct.
# Also, there aren't many cadences that are better than others. 12 months and 6 months is ok
# Ultimately, I bet cadence will not be statistically significant

### Plots of Percentages of DONATED  by Gender

plot(x = donor$SEX, y = donor$DONATED)
table1 <- table(donor$DONATED, donor$SEX)
table2 <- prop.table(table1, 2)
table2[2,]
barplot(table2[2,], main="Percentage of Each Gender Who Actually Donated", 
        xlab = "Gender Codes", ylab = "Percentage")
table(donor$SEX)
477/99191
#Married, Male, and Female are more likely to Donate

### Plots of Percentages of DONATED by CNMON1
table1 <- table(donor$DONATED, donor$CNMON1)
table2 <- prop.table(table1, 2)
table2[2,]
barplot(table2[2,], main="Percentage of Donors by Months Since Latest Donation", 
        xlab = "Months Since Last Donation", ylab = "Percentage")
#Generally, those that have donated more recently are more likely to donate in TARGDOL

### Plots of Percentages of DONATED by CNMONF
table1 <- table(donor$DONATED, donor$CNMONF)
table2 <- prop.table(table1, 2)
table2[2,]
barplot(table2[2,], main="Percentage of Donors by Months Since First Donation", 
        xlab = "Months since first donation", ylab = "Percentage")
#Generally, those that are older are more likely to donate in TARGDOL

### Plots of Percentages of DONATED by CNTMLIF
table1 <- table(donor$DONATED, donor$CNTMLIF)
table2 <- prop.table(table1, 2)
table2[2,]
barplot(table2[2,], main="Percentage of Donors by Number of Lifetime Donations", 
        xlab = "Number of Donations", ylab = "Percentage")
hist(donor$CNTMLIF)
#Generall the more donations the more likely to donated


# Trying to figure out a super variable between CNMON1 CNTMLIF and CNMONF
# 1) It doesn't matter if the donor has donated recently and been a donor for a long time
# 2) It doesn't matter if the donor has donated recently and donated more than once
# 3) It doesn't matter if the donor has donated more than once and been a donor for a long time

subsetdonors <- subset(donor, donor$CNMON1 <= 12 & CNMONF >= 24)
table1 <- prop.table(table(subsetdonors$DONATED, subsetdonors$CNTMLIF),2)
barplot(table1[2,], main="% Who Donated by Number of Donations", 
        xlab = "Count of Donations", ylab = "Percentage")
# Controlling for month's since last donation and month's since first donation
# It DOES matter how many times they have donated

subsetdonors <- subset(donor, donor$CNMON1 <= 12 & CNTMLIF >= 3)
table1 <- prop.table(table(subsetdonors$DONATED, subsetdonors$CNMONF),2)
barplot(table1[2,], main="% Who Donated by Month's Since First Donation", 
        xlab = "Month's Since First Donation", ylab = "Percentage")
# Controlling for months since last donation and count of donations
# It DOESN't matter how long they have been a donor (Except for our 6 and 12 month Cadence Donors)

subsetdonors <- subset(donor, donor$CNTMLIF >= 3 & CNMONF >= 24)
table1 <- prop.table(table(subsetdonors$DONATED, subsetdonors$CNMON1),2)
barplot(table1[2,], main="% Who Donated by Month's Since Last Donation", 
        xlab = "Month's Since Last Donation", ylab = "Percentage")
# Controlling for count of donations and months since first donation
# It DOES matter how long it has been since the last donation

##### Slope ####
table(donor$DONATED, donor$TREND)
prop.table(table(donor$DONATED, donor$TREND),2)
barplot(prop.table(table(donor$DONATED, donor$TREND),2)[2,], ylab = "Percentage", main = "Percent Who Donated by Trend")
# Slope doesn't have any impact. In fact, positive trends did worse than negative trends.


### Plots of Percentages of DONATED by STATECODE
table1 <- prop.table(table(donor$DONATED, donor$STATCODE),2)
names <- c(colnames(table1))
percentage <- c(table1[2,])
table1 <- data.frame(names,percentage)

median(table1$percentage)
table1[table1$percentage > median(table1$percentage),1]

table(donor$STATCODE)

##### CNCOD1TYPE ####
barplot(prop.table(table(donor$DONATED, donor$CNCOD1TYPE),2)[2,], ylab = "Donation Rate", xlab = "Donation Code", main = "Donation Rate by Donation Code")

### Most Generous States ###
donated <- donor[donor$DONATED == 1,]
Generous_states <- ddply(donated,'STATCODE',summarize,mean=mean(TARGDOL))
Generous_states <- Generous_states[with(Generous_states, order(-Generous_states$mean)),]
barplot(Generous_states$mean, names.arg = Generous_states$STATCODE, ylab = "Mean of TARGDOL", xlab = "STATCODE", main = "Average TARGDOL by State")
View(Generous_states)
rm(Generous_states)

### Most Generous Genders ###
donated <- donor[donor$DONATED == 1,]
Generous_genders <- ddply(donated,'SEX',summarize,mean=mean(TARGDOL))
plot(Generous_genders, ylab = "Mean of TARGDOL", xlab = "SEX Code", main = "Average TARGDOL by Gender")
View(Generous_genders)
rm(Generous_genders)

### ROC Curve for Logisitic ###
plot.roc(training$DONATED, glm.fit$fitted.values, xlab = "1-Specificity") # ROC Curve
fit$null.deviance - glm.fit$deviance
qchisq(.95, df=7)        # 7 degrees of freedom 

### Most Generous Codes ###
donated <- donor[donor$DONATED == 1,]
Generous_Codes <- ddply(donated,'CNCOD1TYPE',summarize,mean=mean(TARGDOL))
plot(Generous_Codes, ylab = "Mean of TARGDOL", xlab = "Latest Code", main = "Average TARGDOL by Code")
View(Generous_Codes)
rm(Generous_Codes)