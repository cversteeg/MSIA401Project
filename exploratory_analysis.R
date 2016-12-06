###########################################
###                                     ###
###       Exploratory Analysis          ###
###                                     ###
###########################################

#### Cadence ####

frequency_table <- as.data.frame(table(donor$cadence_final))#  Frequency Table
frequency_table
plot(x = frequency_table$Var1, y = frequency_table$Freq / sum(frequency_table$Freq), labels = TRUE)
# Note that if Cadence == 0, then the donor donated 3 times in the same monthN

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
prop.table(table2,2)
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
barplot(prop.table(table(donor$DONATED, donor$TREND),2)[2,])
# Slope doesn't have any impact. In fact, positive trends did worse than negative trends.
