###########################################
###                                     ###
###          Variable Creation          ###
###                                     ###
###########################################

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
# STATE.LM
donor$STATE.LM <- as.numeric(donor$STATCODE == 'AE'|
                            donor$STATCODE == 'GU'|
                            donor$STATCODE == 'AK'|
                            donor$STATCODE == 'DC'|
                            donor$STATCODE == 'PR'|
                            donor$STATCODE == 'HI'|
                            donor$STATCODE == 'OR'|
                            donor$STATCODE == 'GA'|
                            donor$STATCODE == 'VA'|
                            donor$STATCODE == 'CA'|
                            donor$STATCODE == 'MD'|
                            donor$STATCODE == 'WA'|
                            donor$STATCODE == 'AL'|
                            donor$STATCODE == 'MI'|
                            donor$STATCODE == 'MS'|
                            donor$STATCODE == 'AZ'|
                            donor$STATCODE == 'NV'|
                            donor$STATCODE == 'OK'|
                            donor$STATCODE == 'TX'|
                            donor$STATCODE == 'SC')

donor$CNCODEBIN <- as.numeric(donor$CNCOD1TYPE == 'A')
