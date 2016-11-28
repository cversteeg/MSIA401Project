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



