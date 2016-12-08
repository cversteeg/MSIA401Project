tab <-table(donor$TARGDOL>0, donor$STATCODE)
df <- data.frame(state= colnames(tab), true=tab[2,], false=tab[1,] )
df$percent <- df$true/(df$true + df$false)
df <- df[with(df, order(-df$percent)),] #percent of TARGDOL donators
barplot(df$percent, main="Percent of Donors by State Contributing in TARGDOL",  
        xlab="State", ylab="Percent Donating") #plot