require(reshape2)
require(scales)
key <- c("h","k","m","b","+","-","?","")
key <- c(as.character(0:8),key)
value <- c(rep(10, times = 9), 100, 1000, 1000000, 1000000000, 
           1, 0, 0,0)

rel_data_2 <- subset(rel_data_1, rel_data_1$BGN_DATE > dmy("01/01/1996"))
rel_data_2$PROPDMGEXP <- tolower(rel_data_2$PROPDMGEXP)
rel_data_2$CROPDMGEXP <- tolower(rel_data_2$CROPDMGEXP)

for (i in 1:length(key)){
        rel_data_2$PROPDMGEXP[which(rel_data_2$PROPDMGEXP == key[i])] <- 
                value[i]
        rel_data_2$CROPDMGEXP[which(rel_data_2$CROPDMGEXP == key[i])] <- 
                value[i]
}
rel_data_2$PROPDMGEXP <- as.numeric(rel_data_2$PROPDMGEXP)
rel_data_2$CROPDMGEXP <- as.numeric(rel_data_2$CROPDMGEXP)

x <- mutate(rel_data_2, trueCROPDMG = CROPDMG * CROPDMGEXP, 
            truePROPDMG = PROPDMG *PROPDMGEXP, totalDMG = trueCROPDMG + 
                    truePROPDMG)
x <- group_by(x,EVTYPE)

y <- summarise(x, totaldmg = sum(totalDMG))

nonzerodamagedata <- y[y$totaldmg > 0,]
zerodamagedata <- y[y$totaldmg == 0,]
nonzerodamagedata <- nonzerodamagedata[order(-nonzerodamagedata$totaldmg),]
sumndd <- nonzerodamagedata[1:10,]
ndd <- length(nonzerodamagedata$totaldmg)
restd <- data.frame(EVTYPE = "lower 38", 
                    totaldmg = sum(nonzerodamagedata$totaldmg[11:ndd]))
sumndd <- rbind(sumndd, restd)
sumndd$EVTYPE <- factor(sumndd$EVTYPE, levels = sumndd$EVTYPE)
i <- ggplot(data = sumndd, aes(EVTYPE, totaldmg/1000000000))

j <- i + geom_bar(stat = "identity", position = "dodge") + 
        theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
        labs(x = "Weather Event Type", y = "Damage (billions of US Dollars)",
             title = "Total Damage to Crops and Property, 
             1996 - November 2011")