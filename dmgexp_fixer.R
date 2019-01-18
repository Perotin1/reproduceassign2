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
            truePROPDMG = PROPDMG *PROPDMGEXP)
x <- group_by(x,EVTYPE)

y <- summarise(x, propdmg = sum(truePROPDMG), cropdmg = sum(trueCROPDMG))

z <- melt(y, id.vars = "EVTYPE")

for (i in 1:96) if (z[i,]$value==0) z[i,]$value <- 1

i <- ggplot(data = z, aes(EVTYPE, value, fill = variable))

j <- i + geom_bar(stat = "identity", position = "dodge") + 
        theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
        scale_y_log10()