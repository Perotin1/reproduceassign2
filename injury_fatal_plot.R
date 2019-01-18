require(ggplot2)
rel_data_4 <- subset(rel_data_1, rel_data_1$BGN_DATE > dmy("01/01/1996"))
rel_data_4 <- group_by(rel_data_4, EVTYPE)

fatal_data <- summarise(rel_data_4, Fatalities = sum(FATALITIES))
nonzero_fatal_data <- fatal_data[fatal_data$Fatalities > 0,]
zerofataldata <- fatal_data[fatal_data$Fatalities == 0,]
nonzero_fatal_data <- nonzero_fatal_data[order(-nonzero_fatal_data$Fatalities),]
sumnfd <- nonzero_fatal_data[1:10,]
nfd <- length(nonzero_fatal_data$Fatalities)
restf <- data.frame(EVTYPE = "lower 38", 
                   Fatalities = sum(nonzero_fatal_data$Fatalities[11:nfd]))
sumnfd <- rbind(sumnfd, restf)
sumnfd$EVTYPE <- factor(sumnfd$EVTYPE, levels = sumnfd$EVTYPE)
g <- ggplot(data = sumnfd, aes(EVTYPE, Fatalities))
g <- g + geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
        labs(x = "Weather Event Type", 
             title = "Fatalities, 1996 - November 2011")

injur_data <- summarise(rel_data_4, Injuries = sum(INJURIES))
nonzero_injur_data <- injur_data[injur_data$Injuries > 0,]
zeroinjurydata <- injur_data[injur_data$Injuries == 0,]
nonzero_injur_data <- nonzero_injur_data[order(-nonzero_injur_data$Injuries),]
sumnid <- nonzero_injur_data[1:10,]
nid <- length(nonzero_injur_data$Injuries)
resti <- data.frame(EVTYPE = "lower 38", 
                    Injuries = sum(nonzero_injur_data$Injuries[11:nid]))
sumnid <- rbind(sumnid, resti)
sumnid$EVTYPE <- factor(sumnid$EVTYPE, levels = sumnid$EVTYPE)
h <- ggplot(data = sumnid, aes(EVTYPE, Injuries))
h <- h + geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
        labs(x = "Weather Event Type", 
             title = "Injuries 1996 - November 2011")

