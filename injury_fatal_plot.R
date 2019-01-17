library(ggplot2)
rel_data_4 <- subset(rel_data_1, rel_data_1$BGN_DATE > dmy("01/01/1996"))
rel_data_4 <- group_by(rel_data_4, EVTYPE)

fatal_data <- summarise(rel_data_4, Fatalities = sum(FATALITIES))
nonzero_fatal_data <- fatal_data[fatal_data$Fatalities > 0,]
g <- ggplot(data = nonzero_fatal_data, aes(EVTYPE, Fatalities))
g + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))

injur_data <- summarise(rel_data_4, Injuries = sum(INJURIES))
nonzero_injur_data <- injur_data[injur_data$Injuries > 0,]
h <- ggplot(data = nonzero_injur_data, aes(EVTYPE, Injuries))
h + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))

