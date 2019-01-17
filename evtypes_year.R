d <- character()
for(i in 51:112){
        d <- c(d,paste("01/01/",1900+i,sep=""))
}
d <- dmy(d)
year <- character()
evtypes <- integer()
for(i in 1:61){
        j <- subset(rel_data_1, rel_data_1$BGN_DATE < d[i])
        k <- length(unique(j$EVTYPE))
        year <- append(year, as.character(1950+i))
        evtypes <- append(evtypes, as.integer(k))
}
ev_index <- data.frame(year,evtypes)