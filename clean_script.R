library(lubridate)
library(dplyr)
if(!file.exists("FStormData.csv.bz2")){
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                      "FStormData.csv.bz2")
}
data <- read.csv("FStormData.csv.bz2", nrows = 905000)
data$EVTYPE <- tolower(data$EVTYPE)
data$BGN_DATE <- mdy_hms(as.character(data$BGN_DATE))
data$END_DATE <- mdy_hms(as.character(data$END_DATE))
ev_table <- tolower(readLines("ev_table.txt"))

rel_data <- data[,c("BGN_DATE","END_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP","REMARKS")]
trim.leading <- function (x)  sub("^\\s+", "", x)
rel_data$EVTYPE <- trim.leading(rel_data$EVTYPE)

g1 <- setdiff(rel_data$EVTYPE, ev_table) ##gets typo evtypes
h1 <- sapply(ev_table, grep, x = g1) ##gets indexes of partial matches to typo evtypes
h1 <- h1[lapply(h1,length)>0] ## removes empties
h1 <- lapply(h1,as.integer)
h3 <- unique(unlist(h1))
N <- 367
h4 <- vector("list", N)
for(i in 1:N) { ## trying to get function to return index of lists of which h3[i] is an element
        a <- logical()
        for(j in 1:30){
        a[[j]] <- h3[i] %in% h1[j][[1]]
        }
       b <- names(h1)[a]
       h4[[i]] <- b
}
names(h4) <- g1[as.numeric(h3)]
h5 <- h4[lapply(h4,length)<2]

for(i in 1:269){
        r <- rel_data$EVTYPE==names(h5)[i]
        rel_data$EVTYPE[r] <- h5[i][[1]]
}

h6 <- h4[lapply(h4,length)>1]

h7 <- h6[grep("flash",names(h6))]
for(i in 1:19){
        r <- rel_data$EVTYPE==names(h7)[i]
        rel_data$EVTYPE[r] <- "flash flood"
}
h8 <- h6[-(grep("flash",names(h6)))]

h9 <- h8[grep("blizzard",names(h6))]
for(i in 1:10){
        r <- rel_data$EVTYPE==names(h9)[i]
        rel_data$EVTYPE[r] <- "blizzard"
}
h10 <- h8[-(grep("blizzard",names(h8)))]

h11 <- h10[grep("coast",names(h10))]
for(i in 1:5){
        r <- rel_data$EVTYPE==names(h11)[i]
        rel_data$EVTYPE[r] <- "coastal flood"
}
h12 <- h10[-(grep("coast",names(h10)))]

h13 <- h12[grep("flood",names(h12))]
for(i in 1:15){
        r <- rel_data$EVTYPE==names(h13)[i]
        rel_data$EVTYPE[r] <- "flood"
}
h14 <- h12[-(grep("flood",names(h12)))]

h15 <- h14[grep("snow|winter storm",names(h14))]
for(i in 1:14){
        r <- rel_data$EVTYPE==names(h15)[i]
        rel_data$EVTYPE[r] <- "winter storm"
}
h16 <- h14[-(grep("snow|winter storm",names(h14)))]

h17 <- h16[grep("dust storm",names(h16))]
for(i in 1:2){
        r <- rel_data$EVTYPE==names(h17)[i]
        rel_data$EVTYPE[r] <- "dust storm"
}
h18 <- h16[-(grep("dust storm",names(h16)))]

h19 <- h18[grep("record/excessive heat",names(h18))]

        r <- rel_data$EVTYPE==names(h19)[i]
        rel_data$EVTYPE[r] <- "excessive heat"

h20 <- h18[-(grep("record/excessive heat",names(h18)))]

bad_evtype <- rel_data[rel_data$EVTYPE %in% names(h20),]
rel_data_1 <- rel_data[!(rel_data$EVTYPE %in% names(h20)),]

rel_data_1$EVTYPE <- gsub("tstm", "thunderstorm",rel_data_1$EVTYPE)

greppr <- function(a,b){
        x <- cbind(grep(a,rel_data_1$EVTYPE),grep(a,rel_data_1$EVTYPE, value = TRUE))
        x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
        rel_data_1[x2,]$EVTYPE <<- b
}

greppr("hurricane|typhoon|floyd", "hurricane (typhoon)")
greppr("hot|warm", "heat")
greppr("^th(.)*m|mi(.)*oburst|downburst|gustnado|whirlwind","thunderstorm wind")
greppr("fld","flood")
greppr("^snow(.)*rain|^rain(.)*snow|freezing rain|glaze ice|glaze|wint(.){3,4}mix|glaze|freezing drizzle", "winter weather")
greppr("record low rainfall","other")
greppr("rain|dam failure","heavy rain")
greppr("snow","heavy snow")
greppr("funnel","funnel cloud")

x1 <- setdiff(grep("cold|wind(.)*chill|hyp(.){1,2}thermia",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
x <- cbind(x1,rel_data_1[x1,]$EVTYPE)
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "cold/wind chill"

x1 <- intersect(grep("cold|wind(.)*chill|hyp(.){1,2}thermia",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
x <- cbind(x1,rel_data_1[x1,]$EVTYPE)
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "extreme cold/wind chill"

greppr("fire","wildfire")
greppr("^high(.)*wind*|wnd|apache county","high wind")
greppr("wind","strong wind")
greppr("high temp","heat")
greppr("low temp","cold/wind chill")
greppr("frost|freeze|black ice","frost/freeze")
greppr("volcanic eruption","volcanic ash")
greppr("blow-out","astronomical low tide")
greppr("high tide|coastal surge","coastal flood")
greppr("wet|heavy precip.tation|shower|excessive precipitation","heavy rain")
greppr("smoke","heavy smoke")
greppr("dust devel|landspout","dust devil")
greppr("dust","dust storm")
greppr("avalance","avalanche")
greppr("storm surge", "storm surge/tide")
greppr("surf|high waves|swell","high surf")
greppr("ice jam|flash|rapid","flash flood")
greppr("ice floes|marine mishap","marine high wind")
greppr("ice fog","freezing fog")
greppr("ice pellets","hail")
greppr("ice|icy","frost/freeze")
greppr("lig(.){2,3}ing","lightning")
greppr("slide","debris flow")
greppr("fog","dense fog")
greppr("torndao","tornado")
greppr("w(.)*spout", "waterspout")
greppr("smoke","dense smoke")


bad_evtype <- rbind(bad_evtype,rel_data_1[grep("southeast|mix|eros(.){1,4}n|other|normal|cool|dry|wall cloud|record|high seas|severe turbulence|summary|severe thunderstorm",rel_data_1$EVTYPE),])
rel_data_1 <- rel_data_1[-(grep("southeast|mix|eros(.){1,4}n|other|normal|cool|dry|wall cloud|record|high seas|severe turbulence|summary|severe thunderstorm",rel_data_1$EVTYPE)),]

x <- cbind(grep("urban|small",rel_data_1$EVTYPE),grep("urban|small",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
x3 <- rel_data_1[x2,]
for(i in 1:18){
        if(is.na(x3[i,]$END_DATE)){
                x3[i,]$END_DATE <- x3[i,]$BGN_DATE
        }
        else{}
        if((x3[i,]$END_DATE-x3[i,]$BGN_DATE) > 0){
                x3[i,]$EVTYPE <- "flood"
        }
        else{
                x3[i,]$EVTYPE <- "flash flood"  
        }
}
rel_data_1[x2,] <- x3

bad_evtype <- rbind(bad_evtype,rel_data_1[rel_data_1$EVTYPE %in% g,])
rel_data_1 <- rel_data_1[!(rel_data_1$EVTYPE %in% g),]

g <- setdiff(rel_data_1$EVTYPE, ev_table) ##removes exact matches