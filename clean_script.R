library(lubridate)
library(dplyr)
data <- read.csv("FStormData.csv.bz2")
data$EVTYPE <- tolower(data$EVTYPE)
data$BGN_DATE <- mdy_hms(as.character(data$BGN_DATE))
data$END_DATE <- mdy_hms(as.character(data$END_DATE))
ev_table <- tolower(readLines("ev_table.txt"))

rel_data <- data[,c("BGN_DATE","END_DATE","EVTYPE","FATALITIES","INJURIES","REMARKS")]
trim.leading <- function (x)  sub("^\\s+", "", x)
rel_data$EVTYPE <- trim.leading(rel_data$EVTYPE)

g <- rel_data[!(rel_data$EVTYPE %in% ev_table),] ##removes exact matches

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
greppr("^snow(.)*rain|^rain(.)*snow|freezing rain|glaze ice", "winter weather")
greppr("record low rainfall","indeterminate")
greppr("rain","heavy rain")
greppr("snow","heavy snow")
greppr("funnel","funnel cloud")




x1 <- setdiff(grep("cold|wind(.)*chill",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
x <- cbind(x1,rel_data_1[x1,]$EVTYPE)
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "cold/wind chill"


x1 <- intersect(grep("cold|wind(.)*chill",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
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
greppr("wet","heavy rain")
greppr("smoke","heavy smoke")
greppr("dust devel|landspout","dust devil")
greppr("dust","dust storm")
greppr("cool|dry","nonextreme")
greppr("avalance","avalanche")
greppr("storm surge", "storm surge/tide")
greppr("surf","high surf")
greppr("ice jam","flash flood")
greppr("ice floes|marine mishap","marine high wind")
greppr("ice fog","freezing fog")
greppr("ice pellets","hail")
greppr("ice","frost/freeze")
greppr("lighting","lightning")
greppr("wall cloud|record|high seas|severe turbulence","indeterminate")
greppr("w(.)*spout", "waterspout")

x <- cbind(grep("urban(.)*small",rel_data_1$EVTYPE),grep("urban(.)*small",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
x3 <- rel_data_1[x2,]
x4 <- subset(x3, (x3$END_DATE-x3$BGN_DATE) > 0)
rel_data_1[x4,]$EVTYPE <- "flood"
rel_data_1[setdiff(x3,x4),]$EVTYPE <- "flash flood"


##g <- setdiff(rel_data_1$EVTYPE, ev_table) ##removes exact matches
##h <- sapply(ev_table, grep, x = g) ##indexes partial matches
##i <- unlist(h)
##i <- unique(i)
##j <- g[i]
##k <- setdiff(g,j) ##removes partial matches
##k <- k[-grep("summary",k)]

