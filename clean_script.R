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
h1_intersect <- function(l){
        nms <- combn( names(l) , 2 , FUN = paste0 , collapse = "" , simplify = FALSE )
        ll <- combn( l , 2 , simplify = FALSE )
        out <- lapply( ll , function(x) intersect( x[[1]] , x[[2]] )  )
        setNames( out , nms )
        out
}
h2 <- h1_intersect(h1)

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

