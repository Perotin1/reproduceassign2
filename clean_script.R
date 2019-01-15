library(lubridate)
data <- read.csv("FStormData.csv.bz2")
data$EVTYPE <- tolower(data$EVTYPE)
data$BGN_DATE <- mdy_hms(as.character(data$BGN_DATE))
data$END_DATE <- mdy_hms(as.character(data$END_DATE))
ev_table <- tolower(readLines("ev_table.txt"))

rel_data_1 <- data[,c("BGN_DATE","END_DATE","EVTYPE","FATALITIES","INJURIES","REMARKS")]

trim.leading <- function (x)  sub("^\\s+", "", x)

rel_data_1$EVTYPE <- trim.leading(rel_data_1$EVTYPE)
rel_data_1$EVTYPE <- gsub("tstm", "thunderstorm",rel_data_1$EVTYPE)

x <- cbind(grep("hurricane|typhoon",rel_data_1$EVTYPE),grep("hurricane|typhoon",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "hurricane (typhoon)"

x <- cbind(grep("hot|warm",rel_data_1$EVTYPE),grep("hot|warm",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "heat"


x <- cbind(grep("^th(.)*m|^t(.)*storm|mi(.)*oburst|downburst|gustnado|whirlwind",rel_data_1$EVTYPE),grep("^th(.)*m|^t(.)*storm|mi(.)*oburst|downburst|gustnado|whirlwind",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "thunderstorm wind"

x <- cbind(grep("fld",rel_data_1$EVTYPE),grep("fld",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "flood"

x <- cbind(grep("^snow(.)*rain|^rain(.)*snow|freezing rain",rel_data_1$EVTYPE),grep("^snow(.)*rain|^rain(.)*snow|freezing rain",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "winter weather"

x <- cbind(grep("record low rainfall",rel_data_1$EVTYPE),grep("record low rainfall",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "drought"

x <- cbind(grep("rain",rel_data_1$EVTYPE),grep("rain",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "heavy rain"

x <- cbind(grep("snow",rel_data_1$EVTYPE),grep("snow",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "heavy snow"

x <- cbind(grep("funnel",rel_data_1$EVTYPE),grep("funnel",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "funnel cloud"

x1 <- setdiff(grep("cold|wind(.)*chill",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
x <- cbind(x1,rel_data_1[x1,]$EVTYPE)
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "cold/wind chill"


x1 <- intersect(grep("cold|wind(.)*chill",rel_data_1$EVTYPE),grep("extreme",rel_data_1$EVTYPE))
x <- cbind(x1,rel_data_1[x1,]$EVTYPE)
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "extreme cold/wind chill"

x <- cbind(grep("fire",rel_data_1$EVTYPE),grep("fire",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "wildfire"

x <- cbind(grep("^high(.)*wind*",rel_data_1$EVTYPE),grep("^high(.)*wind*",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "high wind"


x <- cbind(grep("wind",rel_data_1$EVTYPE),grep("wind",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "strong wind"

x <- cbind(grep("high temp",rel_data_1$EVTYPE),grep("high temp",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "heat"

x <- cbind(grep("low temp",rel_data_1$EVTYPE),grep("low temp",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "cold/wind chill"

x <- cbind(grep("frost",rel_data_1$EVTYPE),grep("frost",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "frost/freeze"

x <- cbind(grep("volcanic eruption",rel_data_1$EVTYPE),grep("volcanic eruption",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "volcanic ash"

x <- cbind(grep("blow-out",rel_data_1$EVTYPE),grep("blow-out",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "astronomical low tide"

x <- cbind(grep("high tide",rel_data_1$EVTYPE),grep("high tide",rel_data_1$EVTYPE, value = TRUE))
x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
rel_data_1[x2,]$EVTYPE <- "coastal flood"

g <- setdiff(rel_data_1$EVTYPE, ev_table) ##removes exact matches
h <- sapply(ev_table, grep, x = g) ##indexes partial matches
i <- unlist(h)
i <- unique(i)
j <- g[i]
k <- setdiff(g,j) ##removes partial matches
k <- k[-grep("summary",k)]
k <- k[-grep("dry",k)]
