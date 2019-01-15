greppr <- function(in,out){
        x <- cbind(grep(a,rel_data_1$EVTYPE),grep(a,rel_data_1$EVTYPE, value = TRUE))
        x2 <- as.numeric(subset(x, !(x[,2] %in% ev_table))[,1])
        rel_data_1[x2,]$EVTYPE <- b
}