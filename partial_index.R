h1_intersect <- function(x){
        a <- list()
for(i in 1:length(x)){
        x1 <- x[-i]
        for(j in 1:length(x1)){
                b <- numeric()
                if(length(intersect(x1[j],x[i]))){
                b[j] <- intersect(x1[j],x[i])
                }
        
        }
        a[i] <- b
}
return(a)
}