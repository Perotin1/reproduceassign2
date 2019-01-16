isinh1 <- function(x){
        a <- logical()
        for(i in 1:30){
        a[[i]] <- x %in% h1[i]
        }
        b <- names(h1)[a]
        h4.append(b)
}