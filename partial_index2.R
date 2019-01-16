h1_intersect <- function(l){
        ## nms <- combn( names(l) , 2 , FUN = paste0 , collapse = "" , simplify = FALSE )
        ll <- combn( l , 2 , simplify = FALSE )
        out <- lapply( ll , function(x) intersect( x[[1]] , x[[2]] )  )
        ## setNames( out , nms )
        out
}