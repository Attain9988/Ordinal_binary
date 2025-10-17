score_interval <-function (prop, n, p = NULL,  conf.level = 0.95, correct = TRUE) 
{
    x <- prop*n
    k <- length(x)
    if (is.null(p)) 
        p <- 0.5
   
    correct <- as.logical(correct)
    ESTIMATE <- setNames(x/n,
        "p")
    CINT <- NULL
    YATES <- if (correct) 
        0.5
    else 0
    
        z <- qnorm( 
            (1 + conf.level)/2)
        YATES <- min(YATES, abs(x - n * p))
        z22n <- z^2/(2 * n)
        p.c <- ESTIMATE + YATES/n
        p.u <- if (p.c >= 1) 
            1
        else (p.c + z22n + z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
            n)))/(1 + 2 * z22n)
        p.c <- ESTIMATE - YATES/n
        p.l <- if (p.c <= 0) 
            0
        else (p.c + z22n - z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
            n)))/(1 + 2 * z22n)
        CINT <- c(max(p.l, 0), 
            min(p.u, 1))
    
    return(CINT)
}