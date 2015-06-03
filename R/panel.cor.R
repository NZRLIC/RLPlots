#' Panel Corr
#'
#' @export
#' 
panel.cor <- function(x, y, digits = 3, prefix = "", cex.cor = 2, pch, las)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- formatC(r, digits = digits, format = "f", flag = "0")
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)

    test <- cor.test(x,y)
    text(0.5, 0.5, txt)
}
