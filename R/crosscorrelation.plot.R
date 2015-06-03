#' Cross correlation plot
#'
#' @author Darcy N. Webber
#' @export
#' 
crosscorrelation.plot <- function (x, col = topo.colors(10), ...) 
{
    Nvar <- nvar(x)
    pcorr <- crosscorr(x)
    dens <- ((pcorr + 1) * length(col))%/%2 + (pcorr < 1) + (pcorr < 
        -1)
    cutoffs <- format(seq(from = 1, to = -1, length = length(col) + 
        1), digits = 2)
    leg <- paste("(", cutoffs[-1], ",", cutoffs[-length(cutoffs)], 
        "]", sep = "")
    oldpar <- NULL
    on.exit(par(oldpar))
    oldpar <- c(par(pty = "s", adj = 0.5), oldpar)
    plot(0, 0, type = "n", xlim = c(0, Nvar), ylim = c(0, Nvar), 
        xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    if (!is.R()) {
        par(adj = 1)
    }
    axis(1, at = 1:Nvar - 0.5, las = 2, labels = abbreviate(varnames(x, 
        allow.null = FALSE), minlength = 7))
    axis(2, at = 1:Nvar - 0.5, las = 1, labels = abbreviate(varnames(x, 
        allow.null = FALSE), minlength = 7)[Nvar:1])
    for (cl in 1:Nvar) {
        for (rw in 1:(Nvar - cl + 1)) polygon(y = c(cl - 1, cl - 
            1, cl, cl, cl - 1), x = c(rw - 1, rw, rw, rw - 1, 
            rw - 1), col = col[dens[nrow(dens) - cl + 1, rw]])
    }
    yval <- seq(from = Nvar/2, to = Nvar, length = length(col) + 
        1)
    ydelta <- Nvar/(2 * (length(col) + 1))
    for (i in 1:length(col)) {
        polygon(y = c(yval[i], yval[i + 1], yval[i + 1], yval[i], 
            yval[i]), col = col[i], x = c(Nvar - ydelta, Nvar - 
            ydelta, Nvar, Nvar, Nvar - ydelta))
    }
    text(Nvar - ydelta, Nvar, "1", adj = c(1, 1))
    text(Nvar - ydelta, 0.5 * Nvar, "-1", adj = c(1, 0))
    text(Nvar - ydelta, 0.75 * Nvar, "0", adj = c(1, 0.5))
    invisible()
}
