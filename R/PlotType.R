#' Specify the type of plot you would like to create
#'
#' @param filename the directory and name of the file to save
#' @param PlotOptions a list of options for plotting
#' @param width the width of the plot in mm
#' @param height the height of the plot in mm
#' @return a plot device
#' @export
#' 
PlotType <- function(filename, PlotOptions, width, height)
{
    if ( PlotOptions$plottype %in% c("png","PNG",".png",".PNG","Png",".Png") )
    {
        png(paste(filename, ".png", sep = ""), width = width, height = height, unit = "mm", res = PlotOptions$resolution)
    }
}
