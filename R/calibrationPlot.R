#' Calibration Plot
#'
#' Plots the calibration curve.
#'
#' @param mev Mean estimated value for each bin.
#' @param pop Proportion of positives.
#' @param title The title of the plot.
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @return A calibration plot.
#' @keywords calibration, plot, binary, mean estimated value, proportion of positives
#' @importFrom graphics title
#' @export
#'
#`


calibrationPlot <- function(mev,pop,title,xlab,ylab) {

#create plot dataframe
plot_df = data.frame(mev, pop)

ggplot(plot_df, aes(x=mev, y=pop)) + geom_point(color = 'blue') + geom_abline(color = 'red') + xlim(0,1) + ylim(0,1) + ggtitle(title) + xlab(xlab) + ylab(ylab) +  theme(plot.title = element_text(hjust = 0.5))


}
