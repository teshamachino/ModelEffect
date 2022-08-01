#' Receiver Operating Curve Plot
#'
#' Plots the precision recall curve.
#'
#' @param tpr True positive rate.
#' @param fpr False positive rate.
#' @param title The title of the plot.
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @return A plot.
#' @keywords receiver operating curve, plot, binary
#' @importFrom graphics title
#' @export
#'
#`


rocPlot <- function(tpr,fpr,title,xlab,ylab) {

#create plot dataframe
plot_df = data.frame(tpr, fpr)

#plot the data
ggplot(plot_df, aes(x=fpr, y=tpr)) + geom_line(color = 'blue') + geom_abline(color = 'red', linetype = "dashed") + xlim(0,1) + ylim(0,1) + ggtitle(title) + xlab(xlab) + ylab(ylab) +  theme(plot.title = element_text(hjust = 0.5))

}
