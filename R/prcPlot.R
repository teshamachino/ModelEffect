#' Precision Recall Plot
#'
#' Plots the precision recall curve.
#'
#' @param ppv Positive predicted value rate. Also known as precision.
#' @param tpr True positive rate. Also known as recall.
#' @param mean_rate The mean rate of the positive class.
#' @param title The title of the plot.
#' @param xlab The x label.
#' @param ylab The y axis label.
#' @return A plot.
#' @keywords precision, recall, plot, binary
#' @importFrom graphics title
#' @export
#'
#`

prPlot <- function(ppv,tpr,mean_rate,title,xlab,ylab) {

  #create plot dataframe
plot_df = data.frame(ppv, tpr)

#plot the data
ggplot(plot_df, aes(x=tpr, y=ppv)) + geom_line(color = 'blue') + geom_hline(yintercept=mean_rate, color = 'red',linetype = "dashed" ) + xlim(0,1) + ylim(0,1) + ggtitle(title) + xlab(xlab) + ylab(ylab) +  theme(plot.title = element_text(hjust = 0.5))


}
