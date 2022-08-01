#' Precision Recall Plot
#'
#' Plots the precision recall curve.
#'
#' @param ppv Positive predicted value rate. Also known as precision.
#' @param tpr True positive rate. Also known as recall.
#' @return A plot.
#' @keywords precision, recall, plot, binary
#' @importFrom graphics title
#' @export
#'
#`

prPlot <- function(ppv,tpr) {

  title = paste0("Precision Recall Curve for Predictions")
  graphics::plot(ppv,tpr,type='l',main=title,lwd=2,col="blue",ylab="Precision",xlab="Recall")
  graphics::grid(col = "gray", lty = "dotted", lwd = 2)
  graphics::lines(rep(1,1000),seq(0,1,length.out=1000),col="brown",lwd=2)
  graphics::lines(seq(0,1,length.out=1000),rep(1,1000),col="brown",lwd=2)
  graphics::abline(.5,0, lty = 8, col = "red")
  graphics::legend("bottomleft",legend=c("With Model","Without Model","Ideal"),lwd=c(2,1,2),lty=c(1,2,1),col=c("blue","red","brown"),cex=0.9,bg='white')


}
