#' Receiver Operating Curve Plot
#'
#' Plots the precision recall curve.
#'
#' @param tpr True positive rate.
#' @param fpr False positive rate.
#' @return A plot.
#' @keywords receiver operating curve, plot, binary
#' @importFrom graphics title
#' @export
#'
#`



rocPlot <- function(tpr,fpr) {

  title = paste0("ROC Curve for Predictions")
  graphics::plot(fpr,tpr,type='l',main=title,lwd=2,col="blue",ylab="True Positive Rate",xlab="False Positive Rate")
  graphics::grid(col = "gray", lty = "dotted", lwd = 2)
  graphics::lines(rep(0,1000),seq(0,1,length.out=1000),col="brown",lwd=2)
  graphics::lines(seq(0,1,length.out=1000),rep(1,1000),col="brown",lwd=2)
  graphics::abline(0,1, lty = 8, col = "red")
  graphics::legend("bottomright",legend=c("With Model","Without Model","Ideal"),lwd=c(2,1,2),lty=c(1,2,1),col=c("blue","red","brown"),cex=0.9,bg='white')


}
