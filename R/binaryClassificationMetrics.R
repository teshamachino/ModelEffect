#' Binary Classification Metrics
#'
#' This function calculates classification evaluation metrics for binary outcomes.
#'
#' @param estimations The model probability estimations.
#' @param class The actual true class from the dataset.
#' @param selected_metric The selected metric(s) to pull.
#' @return A vector of metrics.
#' @keywords metric, model, binary, classification
#' @importFrom magrittr '%>%'
#' @importFrom graphics title
#' @export
#'
#`

binaryClassificationMetrics <- function (estimations, class, selected_metric = c("ACC", "ERR", "FPR", "TPR", "FNR","TNR","PPV","NPV","F","RPP","RNP"))

{

#get class(labels) and make the data class numeric (1,0)
class = class %>% as.logical() %>% as.numeric()

#sorting the probability estimations descending effectively sets a threshold
estimations_sort_desc = sort(estimations, decreasing=TRUE)
class_sort_desc = class[order(estimations, decreasing=TRUE)]


# using ROCR package create prediction S4 object
pred <- ROCR::prediction(estimations, class)

#pull classificaiton metrics from

ACC = ROCR::performance(pred, measure = "acc")@y.values[[1]]
ERR = ROCR::performance(pred, measure = "err")@y.values[[1]]
FPR = ROCR::performance(pred, measure = "fpr")@y.values[[1]]
TPR = ROCR::performance(pred, measure = "tpr")@y.values[[1]]
FNR = ROCR::performance(pred, measure = "fnr")@y.values[[1]]
TNR = ROCR::performance(pred, measure = "tnr")@y.values[[1]]

PPV = ROCR::performance(pred, measure = "ppv")@y.values[[1]]
NPV = ROCR::performance(pred, measure = "npv")@y.values[[1]]

F = ROCR::performance(pred, measure = "f")@y.values[[1]]

RPP = ROCR::performance(pred, measure = "rpp")@y.values[[1]]
RNP = ROCR::performance(pred, measure = "rnp")@y.values[[1]]

#binary classification metrics data frame
bcm = data.frame(ACC,ERR,FPR,TPR,FNR,TNR,PPV,NPV,F,RPP,RNP)

return(bcm)
}
