#' Binary Estimation Metrics
#'
#' This function calculates model evaluation metrics for binary outcomes.
#'
#' @param estimations The model probability estimations.
#' @param classes The actual true classes from the dataset.
#' @param selected_metric The selected metric(s).
#' @return A vector of metrics.
#' @keywords metric, model, binary
#' @importFrom magrittr '%>%'
#' @importFrom graphics title
#' @export
#'
#`


binaryEstimationMetrics <- function(estimations, class, selected_metric = c("MSE", "BRI", "RMSE", "MAE", "ROC", "PRC", "LOG", "CIX", "SMD", "TJU", "PEA", "EFR"))
{

   # package dependencies: base, ResourceSelection, pROC, stats, MLmetrics

   #count the estimations
   estimations_cnt = length(estimations)

   #make the dataset classes numeric (1,0)
   class = class %>% as.logical() %>% as.numeric()


   ######################################################################################
   # background calculations
   is_true = testing_dataset[, outcome_var_name] == TRUE | testing_dataset[, outcome_var_name] == 1

   is_false = testing_dataset[, outcome_var_name] == FALSE | testing_dataset[, outcome_var_name] == 0
   true_cnt = sum(is_true)
   false_cnt = sum(is_false)

   true_estimations = estimations[class == 1]
   true_estimations_cnt = length(true_estimations)
   false_estimations = estimations[class == 0]
   false_estimations_cnt = length(false_estimations)

   all_one_outcome_bit = true_cnt > 0 & false_cnt > 0

   #concorance metrics
   if(all_one_outcome_bit) {
   true_estimations = as.numeric(stats::predict(model, testing_dataset[is_true, ], type = "response"))
   false_estimations = as.numeric(stats::predict(model, testing_dataset[is_false,], type = "response"))
   true_estimations[is.na(true_estimations)] = 0.5
   false_estimations[is.na(false_estimations)] = 0.5
   pairs_cnt = true_cnt * false_cnt
   concordant_cnt = 0
   disconcordant_cnt = 0
   for (j in 1:true_cnt) {
       concordant_cnt = sum(true_estimations[j] > false_estimations) +
           concordant_cnt
   }
   for (j in 1:false_cnt) {
       disconcordant_cnt = sum(true_estimations[j] < false_estimations) +
           disconcordant_cnt
   }

   }


   ######################################################################################
   # metrics

   # MSE - Mean squared error
   # BRI - Brier score (MSE for binary data)
   # RMSE - Root mean square error
   # MAE - Mean absolute error
   # ROC - Area under curve receiver operator curve
   # PRC - Area under precision recall curve
   # LOG - Log loss
   # CIX - C index
   # SMD - Sommers D
   # TJU - Tjur's r squared
   # PEA - Pearson's r squared
   # EFR - Efron's r squared

   MSE = BRI = MLmetrics::MSE(y_pred = model_estimations, y_true = class)
   RMSE = MLmetrics::RMSE(y_pred = model_estimations, y_true = class)
   MAE = MLmetrics::MAE(y_pred = model_estimations, y_true = class)
   ROC = MLmetrics::AUC(y_pred = model_estimations, y_true = class)
   PRC = MLmetrics::PRAUC(y_pred = model_estimations, y_true = class)
   LOG = MLmetrics::LogLoss(y_pred = model_estimations, y_true = class)

   # CIX = 0
   # SMD = (concordant_cnt - disconcordant_cnt)/pairs_cnt
   TJU = mean(true_estimations) - mean(false_estimations)
   PEA = stats::cor(class, estimations)^2
   EFR = 0


   metric_name = c(   "MSE", "BRI", "RMSE", "MAE", "ROC", "PRC", "LOG", "CIX", "SMD", "TJU", "PEA", "EFR")
   metric_value = c(MSE, BRI, RMSE, MAE, ROC, PRC, LOG, CIX, SMD, TJU, PEA, EFR)
   names(metric_value) = metric_name
   metric_value = metric_value[selected_metric]
   return(metric_value)
}
