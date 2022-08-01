#' Binary Model Metrics
#'
#' This function calculates model evaluation metrics for binary outcomes.
#'
#' @param model Model glm fit.
#' @param testing_dataset The dataset to test the model.
#' @param selected_metric The selected metric(s).
#' @return A vector of metrics.
#' @keywords metric, model, binary
#' @importFrom magrittr '%>%'
#' @importFrom graphics title
#' @export
#'
#`


binaryModelMetrics <- function(model, testing_dataset, selected_metric = c("AIC", "BIC", "LLM", "LL0", "CXS", "NAG", "DEV", "MCF"))
{

   # package dependencies: base, stats

   ######################################################################################

   #model info
   outcome_var_name = all.vars(stats::as.formula(model))[1]
   model_family = model$family$family


   #model estimations as probabilities
   log_odds = stats::predict(model, testing_dataset)
   outcome_rate = mean(as.logical(testing_dataset[,outcome_var_name]))
   outcome_log_odds = log(outcome_rate / (1 - outcome_rate))
   log_odds[is.na(log_odds)] = outcome_log_odds
   model_estimations = exp(log_odds)/(1 + exp(log_odds))

   estimations_cnt = nrow(testing_dataset)

   #get labels and make the data labels numeric (1,0)
   dataset_labels = testing_dataset[,outcome_var_name] %>% as.logical() %>% as.numeric()


   ######################################################################################
   # metrics

   # AIC -  Akaike information criterion
   # BIC -  Bayesian information criterion
   # LLM -  Log Likelihood current model
   # LL0 -  Log Likelihood null model
   # CXS -  Cox and Sneell pseudo R squared
   # NAG -  Nagelkerke pseudo R squared
   # DEV -  Deviance pseudo R squared
   # MCF -  McFadden pseudo R squared

   AIC = stats::AIC(model)
   BIC = stats::BIC(model)
   LLM = as.numeric(stats::logLik(model))
   LL0 = as.numeric(stats::logLik(stats::glm(y ~ 1, family = model_family)))
   CXS = 1 - exp(-(2/estimations_cnt) * (LLM - LL0))
   NAG = CXS/(1 - exp((2 * estimations_cnt^(-1)) * LL0))
   DEV = -2 * LL0 + 2 * LLM
   MCF = 1 - LLM/LL0


   metric_name = c("AIC", "BIC", "LLM", "LL0", "CXS", "NAG", "DEV", "MCF")
   metric_value = c(AIC, BIC, LLM, LL0, CXS, NAG, DEV, MCF)
   names(metric_value) = metric_name
   metric_value = metric_value[selected_metric]
   return(metric_value)
}
