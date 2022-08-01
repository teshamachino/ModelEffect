#' Calibration Metrics
#'
#' Calculates the mean predicted value and proportion of positives for a model.
#'
#' @param estimations The model's probability estimations.
#' @param class The dataset's binary classification.
#' @param bin_cnt The number of bins to calibrate the probabilities.
#' @return A plot.
#' @keywords probability, binary, calibration, plot, mean predicted value, proportion of positives
#' @importFrom magrittr '%>%'
#' @importFrom graphics title
#' @export
#'
#`

binaryCalibrationMetrics <- function(estimations, class, bin_cnt = 10) {

# package dependencies: base, stats, dplyr

#get class and make the data class numeric (1,0)
class = class %>% as.logical() %>% as.numeric()

# calibration
bin_lbl = estimations %>% dplyr::ntile(bin_cnt)

#combine estimations and class by bins
combine = data.frame(estimations, class, bin_lbl)
combine$class = combine$class %>% as.logical()

#mev = mean estimated value
#pop = proportion of positives
calibration = combine %>% dplyr::group_by(bin_lbl) %>% dplyr::summarise(mev = mean(estimations), pop=mean(class))

return(calibration)
}
