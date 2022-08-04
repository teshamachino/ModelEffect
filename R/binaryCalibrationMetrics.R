#' Calibration Metrics
#'
#' Calculates the mean predicted value and proportion of positives for a model.
#'
#' @param estimations The model's probability estimations.
#' @param labels The actual true labels from the dataset.
#' @param bin_cnt The number of bins to calibrate the probabilities.
#' @return A plot.
#' @keywords probability, binary, calibration, plot, mean predicted value, proportion of positives
#' @importFrom magrittr '%>%'
#' @importFrom graphics title
#' @export
#'
#`

binaryCalibrationMetrics <- function(estimations, labels, bin_cnt = 10) {

# package dependencies: base, stats, dplyr

#get labels and make the data labels numeric (1,0)
labels = labels %>% as.logical() %>% as.numeric()

# calibration
bin_lbl = estimations %>% dplyr::ntile(bin_cnt)

#combine estimations and labels by bins
combine = data.frame(estimations, labels, bin_lbl)
combine$labels = combine$labels %>% as.logical()

#mev = mean estimated value
#pop = proportion of positives
calibration = combine %>% dplyr::group_by(bin_lbl) %>% dplyr::summarise(mev = mean(estimations), pop=mean(labels))

return(calibration)
}
