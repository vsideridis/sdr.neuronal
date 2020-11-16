#' Extract Spiketrain from Deconvolved Signal and Fluorescence Time-series
#'
#' @param deconvolvedSignal vector
#' @param fluorescence vector
#' @param minNonZeroS_noise number
#' @param threshold_dc number
#' @param isHardThreshold logical
#'
#' @return vector
#' @export
#'
#' @examples
SpiketrainThresholding = function(deconvolvedSignal, fluorescence, minNonZeroS_noise = 5, threshold_dc, isHardThreshold = FALSE){

  # Simple hard threshold in deconvolved signal
  if(isHardThreshold){
    threshold_deconvolved = 0.01
    spiketrain = ifelse(deconvolvedSignal > threshold_deconvolved, 1,0)
    return(spiketrain)
  }

  totalFrames = length(deconvolvedSignal)

  # Noise intervals are the frames where fluorescence is negative
  noise_intervals = fluorescence<0
  # Make a circular shift to Noise intervals of 1 frame to the left
  noise_intervals = c(noise_intervals[2:length(noise_intervals)], FALSE)

  # s_noise is the deconvolvedSignal restricted on the noise intervals
  s_noise = deconvolvedSignal[noise_intervals]

  # if the number of nonzero s_noise values are less than minNonZeroS_noise,
  # then the std is calculated from the deconvolved signal s itself.
  numOfNonzeroValues =  sum(s_noise>0.01)

  if(numOfNonzeroValues <= minNonZeroS_noise){
    stdValue = quantile(deconvolvedSignal[which(deconvolvedSignal>0.0001)], .68)
  }else{
    stdValue = quantile(s_noise[which(s_noise>0.0001)], .68)
  }

  #Threshold is calculated by the product of noise's std and the threshold_dc which is given by the user
  threshold_deconvolved = as.numeric(threshold_dc*stdValue)

  spiketrain = ifelse(deconvolvedSignal > threshold_deconvolved, 1,0)
  return(spiketrain)
}
