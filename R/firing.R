#' Calculate the Firing Rate of a Neuron or Neurons given their Spiketrains
#'
#' @param neuronID number
#' @param spiketrain data frame
#'
#' @return a number indicating the firing rate in spikes per frame
#' @export
#'
#' @examples
firing.rate <- function(neuronID, spiketrain){
  if(length(neuronID)==1){
    d = sum(spiketrain[,neuronID])
  }else{
    d = as.numeric(colSums(spiketrain[,neuronID]))
  }
  return(d/nrow(spiketrain))
}
