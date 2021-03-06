% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpiketrainThresholding.R
\name{SpiketrainThresholding}
\alias{SpiketrainThresholding}
\title{Extract Spiketrain from Deconvolved Signal and Fluorescence Time-series}
\usage{
SpiketrainThresholding(
  deconvolvedSignal,
  fluorescence,
  minNonZeroS_noise = 5,
  threshold_dc,
  isHardThreshold = FALSE
)
}
\arguments{
\item{deconvolvedSignal}{vector}

\item{fluorescence}{vector}

\item{minNonZeroS_noise}{number}

\item{threshold_dc}{number}

\item{isHardThreshold}{logical}
}
\value{
vector
}
\description{
Extract Spiketrain from Deconvolved Signal and Fluorescence Time-series
}
