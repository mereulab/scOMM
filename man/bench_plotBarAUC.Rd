% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bench_plotBarAUC.R
\name{bench_plotBarAUC}
\alias{bench_plotBarAUC}
\title{Benchmarking: Plotting the output of the function \code{bench_calcAUC()}}
\usage{
bench_plotBarAUC(
  out,
  bar_cols = NULL,
  ct_mapping = NULL,
  group_case = "Case",
  group_cols = NULL,
  alpha_vec = NULL
)
}
\arguments{
\item{out}{The output of the function \code{bench_calcAUC()}}

\item{bar_cols}{Color vector for the bar plot layer}

\item{ct_mapping}{Option to fill bar plot layer by grouping cols, if not provided, by default will only take information from \code{out}}

\item{group_case}{Column specifying the benchmarking cases}

\item{group_cols}{Color vector for the line and point layers}

\item{alpha_vec}{Alpha vector for the bar plot layer}
}
\value{
A dataset of AUC per cell type of each case
}
\description{
This function plots the output of the function \code{bench_calcAUC()}
}
