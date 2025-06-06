% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bench_plotMeanAUC.R
\name{bench_plotMeanAUC}
\alias{bench_plotMeanAUC}
\title{Benchmarking: Plotting the average AUC of a list of outputs from \code{bench_calcAUC()}}
\usage{
bench_plotMeanAUC(out_list)
}
\arguments{
\item{out_list}{The list of outputs from the function \code{bench_calcAUC()}}

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
This function plots a list of outputs from the function \code{bench_calcAUC()}
}
