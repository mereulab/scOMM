% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bench_plotTileAUC.R
\name{bench_plotTileAUC}
\alias{bench_plotTileAUC}
\title{Benchmarking: Plotting the output of the function \code{bench_calcAUC()}}
\usage{
bench_plotTileAUC(out, tile_cols, tile_leg_title, ct_leg_title, ct_cols)
}
\arguments{
\item{out}{The output of the function \code{bench_calcAUC()}}

\item{tile_cols}{Color vector for the tiles plot layer}

\item{tile_leg_title}{Title of tile legend}

\item{ct_leg_title}{Title of ct legend}

\item{ct_cols}{Color vector for the line and point layers}
}
\value{
A dataset of AUC per cell type of each case
}
\description{
This function plots the output of the function \code{bench_calcAUC()}
}
