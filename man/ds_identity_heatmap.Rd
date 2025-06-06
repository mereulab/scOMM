% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_identity_heatmap.R
\name{ds_identity_heatmap}
\alias{ds_identity_heatmap}
\title{Heatmap comparing two cell type annotations}
\usage{
ds_identity_heatmap(out, cluster)
}
\arguments{
\item{out}{The predicted cell types}

\item{cluster}{A cell type annotation}
}
\value{
A heatmap comparing out and cluster
}
\description{
This function generates a heatmap which compares the predicted cell types with another cell type annotation
}
\examples{
#not run
heatmap<-ds_identity_heatmap(out,cluster)
}
