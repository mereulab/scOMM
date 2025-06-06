% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_feature_importance.R
\name{ds_feature_importance}
\alias{ds_feature_importance}
\title{This function calculates the feature importance of the deep learning model}
\usage{
ds_feature_importance(X, Y, model, metric = "accuracy", features, max_n = 1000)
}
\arguments{
\item{X}{The data with the feature information}

\item{Y}{The data with the output information}

\item{model}{The deep learning model}

\item{metric}{The evaluation metric used to calculate the feature importance, "loss" or "accuracy". (Default="accuracy")}

\item{features}{The manually selected features whose importance will be calculated. If not provided,
the function will use all the features. You can control the number of used features by using the parameter
\code{max_n}.}

\item{max_n}{Number of features selected from X if \code{features} not provided.}
}
\value{
A dataframe with the importance score of each feature
}
\description{
This function generates a dataframe with the feature importance score of the input features calculated
based on the "Permutation Feature Importance" approach.
}
\examples{
ds_feature_importance(X=x_test,Y=y_test,metric="accuracy",max_n=500)

}
