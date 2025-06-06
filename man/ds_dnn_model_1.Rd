% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_dnn_model_1.R
\name{ds_dnn_model_1}
\alias{ds_dnn_model_1}
\title{This function trains, fits and evaluates a deep neural network (DNN) model.}
\usage{
ds_dnn_model_1(
  out,
  hnodes,
  epochs = 10,
  set.seed = TRUE,
  seed,
  batch_size = 32,
  activation = "relu",
  add_dropout = TRUE,
  pct_dropout = 0.2,
  name_mod = "mod",
  lr = 0.001,
  weight_reg = TRUE,
  l1 = 0,
  l2 = 0,
  verbose = TRUE,
  earlystopping = TRUE,
  lr_scheduler = FALSE,
  schedule = NULL,
  patience = 2,
  add_batchnorm = FALSE,
  ...
)
}
\arguments{
\item{out}{The output of the ds_split_data_encoder function. The input data used to train the DNN model.}

\item{hnodes}{The number of the nodes of the intermediate layers of the encoder. The first and last layers are not included.}

\item{epochs}{Keras parameter for the layer_dense function (Default= 30).}

\item{set.seed}{Set a seed (Default=TRUE).}

\item{seed}{Set a seed and a generator.}

\item{batch_size}{Keras parameter for the layer_dense function (Default= 32).}

\item{activation}{Keras parameter for the layer_dense function (Default= "relu").}

\item{add_dropout}{Keras parameter for the layer_dense function (Default= TRUE).}

\item{pct_dropout}{Keras parameter for the layer_dense function (Default= 0.2).}

\item{name_mod}{name of the model. This is mandatory if you want to include the model in a stacked model.}

\item{lr}{Keras parameter that acts as a tuning parameter in the model that determines the step size at each iteration while
moving toward a minimum of a loss function (Default=0.001)}

\item{weight_reg}{Keras parameter for the layer weight regularizers (Default=TRUE)}

\item{l1}{Keras parameter for the layer weight regularizers (Default=0)}

\item{l2}{Keras parameter for the layer weight regularizers (Default=0)}

\item{verbose}{Logical, controls the displaying of additional messages while
running the function. Defaults to TRUE.}

\item{earlystopping}{Keras parameter to stop training when a monitored metric has stopped improving (Default=TRUE)}

\item{lr_scheduler}{Keras parameter to modulate how the learning rate of your optimizer changes over time (Default=FALSE)}

\item{schedule}{Keras parameter to modulate how the learning rate of your optimizer changes over time (Default=NULL)}

\item{patience}{Keras parameter for the number of epochs with no improvement after which training will be stopped (Default=2)}

\item{add_batchnorm}{Add an optional batch normalization layer (Default=FALSE)}

\item{...}{Other keras parameters.}
}
\value{
The DNN model.
}
\description{
This function creates a DNN model from the reference dataset (scRNA-seq) by using the keras package.
}
\examples{
#not run
enc <- ds_encoder(data = scaled,genes = gg,dims = 2000,verbose = T,hnodes = c(5000))
features <- ds_get_features(enc = enc,data = scaled,genes=gg)
out <- ds_split_data_encoder(features = features,clus = cluster,prop = 0.8,verbose = T)
model <- ds_dnn_model(out = out,hnodes = c(1000),verbose = T,epochs = 10,batch_size = 32)
}
