% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CB_plot.contribute.R
\name{CB_plot.contribute}
\alias{CB_plot.contribute}
\title{Bar plot for contribution between two features}
\usage{
CB_plot.contribute(result, class = "_all_")
}
\arguments{
\item{result}{An object from CB_FeatureInteract function}

\item{class}{A class name, (for classification model)}
}
\value{
[graph object] contribution plot
}
\description{
This plot shows amount of contribution between two features.
This graph is only for classification model
}
\examples{
library(CBFI)
# for classification
library(e1071)
model2 <- svm(Species~., data=iris)
result2 <- CB_FeatureInteract(model2, iris, "Species", F1="Petal.Width", F2="Petal.Length",
                              itr=10, task="classification")
CB_plot.contribute(result2, class="_all_")
CB_plot.contribute(result2, class="setosa")
}
