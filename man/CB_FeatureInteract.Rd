% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CB_FeatureInteract.R
\name{CB_FeatureInteract}
\alias{CB_FeatureInteract}
\title{Prediction analysis plot for two features}
\usage{
CB_FeatureInteract(
  model,
  train,
  target.name,
  F1,
  F2 = NULL,
  itr = 10,
  task = "regression",
  parallel = F,
  pkg = NULL
)
}
\arguments{
\item{model}{A prediction model (Classification or regression)}

\item{train}{Training dataset (data frame) that is used to building model}

\item{target.name}{Name of target label name in train dataset}

\item{F1}{Name of first feature}

\item{F2}{Name of second feature}

\item{itr}{number of iteration of experiment. default is 10}

\item{task}{Prediction task. "regression" (default) or "classification".}

\item{parallel}{Apply parallel processing. T of F (default)}

\item{pkg}{When parallel=T, supported package name for model should be given.}

\item{class}{When task is classification, PA plot can be generated for specific class.
If class is NULL, PA plot is generated for all class.}
}
\value{
[graph object] Prediction analysis plot
}
\description{
This plot shows both data distribution and contribution for two features.
If prediction task is 'classification', this plot can be generated for specific class.
}
\examples{
library(CBFI)
# for regression
data("Boston", package = "MASS")
model1 <- lm(medv ~ ., data = Boston)
result1 <- CB_FeatureInteract(model.lm, train=Boston, target.name="medv",
                              F1="lstat", F2=NULL, itr=50, task="regression")
print(result1)

# for classification
library(e1071)
model2 <- svm(Species~., data=iris)
result2 <- CB_FeatureInteract(model2, train=iris, target.name="Species",
                              F1="Petal.Width", F2=NULL, itr=50, task="classification")
print(result2)

# for classification (parallel processing)
library(e1071)
model3 <- svm(Species~., data=iris)
result3 <- CB_FeatureInteract(model3, train=iris, target.name="Species",
                              F1="Petal.Width", F2=NULL, itr=50, task="classification",
                              parallel=T, pkg="e1071")
print(result3)

}
