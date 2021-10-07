################################################################################
# Case-Based feature importance
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################

#' @title Feature importance plot
#' @description This plot shows feature importance for given dataset.
#' feature importance is composed of feature power (contribution) and interaction power
#' @import foreach
#' @import parallel
#' @import doParallel
#' @param model A prediction model (Classification or regression)
#' @param train Training dataset (data frame) that is used to building model
#' @param target.name Name of target label name in train dataset
#' @param itr number of iteration of experiment. default is 10
#' @param task Prediction task. "regression" (default) or "classification".
#' @param parallel Apply parallel processing. T of F (default)
#' @param pkg When parallel=T, supported package name for model should be given.
#' @return [graph object] Feature importance plot
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' result1 <- CB_FeatureImp(model1, train=Boston, target.name="medv", itr=50, task="regression")
#' print(result1)
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FeatureImp(model2, train=iris, target.name="Species", itr=50, task="classification")
#' print(result2)
#'
#' # for classification (with parallel processing)
#' library(e1071)
#' model3 <- svm(Species~., data=iris)
#' result3 <- CB_FeatureImp(model3, train=iris, target.name="Species", itr=50, task="classification", parallel=T, pkg="e1071")
#' print(result3)
#'
#' @export
CB_FeatureImp <- function(model, train, target.name, itr=10, task="regression", parallel=F, pkg=NULL) {

  result <- one_FeatureImp(model, train, target.name, seed=100, task=task)

  if (itr >=2) {

    if(!parallel) {
        for (i in 2:itr) {
          tmp <- one_FeatureImp(model, train, target.name, seed=100+i, task=task)
          result$overall[,3:6] <- result$overall[,3:6] + tmp$overall[,3:6]
          result$importance[,3:4] <- result$importance[,3:4] + tmp$importance[,3:4]
          cat("End iteration ", i, "\n")
        }

    } else {   #/ parallel
        cores=detectCores()
        cl<-makeCluster(cores[1]-1) #change the 2 to your number of CPU cores
        registerDoParallel(cl)

        final <- foreach(i = 2:itr, .packages=c(pkg, 'CBFI')) %dopar% {

           tmp <- one_FeatureImp(model, train, target.name, seed=100+i, task=task)
           tmp    # merge to previous tmp
        }
        stopCluster(cl)

        for (i in 2:itr) {
          result$overall[,3:6] <- result$overall[,3:6] + final[[i-1]]$overall[,3:6]
          result$importance[,3:4] <- result$importance[,3:4] + final[[i-1]]$importance[,3:4]
        }



    }

      result$overall[,3:6] <- result$overall[,3:6] / itr
      result$importance[,3:4] <- result$importance[,3:4] / itr

  }
  result$overall <- result$overall[order(result$overall$class),]
  result$importance <- result$importance[order(result$importance$class),]
  return(result)
}



