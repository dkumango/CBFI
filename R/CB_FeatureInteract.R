################################################################################
# Case-Based feature interaction
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################
#' @title Prediction analysis plot for two features
#' @description This plot shows both data distribution and contribution for two features.
#' If prediction task is 'classification', this plot can be generated for specific class.
#' @param model A prediction model (Classification or regression)
#' @param train Training dataset (data frame) that is used to building model
#' @param target.name Name of target label name in train dataset
#' @param F1 Name of first feature
#' @param F2 Name of second feature
#' @param itr number of iteration of experiment. default is 10
#' @param task Prediction task. "regression" (default) or "classification".
#' @param class When task is classification, PA plot can be generated for specific class.
#' If class is NULL, PA plot is generated for all class.
#' @param parallel Apply parallel processing. T of F (default)
#' @param pkg When parallel=T, supported package name for model should be given.
#' @return [graph object] Prediction analysis plot
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' result1 <- CB_FeatureInteract(model.lm, train=Boston, target.name="medv",
#'                               F1="lstat", F2=NULL, itr=50, task="regression")
#' print(result1)
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FeatureInteract(model2, train=iris, target.name="Species",
#'                               F1="Petal.Width", F2=NULL, itr=50, task="classification")
#' print(result2)
#'
#'# for classification (parallel processing)
#' library(e1071)
#' model3 <- svm(Species~., data=iris)
#' result3 <- CB_FeatureInteract(model3, train=iris, target.name="Species",
#'                               F1="Petal.Width", F2=NULL, itr=50, task="classification",
#'                               parallel=T, pkg="e1071")
#' print(result3)
#'
#' @export
CB_FeatureInteract <- function(model, train, target.name, F1, F2=NULL, itr=10, task="regression", parallel=F, pkg=NULL) {
  # Check error ---------------------------------------------
  col.names <- names(train)
  if (!(target.name %in% col.names)) {
    print("target.name is wrong !"); return(NULL)
  }
  if (!(F1 %in% col.names | F1 == target.name)) {
    print("F1 is wrong !"); return(NULL)
  }
  if(!is.null(F2)) if (!(F2 %in% col.names) | F2 == target.name) {
    print("F2 is wrong !"); return(NULL)
  }
  # ---------------------------------------------------------

  f_class <- c(); f_name_1 <- c() ; f_name_2 <- c()
  f_interact <-c(); f_contribute_F1 <- c();
  f_contribute_F2 <- c(); f_contribute_common <- c()

  if (is.null(F2)) {
    candidate <- col.names[col.names!= target.name & col.names!= F1]
  } else {
    candidate <- c(F2)
  }

  if (task=='regression') {

      for (i in 1:length(candidate)) {
        result <- pair_FeatureInteract(model, train, target.name, F1, candidate[i], itr, task=task, parallel=parallel, pkg=pkg)
        f_class[i] <- NA
        f_name_1[i] <- F1
        f_name_2[i] <- candidate[i]
        f_interact[i] <- result$interact
        f_contribute_F1[i] <- result$cont_F1
        f_contribute_F2[i] <- result$cont_F2
        f_contribute_common[i] <- result$cont_common
        cat("End ", f_name_1[i], ":", f_name_2[i], "\n")
      }


  } else { # classification ----------------------------------------

    class.name <- unique(train[,target.name])
    cnt <- 1
    for (cname in class.name) {
      # idx <- which(train[,target.name]== cname)
      # train2 <- train[idx,]

      for (i in 1:length(candidate)) {
        result <- pair_FeatureInteract(model, train, target.name, F1, candidate[i], itr, task=task, class=cname, parallel=parallel, pkg=pkg)
        f_class[cnt] <- cname
        f_name_1[cnt] <- F1
        f_name_2[cnt] <- candidate[i]
        f_interact[cnt] <- result$interact
        f_contribute_F1[cnt] <- result$cont_F1
        f_contribute_F2[cnt] <- result$cont_F2
        f_contribute_common[cnt] <- result$cont_common
        cnt <- cnt+1
      }
        cat("End class: ", cname, "... \n")
    }

    # calculate for all class
    for (i in 1:length(candidate)) {
      f_class[cnt] <- "_all_"
      f_name_1[cnt] <- F1
      f_name_2[cnt] <- candidate[i]
      f_interact[cnt] <- sum(f_interact[f_name_2==candidate[i]], na.rm=T)
      f_contribute_F1[cnt] <- sum(f_contribute_F1[f_name_2==candidate[i]], na.rm=T)
      f_contribute_F2[cnt] <- sum(f_contribute_F2[f_name_2==candidate[i]], na.rm=T)
      f_contribute_common[cnt] <- sum(f_contribute_common[f_name_2==candidate[i]], na.rm=T)
      cnt <- cnt+1
    }

  }

  final <- data.frame(class=f_class, feature1 = f_name_1, feature2 = f_name_2,
                      interact=f_interact, cont_F1=f_contribute_F1,
                      cont_F2=f_contribute_F2, cont_common=f_contribute_common)
  return(list(task=task, data=final))
}





