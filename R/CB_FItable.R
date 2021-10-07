#' Create a feature interaction table
#'
#' This function create a feature interaction table.
#'
#' @param model A prediction model (Classification or regression)
#' @param train Training dataset (data frame) that is used to building model
#' @param target.name Name of target label name in train dataset
#' @param itr Number of experiments. Default is 50
#' @param task Prediction task. "regression" (default) or "classification".
#' @param class class name (for classification model)
#' @param parallel Apply parallel processing. T of F (default)
#' @param pkg When parallel=T, supported package name for model should be given.
#' @return [list] feature interaction & feature importance table
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' result1 <- CB_FItable(model1, Boston, "medv", itr=50, task="regression")
#' print(result1)
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FItable(model2, iris, "Species", itr=50, task="classification")
#' print(result2)
#'
#' # for classification (parallel processing)
#' library(e1071)
#' model3 <- svm(Species~., data=iris)
#' result3 <- CB_FItable(model3, iris, "Species", itr=50, task="classification",
#'                       parallel=T, pkg="e1071")
#' print(result3)
#'
#' @export
CB_FItable <- function(model, train, target.name, itr=50, task="regression",
                    class="_all_", parallel=F, pkg=NULL) {
  # error handling
  if (!(task %in% c("regression", "classification"))) {
    print("task should be one of regression', 'classification'")
    return(NULL)
  }
  if(task=="classification") {
    cname <- c(unique(train[, target.name]), "_all_")
    if(!(class %in% cname)) {
      print("Error. class name is wrong!"); return(NULL)
    }
  }

  train2 <- train[,-which(names(train)==target.name)]
  cl <-  train[,target.name]

  ## Get pairwise interaction value ------------------------------------------
  cl.names <- names(train2)
  cbn <- combn(cl.names,2)

  tmp <- CB_FeatureInteract(model, train, target.name, F1=cbn[1,1], F2=cbn[2,1], itr=itr, task=task)
  interact <- tmp$data


  if (ncol(cbn) >= 2 )
    for (i in 2:ncol(cbn)) {
       tmp <- CB_FeatureInteract(model, train, target.name, F1=cbn[1,i], F2=cbn[2,i], itr=itr, task=task,
                                 parallel=parallel, pkg=pkg)
       interact <- rbind(interact, tmp$data)
    }

  myint <- interact[,1:4]

  names(myint) <- c("class", "from", "to", "weight")


  ## Get feature importance --------------------------------------------------
  imp <- CB_FeatureImp(model, train, target.name, itr=itr, task=task)
  myimp <- data.frame(imp$importance[,1:3])
  names(myimp) <- c("class","feature", "importance")

  myint$weight <- round(myint$weight, 3)
  myimp$importance <- round(myimp$importance, 3)

  return(list(Fint=myint, Fimp=myimp))
}

