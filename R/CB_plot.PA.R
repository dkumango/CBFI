################################################################################
# Feature Prediction analysis plot for two features
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################
#' @import ggplot2
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
#' If class is "_all_", PA plot is generated for all class.
#' @return [graph object] Prediction analysis plot
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' CB_plot.PA(model1, Boston, "medv", F1="dis", F2="rad", itr=10, task="regression")
#' CB_plot.PA(model1, Boston, "medv", F1="lstat", F2="rm", itr=10, task="regression")
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' CB_plot.PA(model2, iris, "Species", F1="Petal.Width", F2="Petal.Length", itr=10, task="classification")
#' CB_plot.PA(model2, iris, "Species", F1="Petal.Width", F2="Petal.Length", itr=10, task="classification", class="setosa")
#'
#' @export
CB_plot.PA <- function(model, train, target.name, F1, F2, itr=10, task="regression", class="_all_") {
  # Error check ------------------------------------------------------
  feature.name <- names(train)
  if (!(target.name %in% feature.name)) {
    print("Name oftarget.name is wrong..")
    return(NULL)
  }
  if (!(F1 %in% feature.name) | F1 == target.name) {
    print("Name of F1 is wrong..")
    return(NULL)
  }
  if (!(F2 %in% feature.name| F1 == target.name)) {
      print("Name of F2 is wrong..")
      return(NULL)
  }
  if (!(task %in% c("regression" ,"classification"))) {
    print("task should be one of 'regression' or 'classification'.. ")
    return(NULL)
  }
  if (class != "_all_")
    if (!(class %in% unique(train[,target.name]))) {
      print("Name of class is wrong.. ")
      return(NULL)
    }
  # -------------------------------------------------------------------

  print("generate data for plot ....")
  groups <- generate.data(model, train, target.name, F1, F2, itr, task)
  if (task=="regression") {
    groups[groups=='ERR'] <- NA
    groups <- factor(groups, levels=c("G1","G2","G3","G4pos","G4neg"))
  } else {
    groups <- factor(groups, levels=c("G1","G2","G3","G4","ERR"))
  }
  mycolor <- c("#f8766d", "#7cae00", "#c77cff", "#00bfc4", "#fad02c" )

  if (task=="regression") {
    legend.label <-  c(paste0(F1, " decrease"),
                       paste0(F2, " decrease"),
                       "No change",
                       "Positive interact",
                       "negative interact")
  } else {
    legend.label <- c(paste0(F1, " contribute"),
                      paste0(F2, " contribute"),
                      "Common contribute", "Interact",
                      "wrong predict")
  }

  df <- data.frame(F1=train[,F1], F2=train[,F2], groups)
  df <- df[!is.na(df$groups),]

  if (task=='classification' &  class != "_all_") {
    idx <- which(train[,target.name]==class)
    df <- df[idx, ]
  }


  x.cell.boundary <- diff(sort(df$F1))
  y.cell.boundary <- diff(sort(df$F2))

  w <- mean(x.cell.boundary)*5
  h <- mean(y.cell.boundary)*3

  tbl <- table(df$groups)
  idx <- which(tbl==0)
  if(length(idx)>0) {
    legend.label <- legend.label[-idx]
     mycolor <- mycolor[-idx]
  }
  if (class != "_all_") {
    my_sub_title <- paste0(F1," - ", F2, " (class: ", class, " )")
  } else {
    my_sub_title <- paste0(F1," - ", F2)
  }
  p <- ggplot(df, aes(x=F1, y=F2, fill=groups)) +
        geom_tile(na.rm=T,  width=w, height=h) +
        ggtitle(label="Prediction Analysis",
                subtitle = my_sub_title) +
        scale_fill_manual(labels = legend.label,
                          values = mycolor) +
        labs(x=F1,y=F2) +
        xlim(min(train[,F1]), max(train[,F1]))+
        ylim(min(train[,F2]), max(train[,F2]))

  return(p)

}

