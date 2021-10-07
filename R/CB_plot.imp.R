################################################################################
# Feature important plot
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################
#' @title Bar plot for feature importance
#' @description This plot shows amount of interaction between two features
#' @param result An object from CB_FeatureImp function
#' @param class A class name, (for classification model)
#' @param combine if combine=F, graph bar split into contribution and interaction (for  regression model)
#' @return [graph object] feature interaction plot
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' result1 <- CB_FeatureImp(model1, train=Boston, target.name="medv", itr=50, task="regression")
#'
#' CB_plot.imp(result1, combine=F)
#' CB_plot.imp(result1, combine=T)
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FeatureImp(model.svm, train=iris, target.name="Species", itr=50, task="classification")
#' print(result2)
#' CB_plot.imp(result2, class="_all_")
#' CB_plot.imp(result2, class="versicolor")
#'
#' @export
CB_plot.imp <- function(result, class="_all_", combine=F) {
  if (result$task=='regression')  { # regression
    result$overall <- result$overall[,-1]
    result$importance <- result$importance[,-1]
    stitle= NULL
  } else {  # classification
    cname <- c(unique(result$overall$class), "_all_")
    if (!(class %in% cname)) {
      print("Error. class name is not correct. "); return(NULL)
    }

    result$overall <- result$overall[result$overall$class==class,-1]
    stitle= paste0("( class: ", class, " )")
  }

  if (result$task=='regression' & combine) {
      df = data.frame(result$importance[,c(1,2)])
      names(df)[2] = "value"

      p <- ggplot(data=df, aes(x=reorder(feature,value), y=value)) +
        geom_bar(stat="identity") +
        ggtitle("Feature Importance", subtitle=stitle) +
        labs( x = "feature")

  } else {
      cnt <- nrow(result$overall)
      df1 = data.frame(result$overall[,c(1,2)], component=rep("own_power", cnt))
      names(df1)[2] = "value"
      df2 = data.frame(result$overall[,c(1,5)], component=rep("interact", cnt))
      names(df2)[2] = "value"

      df  = rbind(df1,df2)
      p <- ggplot(data=df, aes(x=reorder(feature,value), y=value, fill=component)) +
        geom_bar(stat="identity") +
        ggtitle("Feature Importance", subtitle=stitle) +
        labs( x = "feature")

  }

  p <- p + coord_flip()
  p
}
