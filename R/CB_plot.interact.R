################################################################################
# Feature interaction plot
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################
#' @import ggplot2
#' @title Bar plot for interaction between two features
#' @description This plot shows amount of interaction between two features
#' @param result An object from CB_FeatureInteract function
#' @param class A class name, if result is from classification model
#' @return [graph object] feature interaction plot
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' result1 <- CB_FeatureInteract(model.lm, train=Boston, target.name="medv", F1="lstat", F2=NULL, itr=50, task="regression")
#'
#' print(result1)
#' CB_plot.interact(result1)
#'
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FeatureInteract(model2, train=iris, target.name="Species", F1="Petal.Width", F2=NULL, itr=50, task="classification")
#' CB_plot.interact(result2, class="_all_")
#' CB_plot.interact(result2, class="setosa")
#'
#' result3 <- CB_FeatureInteract(model2, train=iris, target.name="Species", F1="Petal.Width", F2="Petal.Length", itr=50, task="classification")
#' CB_plot.interact(result3, class="_all_")
#' CB_plot.interact(result3, class="versicolor")
#' #'
#' @export
CB_plot.interact <- function(result, class="_all_") {
  mymin <- floor(min(result$data$interact)) ; mymax <- max(result$data$interact)
  if (abs(0-mymin) < 0.01 | nrow(result$data)==1) mymin=0

  if (result$task == "classification") {

      if (class=="_all_") {
        result <- result$data[result$data$class!=class,]
        df <- data.frame(features=paste0(result$feature2,"-",result$feature1),
                         interact=result$interact, class=result$class)

        p <- ggplot(data=df, aes(x=reorder(features,interact), y=interact,  fill=class)) +
          geom_bar(stat="identity") +
          ggtitle("Feature Interaction",
                  subtitle = paste0("( class: ", class, " )")) +
          labs(x = "features", y="value") +
          ylim(mymin, mymax)

      } else {
        result <- result$data[result$data$class==class,]
        df <- data.frame(features=paste0(result$feature2,"-",result$feature1),
                           interact=result$interact, class=result$class)

          p <- ggplot(data=df, aes(x=reorder(features,interact), y=interact, fill=class)) +
            geom_bar(stat="identity") +
            ggtitle("Feature Interaction",
                    subtitle = paste0("( class: ", class, " )")) +
            labs(x = "features", y="value") +
            ylim(mymin, mymax)
      }

  } else {  # regression -------------------------------------------------
      df <- data.frame(features=paste0(result$data$feature2,"-",result$data$feature1),
                       interact=result$data$interact)

      p <- ggplot(data=df, aes(x=reorder(features,interact), y=interact)) +
        geom_bar(stat="identity") +
        ggtitle("Feature Interaction") +
        labs(x = "features", y="value") +
        ylim(mymin, mymax)

  }

  p <- p + coord_flip()

  return(p)
}
