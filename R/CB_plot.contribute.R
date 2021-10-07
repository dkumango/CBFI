################################################################################
# Feature contribution plot
#
# Author : Sejong Oh
# 2021.07.011
# Note : This code is not optimized
################################################################################
#' @title Bar plot for contribution between two features
#' @description This plot shows amount of contribution between two features.
#' This graph is only for classification model
#' @param result An object from CB_FeatureInteract function
#' @param class A class name, (for classification model)
#' @return [graph object] contribution plot
#' @examples
#' library(CBFI)
#' # for classification
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' result2 <- CB_FeatureInteract(model2, iris, "Species", F1="Petal.Width", F2="Petal.Length",
#'                               itr=10, task="classification")
#' CB_plot.contribute(result2, class="_all_")
#' CB_plot.contribute(result2, class="setosa")
#' @export
CB_plot.contribute <- function(result, class="_all_") {
  if (result$task!="classification") {
    print("This plot is only for classification model")
    return(NULL)
  }

  if (!(class %in% result$data$class)) {
    print("Error. class name is wrong!")
    return(NULL)
  }

  result <- result$data[result$data$class==class,]
  F1 <- result[,2]
  F2 <- result[,3]
  df <- data.frame(result[,-c(1:3)])
  df <- t(df)
  df <- data.frame(labels=c("Interact", "F1","F2", "Common"), df)
  names(df)[2] <- "values"
  df$labels = factor(df$labels, levels=c("F1", "F2", "Interact",  "Common"))
  stitle <-  paste0(F1," - ", F2, " ( class: ", class, " )")

  p <- ggplot(data=df, aes(x="", y=values, fill=labels)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    ggtitle(label="Feature contribution", subtitle =stitle) +
    theme_void()

  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_discrete( labels = c(paste0(F1, " contribute"),  paste0(F2, " contribute"), "Interact","Common contribute"))

  return(p)

}

