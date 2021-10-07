#' Draw feature interaction graph
#'
#' This function Draw feature interaction graph.
#'
#' @import tcltk
#' @import igraph
#' @param gtype Graph style. 'S' (default) is for static graph and 'I' is for interactive graph.
#' @param FIobj Result object from FItable function.
#' @param task Type of prediction task, 'regression' (default), 'Classification'
#' @param class.name It is for classification task. You can draw feature interaction graph for specific class.
#' @param show.edge.weight Decide if edge weight value is displayed or not.
#' @param seed According to seed value, The shape of graph is changed.
#' @param th Threshold for showing edge. If th is NULL, th is mean of interaction values of all pair of teatures
#' @return Feature interaction graph
#' @examples
#' library(CBFI)
#' # for regression
#' data("Boston", package = "MASS")
#' model1 <- lm(medv ~ ., data = Boston)
#' FIobj1 <-CB_FItable(model1, Boston, "medv", itr=50, task="regression")
#'
#' CB_plot.FIgraph(gtype="S", FIobj1, task="regression", class="_all_",
#'                show.edge.weight=TRUE, seed=104, th=NULL)
# # Interactive plot
#' CB_plot.FIgraph(gtype="I", FIobj1, task="regression", class="_all_",
#'                    show.edge.weight=TRUE, seed=104, th=NULL)#' # for classification
#'
#' library(e1071)
#' model2 <- svm(Species~., data=iris)
#' FIobj2 <-  CB_FItable(model2, iris, "Species", itr=50, task="classification")
#'
#' # static plot
#' CB_plot.FIgraph( gtype="S", FIobj=FIobj, task="classification", class="_all_",
#'                 show.edge.weight=TRUE, seed=104)
#' CB_plot.FIgraph( gtype="S", FIobj=FIobj, task="classification", class="setosa",
#'                 show.edge.weight=TRUE, seed=104)
#' # interactive plot
#' CB_plot.FIgraph( gtype="I", FIobj=FIobj, task="classification", class="_all_",
#'                 show.edge.weight=TRUE, seed=104)
#'
#' # Please ignore warning error after drawing interactive graph.
#'
#' @export
CB_plot.FIgraph <- function(gtype="S", FIobj, task="regression", class.name="_all_",
                    show.edge.weight=TRUE, seed=100, th=NULL) {
  # check error
  c.name <- c(unique(FIobj$Fint$class), "_all_")
  # if (length(c.name) > 2 & class.name == "_all_") {
  #   print("Error! You should give 'class.name'")
  #   return(NULL)
  # }
  if(task=="classification")
    if (!(class.name %in% c.name) ) {
      cat("Error! 'class.name' is one of [", c.name, "] \n")
      return(NULL)
    }

  # Get Feature interation  & feature importance
  result <- FIobj$Fint   # feature interaction table
  myimp  <- FIobj$Fimp   # feature importance table

  # filter low interaction
  if(is.null(th)) {
    th <- mean(abs(result$weight))
    if (task=='classification') th <- mean(abs(result$weight[result$class==class.name]))
  }
  result <- result[abs(result$weight)>=th,]
  cat("th: ", th, "\n")

  ## Draw interaction Graph #############################################


  if (task=="regression") {
    edges <- result[,-1]
    nodes <- myimp[,-1]
  } else {
    if (is.null(class.name)) class.name = "all"
    edges <- result[result$class==class.name,-1]
    nodes <- data.frame(myimp[myimp$class==class.name,-1])
  }

  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)

  # node size
  impsize <- (V(net)$importance - min(V(net)$importance)) /
    (max(V(net)$importance)-min(V(net)$importance))
  impsize <- ((impsize/1.2)+0.2) *50
  V(net)$size <- impsize # V(net)$importance*9  # node size

  # node color
  pal1 <- heat.colors(nrow(nodes), alpha=1)
  idx <- (V(net)$importance - min(V(net)$importance)) /
    (max(V(net)$importance)-min(V(net)$importance))
  idx <- nrow(nodes)-as.integer(idx*(nrow(nodes)-1)+1)+1
  V(net)$color <- pal1[idx]


  E(net)$label <- edges$weight     # edge weight
  #E(net)$label.cex=1
  #E(net)$label.font=2

  # edge width
  mymax <- max(abs(E(net)$weight)); mymin <- min(abs(E(net)$weight))
  ewidth <- (abs(E(net)$weight) - mymin)/
    (mymax - mymin)
  ewidth <- (ewidth + 0.2)*7
  E(net)$width <- ewidth # edges$weight*20  # edge width

  # edge color
  E(net)$color <- "gray78"
  E(net)$color[E(net)$weight < 0] <- "orange1"

  # Draw graph

  mytitle <- NULL
  if (task=="classification") mytitle <- paste0("class: ", class.name)

  if(!show.edge.weight) {
    elabel <- NA
  } else {
    elabel <- E(net)$value
  }

  set.seed(seed)
  if (gtype=="S") {  # static graph
    plot(net, layout=layout.circle, edge.arrow.size=.4, vertex.label=V(net)$nodes,
         edge.label=elabel, main=mytitle)

  } else {            # interactive graph
    id <- tkplot(net, layout=layout.circle, edge.label=elabel)
    canvas = tk_canvas(id)

    width = as.numeric(tkcget(canvas, "-width"))
    height = as.numeric(tkcget(canvas, "-height"))
    tkcreate(canvas, "text", width/2, 25, text=mytitle,
             justify="center",
             font=tkfont.create(family="helvetica", size=12, weight="bold"))

    #tk_close(id, window.close = T)
  }

}

