#' @name  tosr_visualization
#' @title Visualization of TOS information
#' @description Create a visualization of the Tree of Science using visNetwork
#' @param tosdata,layout a data frame with ToS data
#' @usage tosvisualization(graph.tos, TOS, layout)
#' @author Sebastian Robledo
#' @return Visualization
#'
#' @export
#'

tosvisualization <- function(tosdata, layout){

  graph.tos  <- tosdata$graph
  TOS        <- tosdata$TOS
  data       <- visNetwork::toVisNetworkData(graph.tos)

  tosnodes <- strsplit(TOS$id,split = ",")
  labels    <- c()
  year     <- c()
  DOI      <- c()
  autor    <- c()

  for (i in 1:length(tosnodes)){
    #nodes <- c(nodes,paste(tosnodes[[i]][1],tosnodes[[i]][2],sep = "\n"))
    labels <- c(labels,i)
    autor  <- c(autor,tosnodes[[i]][1])
    year   <- c(year,tosnodes[[i]][2])
    DOI    <- c(DOI,tosnodes[[i]][6])
  }

  nodes <- data.frame(data$nodes, group = TOS$TOS)
  nodes$label <- paste(autor,year,sep = ",")
  nodes$value[nodes$group == "Root"] <- 30
  nodes$value[nodes$group == "Trunk"] <- 20
  nodes$value[nodes$group == "Leaves"] <- 10

  edges <- data.frame(from = data$edges[,1], to = data$edges[,2])

  layaouts <- c("layout_as_star","layout_in_circle","layout_nicely",
                "layout_on_grid","layout_on_sphere","layout_randomly","layout_with_dh",
                "layout_with_fr","layout_with_gem","layout_with_kk",
                "layout_with_lgl","layout_with_mds","layout_with_sugiyama")

  visNetwork::visNetwork(nodes, data$edges) %>%
    visNetwork::visIgraphLayout(layout = layout) %>%
    visNetwork::visEdges(arrows = "to") %>%
    visNetwork::visNodes(size = 20) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T,degree = T),nodesIdSelection = T,selectedBy = "group") %>%
    visNetwork::visGroups(groupname = "Root", color = "green") %>%
    visNetwork::visGroups(groupname = "Trunk", color = "blue") %>%
    visNetwork::visGroups(groupname = "Leaves", color = "red") %>%
    visNetwork::visLegend(width = 0.1, position = "right", main = "Group")
}
