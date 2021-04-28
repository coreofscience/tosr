#' @name  tosSAP
#' @title SAP algorithm for tree of science
#' @description Compute tree of science using SAP algorithm.
#'              see: https://revistas.unal.edu.co/index.php/ingeinv/article/view/77718/0
#' @param graph A graph object with bibliometrix data obtained from function tosr_load
#' @param df Bibliometrix data frame obtained from function tosr_load
#' @param nodes Dataframe with nodes atributes obtained from function tosr_load
#' @author Sebastian Robledo
#' @return Dataframe with tree of science
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' my_tos <- tosSAP(my_tos_load$graph, my_tos_load$df, my_tos_load$nodes)
#' }


tosSAP <- function(graph,df,nodes){

  # Network metrics are created

  metricas.red <- tibble::tibble(
    id        = igraph::V(graph)$name,
    indegree  = igraph::degree(graph, mode = "in"),
    outdegree = igraph::degree(graph, mode = "out"),
    bet       = igraph::betweenness(graph))

  metricas.red <- metricas.red %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$id, "[0-9]{4}")))

  # Roots

  Raices <- metricas.red[metricas.red$outdegree == 0, c("id","indegree")] %>%
    dplyr::arrange(dplyr::desc(.data$indegree))
  Raices <- Raices[1:10,]


  # leaves

  Hojas.ext <- metricas.red[metricas.red$indegree == 0, c("id","outdegree","year")]
  act.year  <- as.numeric(format(Sys.Date(),'%Y'))
  Hojas.ext <- Hojas.ext %>%
    dplyr::mutate(antiguedad = act.year - .data$year) %>%
    dplyr::arrange(.data$antiguedad)
  Hojas     <- dplyr::filter(Hojas.ext, .data$antiguedad <= 5)

  # Number of vertices of leaves

  num.vertices.hojas <- c()
  for (vertice in Hojas$id){
    num.vertices.hojas <- c(num.vertices.hojas,which(metricas.red$id == vertice))
  }

  # Number of vertices of roots

  num.vertices.raices <- c()
  for (vertice in Raices$id){
    num.vertices.raices <- c(num.vertices.raices,which(metricas.red$id == vertice))
  }

  # SAP for leaves

  SAP_hojas <- c()
  for (vert in Hojas$id){
    h <- igraph::get.all.shortest.paths(graph,
                                        from = vert,
                                        to   = Raices$id,
                                        mode = "out")

    SAP_hojas   <- c(SAP_hojas, length(h[[1]]))
  }
  Hojas <- Hojas %>%
    dplyr::mutate(SAP = SAP_hojas) %>%
    dplyr::arrange(dplyr::desc(.data$SAP))

  Hojas <- Hojas[1:60,] %>%
    dplyr::filter(.data$SAP > 0)

  Caminos   <- c()
  for (vert in Hojas$id){
    h <- igraph::get.all.shortest.paths(graph,
                                        from = vert,
                                        to   = Raices$id,
                                        mode = "out")
    lista.nodos <- unique(unlist(h[1]))
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
    Caminos     <- c(Caminos,lista.nodos)
  }



  # Trunk selection

  Tronco     <- metricas.red[unique(Caminos), c("id","indegree","year")]
  mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
  Tronco     <- Tronco %>%
    dplyr::mutate(antiguedad = mas.nuevo - .data$year)



  # Tree of science

  Raices$TOS <- "Root"
  Hojas$TOS  <- "Leaves"
  Tronco$TOS <- "Trunk"

  TOS       <- rbind(Raices[,c(1,3)], Tronco[,c(1,5)], Hojas[,c(1,6)])

  TOS$id       <- gsub(" ","",TOS$id)
  nodes$ID_TOS <- gsub(" ","",nodes$ID_TOS)
  df$ID_TOS    <- gsub(" ","",df$ID_TOS)

  TOS$cite <- NA

  for (i in seq(length(TOS$id))){
    aux <- nodes$CITE[nodes$ID_TOS %in% TOS$id[i]]

    if (length(aux) == 0){
      aux <- df$SR_FULL[df$ID_TOS %in% TOS$id[i]]
      TOS$cite[i] <- aux[[1]]
    }

    if (length(aux) > 0){
      TOS$cite[i] <- aux[[1]]
    }

  }
  return(TOS %>% select(-id))
}
