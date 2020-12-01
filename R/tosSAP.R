#' @name  tosSAP
#' @title SAP algorithm for tree of science
#' @description Compute tree of science
#' @param graph,bibliometrix_object,node_atributes
#' @usage tosSAP(graph,df,nodes)
#' @author Sebastian Robledo
#' @return Dataframe with tree of science
#'
#' @export
#

tosSAP <- function(graph,df,nodes){

  # Se crean la metricas de la red

  metricas.red <- tibble(
    id        = V(graph)$name,
    indegree  = degree(graph, mode = "in"),
    outdegree = degree(graph, mode = "out"),
    bet       = betweenness(graph))

  metricas.red <- metricas.red %>%
    mutate(year = as.numeric(str_extract(id, "[0-9]{4}")))

  # Clasificacion de las raices

  Raices <- metricas.red[metricas.red$outdegree == 0, c("id","indegree")] %>%
    arrange(desc(indegree))
  Raices <- Raices[1:10,]


  # Clasificacion de las hojas
  Hojas.ext <- metricas.red[metricas.red$indegree == 0, c("id","outdegree","year")]
  act.year  <- as.numeric(format(Sys.Date(),'%Y'))
  Hojas.ext <- Hojas.ext %>%
    mutate(antiguedad = act.year - year) %>%
    arrange(antiguedad)
  Hojas     <- filter(Hojas.ext, antiguedad <= 5)

  # Se determina el numero del vertice de las Hojas
  num.vertices.hojas <- c()
  for (vertice in Hojas$id){
    num.vertices.hojas <- c(num.vertices.hojas,which(metricas.red$id == vertice))
  }

  # Se determina el numero del vertice de las raices
  num.vertices.raices <- c()
  for (vertice in Raices$id){
    num.vertices.raices <- c(num.vertices.raices,which(metricas.red$id == vertice))
  }

  # Calculo del SAP de las Hojas
  SAP_hojas <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(graph,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")

    SAP_hojas   <- c(SAP_hojas, length(h[[1]]))
  }
  Hojas <- Hojas %>%
    mutate(SAP = SAP_hojas) %>%
    arrange(desc(SAP))

  Hojas <- Hojas[1:60,] %>%
    filter(SAP > 0)

  Caminos   <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(graph,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")
    lista.nodos <- unique(unlist(h[1]))
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
    Caminos     <- c(Caminos,lista.nodos)
  }



  # Seleccion del tronco

  Tronco     <- metricas.red[unique(Caminos), c("id","indegree","year")]
  mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
  Tronco     <- Tronco %>%
    mutate(antiguedad = mas.nuevo - year)



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
  return(TOS)
}
