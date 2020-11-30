#' @name  tosr_process
#' @title Creating our ToS
#' @description Convert a graph object into the tree of science
#' @param graph a graph object with scopus data
#' @usage tosprocess(graph)
#' @author Sebastian Robledo
#' @return a dataframe with the Tree of science
#' @examples library(treeofscience)
#'           socopus_df <- tosload("scopus.bib") # Create scopus data frame
#'           graph.scopus <- tosgraph(scopus_df) # Create graph from scopus data frame
#'           TOS <- tosprocess(graph.scopus)     # Create Tree of Science
#' @export
#'

tosr_process <- function(g) {
  subfields <- table(get.vertex.attribute(g,'sub_field'))
  subfields <- as.data.frame(subfields) %>%
    arrange(desc(Freq))

  metricas <- dplyr::tibble(
    id        = V(g)$name,
    indegree  = degree(g, mode = "in"),
    outdegree = degree(g, mode = "out"),
    bet       = betweenness(g),
    subfield  = V(g)$sub_field)
  metricas <- metricas %>%
    mutate(year = as.numeric(str_extract(id, "[0-9]{4}")))
  metricas$SAP <- NA

  metricas1 <- metricas %>% filter(subfield == subfields$Var1[1])
  tos1 <- TOS.process(metricas, metricas1, g)
  tos1$subfield <- 1

  metricas2 <- metricas %>% filter(subfield == subfields$Var1[2])
  tos2 <- TOS.process(metricas, metricas2, g)
  tos2$subfield <- 2

  metricas3 <- metricas %>% filter(subfield == subfields$Var1[3])
  tos3 <- TOS.process(metricas, metricas3, g)
  tos3$subfield <- 3


  tos      <- rbind(tos1, tos2, tos3)
  return(tos)

}


TOS.process <- function(metricas, metricasi, g){
  act.year <- as.numeric(format(Sys.Date(),'%Y'))
  Raices   <- metricasi[metricasi$outdegree == 0, c("id","indegree")] %>%
    arrange(desc(indegree))
  Raices <- Raices[1:10,]
  names(Raices)[names(Raices) == 'indegree'] <- 'SAP'

  # Clasificacion de las hojas
  Hojas.ext <- metricasi[metricasi$indegree == 0, c("id","outdegree","year")]
  Hojas.ext <- Hojas.ext %>%
    mutate(antiguedad = act.year - year) %>%
    arrange(antiguedad)
  Hojas     <- filter(Hojas.ext, antiguedad <= 5)

  # Se determina el numero del vertice de las Hojas
  num.vertices.hojas <- c()
  for (vertice in Hojas$id){
    num.vertices.hojas <- c(num.vertices.hojas,which(metricas$id == vertice))
  }

  # Calculo del SAP de las Hojas
  SAP_hojas <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(g,
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

  # Se determina el numero del vertice de las raices
  num.vertices.raices <- c()
  for (vertice in Raices$id){
    num.vertices.raices <- c(num.vertices.raices,which(metricas$id == vertice))
  }

  Caminos <- c()
  for (vert in Hojas$id){
    for (raiz in Raices$id){
      h <- get.all.shortest.paths(g,
                                  from = vert,
                                  to   = raiz,
                                  mode = "out")
      lista.nodos <- unique(unlist(h[1]))
      lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
      lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
      metricas$SAP[lista.nodos] <- Raices[Raices$id == raiz,2]+Hojas[Hojas$id == vert,5]
      Caminos     <- c(Caminos,lista.nodos)
    }
  }

  Tronco     <- metricas[unique(Caminos), c("id","indegree","year","SAP")]

  if (length(Tronco$id) == 0){
    Raices$TOS <- "Root"
    Hojas$TOS  <- "Leaves"
    TOSi       <- rbind(Raices[,c(1,2,3)], Hojas[,c(1,5,6)])


  } else{
    mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
    Tronco$SAP <- as.numeric(Tronco$SAP)
    Tronco     <- Tronco %>%
      mutate(antiguedad = mas.nuevo - year) %>%
      arrange(desc((SAP)))

    # Tree of science
    Raices$TOS <- "Root"
    Hojas$TOS  <- "Leaves"
    Tronco$TOS <- "Trunk"

    TOSi   <- rbind(Raices[,c(1,2,3)], Tronco[,c(1,4,6)], Hojas[,c(1,5,6)])
  }
  return(TOSi)
}










