#' @name  tosr_process
#' @title Tree of science for subfields
#' @description Convert a graph object into the tree of science
#' @param graph Graph object with bibliometrix data obtained from function tosr_load
#' @param df Bibliometrix data frame obtained from function tosr_load
#' @param nodes Dataframe with nodes atributes obtained from function tosr_load
#' @param number_nodes Scalar number for minimum number of nodes to be considered in the subfield
#' @usage TOS_subfields <- tosr_process(g, df, nodes, number_nodes)
#' @author Sebastian Robledo
#' @return Dataframe with the Tree of science for subfields
#' @examples library(tosr)
#' data_info     <- tosload("scopus.bib") # Create scopus data frame
#' TOS_subfields <- tosr_process(g  = data_info$g,
#'                               df = data_info$df,
#'                            nodes = data_info$nodes,
#'                     number_nodes = 50)
#' @export
#'

tosr_process <- function(graph, df, nodes, number_nodes) {
  subfields <- table(get.vertex.attribute(graph,'subfield'))
  subfields <- as.data.frame(subfields) %>%
    arrange(desc(Freq))

  metricas <- dplyr::tibble(
    id        = V(graph)$name,
    indegree  = degree(graph, mode = "in"),
    outdegree = degree(graph, mode = "out"),
    subfield  = V(graph)$subfield)


  metricas <- metricas %>%
    mutate(year = as.numeric(str_extract(id, "[0-9]{4}")))
  metricas$SAP <- NA

  metricas1 <- metricas %>% filter(subfield == subfields$Var1[1])
  tos1 <- TOS.process(metricas, metricas1, graph)
  tos1$subfield <- 1
  tos1$subfield_graph <- subfields$Var1[1]

  TOSi <- tos1

  n <- 10
  if (length(subfields$Var1) <= 10){
    n <- length(subfields$Var1)
  }

 # for (i in seq(2,n)){
 #   metricasi <- metricas %>% filter(subfield == subfields$Var1[i])
 #   if (length(metricasi$id) <= number_nodes){
 #     break
 #   }
 #   tosi <- TOS.process(metricas, metricasi, graph)
 #   tosi$subfield <- i
 #   tosi$subfield_graph <- subfields$Var1[i]
 #   TOSi <-rbind(TOSi, tosi)
 # }

  options( warn = -1 )
  for (i in seq(2,n)){
    metricasi <- metricas %>% filter(subfield == subfields$Var1[i])

    if (length(metricasi$id) <= number_nodes){
      break
    }

    tosi <- tryCatch({TOS.process(metricas, metricasi, graph)},
                     error=function(cond) {
                       return(NA)
                     },
                     warning=function(cond) {
                       return(NULL)
                     })

    if (is.na(tosi[1])){
      break
    } else{
      tosi$subfield <- i
      tosi$subfield_graph <- subfields$Var1[i]
      TOSi <-rbind(TOSi, tosi)
    }
    #print(i)
  }


  TOSi$id       <- gsub(" ","",TOSi$id)
  nodes$ID_TOS <- gsub(" ","",nodes$ID_TOS)
  df$ID_TOS    <- gsub(" ","",df$ID_TOS)

  TOSi$cite <- NA

  for (i in seq(length(TOSi$id))){
    aux <- nodes$CITE[nodes$ID_TOS %in% TOSi$id[i]]

    if (length(aux) == 0){
      aux <- df$SR_FULL[df$ID_TOS %in% TOSi$id[i]]
      TOSi$cite[i] <- aux[[1]]
    }

    if (length(aux) > 0){
      TOSi$cite[i] <- aux[[1]]
    }

  }


  return(TOSi)

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










