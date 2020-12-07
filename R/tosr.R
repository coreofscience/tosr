#' @name  tosr
#' @title tosr process
#' @description Load of files, building of graph, and SAP algorithm for Tree of science
#' @param files a data frame with ToS data
#' @usage tosr(files, layout)
#' @author Sebastian Robledo
#' @return list with graph, data frame of citations, subfields and Tree of science
#'
#' @export
#'
tosr <- function(...){

  df    <- tosr_load(...)
  g     <- df$graph
  nodes <- df$nodes

  df1    <- df$df
  message("Computing TOS SAP")
  TOS    <- tryCatch(tosSAP(g,df1,nodes),
                     error=function(cond) {
                       message('Error en Tos SAP')
                       return(NA)
                     },
                     warning=function(cond) {
                       message("Warning en TOS SAP")
                       # Choose a return value in case of warning
                       return(NULL)
                     })

  number_nodes = 50
  message("Computing TOS subfields")
  TOSi   <- tryCatch(tosr_process(g,df1,nodes, number_nodes),
                     error=function(cond) {
                       message('Error en Tos Proces subfields, cambiar numero de nodos')
                       return(NA)
                     },
                     warning=function(cond) {
                       message("Warning en TOS subfields")
                       return(NULL)
                     })

  TOSi   <- tryCatch(tosr_process(g,df1,nodes, number_nodes),
                     error=function(cond) {
                       message('Error en Tos Proces subfields, cambiar numero de nodos')
                       return(NA)
                     },
                     warning=function(cond) {
                       message("Warning en TOS subfields")
                       return(NULL)
                     })

  tabla_completa_TOS   <- tabla_completa(g,TOSi,nodes)


  ToS.info <- list(bibliometrix_df = df1,
               graph               = g,
               ToS_subfields       = TOSi,
               Tos_comple_table    = tabla_completa_TOS,
               TOs_sap             = TOS,
               cited_references    = tosr.cited_ref(df1),
               nodes_atributes     = nodes)
  return(ToS.info)
}


tabla_completa <- function(g,TOSi,nodes){
  tabla_completa_TOS <- data.frame(names = V(g)$name)
  tabla_completa_TOS$subfield <- V(g)$subfield
  tabla_completa_TOS$names <- gsub(" ","",tabla_completa_TOS$names)
  tabla_completa_TOS$TOS  <- NA
  tabla_completa_TOS$Cite <- NA
  tabla_completa_TOS$doi  <- NA

  tabla_completa_TOS$TOS[(tabla_completa_TOS$names %in% TOSi$id)] <- TOSi$TOS[!duplicated(TOSi$id)]

  nodes$ID_TOS <- gsub(" ","",nodes$ID_TOS)


  for (i in seq(length(tabla_completa_TOS$names))){
    aux <- nodes$CITE[nodes$ID_TOS %in% tabla_completa_TOS$names[i]]

    if (length(aux) > 0){
      tabla_completa_TOS$Cite[i] <- aux[[1]]
    }

  }

  doi_regex <- "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"
  tabla_completa_TOS <- tabla_completa_TOS %>%
    mutate(doi = str_extract(tabla_completa_TOS$Cite, doi_regex)) %>%
    mutate(doi = ifelse(is.na(doi),NA,paste0("https://doi.org/",doi)))
  return(tabla_completa_TOS)
}

