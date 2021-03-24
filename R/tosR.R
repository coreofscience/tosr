#' @name  tosR
#' @title tosR process
#' @description This function load de scopus and web of science files and generate
#'              the graph (relations betwen the cited references), the bibliometrix
#'              dataframe object, the tree of science for the subfields, the tree of
#'              science for all the cited references,
#' @param ... Names of scopus and web of science files
#' @usage data_info <- tosR(...)
#' @author Sebastian Robledo
#' @return list with graph, data frame of citations, subfields and Tree of science
#' @examples library(tosr)
#' tosinfo <- tosR('scopus.bib','save.txt') # Tos process
#' tos.df <- tosinfo$bibliometrix_df # Bibliometrix dataframe
#' tos.g <- tosinfo$graph # Graph
#' tos.tos_sub <- tosinfo$ToS_subfields # Tree of science subfields
#' tos.cited <- tosinfo$cited_references # Cited references
#' tos.sap <- tosinfo$TOs_sap # Tree of science
#' complete_cites <- tosinfo$Tos_comple_table # Complete cites
#' @export
#'
tosR <- function(...){

  info    <- tosr_load(...)
  g     <- info$graph
  nodes <- info$nodes
  biblio_wos_scopus   <- info$df

  message("Computing TOS SAP")
  TOS    <- tryCatch(tosSAP(g,biblio_wos_scopus,nodes),
                     error=function(cond) {
                       message('Error en Tos SAP')
                       return(NA)
                     },
                     warning=function(cond) {
                       message("Warning en TOS SAP")
                       # Choose a return value in case of warning
                       return(NULL)
                     })

  message("Computing TOS subfields")
  TOSi   <- tryCatch(tosr_process(g,biblio_wos_scopus,nodes, number_nodes = 50),
                     error=function(cond) {
                       message('Error en Tos Proces subfields, cambiar numero de nodos')
                       return(NA)
                     })

 # TOSi   <- tryCatch(tosr_process(g,df1,nodes, number_nodes),
  #                   error=function(cond) {
   #                    message('Error en Tos Proces subfields, cambiar numero de nodos')
  #                     return(NA)
   #                  },
  #                   warning=function(cond) {
   #                    return(NULL)
   #                  })

  tabla_completa_TOS   <- tabla_completa(g,TOSi,nodes)


  ToS.info <- list(bibliometrix_df = biblio_wos_scopus,
               graph               = g,
               ToS_subfields       = TOSi,
               Tos_comple_table    = tabla_completa_TOS,
               TOs_sap             = TOS,
               cited_references    = tosr.cited_ref(biblio_wos_scopus),
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

  suppressWarnings(tabla_completa_TOS$TOS[tabla_completa_TOS$names %in% TOSi$id]<- TOSi$TOS)
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

