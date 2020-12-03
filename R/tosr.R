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

  ToS.info <- list(bibliometrix_df = df1,
               graph               = g,
               ToS_subfields       = TOSi,
               TOs_sap             = TOS,
               cited_references    = tosr.cited_ref(df1))
  return(ToS.info)
}




