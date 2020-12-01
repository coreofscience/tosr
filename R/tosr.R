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
  TOS    <- tosSAP(g,df1,nodes)
  TOSi   <- tosr_process(g,df1,nodes)


  ToS.info <- list(bibliometrix_df = df1,
               graph               = g,
               ToS_subfields       = TOSi,
               TOs_sap             = TOS,
               cited_references    = tosr.cited_ref(df1))
  return(ToS.info)
}
