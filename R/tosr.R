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
  cited_references <- tosr.cited_ref(df)
  g1    <- tosr_graph(df)
  ToS   <- tosr_process(g1)
  ToS.info <- list(bibliometrix_df = df,
               graph              = g1,
               ToS_subfields      = ToS,
               cited_references   = cited_references)
  return(ToS.info)
}
