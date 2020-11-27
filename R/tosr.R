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
tosr <- function(..., layout = "layout_with_fr"){

  files = c(...)
  df    = tosr_load(files)
  g1    = tosr_graph(df)
  ToS   = tosr_process(g1)
  ToS.info = list(bibliometrix_df = df[[1]],
               graph              = g1[[1]],
               ToS_subfields      = ToS)
  return(ToS.info)
}
