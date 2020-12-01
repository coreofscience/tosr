#' @name  tosr_bibliometix_analysis
#' @title bibliometric analysis
#' @description Compute the bibliometric analysis using bibliometrix library
#' @param bibliometrix_objetc a bibliometix objetc
#' @usage tosr(df)
#' @author Sebastian Robledo
#' @return bibliometrix analysis plots
#'
#' @export
#'

tosr_bibliometix_analysis <- function(df){
  results <- biblioAnalysis(df, sep = ";")
  S       <- summary(object = results, k = 10, pause = FALSE)
  plot(x = results, k = 10, pause = FALSE)

  #M <- metaTagExtraction(df, Field = "AU_CO", sep = ";")
  #NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

  # Plot the network
  #net=networkPlot(NetMatrix,
  #                n = dim(NetMatrix)[1],
  #                Title = "Country Collaboration",
  #                type = "circle", size=TRUE,
  #                remove.multiple=FALSE,
  #                labelsize=0.7,cluster="none")

  # Plot the network
  #NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
  #net=networkPlot(NetMatrix,
  #                normalize="association",
  #                weighted=T, n = 30,
  #                Title = "Keyword Co-occurrences",
  #                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

  over_time <- df %>%
    group_by(ref_type,PY) %>%
    count()

  ggplot(data = over_time, mapping = aes(x=PY, y=n, color = ref_type)) +
    geom_line()+
    geom_point() +
    labs(title="Publications per year", x ="Year", y = "Frecuency")
}
