#' @name  tosr_graph
#' @title Creating a graph object
#' @description Load a dataframe with scopus .bib data and convert it in a graph
#' @param scopus_dataframe a dataframe with scopus data
#' @usage tosgraph(data = scopus_dataframe)
#' @author Sebastian Robledo
#' @return graph: a graph object
#' @examples library(treeofscience)
#'           socopus_df   <- tosload("scopus.bib") # Create scopus data frame
#'           graph.scopus <- tosgraph(scopus_df) # Create graph from scopus data frame
#' @export
#'
tosr_graph <- function(df){
  df_ref  <- tosr.cited_ref(df)
  g       <- tosr.graph(df_ref)

 return(g)
}

tosr.cited_ref <- function(df){
  df     <- tosr.SRTOS(df)
  df$CR  <- str_replace(df$CR,  "; ", ";")

  pattern_authors <-
    SPC %R%
    one_or_more(WRD) %R%
    SPC %R%
    one_or_more(or(WRD, ANY_CHAR))

  pattern_titles <-
    OPEN_PAREN %R%
    repeated(DGT, 4) %R%
    CLOSE_PAREN %R%
    one_or_more(or(WRD,ANY_CHAR))

  pattern_year <-
    OPEN_PAREN %R%
    repeated(DGT, 4) %R%
    CLOSE_PAREN

  pattern_journal <-
    one_or_more(or(WRD,SPC))

  pattern_volume <-
    one_or_more(or(WRD, SPC))

  pattern_pages <-
    "PP. " %R%
    one_or_more(or(DGT, ANY_CHAR))

  cited_references <-
    df %>%
    separate_rows(CR, sep = ";") %>%
    select(SR_TOS, CR) %>%
    mutate(CR_AUTHOR = str_remove(CR, pattern_authors),
           CR_TITLE_1 = str_extract(CR, pattern_authors),
           CR_TITLE = str_remove(CR_TITLE_1, pattern_titles),
           CR_TITLE = str_trim(CR_TITLE),
           CR_YEAR_1 <- str_extract(CR_TITLE_1, pattern_titles),
           CR_YEAR = str_extract(CR_YEAR_1, repeated(DGT, 4)),
           CR_JOURNAL_1 = str_remove(CR_YEAR_1, pattern_year),
           CR_JOURNAL = str_extract(CR_JOURNAL_1, pattern_journal),
           CR_JOURNAL = str_trim(CR_JOURNAL),
           CR_VOLUME_1 = str_remove(CR_JOURNAL_1, pattern_journal),
           CR_VOLUME = str_extract(CR_VOLUME_1, pattern_volume),
           CR_PAGES = str_extract(CR_VOLUME_1, pattern_pages),
           CR_PAGES = str_remove(CR_PAGES, "PP. ")) %>%
    select(SR_TOS,
           CR,
           CR_AUTHOR,
           CR_TITLE,
           CR_YEAR,
           CR_JOURNAL,
           CR_VOLUME,
           CR_PAGES) %>%
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           CR_SO = CR) %>%
    select(-lastname)
  return(cited_references)
}

tosr.SRTOS <- function(df){
  df$SR_TOS <- rownames(df)
  df$SR_TOS <- ifelse(!is.na(df$VL),
                      paste(df$SR_TOS,
                            df$VL,
                            sep = ", V"),
                      df$SR_TOS)
  df$SR_TOS <- ifelse(!is.na(df$DI),
                      paste(df$SR_TOS,
                            df$DI,
                            sep = ", DOI "),
                      df$SR_TOS)
  return(df)
}

tosr.graph <- function(cited_ref){

  edge_list <-
    cited_ref %>%
    select(SR_TOS,
           CR_SO) %>%
    na.omit() %>%
    unique()


  graph <- igraph::graph.data.frame(edge_list) %>%
    igraph::simplify()

  graph_1 <- igraph::delete.vertices(graph,
                                     which(igraph::degree(graph, mode = "in") == 1 &
                                             igraph::degree(graph, mode = "out") == 0))

  # Choose bigest component
  graph_2 <- CINNA::giant_component_extract(graph_1, directed = TRUE)
  graph_2 <- graph_2[[1]]

  # Create subfields
  subareas <-
    igraph::as.undirected(graph_2,mode = "each") %>%
    igraph::cluster_louvain()

  graph_2 <- graph_2 %>%
    igraph::set_vertex_attr(name = "sub_field",value = igraph::membership(subareas))
  return(graph_2)
}






