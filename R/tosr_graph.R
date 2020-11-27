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
tosr_graph <- function(data){

  tos_dataframe <- data$df
  ext           <- data$ext

  if (ext == "bib"){
    graph1 <- graph.bib(tos_dataframe, ext)
  }
  if (ext == "txt"){
    graph1 <- graph.txt(tos_dataframe, ext)
  }
  if (ext == "comb"){
    graph1 <- graph_ml(tos_dataframe, ext)
  }

 return(graph1)
}


graph.bib <- function(scopus_dataframe, ext){

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
    scopus_dataframe %>%
    separate_rows(CR, sep = "; ") %>%
    select(SR_TOS,
           CR) %>%
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
           CR_SO = str_c(lastname,
                         ", ",
                         CR_YEAR,
                         ", ",
                         CR_JOURNAL)) %>%
    select(-lastname)


  edge_list <-
    cited_references %>%
    select(SR_TOS,
           CR_SO) %>%
    na.omit() %>%
    unique()


  graph <- igraph::graph.data.frame(edge_list) %>%
    igraph::simplify()

  # Delete vertex with indegree = 1 and outdegree = 0
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


  scopus_dataframe <- scopus_dataframe %>%
    mutate(ID_TOS = str_extract(SR, ".*,"))

  nodes_scopus_type <-
    scopus_dataframe %>%
    separate_rows(CR, sep = ";") %>%
    select(ID_TOS, CR) %>%
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>%
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>%
    mutate(ID_TOS = paste0(lastname, ", ", year, ","),
           CITE = CR) %>%
    unique() %>%
    select(ID_TOS, CITE)

  graph.info <- list(graph = graph_2, node_cite = nodes_scopus_type, ext = ext)
  return(graph.info)
}

graph.txt <- function(data_wos, ext){
  data_wos$ID_WOS <- rownames(data_wos)

  data_wos$ID_WOS <- ifelse(!is.na(data_wos$VL),
                            paste(data_wos$ID_WOS,
                                  data_wos$VL,
                                  sep = ", V"),
                            data_wos$ID_WOS)

  data_wos$ID_WOS <- ifelse(!is.na(data_wos$BP),
                            paste(data_wos$ID_WOS,
                                  data_wos$BP,
                                  sep = ", P"),
                            data_wos$ID_WOS)

  data_wos$ID_WOS <- ifelse(!is.na(data_wos$DI),
                            paste(data_wos$ID_WOS,
                                  data_wos$DI,
                                  sep = ", DOI "),
                            data_wos$ID_WOS)

  # Creando la lista de enlases

  edgelist <-
    as_tibble(data_wos) %>%
    mutate(cited_references = CR) %>%
    separate_rows(CR, sep = ";") %>%
    filter(!grepl(pattern = "^[0-9].*",
                  CR)) %>%
    select(ID_WOS, CR) %>%
    filter(CR != "" & is.na(CR) == FALSE) %>%
    mutate(ID_WOS = str_to_upper(ID_WOS),
           CR = str_to_upper(CR)) %>%
    unique()

  graph <- igraph::graph.data.frame(edgelist) %>%
    simplify()

  # Se eliminan los vertices con indegree = 1 y con outdegree = 0
  graph_1 <- igraph::delete.vertices(graph,
                             which(degree(graph, mode = "in") == 1 &
                                     degree(graph, mode = "out") == 0))

  # Se escoge el componente mas grande conectado
  graph_2 <- CINNA::giant_component_extract(graph_1, directed = TRUE)
  graph_2 <- graph_2[[1]]

  # Create subfields
  subareas <-
    igraph::as.undirected(graph_2,mode = "each") %>%
    igraph::cluster_louvain()

  graph_2 <- graph_2 %>%
    igraph::set_vertex_attr(name = "sub_field",value = igraph::membership(subareas))

  nodes_wos_type <-
    data_wos %>%
    separate_rows(CR, sep = ";") %>%
    mutate(CITE = CR,
           ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                        "\\1", CR)) %>%
    select(ID_TOS, CITE) %>%
    na.omit() %>%
    unique()

  graph.info <- list(graph = graph_2, node_cite = nodes_wos_type, ext = ext)
  return(graph.info)
}

graph_ml <- function(biblio_wos_scopus,ext){

  # Organizacion de los datos
  df_wos <-
    biblio_wos_scopus %>%
    dplyr::filter(!grepl("\\(([0-9]{4})\\)",
                         biblio_wos_scopus$CR)) %>%
    mutate(ref_type = "wos")

  df_scopus <-
    biblio_wos_scopus %>%
    dplyr::filter(grepl("\\(([0-9]{4})\\)",
                        biblio_wos_scopus$CR)) %>%
    mutate(ref_type = "scopus")

  biblio_wos_scopus_type <-
    bind_rows(df_wos, df_scopus) %>%
    mutate(ID_TOS = str_extract(SR, ".*,"))


  edge_list_wos_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(ref_type == "wos") %>%
    separate_rows(CR, sep = ";") %>%
    mutate(CR = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                    "\\1", CR)) %>%
    select(ID_TOS, CR) %>%
    na.omit() %>%
    unique()


  edge_list_scopus_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(ref_type == "scopus") %>%
    separate_rows(CR, sep = ";") %>%
    select(ID_TOS, CR) %>%
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>%
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>%
    mutate(CR = paste0(lastname, ", ", year, ",")) %>%
    select(ID_TOS, CR)

  edgelist_wos_scopus <-
    bind_rows(edge_list_wos_type, edge_list_scopus_type)


  # ------------------------- CREACION DEL GRAFO ---------------------------------
  graph_wos_scopus <-
    graph.data.frame(edgelist_wos_scopus) %>%
    simplify()

  graph_cleaned <-
    delete.vertices(graph_wos_scopus,
                    which(degree(graph_wos_scopus,
                                 mode = "in") == 1 &
                            degree(graph_wos_scopus,
                                   mode = "out") == 0)
    )

  giant.component <- function(graph) {
    cl <- clusters(graph)
    induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }

  graph_wos_scopus_gc <- giant.component(graph_cleaned)

  subareas <-
    as.undirected(graph_wos_scopus_gc,
                  mode = "each") %>%
    cluster_louvain()

  graph <-
    graph_wos_scopus_gc %>%
    set_vertex_attr(name = "sub_field",
                    value = membership(subareas))

  nodes_wos_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(ref_type == "wos") %>%
    separate_rows(CR, sep = ";") %>%
    mutate(CITE = CR,
           ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                        "\\1", CR)) %>%
    select(ID_TOS, CITE) %>%
    na.omit() %>%
    unique()


  nodes_scopus_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(ref_type == "scopus") %>%
    separate_rows(CR, sep = ";") %>%
    select(ID_TOS, CR) %>%
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>%
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>%
    mutate(ID_TOS = paste0(lastname, ", ", year, ","),
           CITE = CR) %>%
    unique() %>%
    select(ID_TOS, CITE)

  main_biblio_wos_scopus <-
    biblio_wos_scopus_type %>%
    mutate(CITE = paste0(AU, ", ", PY, ", ", TI, ", " ,SO)) %>%
    select(ID_TOS, CITE)


  nodes_attributes <-
    bind_rows(main_biblio_wos_scopus,
              nodes_wos_type,
              nodes_scopus_type) %>%
    unique() %>%
    na.omit() %>%
    filter(!duplicated(ID_TOS))

  graph.info <- list(graph = graph, node_cite = nodes_attributes, ext = ext)
  return(graph.info)
}











