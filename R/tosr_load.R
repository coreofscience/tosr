#' Load Web of Science and Scopus files
#'
#' Loads files from Web of Science (txt) and from Scopus (bib) to create a citation graph
#'
#' @param ... Web of Science or Scopus files
#'
#' @author Sebastian Robledo
#'
#' @importFrom rlang .data
#' @importFrom rebus "%R%"
#'
#' @return List with the following fields: 'df','g','original_df' and 'extensions'.
#'         'df': bibliometrix dataframe.
#'         'g' : Graph from the cited references.
#'      'nodes': dataframe with the nodes atributes of the graph,
#''original_df': dataframe from the original data, and
#''extensions' : Type of files (.bib or .txt) loaded.
#' @export
#'
#' @examples
#' \dontrun{
#' my_tosr_load <- tosr_load("co-citation_209.txt",
#'                           "co-citation_380.bib")
#' }


tosr_load <- function(...){

  file <- list(...)
  extensions <- unlist(lapply(file, get.extension))

  if (length(file) == 1){
    print('1')
    d <- tosload_aux(file[[1]])
    return(d)
  }

  if (length(file) > 1){
    print(length(file))
    d <- lapply(file, tosload_aux2)
    original_df <- mergeDbSources2(d,remove.duplicated=FALSE)
    M <- mergeDbSources2(d)%>%
      dplyr::mutate(ID_TOS = stringr::str_extract(.data$SR, ".*,"))

    extension_unica <-unique(extensions)
    if (length(extension_unica) > 1){
      df_wos <-
        M %>%
        dplyr::filter(!grepl("\\(([0-9]{4})\\)",
                             M$CR)) %>%
        dplyr::mutate(ref_type = "wos")
      df_wos2 <-
        original_df %>%
        dplyr::filter(!grepl("\\(([0-9]{4})\\)",
                             original_df$CR)) %>%
        dplyr::mutate(ref_type = "wos")

      df_scopus <-
        M %>%
        dplyr::filter(grepl("\\(([0-9]{4})\\)",
                            M$CR)) %>%
        dplyr::mutate(ref_type = "scopus")
      df_scopus2 <-
        original_df %>%
        dplyr::filter(grepl("\\(([0-9]{4})\\)",
                            original_df$CR)) %>%
        dplyr::mutate(ref_type = "scopus")

      M <- dplyr::bind_rows(df_wos, df_scopus)
      original_df <- dplyr::bind_rows(df_wos2, df_scopus2)
    }

    if (length(extension_unica) == 1) {
      if (extension_unica == 'bib') {
        M <- M %>%
          dplyr::mutate(ref_type = "scopus")
      } else {
        M <- M %>%
          dplyr::mutate(ref_type = "wos")
      }
    }
    grafo <- grafo_combinado(M)
    #cited_ref <- tosr.cited_ref(M)
    return(list(df=M,
                graph=grafo$graph,
                nodes = grafo$nodes))
  }
}

tosload_aux <- function(file){

  extension <- get.extension(file)

  if (extension == "bib"){
    scopus_dataframe <- bibliometrix::convert2df(file = file, dbsource = "scopus", format   = "bibtex") %>%
      dplyr::mutate(SR_TOS = stringr::str_extract(.data$SR, rebus::one_or_more(rebus::WRD) %R%
                                                    rebus::SPC %R% rebus::one_or_more(rebus::WRD) %R%
                                                    "," %R% rebus::SPC %R%
                                                    rebus::one_or_more(rebus::DGT) %R% ","),
                    SR_TOS = stringr::str_c(.data$SR_TOS, " ", .data$SO)) %>%
      dplyr::mutate(ID_TOS = stringr::str_extract(.data$SR, ".*,"),
                    ref_type = 'scopus')
    grafo <- grafo.bib(scopus_dataframe)

    return(list(df = scopus_dataframe,
                graph = grafo$graph,
                nodes = grafo$nodes))
  }

  if (extension == "txt"){

    data_wos <- bibliometrix::convert2df(file = file,
                                         dbsource = "wos",
                                         format   = "plaintext")%>%
      dplyr::mutate(ID_TOS   = stringr::str_extract(.data$SR,
                                                    ".*,"),
                    ref_type = 'wos')

    grafo <- grafo.txt(data_wos)

    cited_ref <- tosr.cited_ref(data_wos)

    return(list(df = data_wos,
                graph = grafo$graph,
                nodes = grafo$nodes))
  }

}

get.extension <- function(string){

  lista <- unlist(strsplit(string,split = ".",fixed = T))
  return(lista[2])
}

mergeDbSources2 <- function(L,remove.duplicated=TRUE){

  ###
  #L <- list(...)

  n=length(L)

  Tags=names(L[[1]])

  ## identification of common tags
  for (j in 2:n){
    Tags=intersect(Tags,names(L[[j]]))
  }
  #####
  M=data.frame(matrix(NA,1,length(Tags)))
  names(M)=Tags
  for (j in 1:n){
    L[[j]]=L[[j]][,Tags]

    M=rbind(M,L[[j]])
  }

  ## author data cleaning
  if ("AU" %in% Tags){
    M$AU=gsub(","," ",M$AU)
    AUlist=strsplit(M$AU,";")
    AU=lapply(AUlist,function(l){
      l=bibliometrix::trim(l)
      name=strsplit(l," ")
      lastname=unlist(lapply(name,function(ln){ln[1]}))
      firstname=lapply(name,function(ln){
        f=paste(substr(ln[-1],1,1),collapse=" ")
      })
      AU=paste(lastname,unlist(firstname),sep=" ",collapse=";")
      return(AU)
    })
    M$AU=unlist(AU)

  }
  M=M[-1,]
  M$DB="ISI"

  if (isTRUE(remove.duplicated)){
    M$TI=gsub("[^[:alnum:] ]","",M$TI)
    M$TI=gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", M$TI, perl=TRUE)
    d=duplicated(M$TI)
    cat("\n",sum(d),"duplicated documents have been removed\n")
    M=M[!d,]
  }
  M <- bibliometrix::metaTagExtraction(M, Field = "AU_CO", sep = ";")
  return(M)
}



grafo.bib <- function(scopus_dataframe){
  df_scopus <- scopus_dataframe %>%
    dplyr::mutate(ID_TOS = stringr::str_extract(.data$SR, ".*,"))
  df_scopus$ID_TOS <- stringr::str_trim(df_scopus$ID_TOS)

  edge_list_scopus_type <-
    df_scopus %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    dplyr::mutate(lastname = sub("\\., .*", "", .data$CR),
                  lastname = sub(",", "", .data$lastname),
                  lastname = sub("\\.", "", .data$lastname),
                  year = stringr::str_extract(.data$CR, "\\(([0-9]{4})\\)"),
                  year = stringr::str_remove_all(.data$year, "\\(|\\)")) %>%
    dplyr::filter(!grepl(pattern = "[():[:digit:]]", .data$lastname),
                  stringr::str_length(.data$year) == 4) %>%
    dplyr::mutate(CR = paste0(.data$lastname, ", ", .data$year, ",")) %>%
    dplyr::select(.data$ID_TOS, .data$CR)


  nodes_scopus_type <-
    df_scopus %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    dplyr::mutate(lastname = sub("\\., .*", "", .data$CR),
                  lastname = sub(",", "", .data$lastname),
                  lastname = sub("\\.", "", .data$lastname),
                  year = stringr::str_extract(.data$CR, "\\(([0-9]{4})\\)"),
                  year = stringr::str_remove_all(.data$year, "\\(|\\)")) %>%
    dplyr::filter(!grepl(pattern = "[():[:digit:]]", .data$lastname),
                  stringr::str_length(.data$year) == 4) %>%
    dplyr::mutate(ID_TOS = paste0(.data$lastname, ", ", .data$year, ","),
                  CITE = .data$CR) %>%
    unique() %>%
    dplyr::select(.data$ID_TOS, .data$CITE)



  graph <-
    igraph::graph.data.frame(edge_list_scopus_type) %>%
    igraph::simplify()

  # Vertices with indegree = 1 and outdegree = 0
  graph_1 <-
    igraph::delete.vertices(graph,
                            which(igraph::degree(graph, mode = "in") == 1 &
                                    igraph::degree(graph, mode = "out") == 0))

  # Se escoge el componente mas grande conectado
  graph_2 <- CINNA::giant_component_extract(graph_1, directed = TRUE)
  graph_2 <- graph_2[[1]]

  subareas <-
    igraph::as.undirected(graph_2,
                          mode = "each") %>%
    igraph::cluster_louvain()

  graph_2 <-
    graph_2 %>%
    igraph::set_vertex_attr(name = "subfield",
                            value = igraph::membership(subareas))
  return(list(graph = graph_2, nodes = nodes_scopus_type))
}

grafo.txt <- function(data_wos){
  data_wos <- data_wos %>%
    dplyr::mutate(ID_TOS = stringr::str_extract(.data$SR, ".*,"))
  data_wos$ID_TOS <- stringr::str_trim(data_wos$ID_TOS)

  nodes_wos_type <-
    data_wos %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::mutate(CITE = .data$CR,
                  ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                               "\\1", .data$CR)) %>%
    dplyr::select(.data$ID_TOS, .data$CITE) %>%
    stats::na.omit() %>%
    unique()


  edge_list_wos_type <-
    data_wos %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::mutate(CR = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                           "\\1", .data$CR)) %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    stats::na.omit() %>%
    unique()


  graph_wos_scopus <-
    igraph::graph.data.frame(edge_list_wos_type) %>%
    igraph::simplify()

  graph_cleaned <-
    igraph::delete.vertices(graph_wos_scopus,
                            which(igraph::degree(graph_wos_scopus,
                                                 mode = "in") == 1 &
                                    igraph::degree(graph_wos_scopus,
                                                   mode = "out") == 0)
    )

  giant.component <- function(graph) {
    cl <- igraph::clusters(graph)
    igraph::induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }

  graph_wos_scopus_gc <- giant.component(graph_cleaned)

  subareas <-
    igraph::as.undirected(graph_wos_scopus_gc,
                          mode = "each") %>%
    igraph::cluster_louvain()

  graph <-
    graph_wos_scopus_gc %>%
    igraph::set_vertex_attr(name = "subfield",
                            value = igraph::membership(subareas))

  return(list(graph = graph, nodes = nodes_wos_type))
}

tosload_aux2 <- function(file){

  extension <- get.extension(file)

  if (extension == "bib"){
    scopus_dataframe <- bibliometrix::convert2df(file = file, dbsource = "scopus", format   = "bibtex")
    return(scopus_dataframe)
  }

  if (extension == "txt"){
    data_wos <- bibliometrix::convert2df(file = file, dbsource = "wos", format   = "plaintext")
    return(data_wos)
  }

}


grafo_combinado <- function(biblio_wos_scopus){
  df_wos <-
    biblio_wos_scopus %>%
    dplyr::filter(!grepl("\\(([0-9]{4})\\)",
                         biblio_wos_scopus$CR)) %>%
    dplyr::mutate(ref_type = "wos")

  df_scopus <-
    biblio_wos_scopus %>%
    dplyr::filter(grepl("\\(([0-9]{4})\\)",
                        biblio_wos_scopus$CR)) %>%
    dplyr::mutate(ref_type = "scopus")

  biblio_wos_scopus_type <-
    dplyr::bind_rows(df_wos, df_scopus) %>%
    dplyr::mutate(ID_TOS = stringr::str_extract(.data$SR, ".*,"))

  biblio_wos_scopus_type$ID_TOS <- stringr::str_trim(biblio_wos_scopus_type$ID_TOS)



  edge_list_wos_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(.data$ref_type == "wos") %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::mutate(CR = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                           "\\1", .data$CR)) %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    stats::na.omit() %>%
    unique()

  edge_list_scopus_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(.data$ref_type == "scopus") %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    dplyr::mutate(lastname = sub("\\., .*", "",.data$CR),
                  lastname = sub(",", "", .data$lastname),
                  lastname = sub("\\.", "", .data$lastname),
                  year = stringr::str_extract(.data$CR, "\\(([0-9]{4})\\)"),
                  year = stringr::str_remove_all(.data$year, "\\(|\\)")) %>%
    dplyr::filter(!grepl(pattern = "[():[:digit:]]", .data$lastname),
                  stringr::str_length(.data$year) == 4) %>%
    dplyr::mutate(CR = paste0(.data$lastname, ", ", .data$year, ",")) %>%
    dplyr::select(.data$ID_TOS, .data$CR)

  main_biblio_wos_scopus <-
    biblio_wos_scopus_type %>%
    dplyr::mutate(CITE = paste0(.data$AU, ", ", .data$PY, ", ", .data$TI, ", " ,.data$SO)) %>%
    dplyr::select(.data$ID_TOS, .data$CITE)

  nodes_wos_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(.data$ref_type == "wos") %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::mutate(CITE = .data$CR,
                  ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                               "\\1", .data$CR)) %>%
    dplyr::select(.data$ID_TOS, .data$CITE) %>%
    stats::na.omit() %>%
    unique()

  nodes_scopus_type <-
    biblio_wos_scopus_type %>%
    dplyr::filter(.data$ref_type == "scopus") %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::select(.data$ID_TOS, .data$CR) %>%
    dplyr::mutate(lastname = sub("\\., .*", "", .data$CR),
                  lastname = sub(",", "", .data$lastname),
                  lastname = sub("\\.", "", .data$lastname),
                  year = stringr::str_extract(.data$CR, "\\(([0-9]{4})\\)"),
                  year = stringr::str_remove_all(.data$year, "\\(|\\)")) %>%
    dplyr::filter(!grepl(pattern = "[():[:digit:]]", .data$lastname),
                  stringr::str_length(.data$year) == 4) %>%
    dplyr::mutate(ID_TOS = paste0(.data$lastname, ", ", .data$year, ","),
                  CITE = .data$CR) %>%
    unique() %>%
    dplyr::select(.data$ID_TOS, .data$CITE)

  nodes_attributes <-
    dplyr::bind_rows(main_biblio_wos_scopus,
                     nodes_wos_type,
                     nodes_scopus_type) %>%
    unique() %>%
    stats::na.omit() %>%
    dplyr::filter(!duplicated(.data$ID_TOS))

  edgelist_wos_scopus <-
    dplyr::bind_rows(edge_list_wos_type, edge_list_scopus_type)


  graph_wos_scopus <-
    igraph::graph.data.frame(edgelist_wos_scopus) %>%
    igraph::simplify()

  graph_cleaned <-
    igraph::delete.vertices(graph_wos_scopus,
                            which(igraph::degree(graph_wos_scopus,
                                                 mode = "in") == 1 &
                                    igraph::degree(graph_wos_scopus,
                                                   mode = "out") == 0)
    )

  giant.component <- function(graph) {
    cl <- igraph::clusters(graph)
    igraph::induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }

  graph_wos_scopus_gc <- giant.component(graph_cleaned)

  subareas <-
    igraph::as.undirected(graph_wos_scopus_gc,
                          mode = "each") %>%
    igraph::cluster_louvain()

  graph <-
    graph_wos_scopus_gc %>%
    igraph::set_vertex_attr(name = "subfield",
                            value = igraph::membership(subareas))
  return(list(graph = graph, nodes = nodes_attributes))
}



tosr.cited_ref <- function(df){
  df     <- tosr.SRTOS(df)
  df$CR  <- stringr::str_replace(df$CR,
                                 "; ",
                                 ";")

  pattern_authors <-
    rebus::SPC %R%
    rebus::one_or_more(rebus::WRD) %R%
    rebus::SPC %R%
    rebus::one_or_more(rebus::or(rebus::WRD, rebus::ANY_CHAR))

  pattern_titles <-
    rebus::OPEN_PAREN %R%
    rebus::repeated(rebus::DGT, 4) %R%
    rebus::CLOSE_PAREN %R%
    rebus::one_or_more(rebus::or(rebus::WRD, rebus::ANY_CHAR))

  pattern_year <-
    rebus::OPEN_PAREN %R%
    rebus::repeated(rebus::DGT, 4) %R%
    rebus::CLOSE_PAREN

  pattern_journal <-
    rebus::one_or_more(rebus::or(rebus::WRD,rebus::SPC))

  pattern_volume <-
    rebus::one_or_more(rebus::or(rebus::WRD, rebus::SPC))

  pattern_pages <-
    "PP. " %R%
    rebus::one_or_more(rebus::or(rebus::DGT, rebus::ANY_CHAR))

  cited_references <-
    df %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::select(.data$SR_TOS, .data$CR, .data$SR, .data$ref_type) %>%
    dplyr::mutate(CR_AUTHOR = stringr::str_remove(.data$CR,
                                                  pattern_authors),
                  CR_TITLE_1 = stringr::str_extract(.data$CR,
                                                    pattern_authors),
                  CR_TITLE = stringr::str_remove(.data$CR_TITLE_1,
                                                 pattern_titles),
                  CR_TITLE = stringr::str_trim(.data$CR_TITLE),
                  CR_YEAR_1 = stringr::str_extract(.data$CR_TITLE_1,
                                                   pattern_titles),
                  CR_YEAR = stringr::str_extract(.data$CR_YEAR_1,
                                                 rebus::repeated(rebus::DGT,
                                                                 4)),
                  CR_JOURNAL_1 = stringr::str_remove(.data$CR_YEAR_1,
                                                     pattern_year),
                  CR_JOURNAL = stringr::str_extract(.data$CR_JOURNAL_1,
                                                    pattern_journal),
                  CR_JOURNAL = stringr::str_trim(.data$CR_JOURNAL),
                  CR_VOLUME_1 = stringr::str_remove(.data$CR_JOURNAL_1,
                                                    pattern_journal),
                  CR_VOLUME = stringr::str_extract(.data$CR_VOLUME_1,
                                                   pattern_volume),
                  CR_PAGES = stringr::str_extract(.data$CR_VOLUME_1,
                                                  pattern_pages),
                  CR_PAGES = stringr::str_remove(.data$CR_PAGES,
                                                 "PP. "),
                  ID_TOS = stringr::str_extract(.data$SR,
                                                ".*,")) %>%
    dplyr::select(.data$SR_TOS,
                  .data$CR,
                  .data$CR_AUTHOR,
                  .data$CR_TITLE,
                  .data$CR_YEAR,
                  .data$CR_JOURNAL,
                  .data$CR_VOLUME,
                  .data$CR_PAGES,
                  .data$ID_TOS,
                  .data$ref_type) %>%
    dplyr::mutate(lastname = sub("\\., .*", "", .data$CR),
                  lastname = sub(",", "", .data$lastname),
                  lastname = sub("\\.", "", .data$lastname),
                  CR_SO = .data$CR) %>%
    dplyr::select(-.data$lastname)

  cited_references$ID_TOS <- gsub(" ","",cited_references$ID_TOS)
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
