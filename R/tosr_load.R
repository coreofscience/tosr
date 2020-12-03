#' @name  tosr_load
#' @title Loading .bib files
#' @description load .bib file and convert it to data frame with scupos information
#' @param file.bib the .bib file to be converted
#' @author Sebastian Robledo
#' @usage tosload(file = name of file)
#' @return dataframe with all variables in columns
#' @examples library(treeofscience)
#'           file         <- "scopus.bib"
#'           socopus_df   <- tosload(file) # Create scopus data frame
#' @export

tosr_load <- function(...){

  file <- list(...)
  extensions <- unique(unlist(lapply(file, get.extension)))

  if (length(file) == 1){
    print('1')
    d <- tosload_aux(file[[1]])
    return(d)
  }

  if (length(file) > 1){
    print(length(file))
    d <- lapply(file, tosload_aux2)
    M <- mergeDbSources2(d)%>%
      mutate(ID_TOS = str_extract(SR, ".*,"))

    if (length(extensions) > 1){
      df_wos <-
        M %>%
        dplyr::filter(!grepl("\\(([0-9]{4})\\)",
                             M$CR)) %>%
        mutate(ref_type = "wos")

      df_scopus <-
        M %>%
        dplyr::filter(grepl("\\(([0-9]{4})\\)",
                            M$CR)) %>%
        mutate(ref_type = "scopus")

      M <- bind_rows(df_wos, df_scopus)
    }

    if (length(extensions) == 1) {
      if (extensions == 'bib') {
        M <- M %>%
          mutate(ref_type = "scopus")
      } else {
        M <- M %>%
          mutate(ref_type = "wos")
      }
    }
    grafo <- grafo_combinado(M)
    cited_ref <- tosr.cited_ref(M)
    return(list(df=M, graph=grafo$graph, nodes = grafo$nodes))
  }
}

tosload_aux <- function(file){

  extension <- get.extension(file)

  if (extension == "bib"){
    scopus_dataframe <- bibliometrix::convert2df(file = file, dbsource = "scopus", format   = "bibtex") %>%
      mutate(SR_TOS = str_extract(SR, one_or_more(WRD) %R%
                                    SPC %R% one_or_more(WRD) %R%
                                    "," %R% SPC %R%
                                    one_or_more(DGT) %R% ","),
             SR_TOS = str_c(SR_TOS, " ", SO)) %>%
      mutate(ID_TOS = str_extract(SR, ".*,"),
             ref_type = 'scopus')
    grafo <- grafo.bib(scopus_dataframe)

    return(list(df = scopus_dataframe,
                graph = grafo$graph,
                nodes = grafo$nodes))
  }

  if (extension == "txt"){
    data_wos <- bibliometrix::convert2df(file = file, dbsource = "wos", format   = "plaintext")%>%
                mutate(ID_TOS   = str_extract(SR, ".*,"),
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
      l=trim(l)
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
    mutate(ID_TOS = str_extract(SR, ".*,"))

  edge_list_scopus_type <-
    df_scopus %>%
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


  nodes_scopus_type <-
    df_scopus %>%
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



  graph <- graph.data.frame(edge_list_scopus_type) %>%
    simplify()

  # Se eliminan los vertices con indegree = 1 y con outdegree = 0
  graph_1 <- delete.vertices(graph,
                             which(degree(graph, mode = "in") == 1 &
                                     degree(graph, mode = "out") == 0))

  # Se escoge el componente mas grande conectado
  graph_2 <- giant_component_extract(graph_1, directed = TRUE)
  graph_2 <- graph_2[[1]]

  subareas <-
    as.undirected(graph_2,
                  mode = "each") %>%
    cluster_louvain()

  graph_2 <-
    graph_2 %>%
    set_vertex_attr(name = "subfield",
                    value = membership(subareas))
  return(list(graph = graph_2, nodes = nodes_scopus_type))
}

grafo.txt <- function(data_wos){
  data_wos <- data_wos %>%
    mutate(ID_TOS = str_extract(SR, ".*,"))

  nodes_wos_type <-
    data_wos %>%
    separate_rows(CR, sep = ";") %>%
    mutate(CITE = CR,
           ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                        "\\1", CR)) %>%
    select(ID_TOS, CITE) %>%
    na.omit() %>%
    unique()


  edge_list_wos_type <-
    data_wos %>%
    separate_rows(CR, sep = ";") %>%
    mutate(CR = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma
                    "\\1", CR)) %>%
    select(ID_TOS, CR) %>%
    na.omit() %>%
    unique()


  graph_wos_scopus <-
    graph.data.frame(edge_list_wos_type) %>%
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
    set_vertex_attr(name = "subfield",
                    value = membership(subareas))

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

  main_biblio_wos_scopus <-
    biblio_wos_scopus_type %>%
    mutate(CITE = paste0(AU, ", ", PY, ", ", TI, ", " ,SO)) %>%
    select(ID_TOS, CITE)

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

  nodes_attributes <-
    bind_rows(main_biblio_wos_scopus,
              nodes_wos_type,
              nodes_scopus_type) %>%
    unique() %>%
    na.omit() %>%
    filter(!duplicated(ID_TOS))

  edgelist_wos_scopus <-
    bind_rows(edge_list_wos_type, edge_list_scopus_type)


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
    set_vertex_attr(name = "subfield",
                    value = membership(subareas))
  return(list(graph = graph, nodes = nodes_attributes))
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
    select(SR_TOS, CR, SR, ref_type) %>%
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
           CR_PAGES = str_remove(CR_PAGES, "PP. "),
           ID_TOS = str_extract(SR, ".*,")) %>%
    select(SR_TOS,
           CR,
           CR_AUTHOR,
           CR_TITLE,
           CR_YEAR,
           CR_JOURNAL,
           CR_VOLUME,
           CR_PAGES,
           ID_TOS,
           ref_type) %>%
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           CR_SO = CR) %>%
    select(-lastname)

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








