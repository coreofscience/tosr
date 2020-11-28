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

tosr_load <- function(file){
  nfiles = length(file)
  if (nfiles == 1){
    data = tosload_aux(file)
  }

  if (nfiles > 1){
    #data = tosload_aux(file)
    d <- lapply(file, tosload_aux)
    L <- list()
    i <- 1
    for (i in seq(length(d))){
      L[[i]] <- d[[i]]$df
    }

    dataframe <- mergeDbSources2(L, remove.duplicated = TRUE) %>%
      mutate(SR_TOS = paste(SR_FULL,'; DOI:',DI))


    data      <- list(df = dataframe, ext = 'comb')
  }

  return(data)

}



tosload_aux <- function(file){

  extension <- get.extension(file)

  if (extension == "bib"){
    dataframe <- bibliometrix::convert2df(file = file, dbsource = "scopus", format   = "bibtex") %>%
    mutate(SR_TOS = paste(SR_FULL,'; DOI:',DI))

  }

  if (extension == "txt"){
           dataframe <- bibliometrix::convert2df(file = file, dbsource = "wos", format   = "plaintext") %>%
             mutate(SR_TOS = paste(SR_FULL,'; DOI:',DI))
  }
  data <- list(df = dataframe, ext = extension)
  return(data)
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








