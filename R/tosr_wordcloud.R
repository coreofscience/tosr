#' @name  tosr_wordcloud
#' @title bibliometric analysis
#' @description Compute the bibliometric analysis using bibliometrix library
#' @param bibliometrix_objetc a bibliometix objetc
#' @usage tosr(df)
#' @author Sebastian Robledo
#' @return bibliometrix analysis plots
#'
#' @export
#

tosr_wordcloud <- function(df_ref, tos, words_remove = c(), subfield = 1){
  titles1 <- df_ref$CR_TITLE[df_ref$ID_TOS %in% tos$id[tos$subfield == subfield]]
  titles1 <- titles1[!is.na(titles1)]
  dtmf1   <- tosr.corpus(titles1)
  dtmf1   <- dtmf1[!(dtmf1$word %in% words_remove),]
  wordcloud2(dtmf1)
}


tosr.corpus <- function(titles){
  corp       <- VCorpus(VectorSource(titles))
  corp       <- tm_map(corp, removePunctuation)
  corp       <- tm_map(corp, removeWords, stopwords("spanish"))
  corp       <- tm_map(corp, removeWords, stopwords("english"))
  corp       <- tm_map(corp, removeNumbers)
  corp       <- tm_map(corp, stemDocument, language = "spanish")
  removeAccents <- content_transformer(function(x) chartr("áéíóú", "aeiou", x))
  corp       <- tm_map(corp, removeAccents)

  dtm    <- TermDocumentMatrix(corp)
  matrix <- as.matrix(dtm)
  words  <- sort(rowSums(matrix),decreasing=TRUE)
  df_wc  <- data.frame(word = names(words),freq=words)
  df_wc  <- df_wc[3:length(df_wc$word),]
  return(df_wc)
}

