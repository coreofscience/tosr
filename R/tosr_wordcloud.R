#' @name  tosr_wordcloud
#' @title Wordcloud of the cited references
#' @description Generate wordcloud from the cited references (Only for .bib files)
#' @param df_ref Dataframe of the cited references from the function tosR
#' @param tos Dataframe of the tree of science subfields obtained from the function tosR
#' @param word_remoce List of the words to remove from the wordcloud
#' @param subfield Scalar name of the subfield you want to obtain the wordcloud. By default
#'        is equal to 1, the biggest subfield.
#' @usage tosr(df)
#' @author Sebastian Robledo
#' @return bibliometrix analysis plots
#' @examples library(tosr)
#' tosinfo <- tosr('scopus.bib','save.txt') # Tos process
#' tos.df <- tosinfo$bibliometrix_df # Bibliometrix dataframe
#' tos.g <- tosinfo$graph # Graph
#' tos.tos_sub <- tosinfo$ToS_subfields # Tree of science subfields
#' tos.cited <- tosinfo$cited_references # Cited references
#' tos.sap <- tosinfo$TOs_sap # Tree of science
#' complete_cites <- tosinfo$Tos_comple_table # Complete cites
#' words_remove <- c('the','and','active','for','with','using')
#' tosr_wordcloud(tos.cited, tos.tos_sub, subfield = 2, words_remove = words_remove)
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
  corp       <- tm_map(corp, removeWords, tm::stopwords("en"))
  corp       <- tm_map(corp, removeNumbers)
  #corp       <- tm_map(corp, stemDocument, language = "spanish")
  removeAccents <- content_transformer(function(x) chartr("áéíóú", "aeiou", x))
  corp       <- tm_map(corp, removeAccents)

  dtm    <- TermDocumentMatrix(corp)
  matrix <- as.matrix(dtm)
  words  <- sort(rowSums(matrix),decreasing=TRUE)
  df_wc  <- data.frame(word = names(words),freq=words)
  df_wc  <- df_wc[3:length(df_wc$word),]
  return(df_wc)
}

