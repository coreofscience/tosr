## Paquete tosr 

# Paso 1
Primero se debe instalar el paquete "remote" que sirve para gestionar el paquete 'tosr' en el Rstudio cloud. La siguiente instruccion se copia y se pega en la consala y se ejecuta. 

install.packages("remotes")

# Paso 2
Para instalar el paquete de tosr en el Rstudio cloud se copia la siguiente instruccion y se ejecuta. Se espera un momento a que instale y luego se puede usar el paquete.  

remotes::install_github("https://github.com/coreofscience/tosr" , dependencies = TRUE)

# Ejemplo
library(tosr)
tosinfo <- tosr('save.txt', 'scopus.bib', 'scopus1.bib')

bibliometrix_df <- tosinfo$bibliometrix_df
grafo           <- tosinfo$graph
tos.subfields   <- tosinfo$ToS_subfields
cited_ref       <- tosinfo$cited_references

# **Analisis bibliometrico**
tosr_bibliometix_analysis(bibliometrix_df)

# **Wordcloud**
tosr_wordcloud(cited_ref, tos.subfields, subfield = 1) 
tosr_wordcloud(cited_ref, tos.subfields, subfield = 2)
tosr_wordcloud(cited_ref, tos.subfields, subfield = 3)

# Referencias
1. https://revistas.unal.edu.co/index.php/ingeinv/article/view/77718/0
2. https://rdrr.io/cran/remotes/man/install_git.html
2. https://www.bibliometrix.org/
