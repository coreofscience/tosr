## Paquete tosr 

# Paso 1
Primero se debe instalar el paquete "remote" que sirve para gestionar el paquete 'tosr' en el Rstudio cloud. La siguiente instruccion se copia y se pega en la consala y se ejecuta. 

install.packages("remotes")

# Paso 2
Para instalar el paquete de tosr en el Rstudio cloud se copia la siguiente instruccion y se ejecuta. Se espera un momento a que instale y luego se puede usar el paquete.  

remotes::install_github("https://github.com/coreofscience/tosr" , dependencies = TRUE)

# **Ejemplo**
- library(tosr)                             # Se carga libreria 
- tosinfo <- tosr('scopus.bib','save.txt')  # Tos process

# Tos information
- tos.df         <- tosinfo$bibliometrix_df    # Bibliometrix dataframe
- tos.g          <- tosinfo$graph              # Graph
- tos.tos_sub    <- tosinfo$ToS_subfields      # Tree of science subfields
- tos.cited      <- tosinfo$cited_references   # Cited references
- tos.sap        <- tosinfo$TOs_sap            # Tree of science 
- complete_cites <- tosinfo$Tos_comple_table   # Complete cites 


# **Analisis bibliometrico**
Summary <- tosr_bibliometix_analysis(tos.df)

# Wordcloud (funciona para archivos .bib)
- words_remove <- c('the','and','active','for','with','using')
- tosr_wordcloud(tos.cited, tos.tos_sub, subfield = 2, words_remove = words_remove)

# **Para actualizar**
remotes::update_packages("tosr" , dependencies = TRUE)

# Referencias
1. https://revistas.unal.edu.co/index.php/ingeinv/article/view/77718/0
2. https://rdrr.io/cran/remotes/man/install_git.html
2. https://www.bibliometrix.org/
