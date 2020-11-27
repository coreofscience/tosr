## Paquete tosr 

# Paso 1
Primero se debe instalar el paquete "remote" que sirve para gestionar el paquete 'tosr' en el Rstudio cloud. La siguiente instruccion se copia y se pega en la consala y se ejecuta. 

install.packages("remotes")

# Paso 2
Para instalar el paquete de tosr en el Rstudio cloud se copia la siguiente instruccion y se ejecuta. Se espera un momento a que instale y luego se puede usar el paquete.  

remotes::install_github("https://github.com/coreofscience/tosr" , dependencies = TRUE)

# Ejemplo
**library(tosr)**                                  # Se carga la libreria 


**tosinfo <- tosr('save.txt','scopus.bib')**     # se llama la funcion tosr (se cargan los archivos que se desean procesar al Rstudio cloud)

**bibliometrix_df <- tosinfo$bibliometrix_df**   # Bibliometrix data frame 


**grafo           <- tosinfo$graph**             # Grafo con las subareas 


**tos.subfields   <- tosinfo$ToS_subfields**     # Dataframe con el 'Tree of science' para las tres subareas mas grandes  

# Referencias
1. https://revistas.unal.edu.co/index.php/ingeinv/article/view/77718/0
2. https://rdrr.io/cran/remotes/man/install_git.html
