# tosr

## An R-tool to create the Tree of Science from WoS and Scopus

## Overview

tosr read files from Web of Science (WoS) and Scopus and creates recommendation 
dataframe using the metaphor of the tree. Papers in the root represent the 
seminals. Papers in the trunk represent the structural, and papers in the 
leaves present the current literature. 

## Suggested citation

We would appreciate a citation:

Valencia-Hernández, D. S., Robledo, S., Pinilla, R., Duque-Méndez, N. D., & Olivar-Tost, G. (2020). SAP Algorithm for Citation Analysis: An improvement to Tree of Science. Ingeniería e Investigación, 40(1), 45-49.

## Instalation

install.packages("tosr")

## Developers version

install.packages("devtools")
devtools::install_github("coreofscience/tosr")

Load tosr

library(tosr)

# References
1. https://revistas.unal.edu.co/index.php/ingeinv/article/view/77718/0
2. https://revistas.udistrital.edu.co/index.php/vinculos/article/view/9664
