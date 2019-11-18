# PBTS-VS_pkgs.R
# Bonaventura Pacileo
# This program checks if the packages are installed and installs them if required


UsePkg <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
    } 
  }
}


#libraries
packages <-c("psych","ggrepel","gridExtra","dismo","caret","pls","magrittr","ggplot2","reshape2","stats","foreign","rmarkdown",
"fastDummies","xlsx", "plyr","TAM","openxlsx", "broom","zoo","intsvy","data.table","WrightMap","plotly","shiny","shinydashboard",
"tidyverse", "RColorBrewer","scales")

UsePkg(packages)
