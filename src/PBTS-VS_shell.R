
# PBTS-VS_shell.R
# Bonaventura Pacileo
# This program cleans the space, loads the required packages  and shell calls all the others

#clean the environment
rm(list=ls())

#checking wd
wd<-getwd() #'//main.oecd.org/ASgenEDU/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS'



#libraries
source(paste(wd,'src/init', 'PBTS-VS_pkgs.R', sep="/"))

library('ggrepel')
library('magrittr')
library('RColorBrewer')
library('scales')
library('shiny')


source(paste(wd,'src/config', 'PBTS-VS_techfuns.R', sep="/"))
source(paste(wd,'src/config', 'PBTS-VS_tablefuns.R', sep="/"))
source(paste(wd,'src/config', 'PBTS-VS_analysisfuns.R', sep="/"))

source(paste(wd,'src/dashboard', 'PBTS-VS_ui.R',sep="/"))
source(paste(wd,'src/dashboard', 'PBTS-VS_server.R',sep="/"))

shiny::shinyApp(ui = ui, server = server)-> VSshiny

VSshiny

