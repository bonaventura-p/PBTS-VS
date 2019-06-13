
# Scaling Functions for PISA for Schools
# Bonaventura Pacileo
# This program contains the functions and packages to create the cognitive scales of PBTS

# functions to be created:

# PcaPrep: input stud questionnaire, output dummies+age


# sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source);

#clean the environment
rm(list=ls())

#setting wd

getwd()

#same location as r project
setwd('//main.oecd.org/ASgenEDU/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS')


#libraries
library("psych")
library("ggrepel")
library("gridExtra")
library("dismo")
library("caret")
library("pls")
library("magrittr")
library("ggplot2")
library("reshape2")
library("stats")
library("foreign")
library("rmarkdown")
library("fastDummies")
library("xlsx")
library("plyr")
library("TAM")
library("openxlsx") #for big excel files
library("broom")
library("zoo")
library("intsvy")
library("data.table")
library("WrightMap")
library("plotly")
library("shiny")
library("shinydashboard")
library("tidyverse")
library("RColorBrewer")
library("scales")

# Structure: 
# run initialisation file
source("Validation_Functions.R") 

#########################################################

# tam.math <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Item parameters/tam.math.txt", header=T, sep="\t")
# tam.read <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Item parameters/tam.read.txt", header=T, sep="\t")
# tam.scie <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Item parameters/tam.scie.txt", header=T, sep="\t")

#tam.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Item parameters/tam.data.txt", header=T, sep="\t")
