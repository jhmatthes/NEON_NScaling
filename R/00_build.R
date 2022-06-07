###############################################################################
# last updated: 2020.11.30
# author: Adrienne Keller
# project: NEON N scaling
# file Description: 
#     file builds all data compiling/wrangling
#
# notes: 
###############################################################################
#Sys.info()

rm(list=ls())

#This part may need to go here...
# library(devtools)
# devtools::install_github('NEONScience/NEON-utilities/neonUtilities', ref='2.0')
# #restart R

# Import packages
pkgs <- c("dplyr", "ggplot2", "nlme", "car", "MuMIn",'lme4','cowplot','rstudioapi')
lapply(pkgs, library, character.only = TRUE) # load them

#for creating nice model output tables
# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)

# Set wd
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read in functions
source("Functions.R")

# Run these to get latest data
# source("01_download_data.R")
# source("02_prelim_processing.R")
# source("03_compile-data.R")

# Read in core dataset
cn_data <- read.csv("data/C_N_data_for_analysis.csv")
#length(unique(cn_data$siteID))



