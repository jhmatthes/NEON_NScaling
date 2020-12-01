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

#rm(list=ls())

# Import packages
pkgs <- c("dplyr", "ggplot2", "nlme", "car")
lapply(pkgs, library, character.only = TRUE) # load them

# Set wd

# Read in functions

# Read in files
source("R/download_data.R")
source("R/prelim_processing.R")
source("R/03_compile-data.R")
