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
pkgs <- c("dplyr", "ggplot2", "nlme", "car", "MuMIn")
lapply(pkgs, library, character.only = TRUE) # load them

# Set wd
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read in functions

# Read in files
source("R/01_download_data.R")
source("R/02_prelim_processing.R")
source("R/03_compile-data.R")
