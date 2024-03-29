# This code downloads and stacks 5 NEON data products used in the NEON N Across Scales manuscript.
#
# You will only need to run this code once to download the data and unzip the files
# to stack the data into merged files by data product. 
# 
# The neonUtilities loadByProduct() function downloads and stacks data products by site x year
#
# You could re-run this code periodically to try to find any updated data products, 
# but beware that this might overwrite existing previously downloaded data. 
#
# Data products used in this project include: 
#   canopy foliar chemistry: DP1.10026.001
#   soil chemical properties (distributed plots, periodic): DP1.10086.001
#   litter chemical properties: DP1.10033.001
#   root chemical properties: DP1.10067.001

# NEON token
neonToken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJrZWxsZXJhYkB1bW4uZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzY2ODk2ODgwLCJpYXQiOjE2MDkyMTY4ODAsImVtYWlsIjoia2VsbGVyYWJAdW1uLmVkdSJ9.L2gHraOdcGLWe1dvJDPxpDymwMusPBCLqutgNP2V9bnV3Aqz0hgGJOqvvVjJgP1Qvjc-JV1GIr_cm-61YGl-0g"

#remove.packages(library(neonUtilities))

#Use github version of neonUtilities to download just needed tables
# library(devtools)
# devtools::install_github('NEONScience/NEON-utilities/neonUtilities', ref='2.0')
# #restart R

#for downloading the neonNTrans package
library(devtools)
#install_github("NEONScience/NEON-Nitrogen-Transformations/neonNTrans", dependencies=TRUE)  
#library(neonNTrans)

# Load NEON download/processing R package
library(neonUtilities)

#?loadByProduct

# Download and stack soil chemical properties (distributed plots, periodic): DP1.10078.001
# 26 Oct 20: Bundled into DP1.10086.001
soilCN <- loadByProduct(dpID="DP1.10086.001", site="all", check.size = F, 
                        token = neonToken,
                        tabl = "sls_soilChemistry")
list2env(soilCN, .GlobalEnv)

#create dataframe to see which sites are acid treatment (no C:N data)
# soilCN_original_info = data.frame(soilCN$sls_soilChemistry)
# head(soilCN_original_info)
# soilCN_original_info = soilCN_original_info %>%
#   select(siteID,plotID, acidTreatment,analysisDate) #%>%
#   filter(acidTreatment=='Y')
# 
# # save to file
# acid_treat_sites<-soilCN_original_info[!duplicated(soilCN_original_info),]
# write.csv(acid_treat_sites,'acid_treated_sites.csv')

# Download and stack Root biochemistry
# 26 Oct 20: Bundled into DP1.10067.001
rootCN <- loadByProduct(dpID="DP1.10067.001", site="all", check.size = F,
                        token = neonToken,
                        tabl = "bbc_rootChemistry")
list2env(rootCN, .GlobalEnv)

# Download and stack canopy foliar chemistry: DP1.10026.001
foliarCN <- loadByProduct(dpID="DP1.10026.001", site="all", check.size = F,
                          token = neonToken, tabl = "cfc_carbonNitrogen")
list2env(foliarCN, .GlobalEnv)


# soil inorganic N: ammonium and nitrate
# inorganicN <- loadByProduct(dpID="DP1.10086.001", site="all", check.size = F, 
#                      token = neonToken, tabl='ntr_externalLab')
# 
# list2env(inorganicN, .GlobalEnv) 
# 
# look <- data.frame(inorganicN[2])

# Didn't run into this issue (JHM, 1/5/21)
#sls_soilChemistry <- soilCN$`1` # fix naming scheme!?!

# Download and stack litter chemical properties: DP1.10031.001
# 26 Oct 20: Bundled into DP1.10033.001
litterCN <- loadByProduct(dpID="DP1.10033.001", site="all", check.size = F,
                          token = neonToken, 
                          tabl = "ltr_litterCarbonNitrogen")
list2env(litterCN, .GlobalEnv)


# Soil texture
# 09 Dec 20: Bundled into DP1.10047.001
soiltexture <- loadByProduct(dpID = "DP1.10047.001", site = "all", 
                             check.size = F, token = neonToken,
                             tabl = "spc_particlesize")
list2env(soiltexture, .GlobalEnv)

# # Check if data/ folder exists in path, if not, create it
# if(dir.exists("data/")){
#   print("Will download files to data/ folder in the current path.") 
# } else{
#   dir.create("data/")
#   print("Created a data/ folder in the current path to hold downloaded data.") 
# }


#done


