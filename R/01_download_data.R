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

# Load NEON download/processing R package
library(neonUtilities)

# Download and stack canopy foliar chemistry: DP1.10026.001
foliarCN <- loadByProduct(dpID="DP1.10026.001", site="all", check.size = F)
list2env(foliarCN, .GlobalEnv)

# Download and stack soil chemical properties (distributed plots, periodic): DP1.10078.001
# 26 Oct 20: Bundled into DP1.10086.001
soilCN <- loadByProduct(dpID="DP1.10086.001", site="all", check.size = F, 
                        token = neonToken,
                        tabl = "sls_soilChemistry")
sls_soilChemistry <- soilCN$`1` # fix naming scheme!?!
list2env(sls_soilChemistry, .GlobalEnv)

# Download and stack litter chemical properties: DP1.10031.001
# 26 Oct 20: Bundled into DP1.10033.001
litterCN <- loadByProduct(dpID="DP1.10033.001", site="all", check.size = F)
list2env(litterCN, .GlobalEnv)

# Root biochemistry
# 26 Oct 20: Bundled into DP1.10067.001
rootCN <- loadByProduct(dpID="DP1.10067.001", site="all", check.size = F)
list2env(rootCN, .GlobalEnv)

# Soil texture
# 09 Dec 20: BUndled into DP1.10047.001
soiltexture <- loadByProduct(dpID = "DP1.10047.001", site = "all", check.size = F)
list2env(soiltexture, .GlobalEnv)

# # Check if data/ folder exists in path, if not, create it
# if(dir.exists("data/")){
#   print("Will download files to data/ folder in the current path.") 
# } else{
#   dir.create("data/")
#   print("Created a data/ folder in the current path to hold downloaded data.") 
# }

