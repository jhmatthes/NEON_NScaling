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


# Load NEON download/processing R package
library(neonUtilities)

# Check if data/ folder exists in path, if not, create it
if(dir.exists("data/")){
  print("Will download files to data/ folder in the current path.") 
} else{
  dir.create("data/")
  print("Created a data/ folder in the current path to hold downloaded data.") 
}

# Download and stack canopy foliar chemistry: DP1.10026.001
foliarCN <- loadByProduct(dpID="DP1.10026.001", site="all")
list2env(foliarCN, .GlobalEnv)

# Download and stack soil chemical properties (distributed plots, periodic): DP1.10078.001
# 26 Oct 20: Bundled into DP1.10086.001
soilCN <- loadByProduct(dpID="DP1.10086.001", site="all")
list2env(soilCN, .GlobalEnv)

# Download and stack litter chemical properties: DP1.10031.001
# 26 Oct 20: Bundled into DP1.10033.001
litterCN <- loadByProduct(dpID="DP1.10033.001", site="all")
list2env(litterCN, .GlobalEnv)

# Root biochemistry
# 26 Oct 20: Bundled into DP1.10067.001
rootCN <- loadByProduct(dpID="DP1.10067.001", site="all")
list2env(rootCN, .GlobalEnv)
