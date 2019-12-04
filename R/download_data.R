# This code downloads and stacks 5 NEON data products used in the NEON N Across Scales manuscript.
#
# You will only need to run this code once to download the data and unzip the files
# to stack the data into merged files by data product. 
# 
# The neonUtilities zipsByProduct() function downloads .zip files for data products by site x year
# and the stackByTable() function unzips and combines each data product file into a single spreadsheet.
#
# You could re-run this code periodically to try to find any updated data products, 
# but beware that this might overwrite existing previously downloaded data. 
#
# Data products used in this project include: 
#   canopy foliar chemistry: DP1.10026.001
#   soil chemical properties (distributed plots, periodic): DP1.10078.001
#   soil inorganic N pools and transformations: DP1.10080.001
#   litter chemical properties: DP1.10031.001
#   litter & fine woody debris: DP1.10033.001
# 
# The downloaded .zip files will require ~30MB of space (as of 11/2018)
# and the unzipped and stacked files require ~75MB (as of 11/2018).


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
zipsByProduct(dpID="DP1.10026.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10026/", folder=T)

# Download and stack soil chemical properties (distributed plots, periodic): DP1.10078.001
zipsByProduct(dpID="DP1.10078.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10078/", folder=T)

# Download and stack soil inorganic N pools and transformations: DP1.10080.001
zipsByProduct(dpID="DP1.10080.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10080/", folder=T)

# Download and stack litter chemical properties: DP1.10031.001
zipsByProduct(dpID="DP1.10031.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10031/", folder=T)

# # Download and stack litter & fine woody debris: DP1.10033.001
# zipsByProduct(dpID="DP1.10033.001", site="all", package="basic")
# stackByTable(filepath="filesToStack10033/", folder=T)

# Download and stack soil physical properties (distributed): DP1.10047.001
zipsByProduct(dpID="DP1.10047.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10047/", folder=T)

# Download and stack woody vegetation plot data: DP1.10098.001
zipsByProduct(dpID="DP1.10098.001", site="all", package="basic", savepath="data/")
stackByTable(filepath="data/filesToStack10098/", folder=T)


