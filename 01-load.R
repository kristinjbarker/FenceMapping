
# packages(list needed; install missing; load all)
packages <- c(
  "lubridate", # datetimes
  "cowplot", # muti-panel plotting
  "sf", # tidy spatdat
  "stringr", # cleaning names etc
  "tidyverse") # life
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}    
ipak(packages) ; rm(ipak, packages)



# raw data (exported via Feature Class to Feature Class tool in ArcPro from fence group)
# rawFences <- read_sf("../../Data/Fencemapping/rawFencelines.shp")
rawFences <- read_sf("../../Data/Fencemapping/rawFencelines.shp")
rawFeatures <- read_sf("../../Data/Fencemapping/rawFenceFeatures.shp")
rawWildlife <- read_sf("../../Data/Fencemapping/rawWildlifeObservations.shp")
rawRange <- read_sf("../../Data/Fencemapping/rawRangeImprovements.shp")
rawUSFSall <- read_sf("../../Data/Fencemapping/SNFpasturesNW.shp")
rawUSFSmapped <- read_sf("../../Data/Fencemapping/SNFmappedOnly.shp")

# match crs
rawUSFSall <- st_transform(rawUSFSall, st_crs(rawFences))

# all names entered as data collectors (need cleaning)
rawCollectors <- sort(unique(c(rawFences$Collector_, 
                               rawFeatures$Collector_, 
                               rawWildlife$Collector_,
                               rawRange$Collector_)))
