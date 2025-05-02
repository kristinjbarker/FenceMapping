source("01-load.R")
source("02-function.R")

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

# fencelines
spatFences <- rawFences %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org))) %>%
  # fix instances in which collector didn't enter their name (just techs and kimi)
  mutate(group = ifelse(collector == "Na" & 
                          grepl(Editor, pattern = "cal"), "BYLL", 
                        ifelse(collector == "Na", "AFI", group))) %>%
  # add field season
  mutate(fieldSeason = ifelse(EditDate > "2024-06-05", "summer2024", NA),
         withNewApp = ifelse(Editor == "shapiroPython", "no", "yes")) %>%
  # combine standard and nonstandard wire heights
  mutate(wireHeights = ifelse(Fence_Wire == "Other", Fence_Wi_1, Fence_Wire)) %>%
  # add length
  mutate(lgthMeters = as.numeric(st_length(.))) %>%
  # make column names more intuitive
  rename(fenceID = AFI_FenceI,
         type = Fence_Mate,
         material = Material_T,
         numberWires = Num_Wires,
         numberPoles = Num_Poles,
         condition = Fence_Cond,
         landowner = Land_Agenc,
         WGFDregion = WGFD_Regio,
         dateMapped = Date_of_Co,
         dateAdded = CreationDa,
         dateEdited = EditDate,
         arcAccount = Editor,
         access = Project_Ac,
         urgency = Project_Ur,
         status = Inventory_,
         railtop = Railtop,
         comments = Comments) %>%
  # remove some extraneous columns
  dplyr::select(fenceID, type, material, 
                condition, status, urgency,
                numberWires, wireHeights, railtop,
                numberPoles, access, 
                landowner, collector, group, dateMapped,
                dateAdded, arcAccount, dateEdited,  
                fieldSeason, withNewApp, 
                lgthMeters, Source, Creator,
                WGFDregion, Fence_Type, Fence_Ty_1, GlobalID_1,
                comments)
# also store in nonspatial dataframe
datFences <- st_drop_geometry(spatFences)

# fence features
spatFeatures <- rawFeatures %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))
# also store in nonspatial dataframe
datFeatures <- st_drop_geometry(spatFeatures)

# rangeland improvements
spatRange <- rawRange %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))
# also store in nonspatial dataframe
datRange <- st_drop_geometry(spatRange)

# wildlife observations
spatWildlife <- rawWildlife %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))
# also store in nonspatial dataframe
datWildlife <- st_drop_geometry(spatWildlife)


# # create shapefiles

# all fences mapped during 2024 field season
spatFences2024 <- spatFences %>%
  filter(withNewApp == "yes") %>%
  # add generic landownership (usf/blm/other)
  mutate(Ownership = ifelse(grepl(landowner, pattern = "Game"), "Forest Service", # WGFD is in SNF
                            # state borders BLM
                            ifelse(grepl(landowner, pattern = "State"), "Bureau of Land Management",
                                   ifelse(landowner != "Bureau of Land Management" & 
                                            landowner != "Forest Service", "Other",
                                          landowner))))   
datFences2024 <- st_drop_geometry(spatFences2024)

# all SNF allotments near Basin
fsAll <- rawUSFSall %>%
  # match projection of fences mapped near Basin
  st_transform(crs = st_crs(rawUSFSmapped)) %>%
  # make outlines rather than polygons
  st_cast("MULTILINESTRING") %>%
  # collapse into as few lines as possible
  st_union()

# mapped fences near Basin
fsMap <- rawUSFSmapped %>%
  st_cast("MULTILINESTRING") %>%
  st_union()

# unmapped fences

# buffer to within 100m of a mapped fence
fsBuff <- st_buffer(fsMap, dist = 100)
# find fences outside the buffer
fsRm <- st_difference(fsAll, fsBuff)


#### EXPORT ####

  # all fences currently in the database
  st_write(spatFences,
           "../../Data/Fencemapping/fencelines.shp", append = FALSE)

  # all fences mapped this season
  st_write(spatFences2024,
           "../../Data/Fencemapping/fencelines2024.shp", append = FALSE)
  
  # all fences mapped on USFS this season (by request)
  st_write(filter(spatFences2024, Ownership == "Forest Service"),
           "../../Data/Fencemapping/fencelines2024USFS.shp", append = FALSE)
  
  # fence features
  st_write(spatFeatures,
           "../../Data/Fencemapping/fenceFeatures.shp", append = FALSE)
  
  # range improvements
  st_write(spatRange,
           "../../Data/Fencemapping/rangeImprovements.shp", append = FALSE)
  
  # wildlife
  st_write(spatWildlife,
           "../../Data/Fencemapping/wildlifeSign.shp", append = FALSE)  

  # all fences UNmapped (to check in arc)
  st_write(fsRm,
           "../../Data/Fencemapping/fenceRemoved2024.shp", append = FALSE)