# fencelines
datFences <- rawFences %>%
  # remove spatial attributes
  st_drop_geometry() %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org))) %>%
  # add field season
  mutate(fieldSeason = ifelse(EditDate > "2024-06-05", "summer2024", NA),
         withNewApp = ifelse(Editor == "shapiroPython", "no", "yes")) %>%
  # combine standard and nonstandard wire heights
  mutate(wireHeights = ifelse(Fence_Wire == "Other", Fence_Wi_1, Fence_Wire)) %>%
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
                fieldSeason, withNewApp, Source, Creator,
                WGFDregion, Fence_Type, Fence_Ty_1, GlobalID_1,
                comments)

# fence features
datFeatures <- rawFeatures %>%
  # remove spatial attributes
  st_drop_geometry() %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))

# rangeland improvements
datRange <- rawRange %>%
  # remove spatial attributes
  st_drop_geometry() %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))

# wildlife observations
datWildlife <- rawWildlife %>%
  # remove spatial attributes
  st_drop_geometry() %>%
  # make names characters and make all lowercase for easier string matching
  mutate(Collector_ = ifelse(is.na(Collector_), "NA", tolower(Collector_))) %>%
  # remove anything collected on training day
  filter(Date_of_Co != "2024-06-05") %>%
  # clean and standardize names of collectors
  mutate(collector = as.character(sapply(Collector_, clean_collector))) %>%
  # add organization name based on collector name
  mutate(group = as.character(sapply(collector, clean_org)))


# create shapefiles

# all fences - with clean data including collectors etc
spatFences <- rawFences %>%
  dplyr::select(GlobalID_1, geometry) %>%
  inner_join(datFences)
spatFences$length_m <- as.numeric(st_length(spatFences))

# all fences mapped during 2024 field season
spatFence2024 <- spatFences %>%
  filter(withNewApp == "yes")

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
# 
# 
# #### EXPORT ####
# 
#   # all fences currently in the database
#   st_write(spatFences,
#            "../../Data/Fencemapping/fencelines.shp", append = FALSE)
#   
#   # all fences mapped this season
#   st_write(spatFence2024,
#            "../../Data/Fencemapping/fencelines2024.shp", append = FALSE)
#   
#   # all fences UNmapped (check in arc)
#   st_write(fsRm,
#            "../../Data/Fencemapping/fenceRemoved2024.shp", append = FALSE)