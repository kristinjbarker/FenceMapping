#### FING USFS REMOVED ####

matches <- st_intersects(fsAll, fsMap)
matches

matches <- st_is_within_distance(fsAll, fsMap, dist = 100)
matches

test <- fsAll[1][!matches,]
plot(test)


# identify all names entered as data collectors
rawCollectors <- sort(unique(c(rawFences$Collector_, rawFeatures$Collector_, rawWildlife$Collector_)))
rawCollectors

# usfs fences around Basin (for determining length of fences deleted)
spatUSFSall <- rawUSFSall %>% 
  # match crs of field data
  st_transform(crs = st_crs(rawUSFSmapped)) %>%
  # make polygons into border lines
  st_cast("MULTILINESTRING") 

# usfs fences mapped around Basin
spatUSFSmapped <- rawUSFSmapped %>%
  # match object type of all usfs 
  st_cast("MULTILINESTRING") 

# usfs fences removed from database during field mapping season
buffUSFSmapped <- st_buffer(spatUSFSmapped, dist = 100)


spatUSFSdel <- st_difference(spatUSFSall, buffUSFSmapped)

plot(spatUSFSdel[1])

# try making it just one thing for simplicity
fsAll <- fsAll[6]

plot(fsAll[1], col = "black")

any(!st_is_within_distance(fsAll, fsMap, dist = 100))
any(is.null(fsAll))
fsIn <- st_is_within_distance(fsAll, fsMap, dist = 1)
plot(fsIn)
summary(fsIn)