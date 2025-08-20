#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Pronghorn RSF
# Leveraging WebGIS tools and existing agency monitoring data to efficiently
# map suitable habitat

# Jason Carlisle
# Wyoming Game and Fish Department

# Script 2 of 4:  Prep presence and (pseudo)absence points
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Packages, CRS, and random seed ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
require(sf)
require(terra)
require(dplyr)

# Master coordinate reference system to use for all spatial data
myCRS <- 26913

# Set seed to make random pseudo-absence points reproducible
set.seed(82071)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Study area ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Portion of Medicine Bow herd unit
aoi <- st_read(file.path("InputData",
                                "StudyArea_AllOwnership.kml")) |>
  st_transform(crs = myCRS)


# Public land in the study area
aoi_public <- st_read(file.path("InputData",
                            "StudyArea_Public.kml")) |>
  st_transform(crs = myCRS)


plot(st_geometry(aoi_public), col = "lightgrey", border = FALSE)
plot(st_geometry(aoi), lwd = 3, add = TRUE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Pronghorn locations from aerial line transect (LT) survey ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# LT lines surveyed
l <- st_read(file.path("InputData",
                       "SurveyData_TransectLines.kml")) |>
  st_transform(myCRS) |>
  select(geometry)


# LT observations
pts <- st_read(file.path("InputData",
                         "SurveyData_PronghornPoints.kml")) |>
  st_transform(myCRS) |>
  mutate(PresAbs = 1) |>
  select(PresAbs, geometry)
nrow(pts)  # 188



plot(st_geometry(aoi_public), col = "lightgrey", border = FALSE)
plot(st_geometry(aoi), lwd = 3, add = TRUE)
plot(st_geometry(l), col = "blue", lwd = 2, add = TRUE)
plot(st_geometry(pts), col = "blue", cex = 1.5, add = TRUE)



# Random points
# # Constrain to be on lines only
# r <- st_sf(PresAbs = 0,
#            geometry = st_sample(l,
#                size = nrow(x)))
# 
# # Made one multipoint for each transect, cast into individual points
# r <- r |>
#   filter(!st_is_empty(.)) |>
#   st_cast("POINT",
#           warn = FALSE)
# 
# plot(st_geometry(r), col = "red", add = TRUE)


# Allow anywhere on public land in aoi
pts_rando <- st_sf(PresAbs = 0,
                   geometry = st_sample(aoi_public,
                                        size = nrow(pts)))


plot(st_geometry(aoi_public), col = "lightgrey", border = FALSE)
plot(st_geometry(aoi), lwd = 3, add = TRUE)
plot(st_geometry(l), col = "blue", lwd = 2, add = TRUE)
plot(st_geometry(pts), col = "blue", cex = 1.5, add = TRUE)
plot(st_geometry(pts_rando), col = "red", cex = 1.5, add = TRUE)



# Combine
pts <- pts |>
  bind_rows(pts_rando)

# mapview(l) +
#   mapview(pts, zcol = "PresAbs")


nrow(pts)  # 376
table(pts$PresAbs)
# 0   1 
# 188 188


# Write presence / absence points
pts |>
  saveRDS(file.path("PreppedData",
                     "Pronghorn_PresAbs.rds"))

# END