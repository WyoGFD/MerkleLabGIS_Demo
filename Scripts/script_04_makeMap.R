#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Pronghorn RSF
# Leveraging WebGIS tools and existing agency monitoring data to efficiently
# map suitable habitat

# Jason Carlisle
# Wyoming Game and Fish Department

# Script 4 of 4:  Create predicted habitat suitability map
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Packages and CRS ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
require(sf)
require(terra)
require(dplyr)
require(ggplot2)
require(tidyterra)
require(randomForest)

# Master coordinate reference system to use for all spatial data
myCRS <- 26913



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Read in data ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Portion of Medicine Bow herd unit
aoi <- st_read(file.path("InputData",
                         "StudyArea_AllOwnership.kml")) |>
  st_transform(crs = myCRS)

# Points (pres/absence)
pts <- readRDS(file.path("PreppedData",
                         "Pronghorn_PresAbs.rds"))
nrow(pts)  # 376


# Covariate rasters
covars <- readRDS(file.path("PreppedData",
                            "CovarRasters.rds"))
nlyr(covars)  # 22


# Check plot of one raster
plot(covars$MOD09Q1_2023_SpringLength, main = "Spring Length")
lines(aoi, lwd = 2)
pts |>
  filter(PresAbs == 1) |>
  points(pch = 1, col = "black")
pts |>
  filter(PresAbs == 0) |>
  points(pch = 1, col = "red")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# RSF spatial prediction raster -----------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read in fitted RF model
rf_model <- readRDS(file.path("PreppedData",
                              "Pronghorn_RandomForests_Model.rds"))

# Mask out any cells with NAs
sapply(covars, anyNA)

# Make a raster with NA where any covariate is NA or outside the study area
# and 1 elsewhere
mask_raster <- anyNA(covars) |>
  subst(from = TRUE,
        to = NA,
        others = 1) |>
  mask(mask = aoi,
       updatevalue = NA)
plot(mask_raster)

# Plug in a dummy 0 for NA values to get prediction to run,
# then use mask_raster to mask those cells
covars_noNA <- covars |>
  subst(from = NA,
        to = 0)


# Predict
pred <- predict(object = covars_noNA,
                model = rf_model,
                type = "prob",
                cores = 3,
                cpkgs = "randomForest")

# Mask
plot(pred$X1)
rsf <- pred$X1 * mask_raster
plot(rsf)


# Map with ggplot2 and tidyterra
ggplot() +
  geom_spatraster(data = rsf,
                  maxcell = ncell(rsf)) +
  scale_fill_viridis_c(option = "mako",
                       direction = -1,
                       na.value = NA,
                       limits = c(0, 1)) +
  labs(fill = "Habitat\nsuitability") +
  geom_sf(data = aoi,
          linewidth = 0.8,
          color = "black",
          alpha = 0) +
  geom_sf(data = pts[pts$PresAbs == 1, ],
          size = 0.8) +
  theme_bw()
ggsave(file.path("Output",
                 "Map.png"),
       width = 6, height = 6, units = "in", dpi = 600)

# END