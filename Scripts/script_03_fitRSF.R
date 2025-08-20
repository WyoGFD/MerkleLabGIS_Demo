#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Pronghorn RSF
# Leveraging WebGIS tools and existing agency monitoring data to efficiently
# map suitable habitat

# Jason Carlisle
# Wyoming Game and Fish Department

# Script 3 of 4:  Fit RSF using Random Forests model
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Packages, CRS, and random seed ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
require(sf)
require(terra)
require(dplyr)
require(tibble)
require(tidyr)
require(ggplot2)
require(randomForest)
require(rfUtilities)

# Master coordinate reference system to use for all spatial data
myCRS <- 26913

# Set seed to make Random Forests model reproducible
set.seed(82071)


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
table(pts$Pres)
#   0   1 
# 188 188

# Covariate rasters
covars <- readRDS(file.path("PreppedData",
                            "CovarRasters.rds"))
nlyr(covars)  # 22

# Check plot of one raster
plot(covars$DEM_100m, main = "DEM")
lines(aoi)
pts |>
  filter(PresAbs == 1) |>
  points(pch = 1, col = "black")
pts |>
  filter(PresAbs == 0) |>
  points(pch = 1, col = "red")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Extract raster values at points ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

pts <- pts |>
  mutate(ID = 1:nrow(pts))

df <- terra::extract(covars, pts)

df <- pts |>
  st_drop_geometry() |>
  left_join(df, by = "ID") |>
  select(-ID) |>
  mutate(PresAbs = as.factor(PresAbs))


nrow(df)  # 376


# Remove any NA values (MODIS layers has some for some reason)
df <- na.omit(df)
nrow(df)  # 372



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Screen covariates for multi-collinearity ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Vector of problematic variables
(toRemove <- df |>
   select(-PresAbs) |>
   multi.collinear(p = 0.06))

# Remove them
df <- df |>
  select(-toRemove)


# Quick scatterplot of covariate values at used and pseudo-absence points
df |>
  pivot_longer(cols = -PresAbs) |>
  ggplot(aes(x = value,
             y = as.numeric(as.character(PresAbs)))) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE) +
  labs(x = "Value",
       y = "Probability of Use") +
  facet_wrap("name",
             scales = "free")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit RSF using Random Forests ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Number of trees
n_trees <- 2501

# RF Model Selection
# Uses the model improvement ratio to select a final model
# Tests 10 models (Murphy et al. 2010)
modSelection <- rf.modelSel(y = df[, "PresAbs"],
                            x = df[, names(df) != "PresAbs"],
                            imp.scale = "mir",
                            ntree = n_trees,
                            r = c(seq(0.1, 0.9, 0.1)),
                            seed = 82071)

# Format table of model selection results
(modSelectionTable <- modSelection$test |>
    rownames_to_column(var = "Model") |>
    rename(MIR_Threshold = THRESHOLD,
           OOB_Error = OOBERROR,
           Class_Error = CLASS.ERROR,
           K = NPARAMETERS) |>
    mutate(across(c(MIR_Threshold,
                    OOB_Error,
                    Class_Error),
                  ~ round(.x,
                          digits = 2))))

# Initial variable importance plots for top-ranked model
plot(modSelection)

# Fit final model
# Feed in only the x variables from the top-ranked model (selvars)
(rf_model <- randomForest(y = df[, "PresAbs"],
                          x = df[, names(df) %in% modSelection$selvars],
                          ntree = n_trees,
                          importance = TRUE,
                          norm.votes = TRUE,
                          proximity = TRUE))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Random Forests output ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# OOB error and confusion matrix tables
rf_model$err.rate[n_trees, ] |>
  t() |>
  write.csv(file.path("Output",
                      "RF_ErrorRates.csv"),
            row.names = FALSE)

rf_model$confusion |>
  write.csv(file.path("Output",
                      "RF_ConfusionMatrix.csv"),
            row.names = FALSE)


# Bootstrap error convergence plot
png(file.path("Output",
              "RF_ErrorConvergence.png"),
    width = 5, height = 5, units = "in", res = 600)
plot(rf_model,
     main = "Bootstrap Error Convergence",
     ylim = c(0, 1),
     col = c("black", "red", "blue"))
legend("topright",
       legend = c("Out of Bag (OOB)", "Random Points", "Presence Points"),
       fill = c("black", "red", "blue"))
dev.off()


# Variable importance plots
png(file.path("Output",
              "RF_VariableImportance.png"),
    width = 7.5, height = 5.5, units = "in", res = 600)
# par(mfrow = c(1, 1), mar = c(5.1, 5.1, 4.1, 2.1))  # default
p <- as.matrix(rf_model$importance[, 3])
ord <- rev(order(p[, 1], decreasing = TRUE)[1:dim(p)[1]])  
dotchart(p[ord, 1], pch = 19,
         main = "Variable Importance",
         xlab = "Mean Decrease in Accuracy",
         xlim = c(0, max(p[, 1])))

dev.off()


# Partial-effects plots
png(file.path("Output",
              "RF_PartialEffects.png"),
    width = 15, height = 8, units = "in", res = 600)
par(mfrow = c(3, 5), mar = c(5.1, 4.1, 0.6, 0.6))
for (i in modSelection$selvars[rev(ord)]) {
  rf.partial.prob(x = rf_model,
                  pred.data = df,
                  xname = i,
                  which.class = "1",
                  main = "",
                  ylab = "Probability")
}
dev.off()


# Write Random Forests model as an RDS object to use in prediction step
rf_model |>
  saveRDS(file.path("PreppedData",
                    "Pronghorn_RandomForests_Model.rds"))

# END