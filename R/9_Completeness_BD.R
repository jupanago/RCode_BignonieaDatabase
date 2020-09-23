## Completeness and Survey effort quality of the Bignonieae Database (BD)

# This script contains the steps taken to applied the KnowBR package to
# analyze the completeness of the BD.

# Libraries 
library(dplyr)
library(KnowBR)
library(viridis)

# 1. Loading data and preparing format A ####

Total_DB <- read.csv("./data/Localidades_Total_v14.csv")

DB_knowbr <- Total_DB %>% 
  dplyr::select(NAME1, XCOOR, YCOOR) %>%
  group_by(NAME1, XCOOR, YCOOR) %>%
  summarise(Count = n())

glimpse(DB_knowbr)

# write.csv(DB_knowbr, file = "./output/DB_knowbr.csv")
# DB_knowbr <- read.csv("./output/DB_knowbr.csv")

# 2. Calculating completeness and Slope ####
data("adworld")

KnowB(data = as.data.frame(DB_knowbr), ### This function only creates external data, it does not assign a value to a name.
      format = "A",
      cell = 60,
      cutoff = 1,
      estimator = 1,
      extent = TRUE,
      save = "RData",
      colscale = viridis(9),
      jpg = TRUE)

comp_ras <- raster(as.matrix(Completeness))
plot(comp_ras)

# 3. Calculating survey effort ####
load("./output/KnowBR_output/v2/Estimators.RData")
load("./output/KnowBR_output/v2/Completeness.RData")
load("./output/KnowBR_output/v2/Slope.RData")

data("adworld")

# NOTE: This function has several internal problems to save the images ouput. I have already save the map, and the ratio and polar coordinates plot
# were saved from RStudio plot viewer after inactivating the dev.new() function in the lines 730, 868, 970, and 1022 of the SurveyQ function. Source code 
# from:

source("./R/KnowBr/function_SurveyQ_JPNG.R") # NOTE: Changes not shown.

SurveyQ_jpng(data = values, 
             Longitude = DB_knowbr$XCOOR, 
             Latitude = DB_knowbr$YCOOR, 
             minLon = (min(DB_knowbr$XCOOR) -1) , 
             maxLon = (max(DB_knowbr$XCOOR) + 1), 
             minLat = min(DB_knowbr$YCOOR), 
             maxLat = max(DB_knowbr$YCOOR),
             palette = viridis(3), 
             COLOR = viridis(3, direction = -1))

#### 3.1. Intelligence_SurveyQ1: SurveyQ dataframe ####

# NOTE: Dataframe with the values for survey quality. This was obtained after looking the SurveyQ internals

SurveyQ_df <- read.csv("./output/KnowBR_output/v2/SurveyQ_dataframe.csv")[-1]

SurveyQ_df %>%
  group_by(Survey) %>%
  summarise(n = n())

# # A tibble: 3 x 2
# Survey     n
#        <int> <int>
# Good       1     2
# Fair       2   390
# Poor       3   397


#### 3.2. Intelligence_SurveyQ2: SurveyQ matrix ####

# NOTE: Matrix with the values for survey quality. This was obtained after looking the SurveyQ::adareas internals
# Remember to replace -9999 with NA: SurveyQ_mat <- replace(SurveyQ_mat, SurveyQ_mat == -9999, NA)

SurveyQ_mat <- readRDS(file = "./output/KnowBR_output/v2/SurveyQ_matrix.rds")
ras_SurveyQ <- raster(SurveyQ_mat)
plot(ras_SurveyQ)

### 4. Survey quality MAP ####

# Survey quality
SurveyQ_ras <- readRDS(file = "./output/KnowBR_output/v2/SurveyQ_matrix.rds")
SurveyQ_ras <- raster(SurveyQ_ras)
# Rasters SurveyQ and sambias_rec don't have the same number of columns and rows.
# Correcting with modify_raster_margins
SurveyQ_ras <- modify_raster_margins(SurveyQ_ras, extent_delta = c(-1,0,-1,0), value = 0)
extent(SurveyQ_ras) <- extent(sampbias_rec)
SurveyQ_ras[SurveyQ_ras %in% c(-9999)] <- NA

test_spdf <- as(SurveyQ_ras, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

test_df <- test_df %>% 
  mutate(surQua = case_when(
    value == 0 ~ "No data",
    value == 1 ~ "High",
    value == 2 ~ "Fair",
    value == 3 ~ "Poor"
  ))

test_df$surQua <- factor(test_df$surQua, levels = c("No data", "Poor", "Fair", "High"))

world <- ne_countries(scale = "medium", returnclass = "sf")
Brazil <- ne_states(returnclass = "sf", country = "Brazil")

jppal <- viridis(3, alpha = 0.8, direction = 1)

SurveyQuality_map <- ggplot()  + 
  geom_tile(data = test_df, aes(x = x, y = y, fill = surQua), size = 0.1, color = "black") +
  geom_sf(data = world, fill = NA, colour = "black", lwd = 0.2) +
  geom_sf(data = Brazil, fill = NA, lwd = 0.2) +
  #geom_sf(data = brbioma, fill = NA, colour = "black", lwd = 0.5) +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = FALSE) +
  #scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  scale_fill_manual(values = jppal) +
  theme_few() + 
  guides(fill = guide_legend(title = "Survey Quality", 
                             barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points")) +
  labs(x = NULL, y = NULL)

tiff("./figs/9_SurveyQ.tiff", res=300, compression = "lzw", width=8, height=8, units="in")
SurveyQuality_map
dev.off()