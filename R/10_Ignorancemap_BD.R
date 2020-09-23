## Spatial prioritization and ignorance map of Bignonieae Database (BD)

# This script contains the steps taken to join the results of the sampbias 
# and the survey effort analyses to draw a map of ignorance of the BD and
# define new survey areas at the regional scale for Bignonieae (Bignoniaceae).

library(tidyverse)
library(raster)
library(spatial.tools)
library(rnaturalearth)

### 1. Loading and reclasifying rasters ####

# NOTE: The raster files the original analyses have different column and row numbers.
# All except the SurveyQ raster have 74 and 76. I increased by one the number of columns and 
# rows. This worked well.
# Masking the rasters erased some cells near the border of the continent. 
# The sampbias raster don't cover exactly the same regions at the border.
# The new rasters for every analysis were saved. Their name was altered by 
# adding the number 2 at their end. 

# Bias effect
sampbias_ras <- raster("./output/Sampbias/Sampbias_raster_ALLBFACTORS.tif")
sampbias_ras <- modify_raster_margins(sampbias_ras, extent_delta = c(1,0,1,0), value = NA) # Increasing one row and one column

# Survey quality
SurveyQ_ras <- readRDS(file = "./output/KnowBR_output/v2/SurveyQ_matrix.rds")
SurveyQ_ras <- raster(SurveyQ_ras)
#SurveyQ_ras <- modify_raster_margins(SurveyQ_ras, extent_delta = c(-1,0,-1,0), value = 0) # Reducing one row and one column
extent(SurveyQ_ras) <- extent(sampbias_ras)
# SurveyQ_ras <- resample(SurveyQ_ras, sampbias_ras, method='ngb')
# SurveyQ_ras <- mask(SurveyQ_ras, sampbias_ras)
SurveyQ_ras[SurveyQ_ras %in% c(1,2,-9999)] <- 0
#SurveyQ_ras[SurveyQ_ras == 3] <- 3
#writeRaster(SurveyQ_ras, "./output/Prioritisation/SurveyQ_poor2.tif")

table(values(SurveyQ_ras))

# Occurrence raster
sbias_object <- readRDS("./output/Sampbias/SampbiasV2_object.rds")
occ_ras <- sbias_object$occurrences
occ_ras <- modify_raster_margins(occ_ras, extent_delta = c(1,0,1,0), value = NA) # Increasing one row and one column
occ_ras <- mask(occ_ras, sampbias_ras)
occ_ras <- calc(occ_ras, fun=function(x){ x[x > 0] <- 10; return(x)} )
occ_ras <- calc(occ_ras, fun=function(x){ x[x < 10] <- 1; return(x)} )
occ_ras <- calc(occ_ras, fun=function(x){ x[x == 10] <- 0; return(x)} )
#writeRaster(occ_ras, "./output/Prioritisation/No_ccurrences2.tif")

# Reclasifying sambias_ras
# max(values(sampbias_ras), na.rm = T) # max:1.395441
# min(values(sampbias_ras), na.rm = T) # min: 0.2109771
sampbias_rec <- calc(sampbias_ras, fun = function(x) {x[x > 0.2 & x <= 0.7] <- 10; return(x)} )
sampbias_rec <- calc(sampbias_rec, fun = function(x) {x[x < 10] <- 0;  return(x)} )
sampbias_rec <- calc(sampbias_rec, fun = function(x) {x[x == 10] <- 5; return(x)} )
#writeRaster(sampbias_rec, "./output/Prioritisation/sampbias_reclass2.tif")

### 2. Final rasters ####

SurveyQ_ras <- raster("./output/Prioritisation/SurveyQ_poor2.tif")
occ_ras <- raster("./output/Prioritisation/No_ccurrences2.tif")
sampbias_rec <- raster("./output/Prioritisation/sampbias_reclass2.tif")

ignorance_ras <- occ_ras + SurveyQ_ras 
unique(values(ignorance_ras)) # [1] NA  1  0  3

# Summing rasters
Surv_igno_ras <- ignorance_ras + sampbias_rec
unique(values(Surv_igno_ras)) # [1] NA  1  0  3  5  6  8

par(mfrow = c(2, 2), mar = c(1,1,1,1))
plot(occ_ras)
plot(SurveyQ_ras)
plot(sampbias_rec)
plot(Surv_igno_ras)

### 2. Visualization ####

library(tidyverse)
library(ggplot2)
library(viridis)
library(ggthemes)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

### 2.1. Legend ####

ras_spdf <- as(Surv_igno_ras, "SpatialPixelsDataFrame")
ras_df <- as.data.frame(ras_spdf)
colnames(ras_df) <- c("value", "x", "y")

ras_df <- ras_df %>%
  mutate(grouping = case_when(
    value == 0 ~ "Fair",
    value == 1 ~ "No-sampled",
    value == 3 ~ "Poor_surveyed",
    value == 5 ~ "Lowest_rates",
    value == 6 ~ "No-sampled_LR",
    value == 8 ~ "PoorSurv_LR"
  ))

ras_df$grouping <- factor(ras_df$grouping, levels = c("Fair", "No-sampled", "Poor_surveyed", "Lowest_rates", "No-sampled_LR", "PoorSurv_LR"), ordered = TRUE)

jpng_pal <- c("white", viridis(5))

p <- ras_df %>%
  ggplot(aes(x = x, y = y, group = grouping, fill = grouping)) +
  scale_fill_manual(values = jpng_pal) +
  geom_point(shape = 22, color = "black", size = 2, alpha = 0.8) +
  labs(fill = "Cell status") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

legend <- ggpubr::get_legend(p)

### 2.2. Ploting maps  ####
plotmap_ras <- function(x, opt_plot) {
  
  library(sf)
  library(ggthemes)
  library(rnaturalearth)
  
  amer <- ne_countries(scale = "medium", returnclass = "sf")
  Brazil <- ne_states(returnclass = "sf", country = "Brazil")
  
  ras_spdf <- as(x, "SpatialPixelsDataFrame")
  ras_df <- as.data.frame(ras_spdf)
  colnames(ras_df) <- c("value", "x", "y")
  
  jpng_pal <- c("white", viridis(5))
  
  if (opt_plot == 1) {
    
    legendpos <- "none"
    title_plot <- "No-sampled cells"
    jpng_pal <- jpng_pal[c(1,2)]
    
  }
  if (opt_plot == 2) {
    
    legendpos <- "none"
    title_plot <- "Poor-surveyed cells"
    jpng_pal <- jpng_pal[c(1,2)]
    
  }
  if (opt_plot == 3) {
    
    legendpos <- "none"
    title_plot <- "The lowest sampling rates"
    jpng_pal <- jpng_pal[c(1,2)]
    
  }
  if (opt_plot == 4) {
    
    legendpos <- "none"
    title_plot <- "Ignorance map"
    
  }
  
  rasplot <- ggplot() +  
    geom_tile(data=ras_df, aes(x=x, y=y, fill=value), alpha=0.8, color = "black") + 
    geom_sf(data = amer, fill = NA) +
    geom_sf(data = Brazil, fill = NA, lwd = 0.2) +
    coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = FALSE) +
    scale_fill_gradientn(colors = jpng_pal) +
    theme_few() +
    theme(legend.position = legendpos) +
    labs(x = "Longitude", 
         y = "Latitude",
         title = title_plot)
  
  return(rasplot)
  
}

plot1 <- plotmap_ras(x = occ_ras, opt_plot = 1)
plot2 <- plotmap_ras(x =  mask(SurveyQ_ras, sampbias_ras), opt_plot = 2)
plot3 <- plotmap_ras(x = sampbias_rec, opt_plot = 3)
#plot4 <- plotmap_ras(x = Surv_igno_ras, opt_plot = 4)

world <- ne_countries(scale = "medium", returnclass = "sf")
Brazil <- ne_states(returnclass = "sf", country = "Brazil")

igno_ras <- as(Surv_igno_ras, "SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  rename(value = layer, x = x, y = y)

unique(igno_ras$value)

igno_ras <- igno_ras %>% 
  mutate(cell_status = case_when(
    value == 0 ~ "Fair",
    value == 1 ~ "Unsampled (Un)",
    value == 3 ~ "Poor surveyed (Ps)",
    value == 5 ~ "Lowest sampling rates (LR)",
    value == 6 ~ "Un-LR",
    value == 8 ~ "Ps-LR"
  ))

igno_ras$cell_status <- factor(igno_ras$cell_status, levels = c("Fair", "Unsampled (Un)", "Poor surveyed (Ps)", "Lowest sampling rates (LR)", "Un-LR", "Ps-LR"), ordered = TRUE)

plot4 <- ggplot() +  
  geom_tile(data=igno_ras, aes(x=x, y=y, fill=cell_status), alpha=0.8, color = "black") + 
  geom_sf(data = world, fill = NA, colour = "black", lwd = 0.2) +
  geom_sf(data = Brazil, fill = NA, lwd = 0.2) +
  #geom_sf(data = brbioma, fill = NA, colour = "black", lwd = 0.5) +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = FALSE) +
  scale_fill_manual(values = jpng_pal) + 
  guides(fill = guide_legend(title = "Cell status", 
                             barheight = unit(10, units = "points"))) +
  theme_few() +
  theme(legend.position = "none") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Ignorance Maps: 1. States"
  )

brbioma <- st_read("./data/GIS/bioma.shp")

plot5 <- ggplot() +
  geom_tile(data=igno_ras, aes(x=x, y=y, fill=value), alpha=0.8, color = "black") +
  #geom_sf(data = amer, fill = NA) +
  geom_sf(data = world, fill = NA, colour = "black", lwd = 0.2) +
  #geom_sf(data = Brazil, fill = NA, lwd = 0.2) +
  geom_sf(data = brbioma, fill = NA, lwd = 0.5) +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = FALSE) +
  scale_fill_gradientn(colors = jpng_pal) +
  theme_few() +
  theme(legend.position = "none") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "2. Phytogeographic Domains")

### 3. Saving map ####

#png("./output/Prioritisation/prior_igno_map.png", width = 1000, height = 1000, res = 120)
(plot1 | plot2) / (plot3 | plot4) + patchwork::plot_annotation(tag_levels = "A", theme = theme(plot.tag = element_text(face = "bold")))
#dev.off()

#png("./output/Prioritisation/legend_map.png", width = 250, height = 500, res = 150)
ggplotify::as.ggplot(legend)
#dev.off()

### 3.1. Saving individual plots ####

png("./output/Prioritisation/legend_map2.png", width = 700, height = 100, res = 150)
ggplotify::as.ggplot(legend)
dev.off()

png("./output/Prioritisation/NEWFIG_plot1.png", width = 800, height = 800, res = 120)
plot1
dev.off()

png("./output/Prioritisation/NEWFIG_plot2.png", width = 800, height = 800, res = 120)
plot2
dev.off()

png("./output/Prioritisation/NEWFIG_plot3.png", width = 800, height = 800, res = 120)
plot3
dev.off()

png("./output/Prioritisation/NEWFIG_plot4.png", width = 800, height = 800, res = 120)
plot4
dev.off()

png("./output/Prioritisation/NEWFIG_plot5.png", width = 800, height = 800, res = 120)
plot5
dev.off()

lay <- "ADD
        BDD
        CEE
        #EE"

lay <- "AABBCC
        DDDEEE
        DDDEEE
        "

lay <- "AADDDD
        ##DDDD
        BBDDDD
        ##EEEE
        CCEEEE
        ##EEEE"

plot1 + plot2 + plot3 + plot4 + plot5 +
  plot_layout(design = lay)

### 3.2. Main Raster prioritization: Ignorance map ####

igno_ras <- igno_ras %>% 
  mutate(cell_status = case_when(
    value == 0 ~ "Fair",
    value == 1 ~ "Unsampled (Un)",
    value == 3 ~ "Poor surveyed (Ps)",
    value == 5 ~ "Lowest sampling rates (LR)",
    value == 6 ~ "Un-LR",
    value == 8 ~ "Ps-LR"
  ))

igno_ras$cell_status <- factor(igno_ras$cell_status, levels = c("Fair", "Unsampled (Un)", "Poor surveyed (Ps)", "Lowest sampling rates (LR)", "Un-LR", "Ps-LR"), ordered = TRUE)

plot6 <- ggplot() +  
  geom_tile(data=igno_ras, aes(x=x, y=y, fill=cell_status), alpha=0.8, size = 0.1, color = "black") + 
  geom_sf(data = world, fill = NA, colour = "black", lwd = 0.2) +
  geom_sf(data = Brazil, fill = NA, lwd = 0.2) +
  #geom_sf(data = brbioma, fill = NA, colour = "black", lwd = 0.5) +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = FALSE) +
  scale_fill_manual(values = jpng_pal) + 
  guides(fill = guide_legend(title = "Cell status", 
                             barheight = unit(10, units = "points"))) +
  theme_few() +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL
  )

tiff("./figs/10_IgnoMap.tiff", res=300, compression = "lzw", width=8, height=8, units="in")
plot6 
dev.off()

### 4. Other relevant information from rasters ####

# Number of cells with and without records 

length(occ_ras[occ_ras == 1]) # Cells with no records
length(occ_ras[occ_ras == 0]) # Cells with records
length(occ_ras[occ_ras == 0]) + length(occ_ras[occ_ras == 1]) # Total number of cells

(length(occ_ras[occ_ras == 1]) / (length(occ_ras[occ_ras == 0]) + length(occ_ras[occ_ras == 1]))) * 100 
# [1] 37.83494 # Percentage of cells without records

(length(occ_ras[occ_ras == 0]) / (length(occ_ras[occ_ras == 0]) + length(occ_ras[occ_ras == 1]))) * 100 
# [1] 62.16506 # percentage of cells with records

length(occ_ras[occ_ras == 1]) / length(occ_ras)
