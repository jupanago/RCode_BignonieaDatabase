## Sampling bias analysis of the Bignonieae Database (BD)

# This script contains the steps taken to applied the sampbias package to
# analyze the geographical bias factors of the BD.

# Libraries

library(dplyr)
library(readxl)
library(sampbias)
library(raster)

#### 1) Running sampbias ####

# Loading data

Total_DB <- read.csv("./data/Localidades_Total_v14.csv")
sbias_DB <- dplyr::select(Total_DB, NAME1, XCOOR, YCOOR)
colnames(sbias_DB) <- c("species", "decimalLongitude", "decimalLatitude")
# write.csv(sbias_DB, "./output/sbias_DB.csv")

sbias_object <- calculate_bias(sbias_DB, 
                               res = 1, 
                               gaz = NULL) # Using default gazetteer

summary(sbias_object)


#### 2) Plots ####
#saveRDS(sbias_object, "./output/Sampbias/SampbiasV2_object.rds")
sbias_object <- readRDS("./output/Sampbias/SampbiasV2_object.rds")

boxp_bias <- plot(sbias_object)

boxp_bias +
  geom_boxplot(size = 0.1)

tiff("./figs/8_sampbias_boxplot.tiff", res=300, compression = "lzw", width=10, height=10, units="in")
boxp_bias +
  geom_boxplot(size = 0.1)
dev.off()

#projecting the bias effect in space 

tiff("./figs/8_sampbias_effectmap.tiff", res=300, compression = "lzw", width=10, height=10, units="in")
plots +
  theme_few()
dev.off()

#### 3) Raster extraction for further analysis (Prioritization) ####

sppoints_df <- plots$data %>%
  filter(split == "cities.rivers.airports.roads") %>%
  #mutate(val = log10(val)) %>%
  dplyr::select(-starts_with("split"))

coordinates(sppoints_df) <- ~ geo_lon + geo_lat

gridded(sppoints_df) <- TRUE

sampbias_ras <- raster(sppoints_df)                     

#writeRaster(sampbias_ras, "./output/Sampbias/Sampbias_raster_ALLBFACTORS.tif")
sampbias_ras <- raster("./output/Sampbias/Sampbias_raster_ALLBFACTORS.tif")

par(mfrow = c(1, 2))
plot(sampbias_ras)
plot(sampbias_ras2)
identical(sampbias_ras, sampbias_ras2)

## Extracting occurrence raster 

sbias_object$occurrences
