## Spatial coverage of the Bignonieae Database (BD)

# This script contains the steps taken to assess the temporal and taxonomic
# coverage.


library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)
library(red)
library(raster)
library(rasterVis)
library(rnaturalearth)
library(rnaturalearthdata)
library(paletteer)
library(ggthemes)
library(viridis)
library(scales)
library(sf)
library(rgeos)
library(rgdal)

#### 1) Biogeography table ####

# A table to caount the species presence in biogeographical units from 
# different regionalizations

#### 1.1) Morrone ####

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

big_df <- read.csv("./data/Localidades_Total_v14.csv")
big_df_pts <- st_as_sf(big_df, coords = c("XCOOR", "YCOOR"), crs = wgs84)

# Prepearing morrone

morr <- st_read("./data/GIS/Lowenberg_Neto_2014.shp")
morr <- st_transform(morr, crs = wgs84)

level_prov <- levels(morr$Province_1)
level_prov <- c(level_prov, "x_Andean region", "x_Neartic region")
morr$Province_1 <- factor(morr$Province_1, levels = level_prov)

morr$Province_1[15] <- "x_Andean region"
morr$Province_1[55] <- "x_Neartic region"

level_dom <- levels(morr$Dominions)
level_dom <- c(level_dom, "x_South American Transition Zone", "x_Mexican Transition Zone", "x_Antillean subregion", 
               "x_Andean region", "x_Neartic region")
morr$Dominions <- factor(morr$Dominions, levels = level_dom)

morr$Dominions[14] <- "x_South American Transition Zone"
morr$Dominions[15] <- "x_Andean region"
morr$Dominions[16] <- "x_South American Transition Zone"
morr$Dominions[19:21] <- "x_South American Transition Zone"
morr$Dominions[34] <- "x_South American Transition Zone"
morr$Dominions[37] <- "x_Mexican Transition Zone"
morr$Dominions[39] <- "x_Mexican Transition Zone"
morr$Dominions[41:42] <- "x_Mexican Transition Zone"
morr$Dominions[45] <- "x_Mexican Transition Zone"
morr$Dominions[47:53] <- "x_Antillean subregion"
morr$Dominions[55] <- "x_Neartic region"

level_subr <- levels(morr$Subregio_1)
level_subr <- c(level_subr, "x_Andean region", "x_Neartic region")
morr$Subregio_1 <- factor(morr$Subregio_1, levels = level_subr)

morr$Subregio_1[15] <- "x_Andean region"
morr$Subregio_1[55] <- "x_Neartic region"

#### 1.2) Ecorregions ####

ecoreg <- st_read("./data/GIS/tnc_terr_ecoregions.shp")

big_df_pts <- st_join(big_df_pts, ecoreg["ECO_NAME"])

glimpse(big_df_pts)

sum(is.na(big_df_pts$ECO_NAME))
View(big_df_pts)

save_biogtable <- as.data.frame(big_df_pts)
save_biogtable$XCOOR <- big_biog_df$XCOOR
save_biogtable$YCOOR <- big_biog_df$YCOOR
glimpse(save_biogtable)
#write.csv(save_biogtable[-12], "./output/Biogeography_tableECO.csv")

# Resolving NAs in Ecorregions 

eco_nas <- big_df_pts 

eco_nas$XCOOR <- big_df[which(is.na(big_df_pts$ECO_NAME)), ]$XCOOR
eco_nas$YCOOR <- big_df[which(is.na(big_df_pts$ECO_NAME)), ]$YCOOR

plot(st_geometry(ecoreg), xlim = c(-106.4833, -36.31), ylim = c(-27.583, 30.47131))
plot(st_geometry(eco_nas), col = "red", add = TRUE)

writeOGR(econas_sp, dsn = "./output/", layer = "nas_eco.shp", driver = "ESRI Shapefile")

### After manual edition using QGIS and Google Earth

eco_clean_partial <- read.csv("./output/ecoregion_nas_EDITEDQGIS.csv", stringsAsFactors = FALSE)
View(eco_clean_partial)

biogeo_table <- read.csv("./output/Biogeography_tableECO.csv", stringsAsFactors = FALSE)
View(biogeo_table)

IDs_clean <- eco_clean_partial$ID

ids_to_change <- which(biogeo_table$ID %in% IDs_clean) 

biogeo_table[ids_to_change, ]$ECO_NAME <- eco_clean_partial$ECO_NAME

View(biogeo_table)
sum(!is.na(biogeo_table$ECO_NAME)) # From 255 NAs to 0 NAs.

#write.csv(biogeo_table, "./output/Biogeography_table_RIP.csv")

# FROM: ./R_projects/endemism/Biogeography_tables.R
Biogeo_table <- read.csv("./output/Biogeography_table_RIP.csv")[-c(1, 2)]
glimpse(Biogeo_table)

sum(is.na(Biogeo_table$ECO_NAME)) ### This must be 0!!!

#### Filling NAs in $COUNTRY
Biogeo_table$COUNTRY[7832] <- "Guyana"
Biogeo_table$COUNTRY[7833] <- "Venezuela"
Biogeo_table$COUNTRY[8524] <- "Venezuela"
Biogeo_table$COUNTRY[13221] <- "Bolivia"
Biogeo_table$COUNTRY[13222] <- "Bolivia"
Biogeo_table$COUNTRY[16129] <- "Peru"


Biogeo_table <- Biogeo_table %>%
  group_by(ECO_NAME) %>%
  mutate(ECO_records = n(),
         ECO_spp = n_distinct(NAME1),
         ECO_end = 0) %>%
  group_by(Mor_subreg) %>%
  mutate(Sub_records = n(),
         Sub_spp = n_distinct(NAME1),
         Sub_end = 0) %>%
  group_by(Mor_domin) %>%
  mutate(domin_records = n(),
         domin_spp = n_distinct(NAME1),
         domin_end = 0) %>%
  group_by(Mor_prov) %>%
  mutate(prov_records = n(),
         prov_spp = n_distinct(NAME1),
         prov_end = 0) %>%
  group_by(COUNTRY) %>%
  mutate(country_records = n(),
         country_spp = n_distinct(NAME1),
         country_end = 0) %>%
  group_by(Gen_reg) %>%
  mutate(gen_records = n(),
         gen_spp = n_distinct(NAME1),
         gen_end = 0) %>%
  group_by(NAME1) %>%
  mutate(EOO = red::eoo(data.frame(decimalLongitude = XCOOR, decimalLatitude = YCOOR)), #km2 with lower confidence limit, consensus and upper confidence limit (probabilities 0.975, 0.5 and 0.025 respectively).
         AOO = red::aoo(data.frame(decimalLongitude = XCOOR, decimalLatitude = YCOOR))) %>% # Resolution 2x2km as required by IUCN. km2 with lower confidence limit, consensus and upper confidence limit (probabilities 0.975, 0.5 and 0.025 respectively).
  ungroup()


#### 2) Counting endemics (RAW: absolute points inside vs outside areas) per region ####

presence_table <- Biogeo_table %>%
  group_by(NAME1) %>%
  summarise(pres_sub = n_distinct(Mor_subreg),
            pres_dom = n_distinct(Mor_domin),
            pres_pro = n_distinct(Mor_prov),
            pres_eco = n_distinct(ECO_NAME),
            pres_country = n_distinct(COUNTRY))

presence_table %>%
  filter(pres_gen == 1) %>%
  summarise(pres = n()) 
# 207 in one subregion, 116 in one dominion, 60 in one province, and 33 in one ecorregion, and 129 in one Gentry region.

count_endemics <- function(x_pres, y_reg, reg_level) {
  
  x_pres <- enquo(x_pres)
  y_reg <- enquo(y_reg)
  
  a <- presence_table %>%
    filter(!! x_pres == 1) %>%
    dplyr::select(NAME1) %>%
    pull()
  
  endemics <- Biogeo_table %>%
    filter(NAME1 %in% a) %>%
    group_by(NAME1) %>%
    summarise(endemic = n_distinct(!! y_reg),
              Area_name = paste(unique(!! y_reg))) %>%
    group_by(Area_name) %>%
    summarise(Endemics = n())
  
  endemics$category <- reg_level
  
  return(endemics)
  
}

big_endemics <- rbind(
  count_endemics(x_pres = pres_sub, y_reg = Mor_subreg, reg_level = "Subregion"),
  count_endemics(x_pres = pres_dom, y_reg = Mor_domin, reg_level = "Dominion"),
  count_endemics(x_pres = pres_pro, y_reg = Mor_prov, reg_level = "Province"),
  count_endemics(x_pres = pres_eco, y_reg = ECO_NAME, reg_level = "Ecoregion"),
  count_endemics(x_pres = pres_country, y_reg = COUNTRY, reg_level = "Administrative")
)

View(big_endemics)

#### Including endemics count in biogeo_table

# Subregions
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$Mor_subreg[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$Sub_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$Mor_subreg[i], ]$Endemics
    
  } else {
    
    Biogeo_table$Sub_end[i] <- 0
  }
  
}
# Dominions
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$Mor_domin[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$domin_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$Mor_domin[i], ]$Endemics
    
  } else {
    
    Biogeo_table$domin_end[i] <- 0
  }
  
}
# Provinces
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$Mor_prov[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$prov_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$Mor_prov[i], ]$Endemics
    
  } else {
    
    Biogeo_table$prov_end[i] <- 0
  }
  
}
# Ecoregions
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$ECO_NAME[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$ECO_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$ECO_NAME[i], ]$Endemics
    
  } else {
    
    Biogeo_table$ECO_end[i] <- 0
  }
  
}
# Administrative
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$COUNTRY[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$country_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$COUNTRY[i], ]$Endemics
    
  } else {
    
    Biogeo_table$country_end[i] <- 0
  }
  
}
# Phytogeographical
for (i in seq_along(Biogeo_table$ID)) {
  
  if (Biogeo_table$Gen_reg[i] %in% big_endemics$Area_name) {
    
    Biogeo_table$gen_end[i] <- big_endemics[big_endemics$Area_name == Biogeo_table$Gen_reg[i], ]$Endemics
    
  } else {
    
    Biogeo_table$gen_end[i] <- 0
  }
  
}

#### 3) Plotting Species and endemics bar plots ####

plot_end <- function(regs, spp, endem, recs, kind) {
  
  library(grid)
  library(gridExtra)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  
  #### Drawing legend ####
  
  legend_labels <- c("Endemic")
  
  legend_position <- c(0.78)
  
  text_legend <- textGrob(label = legend_labels, x = 0.6, y = legend_position, just = "left") 
  
  rect_position_y <- c(0.78)
  rect_position_x <- rep(0.57, length(rect_position_y))
  
  square_legend <- rectGrob(x = rect_position_x, y = rect_position_y, 
                            width = 0.05, height = 0.05,
                            gp = gpar(fill = c("grey31", "darkgrey")))
  
  # square_legend_back <- rectGrob(x = 0.61, y = 0.745, 
  #                                width = 0.20, height = 0.15,
  #                                gp = gpar(fill = "white"))
  
  grob_legend <- grobTree(text_legend, square_legend, name = "jp_legendgrob")
  
  #### subsetting df and choosing graphical parameters ####
  regs <- enquo(regs)
  spp <- enquo(spp)
  endem <- enquo(endem)
  recs <- enquo(recs)
  
  if (kind == "Subregion") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 386, 50))
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -2, xmax = 2, 
                                          ymin = 60, ymax = 370)
    
  }
  
  if (kind == "Dominion") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs) %>%
      filter(!!regs %in% c("Boreal Brazilian dominion", "Chacoan dominion", "Mesoamerican dominion", 
                           "Pacific dominion", "Parana dominion", "South Brazilian dominion", "South-eastern Amazonian dominion"))
    
    y_scale <- scale_y_continuous(breaks = seq(0, 386, 30)) 
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -2, xmax = 2, 
                                          ymin = 60, ymax = 240)
    
  }
  
  if (kind == "Province") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs) %>%
      filter(!(!!regs) %in% c("x_Neartic region"))
    
    y_scale <- scale_y_continuous(breaks = seq(0, 386, 10))
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -17, xmax = 12, 
                                          ymin = 50, ymax = 160)
    
  }
  
  if (kind == "Ecoregion") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs) %>%
      filter(!!spp > 20)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 386, 10))
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -20, xmax = 15, 
                                          ymin = 50, ymax = 165)
    
  }
  
  if (kind == "Administrative") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 310, 20))
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -10, xmax = 7, 
                                          ymin = 70, ymax = 390)
    
  }
  
  if (kind == "Phytogeographical") {
    
    df <- Biogeo_table %>%
      distinct(!!regs, !!spp, !!endem, !!recs) %>%
      arrange(!!recs)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 310, 20))
    
    annotate_legend <-  annotation_custom(grob = grob_legend, 
                                          xmin = -10, xmax = 7, 
                                          ymin = 70, ymax = 390)
    
  }
  
  
  #### generating plot ####
  df[,1] <- factor(as.character(pull(df[,1])), levels =  unique(as.character(pull(df[,1])), ordered = TRUE))
  
  jpgray <- gray.colors(n = 4)
  #show_col(jpgray)
  
  p <-  df %>%
    ggplot(aes(x = !!regs, y = !!spp)) +
    geom_col(fill = jpgray[3]) +
    geom_col(aes(x = !!regs, y = !!endem)) +
    coord_flip() +
    y_scale +
    scale_fill_manual(values = jpgray[1]) +
    theme_map() +
    theme(axis.text.y = element_blank()) +
    labs(x = NULL,
         y = "Species") +
    annotate_legend
  
  p 
  
  
}

psub_end <- plot_end(regs = Mor_subreg, spp = Sub_spp, endem = Sub_end, recs = Sub_records, kind = "Subregion") # 6 (2 TZ, 1 x_, abs = 3)
pdom_end <- plot_end(regs = Mor_domin, spp = domin_spp, endem = domin_end, recs = domin_records, kind = "Dominion") # 11 dominions (2 TZ, 1 x_, abs =)
ppro_end <- plot_end(regs = Mor_prov, spp = prov_spp, endem = prov_end, recs = prov_records, kind = "Province") # 51 provinces
peco_end <- plot_end(regs = ECO_NAME, spp = ECO_spp, endem = ECO_end, recs = ECO_records, kind = "Ecoregion") # 217 ecorregions
padm_end <- plot_end(regs = COUNTRY, spp = country_spp, endem = country_end, recs = country_records, kind = "Administrative") # 217 ecorregions

# ggarrange(psub_end, pdom_end, ppro_end, peco_end, padm_end,
#           ncol = 2, nrow = 3)

#### 4) Plotting records bar plots ####

plot_records <- function(recs, regs, kind) {
  
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  
  #### subsetting df and choosing graphical parameters ####
  recs <- enquo(recs)
  regs <- enquo(regs)
  
  if (kind == "Ecoregion") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      filter(!! recs >= 100) %>%
      dplyr::arrange(!! recs)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 2000, 250))
    
  }
  
  if (kind == "Subregion") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      dplyr::arrange(!! recs)
    
    y_scale <- scale_y_continuous(breaks = seq(0, 20000, 2000))
    
  }
  
  if (kind == "Dominion") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      dplyr::arrange(!! recs) %>%
      filter(!! regs %in% c("Boreal Brazilian dominion", "Chacoan dominion", "Mesoamerican dominion", 
                            "Pacific dominion", "Parana dominion", "South Brazilian dominion", "South-eastern Amazonian dominion"))
    
    
    y_scale <- scale_y_continuous(breaks = seq(0, 6500, 1000))
    
  }
  
  if (kind == "Province") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      dplyr::arrange(!! recs)  %>%
      filter(!(!!regs) %in% c("x_Neartic region"))
    
    y_scale <- scale_y_continuous(breaks = seq(0, 3500, 250))
    
  }
  
  if (kind == "Administrative") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      dplyr::arrange(!! recs) 
    
    y_scale <- scale_y_continuous(breaks = seq(0, 9400, 1000))
    
  }
  
  if (kind == "Phytogeographical") {
    
    df <- Biogeo_table %>%
      dplyr::select(!! regs, !! recs) %>% 
      distinct() %>%
      dplyr::arrange(!! recs) 
    
    y_scale <- scale_y_continuous(breaks = seq(0, 7000, 1000))
    
  }
  
  #### generating plot ####
  df[,1] <- factor(as.character(pull(df[,1])), levels = unique(as.character(pull(df[,1])), ordered = TRUE))
  
  jpgray <- gray.colors(n = 4)
  #show_col(jpgray)
  
  plot_fig <- df %>%
    ggplot(aes(x = !! regs, y = !! recs)) +
    geom_col(fill = jpgray[3]) +
    coord_flip() +
    y_scale +
    theme_map() +
    labs(y = "Records", x = NULL)
  
  plot_fig
  
}

peco_recs <- plot_records(regs = ECO_NAME, recs = ECO_records, kind = "Ecoregion")  
psub_recs <- plot_records(regs = Mor_subreg, recs = Sub_records, kind = "Subregion")
pdom_recs <- plot_records(regs = Mor_domin, recs = domin_records, kind = "Dominion")
ppro_recs <- plot_records(regs = Mor_prov, recs = prov_records, kind = "Province")
padm_recs <- plot_records(regs = COUNTRY, recs = country_records, kind = "Administrative")
pgen_recs <- plot_records(regs = Gen_reg, recs = gen_records, kind = "Phytogeographical")

# ggarrange(peco_recs, psub_recs, pdom_recs, ppro_recs, padm_recs,
#           ncol = 2, nrow = 3)


#### 5) Producing maps for geographical units ####

# Using Biogeo table
glimpse(Biogeo_table)

#### 5.1) Morrone ####
# FROM: Section 1

library(sf)
library(dplyr)
library(readxl)
library(patchwork)
library(paletteer)
library(ggthemes)

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


big_df <- read.csv("./data/Localidades_Total_v14.csv")
big_df_pts <- st_as_sf(big_df, coords = c("XCOOR", "YCOOR"), crs = wgs84)

# Preparing Morrone

morr <- st_read("./data/GIS/Lowenberg_Neto_2014.shp")
morr <- st_transform(morr, crs = wgs84)

level_prov <- levels(morr$Province_1)
level_prov <- c(level_prov, "x_Andean region", "x_Neartic region")
morr$Province_1 <- factor(morr$Province_1, levels = level_prov)

morr$Province_1[15] <- "x_Andean region"
morr$Province_1[55] <- "x_Neartic region"

level_dom <- levels(morr$Dominions)
level_dom <- c(level_dom, "x_South American Transition Zone", "x_Mexican Transition Zone", "x_Antillean subregion", 
               "x_Andean region", "x_Neartic region")
morr$Dominions <- factor(morr$Dominions, levels = level_dom)

morr$Dominions[14] <- "x_South American Transition Zone"
morr$Dominions[15] <- "x_Andean region"
morr$Dominions[16] <- "x_South American Transition Zone"
morr$Dominions[19:21] <- "x_South American Transition Zone"
morr$Dominions[34] <- "x_South American Transition Zone"
morr$Dominions[37] <- "x_Mexican Transition Zone"
morr$Dominions[39] <- "x_Mexican Transition Zone"
morr$Dominions[41:42] <- "x_Mexican Transition Zone"
morr$Dominions[45] <- "x_Mexican Transition Zone"
morr$Dominions[47:53] <- "x_Antillean subregion"
morr$Dominions[55] <- "x_Neartic region"

level_subr <- levels(morr$Subregio_1)
level_subr <- c(level_subr, "x_Andean region", "x_Neartic region")
morr$Subregio_1 <- factor(morr$Subregio_1, levels = level_subr)

morr$Subregio_1[15] <- "x_Andean region"
morr$Subregio_1[55] <- "x_Neartic region"

# Preparing sf object for Morrone

subreg_morr <- Biogeo_table %>% 
  dplyr::select(Mor_subreg, starts_with("Sub")) %>%
  distinct()

dom_morr <- Biogeo_table %>% 
  dplyr::select(Mor_domin, starts_with("domin")) %>%
  distinct()

prov_morr <- Biogeo_table %>% 
  dplyr::select(Mor_prov, starts_with("prov")) %>%
  distinct()

morr_data_sf <- left_join(morr, subreg_morr, by = c("Subregio_1" = "Mor_subreg") )
morr_data_sf <- left_join(morr_data_sf, dom_morr, by = c("Dominions" = "Mor_domin") )
morr_data_sf <- left_join(morr_data_sf, prov_morr, by = c("Province_1" = "Mor_prov") )

# Test

plot(morr_data_sf ["Sub_records"])
plot(morr_data_sf ["Sub_spp"])
plot(morr_data_sf ["Sub_end"])

plot(morr_data_sf ["domin_records"])
plot(morr_data_sf ["domin_spp"])
plot(morr_data_sf ["domin_end"])

plot(morr_data_sf ["prov_records"])
plot(morr_data_sf ["prov_spp"])
plot(morr_data_sf ["prov_end"])

# ggplot 

subrec <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = Sub_records), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()+ 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

subspp <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = Sub_spp), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  #scale_fill_gradientn(colours = wes_palette("Zissou1", n = 6, type = "continuous")) +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

subend <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = Sub_end), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Endemic\nSpp", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

domrec <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = domin_records), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()  + 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

domspp <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = domin_spp), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  #scale_fill_gradientn(colours = wes_palette("Zissou1", n = 6, type = "continuous")) +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

domend <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = domin_end), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Endemic\nSpp", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

prorec <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = prov_records), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()  +
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

prospp <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = prov_spp), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()  + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

proend <- ggplot() + 
  geom_sf(data = morr_data_sf, aes(fill = prov_end), size = 0.3, color = "black") +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Endemic\nSpp", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))


tiff("./figs/3_Geocov_Morr.tiff", res=300, compression = "lzw", width=6, height=8, units="in")
(subrec | subspp | subend) /
  (domrec | domspp | domend) /
  (prorec | prospp | proend) 
dev.off()


#### 5.2) Ecorregions ####

ecoreg <- st_read("./data/GIS/tnc_terr_ecoregions.shp")

ecorregion_sf <- ecoreg %>%
  filter(WWF_REALM %in% c("NT", "NA")) #%>%
#st_simplify(dTolerance = 0.05)
glimpse(ecorregion_sf)

plot(st_geometry(ecorregion_sf), xlim = c(-90.1159, -45.70972), ylim = c(-42.76178, 30.74353))

ecorregion_df <- Biogeo_table %>% 
  dplyr::select(ECO_NAME, starts_with("ECO")) %>%
  distinct()

ecorregion_sf <- left_join(ecorregion_sf, ecorregion_df, by = "ECO_NAME" )

#  ggplot

ecorec <- ggplot() + 
  geom_sf(data = ecorregion_sf, aes(fill = ECO_records), size = 0.1, color = "black") +
  coord_sf(xlim = c(-120, -34), ylim = c(-55, 40)) +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()+ 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

ecospp <- ggplot() + 
  geom_sf(data = ecorregion_sf, aes(fill = ECO_spp), size = 0.1, color = "black") +
  coord_sf(xlim = c(-120, -34), ylim = c(-55, 40)) +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()+ 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

ecoend <- ggplot() + 
  geom_sf(data = ecorregion_sf, aes(fill = ECO_end), size = 0.1, color = "black") +
  coord_sf(xlim = c(-120, -34), ylim = c(-55, 40)) +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()+ 
  guides(fill = guide_colorbar(title = "Endemic\nSpp", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

tiff("./figs/3_Geocov_Ecoreg.tiff", res=600, compression = "lzw", width=8, height=3, units="in")
(ecorec | ecospp | ecoend)
dev.off()


#### 5.3) Administrative areas ####

unique(Total_DB$COUNTRY)
which(is.na(Total_DB$COUNTRY))
Total_DB$COUNTRY[7832] <- "Guyana"
Total_DB$COUNTRY[7833] <- "Venezuela"
Total_DB$COUNTRY[8524] <- "Venezuela"
Total_DB$COUNTRY[13221] <- "Bolivia"
Total_DB$COUNTRY[13222] <- "Bolivia"
Total_DB$COUNTRY[16129] <- "Peru"

admin_df <- Total_DB %>% 
  dplyr::select(ID, NAME1, XCOOR, YCOOR, COUNTRY, MAJOR_AREA) %>% 
  mutate(COUNTRYJP = ifelse(COUNTRY == "Caribbean", MAJOR_AREA, COUNTRY))

admin_df %>% 
  filter(COUNTRY == "Caribbean")

cn <- sp_data('pt_countries')
# proj4string(cn) <- CRS("+proj=longlat +datum=WGS84")
# class(cn)
# clb <- spTransform(cn, CRS("+proj=longlat +datum=WGS84"))

tolower(cn$COUNTRY)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
name_vec_cn <- firstup(tolower(cn$COUNTRY))

unique(admin_df$COUNTRYJP)
name_vec_cn[name_vec_cn == "Brasil"] <- "Brazil"
name_vec_cn[name_vec_cn == "Suriname"] <- "Surinam"
name_vec_cn[name_vec_cn == "El salvador"] <- "El Salvador"
name_vec_cn[name_vec_cn == "Guyane"] <- "French Guiana"
name_vec_cn[name_vec_cn == "United states, the"] <- "United States"
name_vec_cn[name_vec_cn == "Costa rica"] <- "Costa Rica"
name_vec_cn[name_vec_cn == "Dominican republic"] <- "Dominican Republic"
name_vec_cn[name_vec_cn == "Bahamas, the"] <- "Bahama"
name_vec_cn[name_vec_cn == "Puerto rico"] <- "Puerto Rico"
#name_vec_cn[name_vec_cn == "Trinidad and tobago"] <- "Trinidad and Tobago"
name_vec_cn[name_vec_cn %in% c("Dominica", "Grenada", "St. lucia", "Barbados",
                               "St. vincent and the grenadines", "Martinique",
                               "Trinidad and tobago")] <- "Windward Islands"
name_vec_cn[name_vec_cn %in% c("Antigua and barbuda", "Guadeloupe", 
                               "St. christopher-nevis")] <- "Leeward Islands"

cn$COUNTRY <- name_vec_cn
admin_sf <- st_as_sf(cn)

Biogeo_table$COUNTRYJP <- admin_df$COUNTRYJP

administrative_df <- Biogeo_table %>% 
  dplyr::select(COUNTRYJP, starts_with("country_")) %>%
  distinct()

administrative_sf <- left_join(admin_sf, administrative_df, by = c("COUNTRY" = "COUNTRYJP"))

admrec <- ggplot() + 
  geom_sf(data = administrative_sf, aes(fill = country_records), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  # scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map()+ 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

admspp <- ggplot() + 
  geom_sf(data = administrative_sf, aes(fill = country_spp), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  #scale_fill_gradientn(colours = wes_palette("Zissou1", n = 6, type = "continuous")) +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

admend <- ggplot() + 
  geom_sf(data = administrative_sf, aes(fill = country_end), size = 0.3, color = "black") +
  #scale_fill_viridis(option = "D", alpha = 0.5) +
  #scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Endemic\nSpp", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))


tiff("./figs/3_Geocov_Admin.tiff", res=300, compression = "lzw", width=8, height=3, units="in")
(admrec | admspp | admend)
dev.off()

#### 5.4) Grid maps ####

sp <- SpatialPoints( admin_df[, c('XCOOR', 'YCOOR')],
                     proj4string=CRS("+proj=longlat +datum=WGS84") )
sp <- SpatialPointsDataFrame(sp, admin_df)

cn <- sp_data('pt_countries')

r <- raster(cn)
# 200 km = 200000 m
res(r) <- 1

rich <- rasterize(sp, r, 'NAME1', function(x, ...) length(unique(na.omit(x))))
occs <- rasterize(sp, r, 'NAME1', function(x, ...) length(na.omit(x)))
plot(rich)
plot(occs)
plot(cn, add=TRUE)

map_sf <- st_as_sf(cn)
points_sf <- st_as_sf(sp)

# Richness
test_spdf <- as(rich, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

quarec <- ggplot()  + 
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

quarec_prin <- ggplot()  + 
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = T) +
  theme_few() + 
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

# Occurrences 
test_spdf <- as(occs, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

quaspp <- ggplot()  + 
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  #scale_fill_viridis_c() +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_map() + 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

quaspp_prin <- ggplot()  + 
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = T) +
  theme_few() + 
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))

tiff("./figs/4_Geocov_1dgQuad.tiff", res=600, compression = "lzw", width=5, height=3, units="in")
(quarec | quaspp)
dev.off()

#### 5.5) Grid maps principal ####

sp <- SpatialPoints( admin_df[, c('XCOOR', 'YCOOR')],
                     proj4string=CRS("+proj=longlat +datum=WGS84") )
sp <- SpatialPointsDataFrame(sp, admin_df)

cn <- sp_data('pt_countries')

r <- raster(cn)
# 200 km = 200000 m
res(r) <- 1

rich <- rasterize(sp, r, 'NAME1', function(x, ...) length(unique(na.omit(x))))
occs <- rasterize(sp, r, 'NAME1', function(x, ...) length(na.omit(x)))

berhmann.proj4 <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs +wktext"

# rich <- projectRaster(rich, crs = berhmann.proj4)
# occs <- projectRaster(occs, crs = berhmann.proj4)

map_sf <- st_as_sf(sp_data('pt_countries'))
#map_sf <- st_transform(map_sf, crs = berhmann.proj4)

# Richness
test_spdf <- as(rich, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

quarec_prin <- ggplot()  +
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  # coord_sf(xlim = c(-10634332, -3362546),
  #          ylim = c(-4308186, 4569441)) +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = T) +
  theme_few() +
  guides(fill = guide_colorbar(title = "Species", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points")) +
  labs(x = NULL, y = NULL)

# Occurrences
test_spdf <- as(occs, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

quaspp_prin <- ggplot()  +
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), size = 0.1, color = "black") +
  geom_sf(data = map_sf, fill = NA, size = 0.1) +
  coord_sf(crs = sf::st_crs("EPSG:4326"))  + #54017
  # coord_sf(xlim = c(-10634332, -3362546),
  #          ylim = c(-4308186, 4569441)) +
  scale_fill_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  coord_sf(xlim = c(-110, -34), ylim = c(-35, 39), expand = T) +
  theme_few() +
  guides(fill = guide_colorbar(title = "Records", barheight = unit(10, units = "points"))) +
  theme(legend.position = "bottom",
        legend.key.width = unit(20, units = "points")) +
  labs(x = NULL, y = NULL)

tiff("./figs/11_Richnesmain.tiff", res=300, compression = "lzw", width=8, height=8, units="in")
quarec_prin 
dev.off()

tiff("./figs/12_Ocurrencesmain.tiff", res=300, compression = "lzw", width=8, height=8, units="in")
quaspp_prin 
dev.off()

#### 6) Area of the Neotropics ####

sp <- SpatialPoints( admin_df[, c('XCOOR', 'YCOOR')],
                     proj4string=CRS("+proj=longlat +datum=WGS84") )
sp <- SpatialPointsDataFrame(sp, admin_df)

cn <- sp_data('pt_countries')

r <- raster(cn)
# 200 km = 200000 m
res(r) <- 1

sp_noBigCap <- st_as_sf(sp) %>% 
  filter(NAME1 != "Bignonia capreolata") %>% 
  as_Spatial()

occs_noBigCap <- rasterize(sp_noBigCap, r, 'NAME1', function(x, ...) length(na.omit(x)))
occs_noBigCap[occs_noBigCap >= 1] <- 1
plot(occs_noBigCap)
unique(values(occs_noBigCap))

netropR <- morr %>% filter(REGION == "Neotropical region")
neo_ras <- fasterize(sf = netropR , raster = r)
plot(neo_ras)
unique(values(mor_ras))

area_ras <- merge(neo_ras, occs_noBigCap, fun = mean, na.rm = T)
plot(area_ras)
unique(values(area_ras))

# Area of the Neotropical Region covered by Bignonieae
# Area as number of one degre lat/long cells
# The #cells 
length(occs_noBigCap[occs_noBigCap == 1]) / length(area_ras[area_ras == 1])
#[1] 0.7206312

#### 7) Database map ####

# Bignonieae Database
Total_DB <- read.csv("./data/Localidades_Total_v14.csv", stringsAsFactors = FALSE)

Amer <- st_read("./data/GIS/Amer_land.shp") 
plot(st_geometry(Amer))

berhmann.proj4 <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs +wktext"
Amer <- st_transform(Amer, crs = berhmann.proj4)
plot(st_geometry(Amer))

Total_DB_sf <- st_as_sf(Total_DB, coords = c("XCOOR", "YCOOR"))
st_crs(Total_DB_sf) <- 4326
plot(st_geometry(Total_DB_sf), cex = 0.1, col = "blue", add = TRUE)

Total_DB_sf <- st_transform(Total_DB_sf, crs = berhmann.proj4)
plot(st_geometry(Total_DB_sf), cex = 0.1, col = "blue", add = TRUE)

## Trying to include the raster in the plot:
alt_ras <- raster("./data/GIS/Amer_elev.tif")
plot(alt_ras)
plot(st_geometry(Amer), add = TRUE)

alt_ras_proj <- projectRaster(alt_ras, crs = berhmann.proj4)
plot(alt_ras_proj)
plot(st_geometry(Amer), add = TRUE)

DB_map <- ggplot()  + 
  geom_sf(data = Amer, fill = NA, size = 0.5, color = "black") +
  geom_sf(data = Total_DB_sf, size = 0.4, color = "#005089FF", alpha = 0.5) +
  coord_sf(xlim = c(-10634332, -3362546), ylim = c(-4308186, 4569441)) +
  scale_color_paletteer_c("grDevices::Blues 3", direction = -1,  na.value = "white") +
  theme_few() + 
  theme(legend.position = "bottom",
        legend.key.width = unit(17, units = "points"))


tiff("./figs/5_DBmap_RAW.tiff", res=600, compression = "lzw", width=10, height=10, units="in")
DB_map
dev.off()
