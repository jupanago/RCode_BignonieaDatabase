## Altitudinal profile of the Bignonieae Database (BD)

# This script contains the steps taken to estimate the species altitudinal range.

# NOTE: 
# 1) The altitudinal profile was done with the first version of the Bignonieae
# database I received from Dr. Lohmann. After finding outliers, checking
# georeferences, and finishing the data extraction for the altitudinal profile,
# we decided to run a new phase of database cleaning using the CoordinateCleaner
# package. 

# 2) Therefore the order of versions of the database during the cleaning procedure
# was as follows:

# i. database_v12: Detecting altitudinal outliers and associated georeference 
# errors during the assessment of the altitudinal profile.
# ii. database_v13: Integration of the new georeferences obtained from the 
# assessment of the altitudinal profile.
# iii. database_v14: after applying the CoordinateCleaner on database_v13 and 
# corrected the flagged georeference issues. 

# Please go to "/R/1_Coord_Cleaning_procedure.R" to see the cleaning procedure.

# Altitudinal profile script

# Libraries

library(dplyr)
library(ggplot2)
library(readxl)
library(raster)

#### 1) Functions ####

# get_elevation() function
# This function extracts elevation data from raster for all the species in database.

get_elevation <- function (df_xy, elev_layer, write_df = "NO", filecon = "./output/df_elev.csv") {
  
  # df_xy: data frame with fields: ID, GENUS, XCOOR, YCOOR
  # elev_layer: raster with elevation data
  # write_df: "YES" to save data frame in output directory as csv
  # filecon: conection to output directory with the name of the csv file
  
  require(dplyr)
  require(raster)
  
  df_elev <- data.frame(
    ID = numeric(),
    GENUS = character(),
    NAME1 = character(),
    XCOOR = numeric(),
    YCOOR = numeric(),
    elevation = numeric(),
    elev_min = numeric(),
    elev_1stQ = numeric(),
    elev_median = numeric(),
    elev_mean = numeric(),
    elev_3rdQ = numeric(),
    elev_max = numeric()
  )
  
  name_vec <- unique(df_xy$NAME1)
  name_vec[2]
  for (i in 1:length(name_vec)) {
    
    intermediate_df <- df_xy %>%
      dplyr::select(ID, GENUS, NAME1, XCOOR, YCOOR) %>%
      filter(NAME1 == name_vec[i])
    
    point_mat <- cbind(intermediate_df$XCOOR, intermediate_df$YCOOR)
    
    alt_val <- extract(x = elev_layer, y = point_mat)
    elev_summ <- summary(alt_val, digits = max(5, getOption("digits")-3))
    
    intermediate_df$elevation <- alt_val
    intermediate_df$elev_min <- elev_summ[1]
    intermediate_df$elev_1stQ <- elev_summ[2]
    intermediate_df$elev_median <- elev_summ[3]
    intermediate_df$elev_mean <- elev_summ[4]
    intermediate_df$elev_3rdQ <- elev_summ[5]
    intermediate_df$elev_max <- elev_summ[6]
    
    df_elev <- bind_rows(df_elev, intermediate_df)
    
  }
  
  if (write_df == "YES") {
    
    write.csv(df_elev, file = filecon)
    print(paste("Data frame saved as csv in", filecon))
    
  } 
  
  return(df_elev)
}

# Ploting function
# This function creates a boxplot for the elevation data of a specified species.

box_plotjp <- function (x = 1, df, spp_names) {
  
  # x: a natural number indicating the position of species name in vecto spp_names
  # spp_names: vector of the species names in database
  
  df %>% 
    filter(NAME1 == spp_names[x]) %>%
    ggplot(aes(x = NAME1, y = elevation)) +
    geom_boxplot() +
    labs(title = spp_names[x])
}

## summ_elevation() funtion
# This function makes a statistical summary of elevation data for each species in database

summ_elevation <- function (df_xy, write_df = "NO", filecon = "./output/elev_summ.csv") {
  
  # df_xy: data frame with fields: ID, GENUS, NAME1, XCOOR, YCOOR, elevation
  # write_df: "YES" to save data frame in output directory as csv
  # filecon: conection to output directory with the name of the csv file
  
  require(dplyr)
  require(raster)
  
  df_elev <- data.frame(
    ID = numeric(),
    GENUS = character(),
    NAME1 = character(),
    XCOOR = numeric(),
    YCOOR = numeric(),
    elevation = numeric(),
    elev_min = numeric(),
    elev_1stQ = numeric(),
    elev_median = numeric(),
    elev_mean = numeric(),
    elev_3rdQ = numeric(),
    elev_max = numeric()
  )
  
  name_vec <- unique(df_xy$NAME1)
  
  for (i in 1:length(name_vec)) {
    
    intermediate_df <- df_xy %>%
      dplyr::select(ID, GENUS, NAME1, XCOOR, YCOOR, elevation) %>%
      filter(NAME1 == name_vec[i])
    
    elev_summ <- summary(intermediate_df$elevation, digits = max(5, getOption("digits")-3))
    
    intermediate_df$elev_min <- elev_summ[1]
    intermediate_df$elev_1stQ <- elev_summ[2]
    intermediate_df$elev_median <- elev_summ[3]
    intermediate_df$elev_mean <- elev_summ[4]
    intermediate_df$elev_3rdQ <- elev_summ[5]
    intermediate_df$elev_max <- elev_summ[6]
    
    df_elev <- bind_rows(df_elev, intermediate_df)
    
  }
  
  if (write_df == "YES") {
    
    write.csv(df_elev, file = filecon)
    print(paste("Data frame saved as csv in", filecon))
    
  } 
  
  return(df_elev)
}


#### 2) Elevation data extraction ####

# Raster used: GTOPO30 DEM USGS service -> Amer_elev.tif
# alt <- raster("../data/GIS/Amer_elev.tif") 

# Data extraction
# elev_df <- get_elevation(df_xy = Locality_DB, elev_layer = alt, write_df = "YES", filecon = "../output/df_elev.csv")

#### 3) Loading data produced with get_elevation() function ####

elev_df <- read.csv("./output/df_elev.csv")
Total_DB <- read_excel("./data/Localidades_Total_v12.xlsx") 
spp_list <- unique(Total_DB$NAME1)

#### 4) Corrections from Google Earth & Database #####

## Minimum difference between the raster altitude and Google Earth for correction: 100 m
## Outliers visible in species elevation boxplots were inspected and corrected.
## Collector data is conserved when:
##  (1) Google Earth and rast_alt differ substantially: >> 500
##  (2) Collector's max and min values are different from the extracted raster values.

## NEW GEOREFERENCES: 
## Outliers were checked looking for points in the sea (elevation = 0 m) and 
## for points with very extreme altitude values and atypical for Bignonieae
## (elevation > 3000 m). All these points were corrected using the locality
## description. All points were saved in ./data/Bignonieae_database_newgeorefs.ods
## Vectors of the points IDs in the sea (en_el_mar), incorrect georefereces (geoerrors_ID),
## and the checked outliers (checked_outliers) were created.

## Data taken from database is marked as 'info from database'
## Data with new geogreferences ismarked as 'NEWGEOREF'
## Data corrected with Google Earth is without specific marks
## Changed values are indicated after species name as: 0 -> 100,
## that is from 0 meters to 100 meters. The first value is that obtained by raster extraction.
## The second value that taken from Google Earth or Database.

## Each change in the database was recorded using the ID of the species record.
## Three examples:

## Case: "Adenocalymma bracteosum" 826 -> 119
##  elev_df[893, "elevation"] <- 119

## Case: "Adenocalymma hirtum" 0 -> 124 NEWGEOREF
## elev_df$elevation[1723 - 1] <- 1240

## "Amphilophium mansoanum"1749 -> 1200 info from database
## elev_df$elevation[5226] <- 1200

## The list of changes is omitted for matters of space. 

#### 5) Summary of changes ####

## This module should not be ran again. It contains:
##  (1) Vector with species names whose values were corrected: elim_outliers 
##  (2) Vector with species ID identifying points in the sea: enelmar_ID
##  (3) Vector wit species ID identifying points that were georeferenced: geoerrors_ID
##  (4) The final elevation dataframe: elev_curated

# elev_df[28548 -1, ] # To look for record
# 
# #elev_df <- read.csv("./output/df_elev.csv")
# #spp_list <- unique(Total_DB$NAME1)
# 
# box_plotjp <- function (x = 1, df, spp_names) { # To visualize boxplot
#   df %>% 
#     filter(NAME1 == spp_names[x]) %>%
#     ggplot(aes(x = NAME1, y = elevation)) +
#     geom_boxplot() +
#     labs(title = spp_names[x])
# }
# 
# box_plotjp(x = 386, df = elev_df, spp_names = spp_list)
# 
# elev_df %>% 
#   filter(NAME1 == spp_list[386]) %>%
#   filter(elevation <= 1000 & elevation > 470) # To get the minimum and maximum values
# 
# View(Total_DB %>% filter(ID == 1723)) # To view record information in database
#
# checked_outliers <- c("Adenocalymma albiflorum", "Adenocalymma bracteolatum", "Adenocalymma bracteosum", "Adenocalymma coriaceum",
#                    "Adenocalymma dusenii", "Adenocalymma flaviflorum", "Adenocalymma gibbosum", "Adenocalymma hirtum",
#                    "Adenocalymma impressum",  "Adenocalymma magnificum", "Adenocalymma marginatum", "Adenocalymma mirabile",
#                    "Adenocalymma schomburgkii", "Adenocalymma subsessilifolium", "Adenocalymma tanaeciicarpum", "Adenocalymma trifoliatum",
#                    "Adenocalymma validum", "Adenocalymma velutinum", "Amphilophium arenarium", "Amphilophium aschersonii",
#                    "Amphilophium blanchetii", "Amphilophium bracteatum", "Amphilophium buccinatorium", "Amphilophium carolinae",
#                    "Amphilophium crucigerum", "Amphilophium dasytrichum", "Amphilophium dolychoides", "Amphilophium ecuadorense",
#                    "Amphilophium frutescens", "Amphilophium gnaphalanthum", "Amphilophium granulosum", "Amphilophium lactiflorum",
#                    "Amphilophium mansoanum", "Amphilophium monophyllum", "Amphilophium nunezii", "Amphilophium obovatum",
#                    "Amphilophium occidentale", "Amphilophium paniculatum", "Amphilophium pannosum", "Amphilophium pilosum", 
#                    "Amphilophium porphyrotrichum", "Amphilophium pulverulentum", "Amphilophium racemosum", 
#                    "Amphilophium rodriguesii", "Amphilophium scabriusculum", "Anemopaegma acutifolium", "Anemopaegma arvense",
#                    "Anemopaegma chamberlaynii", "Anemopaegma chrysanthum", "Anemopaegma chrysoleucum", "Anemopaegma colombianum", 
#                    "Anemopaegma flavum", "Anemopaegma floridum", "Anemopaegma gracile", "Anemopaegma grandifolium", 
#                    "Anemopaegma granvillei", "Anemopaegma insculptum",  "Anemopaegma karstenii", "Anemopaegma laeve", "Anemopaegma longidens",
#                    "Anemopaegma longipetiolatum", "Anemopaegma mirabile", "Anemopaegma nebulosum",  "Anemopaegma oligoneuron",
#                    "Anemopaegma orbiculatum", "Anemopaegma paraense", "Anemopaegma parkeri", "Anemopaegma prostratum", "Anemopaegma puberulum",
#                    "Anemopaegma robustum", "Anemopaegma rugosum", "Anemopaegma salicifolium", "Anemopaegma santarinense",
#                    "Anemopaegma scabriusculum", "Anemopaegma setilobum", "Anemopaegma velutinum", "Bignonia aequinoctialis",
#                    "Bignonia binata", "Bignonia bracteomana", "Bignonia callistegioides", "Bignonia corymbosa", "Bignonia costata",
#                    "Bignonia cuneata", "Bignonia decora", "Bignonia diversifolia", "Bignonia hyacinthina", "Bignonia lilacina",
#                    "Bignonia longiflora", "Bignonia magnifica", "Bignonia microcalyx", "Bignonia neoheterophylla", "Bignonia neouliginosa",
#                    "Bignonia nocturna",  "Bignonia noterophila", "Bignonia phellosperma", "Bignonia potosina", "Bignonia prieurii", 
#                    "Bignonia pterocayx", "Bignonia ramentaceae", "Bignonia sactae-crucis",  "Bignonia sciuripabulum", "Bignonia sordida",
#                    "Bignonia uleana", "Callichlamys latifolia", "Cuspidaria bracteolata", "Cuspidaria emmonsii", "Cuspidaria floribunda",
#                    "Cuspidaria inaequalis", "Cuspidaria lachnaea", "Cuspidaria lateriflora", "Cuspidaria monophylla", "Cuspidaria multiflora",
#                    "Cuspidaria pulchra", "Cuspidaria sceptrum", "Cuspidaria subincana",  "Cuspidaria weberbaueri", "Dolichandra cynanchoides",
#                    "Dolichandra quadrivalvis", "Dolichandra steyermarkii", "Dolichandra uncata", "Dolichandra unguis-cati",
#                    "Fridericia arthrerion", "Fridericia candicans", "Fridericia caricachensis", "Fridericia chica", "Fridericia conjugata",
#                    "Fridericia costaricensis", "Fridericia craterophora", "Fridericia cuneifolia", "Fridericia dispar", "Fridericia egensis",
#                    "Fridericia elegans", "Fridericia fagoides", "Fridericia floribunda", "Fridericia florida", "Fridericia formosa",
#                    "Fridericia grosourdyana", "Fridericia japurensis", "Fridericia leucopogon", "Fridericia mollis", "Fridericia mollissima",
#                    "Fridericia mutabilis", "Fridericia nicotianiflora", "Fridericia nigrescens", "Fridericia oligantha", "Fridericia oxycarpa",
#                    "Fridericia patellifera", "Fridericia pearcei", "Fridericia platyphylla", "Fridericia podopogon", "Fridericia poeppigii",
#                    "Fridericia prancei", "Fridericia pubescens", "Fridericia rego", "Fridericia simplex", "Fridericia spicata",
#                    "Fridericia subincana", "Fridericia trailii", "Fridericia triplinervia",  "Fridericia truncata", "Fridericia tuberculata",
#                    "Fridericia viscida", "Fridericia whitei", "Lundia corymbifera", "Lundia damazioi","Lundia densiflora",
#                    "Lundia erionema", "Lundia gardneri", "Lundia helicocalyx", "Lundia longa", "Lundia nitidula",  "Lundia puberula",
#                    "Lundia spruceana", "Manaosella cordifolia", "Mansoa alliacea", "Mansoa difficilis", "Mansoa glaziovii",
#                    "Mansoa hirsuta","Mansoa hymenaea", "Mansoa lanceolata", "Mansoa minensis", "Mansoa onohualcoides", "Mansoa paganuccii",
#                    "Mansoa parvifolia", "Mansoa sagotii", "Mansoa standleyi", "Mansoa verrucifera", "Martinella insignis",
#                    "Martinella iquitoensis", "Martinella obovata", "Pachyptera erythraea", "Pachyptera kerere", "Perianthomega vellozoi",
#                    "Pleonotoma castelnaei", "Pleonotoma clematis", "Pleonotoma echitidea", "Pleonotoma exserta", "Pleonotoma fluminensis",
#                    "Pleonotoma jasminifolia", "Pleonotoma melioides", "Pleonotoma orientalis", "Pleonotoma pavettiflora", 
#                    "Pleonotoma stichadenia", "Pleonotoma tetraquetra", "Pleonotoma variabilis", "Pyrostegia venusta",
#                    "Stizophyllum inaequilaterum", "Stizophyllum perforatum", "Stizophyllum riparium", "Tanaecium affine",
#                    "Tanaecium bilabiatum", "Tanaecium caudiculatum", "Tanaecium crucigerum", "Tanaecium dichotomum", "Tanaecium duckei",
#                    "Tanaecium duckei", "Tanaecium exitiosum", "Tanaecium jaroba", "Tanaecium neobrasiliense", "Tanaecium ornithophilum",
#                    "Tanaecium paradoxum", "Tanaecium pyramidatum", "Tanaecium revillae", "Tanaecium selloi", "Tanaecium tetragonolobum",
#                    "Tanaecium tetramerum", "Tanaecium truncatum", "Tanaecium xanthophyllum", "Tynanthus cognatus", "Tynanthus croatianus",
#                    "Tynanthus densiflorus", "Tynanthus espiritosantensis", "Tynanthus fasciculatus", "Tynanthus guatemalensis",
#                    "Tynanthus macranthus", "Tynanthus micranthus", "Tynanthus panurensis", "Tynanthus polyanthus", "Tynanthus pubescens",
#                    "Tynanthus schumannianus", "Xylophragma harleyi", "Xylophragma myrianthum", "Xylophragma  platyphyllum",
#                    "Xylophragma  pratense")

# enelmar_ID <- c(971, 1514, 1709, 1723, 2144, 2601, 3394, 3396, 3399, 3448, 3454, 3478, 3479, 3480, 3501, 3537, 3563, 3568, 3569,
#                 3667, 3757, 4158, 4290, 4576, 4579, 4594, 5014, 5015, 5016, 5085, 5122, 5123, 5133, 5148, 5151, 5654, 5657, 6022,
#                 6166, 6442, 6885, 7555, 7830, 8489, 8816, 8824, 8842, 8846, 8850, 8851, 8852, 9220, 9235, 9307, 9514, 9685, 10190,
#                 10577, 10726, 10891, 10984, 10911, 10912, 10913, 11060, 11919, 12007, 12191, 12213, 12282, 12283, 12284, 14858, 
#                 14881, 14882, 14887, 15043, 15451, 15457, 15458, 15466, 15467, 15468, 15504, 15539, 15548, 15558, 15559, 15560,
#                 15564, 15565, 15566, 15567, 15568, 15569, 15570, 15571, 1557, 15572, 15573, 15574, 15775, 15776, 15588, 15593,
#                 15749, 15764, 16397, 16695, 16814, 16815, 16946, 17074, 17680, 17681, 17700, 17735, 17940, 18531, 18585, 18649,
#                 18836, 18995, 19994, 19995, 19996, 20154, 20230, 20251, 20258, 20259, 20260, 20261, 20262, 20285, 20489, 20563,
#                 20610, 21291, 21292, 21293, 21294, 21296, 21795, 22236, 22796, 22886, 22953, 23358, 23359, 23966, 25124, 25328,
#                 25335, 25336, 25337, 25370, 25673, 25674, 25950, 25960, 25961, 26031, 26077, 26078, 26079, 26085, 26176, 26300,
#                 26309, 26310, 26795, 26796, 26890, 26891, 26892, 26909, 27252, 27524, 27894, 28562, 28569)
# 
# geoerrors_ID <- c(11395, 11560, 12015, 12000, 12001, 12193, 12875, 12876, 13166, 13695, 14194, 14828, 15144, 15145, 16162, 16535,
#                17136, 19156, 20046, 20061, 20400,20905, 20401, 21022, 21538, 22576, 23080, 23088, 23889, 23742, 23969, 24864, 24865,
#                24982, 25664, 25665, 25655, 25834, 25897, 26100, 26253, 27426, 27619, 28237, 28548)
#
#glimpse(elev_curated)
# 
# elev_curated <- dplyr::select(elev_df, ID, GENUS, NAME1, XCOOR, YCOOR, elevation)
# 
# #write.csv(elev_curated, file = "./output/elevation_bignonieae.csv")

#### 6) Altitudinal profile figure ####

elev_summ <- read.csv("./output/elev_summ_FINAL.csv")

## Ordering data frame by increasing altitudinal range amplitude

df_range <- elev_summ %>%
  group_by(GENUS) %>%
  arrange(desc(elev_min), elev_max, .by_group = TRUE) %>%
  mutate(sp_randiff = elev_max - elev_min)

df_range_fact <-df_range %>%
  dplyr::select(GENUS, NAME1, sp_randiff) %>%
  distinct() %>%
  arrange(sp_randiff)

df_range$NAME1 <- factor(df_range$NAME1, 
                         levels = df_range_fact$NAME1)

# linear range plot for the tribe and discriminated by GENUS (facet_wrap)

range_profile_tribe <- ggplot(df_range, aes(x = NAME1, y = elevation)) + 
  geom_point(aes(x = NAME1, y = elev_median), col = "red", size = 0.2, alpha = 0.5)  +
  geom_linerange(aes(ymin = elev_min, ymax = elev_max, col = GENUS), lwd = 0.1) +
  scale_color_viridis_d() +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
  geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
  geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
  #geom_hline(yintercept = 4500, lty = 2, lwd = 0.2) +
  #facet_wrap(~GENUS) +
  theme_few() +
  labs(x = "Species",
       y = "m.a.s.l") +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold.italic"))

tiff("./figs/6_Elevationprofile_ALL.tiff", res=300, compression = "lzw", width=10, height=7, units="in")
range_profile_tribe
dev.off()

range_profile_gen <- ggplot(df_range, aes(x = NAME1, y = elevation)) + 
  geom_point(aes(x = NAME1, y = elev_median), colour = "red", size = 0.2, alpha = 0.5)  +
  geom_linerange(aes(ymin = elev_min, ymax = elev_max, col = GENUS), lwd = 0.1) +
  scale_color_viridis_d() +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
  geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
  geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
  #geom_hline(yintercept = 4500, lty = 2, lwd = 0.2) +
  facet_wrap(~GENUS) +
  theme_few() +
  labs(x = "Species",
       y = "m.a.s.l") +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold.italic"))


png("./figs/Altitudinal_range/gen_spp_elev.png",
    width = 1000, height = 1000, res = 150)
range_profile_gen
dev.off()

tiff("./figs/6_Elevationprofile_GENUS.tiff", res=300, compression = "lzw", 
     width=10, height=7, units="in")
range_profile_gen
dev.off()


