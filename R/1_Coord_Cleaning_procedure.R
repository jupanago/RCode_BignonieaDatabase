## Coordinate quality assessment of the Bignonieae Database (BD)

####### From database_v12 to database_v13 #########

# NOTE: This script integrates the new georeferences obtained after checking
# outliers and errors during the assessment of the altitudinal profile. 

library(readxl)
library(dplyr)
library(ggplot2)
library(readODS)

# Loading database
bigv12 <- read_xlsx("./data/Localidades_Total_v12.xlsx")

# Loading new georeferences
new_georefsea <- read_ods("./data/Bignonieae_database_newgeoref.ods", sheet = 1, col_names = TRUE)
new_georefsea <- new_georefsea[, 1:3]
new_georeferr <- read_ods("./data/Bignonieae_database_newgeoref.ods", sheet = 2, col_names = TRUE)
new_georeferr <- new_georeferr[, 1:3]

total_changes <- arrange(rbind(new_georeferr, new_georefsea), ID)

indexsea <- which(bigv12$ID %in% new_georefsea$ID)
indexerror <- which(bigv12$ID %in% new_georeferr$ID)

BIGDB_V13 <- bigv12 %>% left_join(total_changes, by = "ID") %>%
  mutate(XCOOR = coalesce(XCOOR.y, XCOOR.x),
         YCOOR = coalesce(YCOOR.y, YCOOR.x)) %>%
  select(-XCOOR.x,-XCOOR.y, -YCOOR.x, -YCOOR.y) %>%
  select(ID, SOURCE, GENUS, SPECIES, AUTHOR, NAME, NAME1, COUNTRY, MAJOR_AREA, MINOR_AREA, LOCALITY, YCOOR, XCOOR, ALT_MIN,
         ALT_MAX, COLLECTOR, NUMBER, COLL_DAY, COLL_MONTH, COLL_YEAR,  HERBARIUM, DET_BY, PHENOLOGY, everything())

View(BIGDB_V13)

identical(bigv12$ID, BIGDB_V13$ID) # If everything is correct, this will be TRUE
identical(bigv12$XCOOR, BIGDB_V13$XCOOR) # If everything is correct, this will be FALSE
identical(bigv12$YCOOR, BIGDB_V13$YCOOR) # If everything is correct, this will be FALSE
identical(bigv12$AUTHOR, BIGDB_V13$AUTHOR) # If everything is correct, this will be TRUE

# write.csv(BIGDB_V13, file = "./output/Localidades_Total_v13.csv")

####### CoordinateCleaner on database_v13 #########

# NOTE: This script run the CoordinateCleaner package and corrected the flagged 
# issues. New georeferences were assigned using GoogleEarth and saved in 
# "./data/Bignonieae_database_newgeorefv1_1.ods".


# Libraries

library(readxl)
library(readODS)
library(dplyr)
library(CoordinateCleaner)

#### 1. Load data ####

big_db <- read_xlsx("./data/Localidades_Total_v13.xlsx")
big_db_red <- big_db %>% dplyr::select(7, 8, 12, 13)
glimpse(big_db_red)
View(big_db)
 
#### 2. General cleaning issues ####
big_db_rectest <- clean_coordinates(x = big_db_red,
                                    lon = "XCOOR",
                                    lat = "YCOOR",
                                    species = "NAME1",
                                    countries = "COUNTRY")

summary(big_db_rectest)
# .val=0 | .equ = 0 | .zer = 8 | .cap = 562 | .cen = 15 | .sea = 509 | .otl = 65 
# | .gbf = 0 | .inst = 6 | .summary = 1147

# Note: the number of records in the sea is lower. See below.
# The discrepancy occurs because of the continent shapefile used by CoordinateCleaner.
# Outliers are not a problem because they were already reviewed manually by the expert.
# Institutions are remarkably lower.
# The number of centroids is also remarkably lower.
# The number of capitals is also remarkably lower.

plot(big_db_rectest, lon = "XCOOR",
     lat = "YCOOR",
     species = "NAME1")


#### 3. Duplicates ####

big_db_dup <- cc_dupl(x = big_db_rectest,
                      lon = "XCOOR",
                      lat = "YCOOR",
                      species = "NAME1",
                      value = "flagged")

View(big_db[!big_db_dup, ])

# Flagged 7593 records.
# Note: Some duplicated records correspond to collections by different collectors at different times.
# Several points occur over the centroid of Natural Areas, and some others over centroids of cities.

#### 4. Capitals ####

caps <- big_db[!big_db_rectest$.cap,]
View(caps)
# 562 points

#### 5. In the sea #####

View(big_db[which(!big_db_rectest$.sea), ])
# 509 points

# #write.csv(big_db[which(!big_db_rectest$.sea), ], "./output/SEA_bigdbv13_cc.csv")
# All 509 points were checked using GoogleEarth. 
# Only 71 out of 509 were really in the SEA, and were georeferenced.
# The polygon of America coast borders used in CoordinateCleaner do not describe
# good enough the continental margins.

big_db_SEA <- read_ods("./data/SEA_bigdbv13_cc_checked.ods") %>%
  filter(SEA == 1)

SEA_ID <- big_db_SEA$ID

# NOTE: From the altitudinal range profile with the GTOPO30 DEM. While 
# researching the altitudinal profile some 0 m.a.s.l points were located at
# the sea particularly in the Antilles in the Caribbean sea and in the middle of 
# rivers in the Amazonia. Those points were also corrected when necessary
# using GoogleEarth and integrated into a new version of the BD.

# From 0_Altitudinal_profile_BD.R 
enelmar_ID <- c(971, 1514, 1709, 1723, 2144, 2601, 3394, 3396, 3399, 3448, 3454,
                3478, 3479, 3480, 3501, 3537, 3563, 3568, 3569, 3667, 3757, 4158,
                4290, 4576, 4579, 4594, 5014, 5015, 5016, 5085, 5122, 5123, 5133, 
                5148, 5151, 5654, 5657, 6022, 6166, 6442, 6885, 7555, 7830, 8489,
                8816, 8824, 8842, 8846, 8850, 8851, 8852, 9220, 9235, 9307, 9514, 
                9685, 10190, 10577, 10726, 10891, 10984, 10911, 10912, 10913,
                11060, 11919, 12007, 12191, 12213, 12282, 12283, 12284, 14858,
                14881, 14882, 14887, 15043, 15451, 15457, 15458, 15466, 15467,
                15468, 15504, 15539, 15548, 15558, 15559, 15560, 15564, 15565, 
                15566, 15567, 15568, 15569, 15570, 15571, 1557, 15572, 15573, 
                15574, 15775, 15776, 15588, 15593, 15749, 15764, 16397, 16695, 
                16814, 16815, 16946, 17074, 17680, 17681, 17700, 17735, 17940, 
                18531, 18585, 18649, 18836, 18995, 19994, 19995, 19996, 20154, 
                20230, 20251, 20258, 20259, 20260, 20261, 20262, 20285, 20489, 
                20563, 20610, 21291, 21292, 21293, 21294, 21296, 21795, 22236, 
                22796, 22886, 22953, 23358, 23359, 23966, 25124, 25328, 25335,
                25336, 25337, 25370, 25673, 25674, 25950, 25960, 25961, 26031, 
                26077, 26078, 26079, 26085, 26176, 26300, 26309, 26310, 26795, 
                26796, 26890, 26891, 26892, 26909, 27252, 27524, 27894, 28562, 
                28569)


# From Bignonia_database_new_georef.ods
past_georefsea <- read_ods("./data/Bignonieae_database_newgeoref.ods", 
                           sheet = 1, col_names = TRUE)
past_georeferr <- read_ods("./data/Bignonieae_database_newgeoref.ods", 
                           sheet = 2, col_names = TRUE)
new_georefsea <- new_georefsea[, 1:3]
new_georeferr <- new_georeferr[, 1:3]
past_geotot <- arrange(rbind(new_georeferr, new_georefsea), ID)


## 1) Selecting records absent in past_georeftot but present in enelmar_ID

diff_past_enelmar <- setdiff(enelmar_ID, past_geotot$ID)

View(filter(big_db, ID %in% diff_past_enelmar) %>% select(1,7,12,13, 8:11))

# Individual inspection in GoogelEarth
not_in_SEA <- c(1709,3396,3448,3454,3478,3479,3480,3501,3537,3563,3568,3569,
                10984,15775,15776) 


diff_past_enelmar <- setdiff(diff_past_enelmar, not_in_SEA)


## 2) Selecting records absent in past_georeftot but present in SEA_ID 
## from CoordinateCleaner

diff_past_seacc <- setdiff(SEA_ID, past_geotot$ID)

## 3) Total list of records in the sea for Big_db v13 that must be georef

TOTAL_inthe_SEA <- unique(sort(c(diff_past_enelmar, diff_past_seacc)))

## 4) Generating dataframe with points to be georef

NEW_GEOF <- big_db %>% filter(ID %in% TOTAL_inthe_SEA)
#write_ods(NEW_GEOF, "./output/NEW_GEOREF_CC.ods")
# NEW_GEOREF_CC.ods was edited manually. Every record in the sea was georeferenced. In total 75 points.
# A new database Localidades_Total_v14 was created from this new data. Localidades_Total_v14_script.R

#### 6. Zero coordinates ####

zeros <- big_db[!big_db_rectest$.zer,]
View(zeros)
# 8 points with zero coordinates.
# 7 at the Cuyabeno Natural reserve centroid. Correctly assigned.
# 1 at Camaipi, Amapa. Correctly assigned.


#### 7. Centroids ####

centroids <- big_db[!big_db_rectest$.cen, ]
View(centroids)
# 15 points.
# Correctly assigned. Some in island centroids, other in centroids of rivers.


#### 8. Institutions ####

institut <- big_db[!big_db_rectest$.inst, ]
View(institut)
# 6 points.
# They are over institutions, but that doesn't mean the species are not there. 
# For example, Chamela Biological Station, Mex. Furtheremore, the record on the
# Missouri Botanical garden is B. capreolata, the only species of Bignonieae to 
# reach North America.

#### 10. Outliers ####

outlier <- big_db[!big_db_rectest$.otl, ]
View(outlier)
# 65 points
# This points are conserved as such. 
# The review of outliers was done by specialist: LGL, LFHM, LM, JPNG


####### From database_v13 to database_v14 #########

# Localidades Total V14
# After cleaning points in the sea with CoordinateCleaner

library(readxl)
library(dplyr)
library(ggplot2)
library(readODS)

# Loading database
bigv13 <- read_xlsx("./data/Localidades_Total_v13.xlsx")

# Loading new georeferences
new_georefsea <- read_ods("./data/Bignonieae_database_newgeorefv1_1.ods", sheet = 1, col_names = TRUE)
new_georefsea <- new_georefsea[, 1:3]
new_georeferr <- read_ods("./data/Bignonieae_database_newgeorefv1_1.ods", sheet = 2, col_names = TRUE)
new_georeferr <- new_georeferr[, 1:3]

total_changes <- arrange(rbind(new_georeferr, new_georefsea), ID)

indexsea <- which(bigv13$ID %in% new_georefsea$ID)
indexerror <- which(bigv13$ID %in% new_georeferr$ID)

BIGDB_V14 <- bigv13 %>% 
  left_join(total_changes, by = "ID") %>%
  mutate(XCOOR = coalesce(XCOOR.y, XCOOR.x),
         YCOOR = coalesce(YCOOR.y, YCOOR.x)) %>%
  select(-XCOOR.x,-XCOOR.y, -YCOOR.x, -YCOOR.y) %>%
  select(ID, SOURCE, GENUS, SPECIES, AUTHOR, NAME, NAME1, COUNTRY, MAJOR_AREA, MINOR_AREA, LOCALITY, YCOOR, XCOOR, ALT_MIN,
         ALT_MAX, COLLECTOR, NUMBER, COLL_DAY, COLL_MONTH, COLL_YEAR,  HERBARIUM, DET_BY, PHENOLOGY, everything())

View(BIGDB_V14)

identical(bigv13$ID, BIGDB_V14$ID) # If everything is correct, this will be TRUE
identical(bigv13$XCOOR, BIGDB_V14$XCOOR) # If everything is correct, this will be FALSE
identical(bigv13$YCOOR, BIGDB_V14$YCOOR) # If everything is correct, this will be FALSE
identical(bigv13$AUTHOR, BIGDB_V14$AUTHOR) # If everything is correct, this will be TRUE

# write_ods(BIGDB_V14, path = "./output/Localidades_Total_v14.ods")
write.csv(BIGDB_V14, file = "./output/Localidades_Total_v14.csv")

### Test with CoordinateCleaner on V14

BIGDB_v14_test <- clean_coordinates(x = BIGDB_V14,
                                    lon = "XCOOR",
                                    lat = "YCOOR",
                                    species = "NAME1",
                                    countries = "COUNTRY")

summary(BIGDB_v14_test)
#.val     .equ     .zer     .cap     .cen     .sea     .otl     .gbf    .inst .summary 
# 0        0        8      562       15      469       65        0        8     1113
# From 509 records in the sea in db_v14, they were reduced to 469. Remember: Points at the sea
# depend on the shape file use to account for the continent. After reviewing them on GoogleEarth,
# only 71 were really at the sea. Look: SEA_bigdbv13_cc_checked.ods, NEW_GEOREF_CC.ods and Bignonieae_database_newgeorefv1_1.ods. Stored
# definitively in MEGA/Datos Generados/