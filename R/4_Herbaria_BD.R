## Herbaria analysis of the Bignonieae Database (BD)

# This script contains the steps taken to assess where the information of the BD 
# is kept.

# Libraries

library(dplyr)
library(stringr)
library(stringi)
library(rebus)
library(ggplot2)
library(treemapify)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)

# Loading database
Total_DB <- read.csv("./data/Localidades_Total_v14.csv", stringsAsFactors = FALSE)

# 1) Looking for patterns #### 

# Extracting Herbaria character string vector
institutions <- Total_DB$HERBARIUM %>%
  #unique() %>%
  trimws(which = "both") %>%
  toupper() %>%
  gsub(pattern = " ", replacement = "") 

# The number of unique combinations of character strings is 15239
length(unique(institutions))

# The number of entries without institutional information is 9364
sum(is.na(Total_DB$HERBARIUM))

# 2) Cleaning strings #####  

# A first data analysis showed typos and nonexistent herbarium codes. 
# Here I resolved those issues.
# First, codes absent in th original index_herbariorum.cvs were added after 
# looking for them on the internet. Institutional herbaria names were confirmed 
# using Index Herbariorum NY and Tropicos MO 
# The corrected codes were saved as index_herbariorum_JPNG.cvs
# However, 12 codes could not be identified: 
# > Unnamed MV ARAR MJC CPFCN   BOLFOR  SCNLS   BHMB    CBR     CVG     HT  RVH
# Unnamed refers to all the fields that did not have information.
# Second, typos were corrected using the following lines of code.

# 2.1) TYPOS ####

#NA replaced by "Unnamed"
institutions <- str_replace_na(institutions, replacement = "Unnamed")

# SPFS == SPSF
sum(str_count(institutions, pattern = "SPFS")) # 12
institutions <- str_replace_all(institutions, pattern = "SPFS", replacement = "SPSF")

# MED == MEDEL
sum(str_count(institutions, pattern = "MED")) # 13
institutions <- str_replace_all(institutions, pattern = "MED", replacement = "MEDEL")
which(grepl(pattern = "MEDELEL", x = institutions)) # 4324 15458
institutions[4324] <- "MEDEL"
institutions[15458] <- "MEDEL"

# GDC == G-DC The pattern appears in the orignal database as G-DC. I created this error when eliminating the "-".
# sum(str_count(institutions, pattern = "G-DC")) # 7
# institutions <- str_replace_all(institutions, pattern = "GDC", replacement = "G-DC")

#UFAC == UFACPZ
sum(str_count(institutions, pattern = "UFAC")) # 7
institutions <- str_replace_all(institutions, pattern = "UFAC", replacement = "UFACPZ")

#LIVERP == UNIV,LIVERP == LIV
sum(str_count(institutions, pattern = "UNIV,LIVERP")) # 2
institutions <- str_replace_all(institutions, pattern = "UNIV,LIVERP", replacement = "LIV")

#GHA == GH-A == A
sum(str_count(institutions, pattern = "GH-A")) # 1
institutions <- str_replace_all(institutions, pattern = "GH-A", replacement = "A")

#HRBC == (Typo) HRCB
sum(str_count(institutions, pattern = "HRBC")) # 1
institutions <- str_replace_all(institutions, pattern = "HRBC", replacement = "HRCB")

#IBUSP == SPF
sum(str_count(institutions, pattern = "IBUSP")) # 1
institutions <- str_replace_all(institutions, pattern = "IBUSP", replacement = "SPF")

#KP == K, P
sum(str_count(institutions, pattern = "KP")) # 1
institutions <- str_replace_all(institutions, pattern = "KP", replacement = "K,P")

#UNAN == HULE
sum(str_count(institutions, pattern = "UNAN")) # 1
institutions <- str_replace_all(institutions, pattern = "UNAN", replacement = "HULE")

#UNAP == AMAZ
sum(str_count(institutions, pattern = "UNAP")) # 1
institutions <- str_replace_all(institutions, pattern = "UNAP", replacement = "AMAZ")

#HUESF == HUEFS
sum(str_count(institutions, pattern = "HUESF")) # 1
institutions <- str_replace_all(institutions, pattern = "HUESF", replacement = "HUEFS")

#VEM == VEN
sum(str_count(institutions, pattern = "VEM")) # 22
institutions <- str_replace_all(institutions, pattern = "VEM", replacement = "VEN")

#MIC == MICH
sum(str_count(institutions, pattern = "MIC$")) # 1
institutions <- str_replace_all(institutions, pattern = "MIC$", replacement = "MICH")

# 2.2) Homogenizing names ####

# Suppressing digits from MOBOT records
institutions_nn <- str_replace_all(institutions, pattern = DGT, replacement = "")  
# Suppressing separators
institutions_nn <- str_replace_all(institutions_nn, pattern = char_class("- "), replacement = "") 
# Test for G-DC: str_subset(institutions_nn, pattern = "GDC"). Correcting:
institutions_nn <- str_replace_all(institutions_nn, pattern = "GDC", replacement = "G-DC")

# 3) Table of records per herbaria ####
institutions_split <- str_split(institutions_nn, pattern = ",", simplify = TRUE)

institutions_table <- as_tibble(sort(table(institutions_split, exclude = c("", NA)), decreasing = TRUE))
colnames(institutions_table) <- c("Herbaria", "Records")

institutions_table <- institutions_table %>%
  mutate(Percentage_tot = round(100 * Records / 28763, digits = 3),
         Percentage_onlyinfo = round(100 * Records / 19399, digits = 3))

institutions_table$Herbaria <- factor(institutions_table$Herbaria,
                                      levels = institutions_table$Herbaria,
                                      ordered = TRUE)

View(institutions_table)

# Number of Herbaria in the Database: 196
length(institutions_table$Herbaria)

# Index Herbariorum
IH <- read.csv("./data/index_herbariorum_JPNG.csv", stringsAsFactors = FALSE)

index_absence <- !institutions_table$Herbaria %in% IH$Organization.Acronym
length(institutions_table$Herbaria[index_absence]) # 11
# 10 herbaria are absent from the list of Index Herbariorum_JPNG
# Institutional names were confirmed using Index Herbariorum NY and Tropicos MO (See above in the description of this section)
#Unnamed MV      ARAR    MJC     CPFCN   BOLFOR  SCNLS   BHMB    CBR     CVG     RVH    

# Table with herbaria in the database
index_herbariorum <- IH$Organization.Acronym %in% institutions_table$Herbaria 

HERBARIA_table <- IH[index_herbariorum, ] %>% 
  dplyr::select(3, 2, 12)

HERBARIA_table$Organization.Acronym <- factor(HERBARIA_table$Organization.Acronym ,
                                              levels = institutions_table$Herbaria, 
                                              ordered = TRUE)

colnames(HERBARIA_table) <- c("Herbaria", "Organization", "Country" )

HERBARIA_table <- left_join(institutions_table,HERBARIA_table, by = "Herbaria") %>%
  mutate(Internal = ifelse(Country == "Brazil", "Brazil", "FOREIGN"))

HERBARIA_table$Organization <- str_replace_na(HERBARIA_table$Organization, replacement = "Unknown")
HERBARIA_table$Country <- str_replace_na(HERBARIA_table$Country, replacement = "Unknown")
HERBARIA_table$Internal <- str_replace_na(HERBARIA_table$Internal, replacement = "Unknown")

which(HERBARIA_table$Herbaria == "CPFCN")
HERBARIA_table$Country[126] <- "Brazil"
HERBARIA_table$Internal[126] <- "Brazil"

which(HERBARIA_table$Herbaria == "BOLFOR")
HERBARIA_table$Country[134] <- "Bolivia"
HERBARIA_table$Internal[134] <- "FOREIGN"

which(HERBARIA_table$Herbaria == "SCNLS")
HERBARIA_table$Country[145] <- "Peru"
HERBARIA_table$Internal[145] <- "FOREIGN"

#write.csv(HERBARIA_table, "./output/RECORDS_HERBARIA_v3.csv")
HERBARIA_table <- read.csv("./output/RECORDS_HERBARIA_v3.csv")

# 4) Treemap chart ####

treemap_plot <- HERBARIA_table %>%
  filter(Herbaria != "Unnamed") %>% # To exclude the records without data.
  ggplot(aes(area = Percentage_onlyinfo, fill = Herbaria, label = Herbaria,
             subgroup = Country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    reflow = TRUE) +
  geom_treemap_subgroup_border(colour = "black") +
  geom_treemap_subgroup_text(place = "bottomleft", alpha = 0.5, colour =
                               "black", min.size = 0) +
  scale_fill_grey() + 
  #scale_fill_viridis_d(alpha = 0.8) +
  #facet_wrap(~Internal) +
  theme(legend.position = "none")

png("./figs/Treemap_Herbaria.png", width = 1000, height = 800, res = 120)
treemap_plot
dev.off()

HERBARIA_table %>%
  filter(Herbaria != "Unnamed") %>% # To exclude the records without data.
  ggplot(aes(area = Percentage_onlyinfo, fill = Herbaria, label = Herbaria,
             subgroup = Country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    reflow = TRUE) +
  geom_treemap_subgroup_border(colour = "black") +
  geom_treemap_subgroup_text(place = "bottomleft", alpha = 0.5, colour =
                               "black", min.size = 0) +
  scale_fill_grey() + 
  #scale_fill_viridis_d(alpha = 0.8) +
  facet_wrap(~Internal) +
  theme(legend.position = "none")

# 5) Lúcia ####

# Latin American countries

LATAM_vec <- c("Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Cayman Islands", "Cuba", "Dominica", "Dominican Republic",
               "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Puerto Rico", "Saint Barthélemy", "St. Kitts and Nevis",
               "St. Lucia", "St. Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands", "Virgin Islands",
               "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Argentina",
               "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname",
               "Uruguay", "Venezuela")

latamHERB_df <- HERBARIA_table %>% 
  filter(Country %in% LATAM_vec)

sum(latamHERB_df$Percentage_onlyinfo)
# [1] 35.265

a <- latamHERB_df %>% 
  group_by(Country) %>%
  summarise(Countryperc = sum(Percentage_onlyinfo)) %>%
  arrange(desc(Countryperc)) 

sum(a$Countryperc) - a$Countryperc[1] 

EuroHERB_df <- HERBARIA_table %>% 
  filter(!Country %in% c(LATAM_vec, "U.S.A.", "People's Republic of China", "Unknown", "Madagascar", "India",
                         "Canada"))  %>% 
  group_by(Country) %>%
  summarise(Countryperc = sum(Percentage_onlyinfo)) %>%
  arrange(desc(Countryperc)) 

sum(EuroHERB_df$Countryperc)

# 6) Countries table ####

countries_name_list
institutions_split

institutions_split[1,1] %in% countries_name_list[[1]]

dim(institutions_split)
country_mat <- matrix(nrow = 28763, ncol = 11)

for(i in 1:nrow(country_mat)) {
  for(j in 1:ncol(country_mat)) {
    
    index <- which(lapply(seq_along(countries_name_list), 
                          function(x) institutions_split[i, j] %in% countries_name_list[[x]]) == TRUE)
    
    if (length(index) > 0) { 
      
      country_mat[i, j] <- names(countries_name_list)[index]
      
    } else {
      
      next
      
    }
    
  }
}

records_country_list <- lapply(seq_len(nrow(country_mat)), function(i) {unique(country_mat[i,])})

ALLCountryHerb_df <- data.frame(sort(table(unlist(records_country_list)), decreasing = TRUE)) %>%
  mutate(Percentage_tot = round(100 * Freq / 28763, digits = 3),
         Percentage_onlyinfo = round(100 * Freq / 19399, digits = 3)) %>%
  rename(Country = Var1, Records = Freq)

#write.csv(ALLCountryHerb_df, "./output/RECORDS_HERBARIA_Country_ALL.csv")
ALLCountryHerb_df <- read.csv("./output/RECORDS_HERBARIA_Country_ALL.csv")


# 7) Manual count for Countries #### 

# The object institutions_split as produced in the line 116 above, numeral 3)

# List of herbaria per record in database
records_herb_list <- lapply(seq_len(nrow(institutions_split)), function(i) {institutions_split[i,]})

# List of countries with their herbaria 
countries_name_vec <- as.character(unique(HERBARIA_table$Country))

countries_name_list <- vector(mode = "list", length = length(countries_name_vec))
names(countries_name_list) <- countries_name_vec

for (i in seq_along(countries_name_vec)) {
  
  countries_name_list[[i]] <- HERBARIA_table %>% 
    filter(Country == countries_name_vec[i]) %>%
    pull(Herbaria) %>% 
    as.character()
  
}

countries_name_list[2]

#records_herb_list[[1]] %in% countries_name_list[[1]]

# Vectors to count: USA, Brazil, Latin_America, Europe, Unnamed (no info), Unknown (herbaria not found in IH)

LATAM_vec <- c("Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Cayman Islands", "Cuba", "Dominica", "Dominican Republic",
               "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Puerto Rico", "Saint Barthélemy", "St. Kitts and Nevis",
               "St. Lucia", "St. Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands", "Virgin Islands",
               "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Argentina",
               "Bolivia",  "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
               "Uruguay", "Venezuela") # French Guiana was included in the Europe_vec

Europe_vec <- c("U.K.", "Netherlands", "Sweden", "Denmark", "Germany", "Switzerland", "France", "French Guiana", "Spain", "Belgium","Austria",
                "Italy", "Russia", "Finland", "Lithuania")

LATAM_CountryHerb_list <- countries_name_list[names(countries_name_list) %in% LATAM_vec]

Europe_CountryHerb_list <- countries_name_list[names(countries_name_list) %in% Europe_vec]

USA_CountryHerb_list <- countries_name_list["U.S.A."]

Brazil_CountryHerb_list <- countries_name_list["Brazil"]

CountryHerb_df <- Total_DB %>%
  dplyr::select(ID, NAME1, HERBARIUM) %>%
  mutate(usa_h = NA, brazil_h = NA, latam_h = NA, europe_h = NA, unnamed_h = NA, unknown_h = NA) 

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] %in% USA_CountryHerb_list[[1]]) > 0 ) {
    
    CountryHerb_df$usa_h[i] <- 1
    
  } else {
    
    CountryHerb_df$usa_h[i] <- 0
    
  }
  
  
}

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] %in% Brazil_CountryHerb_list[[1]]) > 0 ) {
    
    CountryHerb_df$brazil_h[i] <- 1
    
  } else {
    
    CountryHerb_df$brazil_h[i] <- 0
    
  }
  
  
}

latam_x <- unlist(LATAM_CountryHerb_list)

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] %in% latam_x) > 0 ) {
    
    CountryHerb_df$latam_h[i] <- 1
    
  } else {
    
    CountryHerb_df$latam_h[i] <- 0
    
  }
  
  
}

europe_x <- unlist(Europe_CountryHerb_list)

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] %in% europe_x) > 0 ) {
    
    CountryHerb_df$europe_h[i] <- 1
    
  } else {
    
    CountryHerb_df$europe_h[i] <- 0
    
  }
  
  
}

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] == "Unnamed") > 0 ) {
    
    CountryHerb_df$unnamed_h[i] <- 1
    
  } else {
    
    CountryHerb_df$unnamed_h[i] <- 0
    
  }
  
  
}

Unknown_x <- unlist(countries_name_list["Unknown"], use.names = FALSE)[-1]

for (i in seq_along(records_herb_list)) {
  
  if( sum(records_herb_list[[i]] %in% Unknown_x) > 0 ) {
    
    CountryHerb_df$unknown_h[i] <- 1
    
  } else {
    
    CountryHerb_df$unknown_h[i] <- 0
    
  }
  
  
}


#write.csv(CountryHerb_df, "./output/RECORDS_HERBARIA_Country_dummy.csv")
CountryHerb_df <- read.csv("./output/RECORDS_HERBARIA_Country_dummy.csv")

# Using the total number of records
#USA 60.59173
sum(CountryHerb_df$usa_h) / length(CountryHerb_df$ID) * 100
#Brazil 9.905086
sum(CountryHerb_df$brazil_h) / length(CountryHerb_df$ID) * 100
#Latam 21.97615 (Including Brazil)
sum(CountryHerb_df$latam_h) / length(CountryHerb_df$ID) * 100
#Europe 6.577895
sum(CountryHerb_df$europe_h) / length(CountryHerb_df$ID) * 100

# Using only records with named Herbaria

sum(is.na(CountryHerb_df$HERBARIUM)) # Records with NA in HERBARIUM column 9364, records with info 19399

info_only_df <- CountryHerb_df %>% 
  filter(unnamed_h == 0)

#USA 89.83968
sum(CountryHerb_df$usa_h) / length(info_only_df$ID) * 100
#Brazil 14.68632
sum(CountryHerb_df$brazil_h) / length(info_only_df$ID) * 100
#Latam 32.58415 (Including Brazil)
sum(CountryHerb_df$latam_h) / length(info_only_df$ID) * 100
#Europe 9.75308
sum(CountryHerb_df$europe_h) / length(info_only_df$ID) * 100

# Latam without brazil 17.89783
CountryHerb_df %>%
  filter(latam_h == 1, brazil_h == 0) %>%
  summarise(perc_latam = sum(latam_h) / length(info_only_df$ID) * 100)

# 8) Map of where the Knowledge of the database is kept ####

ALLCountryHerb_df <- read.csv("./output/RECORDS_HERBARIA_Country_ALL.csv")

"./data/GIS/"

world <- ne_countries() %>%
  st_as_sf()

notin_naturalearth <- which(!as.character(ALLCountryHerb_df$Country) %in% unique(world$admin))
ALLCountryHerb_df$Country[notin_naturalearth]

# "U.S.A." = "United States"
# "U.K." = "United Kingdom"
# "French Guiana" = ? Appears as a part of France. 
# It does not affect the position of the first three european countries.
# "People's Republic of China" = "China"

# NOTE: spliting French Guiana did not work
# france_split_sf <- world %>%
#   filter(admin == "France") %>%
#   st_cast("POLYGON") %>%
#   slice(1)
# 
# france_split_sf$admin <- "French Guiana"
# glimpse(france_split_sf)
# plot(france_split_sf)
# 
# world <- bind_rows(world, france_split_sf)
# plot(st_geometry(world))

# NOTE: Only for the purpose of this map, the points from French Guiana will be summed to France. 
ALLCountryHerb_df[c(17, 18), ]
ALLCountryHerb_df[17, 2] <- 139 + 121
ALLCountryHerb_df[17, 3] <- (139 + 121) / 28763 * 100
ALLCountryHerb_df[17, 4] <- (139 + 121) / 19399 * 100

ALLCountryHerb_df <- ALLCountryHerb_df %>%
  arrange(desc(Records))

# Changing names:

ALLCountryHerb_df$Country <- as.character(ALLCountryHerb_df$Country)

ALLCountryHerb_df[1, 1] <- "United States of America"
ALLCountryHerb_df[9, 1] <- "United Kingdom"
ALLCountryHerb_df[37, 1] <- "China"

notin_naturalearth <- which(!as.character(ALLCountryHerb_df$Country) %in% unique(world$admin))
ALLCountryHerb_df$Country[notin_naturalearth]
HERBARIA_table %>%
  filter(Herbaria != "Unnamed") %>% # To exclude the records without data.
  ggplot(aes(area = Percentage_onlyinfo, fill = Herbaria, label = Herbaria,
             subgroup = Country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    reflow = TRUE) +
  geom_treemap_subgroup_border(colour = "black") +
  geom_treemap_subgroup_text(place = "bottomleft", alpha = 0.5, colour =
                               "black", min.size = 0) +
  scale_fill_grey() + 
  #scale_fill_viridis_d(alpha = 0.8) +
  #facet_wrap(~Internal) +
  
  world_jp <- left_join(world, ALLCountryHerb_df, by = c("admin" = "Country"))

map_herb_country <- ggplot() +
  geom_sf(data = world_jp, aes(fill = Percentage_onlyinfo)) +
  #scale_fill_gradient(low = "#DCDCDC", high = "#C0C0C0", na.value = "white") +
  scale_fill_continuous(type = "viridis", na.value = "white", alpha = 0.7) +
  labs(x = NULL, y = NULL) +
  theme_linedraw() +
  theme(#legend.position = "bottom", #c(0.12,0.3)
    legend.background = element_rect(color = "black")) +
  labs(#title = "Record's Information Held at International Herbaria",
    fill = "% Records\nper Country")

png("./figs/Treemap_Country_map.png", width = 1000, height = 500, res = 120)
map_herb_country
dev.off()


# lay <- "
#         11
#         22
#         22
#         "
# map_herb_country + treemap_plot &
#   plot_layout(design = lay)

map_herb_country &
  plot_annotation(tag_levels = "A")

treemap_plot &
  plot_annotation(tag_levels = "A", )

# 9) Bignonieae Database table as image ####

library(tidyverse)
library(grid)
library(gridExtra)

Total_DB <- read.csv("./data/Localidades_Total_v14.csv", stringsAsFactors = FALSE)

glimpse(Total_DB[, 1:22])


grid.table(head(Total_DB[, 1:5], n = 4))

importance_g <- tableGrob(head(Total_DB[, 16:22], n = 4), theme = ttheme_minimal())

importance_g <- gtable_add_grob(importance_g,
                                grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                t = 1, l = 1, r = ncol(importance_g))

importance_g <- gtable_add_grob(importance_g,
                                grobs = segmentsGrob( # line across the bottom
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lwd = 2.0)),
                                t = nrow(importance_g),  nrow(importance_g), l = 1, r =  ncol(importance_g))

png("./figs/Bignonieae_DB1.png", width = 1000, height = 200, res = 75)
grid.draw(importance_g)
dev.off()

png("./figs/Bignonieae_DB2.png", width = 1000, height = 200, res = 75)
grid.draw(importance_g)
dev.off()

png("./figs/Bignonieae_DB3.png", width = 1000, height = 200, res = 75)
grid.draw(importance_g)
dev.off()
