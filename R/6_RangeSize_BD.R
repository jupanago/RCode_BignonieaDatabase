## Range size profile of the Bignonieae Database (BD)

# This script contains the steps taken to estimate the species range size.

# Libraries

library(dplyr)
library(tidyr)
library(readxl)
library(speciesgeocodeR)
library(rgdal)
library(sp)
library(raster)
library(ggplot2)

#### 1) Range size ####

Total_DB <- read_excel("./data/Localidades_Total_v14.csv")

DB_spgeocode <- Total_DB %>% 
  dplyr::select(NAME1, XCOOR, YCOOR)

# EOO 
# CalcRangeSize
DB_spgeocode$identifier <- as.factor(DB_spgeocode$identifier)

rangesize_df <- CalcRange(
  x = DB_spgeocode,
  mode = "EOO",
  value = "area"
)

# write.csv("./data/Range_size_spgeocoder.csv")

# Kmeans
range_df <- read.csv("./data/Range_size_spgeocoder.csv")

kme_df <- kmeans(range_df$EOO, centers = 4, iter.max = 100, nstart = 100)
kme_df$cluster <- as.factor(kme_df$cluster)

kme_df$centers

big_centers <- as.data.frame(kme_df$centers)
write.csv(big_centers, "./data/big_centers.csv")
#big_centers$s_class <- c("Wide", "Narrow", "Medium-narrow", "Medium-wide")

big_cluster <- factor(kme_df$cluster, levels = c("3", "4", "2", "1"))

range_df$cluster <- as.vector(kme_df$cluster)
range_df$Size_class <- ifelse(range_df$cluster == "3", "Narrow", 
                              ifelse(range_df$cluster == "4", "Medium-narrow", 
                                     ifelse(range_df$cluster == "2", "Medium-wide",
                                            ifelse(range_df$cluster == "1", "Wide", NA))))

range_df$Size_class <- factor(range_df$Size_class, levels = c("Narrow", "Medium-narrow", "Medium-wide", "Wide"))

#write.csv(range_df, "./data/Range_size_FINAL.csv")

#### 1.1) Species range size profile ####

jpgray <- gray.colors(n = 4)
show_col(jpgray)

range_df <- read.csv("./data/Range_size_FINAL.csv")
range_df$class_k <- factor(range_df$class_k, levels = c("Narrow", "Medium-narrow", "Medium-wide", "Wide"))
big_centers <- read.csv("./data/big_centers.csv")

# Color palette
pal_vir4 <- viridis(4)

rs_distribution <- ggplot(range_df, aes(x = EOO)) +
  geom_histogram(fill = jpgray[1]) +
  labs(y = "Species",
       x = "Range Size (km^2)"
  ) +
  geom_rug() +
  scale_x_continuous(breaks = seq(0, 30000000, by = 5000000)) +
  theme_few() 

tiff("./figs/7_RangeSize_hist.tiff", res=300, compression = "lzw", width=5, height=5, units="in")
rs_distribution 
dev.off()

# Clusters of species range size by kmeans using four centers
rs_clusters_scatter <- ggplot(range_df, aes(x = 1, y = EOO, col = class_k)) +
  geom_point(position = "jitter", size = 0.8) +
  scale_y_continuous(breaks = seq(0, 40000000, by = 5000000),
                     sec.axis = sec_axis(trans =  ~. * 1, breaks = as.vector(big_centers$V1))) +
  scale_color_viridis_d(name = "Cluster", alpha = 0.5) +
  geom_hline(yintercept = big_centers$V1[2], lty = 2, colour = pal_vir4[1]) +
  geom_hline(yintercept = big_centers$V1[1], lty = 2, colour = pal_vir4[2]) +
  geom_hline(yintercept = big_centers$V1[3], lty = 2, colour = pal_vir4[3]) +
  geom_hline(yintercept = big_centers$V1[4], lty = 2, colour = pal_vir4[4]) +
  theme_few() +
  labs(y = "Range size (km^2)",
       x = NULL) +
  theme(text = element_text(family = "Helvetica", size = 10),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
  ) 

tiff("./figs/7_RangeSize_clustscatter.tiff", res=300, compression = "lzw", width=5, height=5, units="in")
rs_clusters_scatter
dev.off()

# Statistical description by boxplots of clusters
rs_cluster_boxplots <- ggplot(range_df, aes(x = class_k, y = EOO, col = class_k)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 40000000, by = 5000000)) +
  scale_color_viridis_d(name = "Size category") +
  labs(x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "Helvetica", size = 10),
        legend.position = "none"
  )

# png("./figs/Range_size/range_clustboxplot.png", width = 800, height = 710, res = 200)
# rs_cluster_boxplots
# dev.off()


size_class_stat <- ggplot_build(rs_cluster_boxplots)$data[1]
size_class_stat <- as.data.frame(size_class_stat)
size_class_stat <- size_class_stat[1:4, 1:6]
size_class_stat$colour <- c("Narrow", "Medium-narrow", "Medium-wide", "Wide")
colnames(size_class_stat) <- c("Size class", "min", "1stQ", "2ndQ", "3rdQ", "max")
size_class_stat$Species <- c(223, 69, 45, 22) # from ggplot_built(size_class_barplot) below
big_centers <- big_centers[order(big_centers$V1), ]
size_class_stat$Centroid <- big_centers$V1
size_class_stat <- size_class_stat %>% dplyr::select("Size class", "Species", "Centroid", "min", "1stQ", "2ndQ", "3rdQ", "max")


## Species data frame

index <- which(!unique(Total_DB$NAME1) %in% range_df$Species)
spp_lessp <- as.data.frame(unique(Total_DB$NAME1)[index])
colnames(spp_lessp) <- "Species"

new_range_df <- bind_rows(range_df, spp_lessp)
new_range_df$class_k <- factor(new_range_df$class_k, levels = c("Micro", "Narrow", "Medium-narrow", "Medium-wide", "Wide"))
new_range_df$class_k[360:386] <- "Micro"         

Species_class <- new_range_df[order(new_range_df$Species),]
Species_class <- Species_class[c(-1, -4)]


## Barplot for size classes

palette_jp <- c("black", "#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")

size_clas_barplot <- ggplot(Species_class, aes(x = class_k, fill = class_k)) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = palette_jp) +
  scale_y_continuous(breaks = seq(0, 386, by = 25)) +
  theme_few() +
  labs(y = "Species",
       x = "Range size category") +
  theme(#axis.text.x = element_text(angle = 45, vjust=1, hjust=1),
    legend.position = "none")

tiff("./figs/7_RangeSize_categoriesbarplot.tiff", res=300, compression = "lzw", width=6, height=5, units="in")
size_clas_barplot
dev.off()




