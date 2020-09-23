## Taxonomic and Temporal coverage of the Bignonieae Database (BD)

# This script contains the steps taken to assess the temporal and taxonomic
# coverage.

# Libraries

library(dplyr)
library(stringr)
library(stringi)
library(rebus)
library(ggplot2)
library(tidyr)

# Loading database
Total_DB <- read.csv("./data/Localidades_Total_v14.csv", stringsAsFactors = FALSE)
View(Total_DB)

# 1) Taxonomic coverage ####

Unique_gen <- Total_DB %>%
  group_by(GENUS) %>% 
  summarise(Spp = n_distinct(NAME1),
            n_Records = n(),
            perc_nRecords = (n_Records/29763)*100,
            Unique_localities = n_distinct(YCOOR, XCOOR)) %>%
  mutate(duplicated = n_Records - Unique_localities) %>% 
  arrange(desc(n_Records))

Unique_gen$GENUS <- factor(Unique_gen$GENUS, 
                           levels = Unique_gen$GENUS[order(Unique_gen$Spp)])

SppxGen <- ggplot(Unique_gen, aes(x = GENUS, y = Spp, order = factor(GENUS))) + 
  geom_bar(stat = "identity", fill = jpgray[1]) +
  scale_y_continuous(breaks = seq(0, 75, 5)) +
  coord_flip()  +
  theme_few() +
  theme(
    axis.text.y = element_text(face = "italic", size = 12),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = NULL,
    x = NULL, 
    y = "Species"
  )


# Number of duplicated records per genera

Unique_gen$GENUS <- factor(Unique_gen$GENUS, 
                           levels = Unique_gen$GENUS[order(Unique_gen$n_Records)])

Unique_gen_melt <- as.data.frame(Unique_gen[, c(1, 5, 6, 3)])
colnames(Unique_gen_melt) <- c("Genus", "Unique", "Duplicated", "Total")

Unique_gen_melt <- reshape::melt(Unique_gen_melt[,-4], idvar = "Genus")

Gen_records <- ggplot(Unique_gen_melt, aes(x = Genus, y = value, fill = factor(variable))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(jpgray[1], jpgray[3]), labels = c("Unique", "Duplicated")) + 
  scale_y_continuous(breaks = seq(0, 5200, 500)) +
  coord_flip() +
  theme_few() +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6),
    axis.text.y = element_blank(),
    legend.text  = element_text(face = "bold", size = 10)
  ) +
  labs(
    title = NULL,
    x = NULL, 
    y = "Locality records", 
    fill = ""
  ) 

tiff("./figs/1_TaxCov_SppxGen.tiff", res=300, compression = "lzw", width=10, height=5, units="in")
ggarrange(SppxGen, Gen_records,
          ncol = 2,
          nrow = 1)
dev.off()

# 2) Temporal coverage ####

col_year <- Total_DB %>%
  dplyr::select(NAME1, COLL_YEAR, COLL_MONTH, COLL_DAY)

col_year$COLL_YEAR <- as.numeric(col_year$COLL_YEAR)
col_year$COLL_MONTH <- as.numeric(col_year$COLL_MONTH)

year_samp <- col_year %>%
  ggplot(aes(x = COLL_YEAR)) +
  geom_histogram(na.rm = TRUE, fill = jpgray[1]) +
  scale_x_continuous(breaks = pretty(col_year$COLL_YEAR, n = 10)) +
  scale_y_continuous(breaks = seq(0, 5500, 250)) +
  labs(y = "Locality records",
       x = "Year"
  ) +
  theme_few() 

a <- col_year %>%
  group_by(COLL_MONTH) %>%
  summarise(Records = n()) 

a <- a[1:12, ]

month_samp <- ggplot(a, aes(x = COLL_MONTH, y = Records)) +
  geom_col(fill = jpgray[1]) +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = seq(0, 3000, 250)) +
  labs(y = "Locality records", x = "Month") +
  theme_few()


tiff("./figs/2_Tempcov_year_month.tiff", res=300, compression = "lzw", width=10, height=5, units="in")
ggarrange(year_samp, month_samp, 
          ncol = 2, 
          nrow = 1)
dev.off()


