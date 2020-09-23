## Coordinate quality assessment of the Bignonieae Database (BD)

# This script contains the steps taken to assess the collectors of the BD.


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

# 1) Collectors ####

# collectors

collectors <- trimws(Total_DB$COLLECTOR)
collectors <- str_remove_all(collectors, pattern = ",")

# Unique collectors: 3116
length(unique(collectors))

nana_index <- which(is.na(collectors))
View(Total_DB[nana_index,])
#write.csv(Total_DB[nana_index,], "./data/NA_Collectors_BIG_DB.csv")

# Collectors table

collect_table <-  as_tibble(sort(table(collectors, exclude = c("", NA)), decreasing = TRUE)) %>% 
  mutate(sampleperc_tot = (n/28763)*100)
collect_table$collectors <- factor(collect_table$collectors,
                                   levels = collect_table$collectors,
                                   ordered = TRUE)
View(collect_table)
sum(collect_table$n)
#write.csv(collect_table, "./data/Collectors_BIG_DB.csv")

# Bar diagram: collectors with more than 100 collections

png(filename = "./figs/collectors_count.png", width = 800, height = 710, res = 120)
collect_table %>%
  filter(n >= 100) %>%
  ggplot(aes(x = collectors, y = n)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 4000, 250)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank()) +
  labs(y = "Collections")
dev.off()

# 3.1) Collectors and time #####

col_year <- Total_DB %>%
  dplyr::select(NAME1, COLL_YEAR, COLL_MONTH, COLL_DAY)

col_year$COLL_YEAR <- as.numeric(col_year$COLL_YEAR)
col_year$COLL_MONTH <- as.numeric(col_year$COLL_MONTH)

a <- col_year %>%
  group_by(COLL_MONTH) %>%
  summarise(Records = n())

a <- a[1:12, ]

# Total collections per month
png(filename = "./figs/collectors_month.png", width = 800, height = 710, res = 120)
ggplot(a, aes(x = COLL_MONTH, y = Records)) +
  geom_col(fill = "grey") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = seq(0, 3000, 250)) +
  labs(y = "Records", x = "Month") +
  theme(text = element_text(family = "Helvetica", size = 10)) +
  theme_minimal()
dev.off()

b <- Total_DB %>%
  group_by(COLL_YEAR, COLL_MONTH, COLL_DAY, COLLECTOR) %>%
  #filter(COLLECTOR %in% as.character(collect_table$collectors[1:10]))
  summarise(Records = n(), .) %>% 
  drop_na() %>%
  mutate(DATE_COLL = as.Date(paste(COLL_YEAR, COLL_MONTH, COLL_YEAR, sep = "-"), "%Y-%m-%d"))

Total_DB$COLLECTOR <- factor(Total_DB$COLLECTOR, levels = collect_table$collectors, ordered = TRUE)

# Collections per collector in time
png(filename = "./figs/collectors_tempcollection.png", width = 1000, height = 710, res = 120)
b %>%
  filter(COLLECTOR %in% as.character(collect_table$collectors[1:12])) %>%
  ggplot(aes(x = DATE_COLL, y = Records, group = COLLECTOR)) +
  geom_path() +
  facet_wrap(~COLLECTOR) +
  scale_x_date(date_breaks = "10 years") +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  labs(y = "Records",
       x = "Collection date") +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
        strip.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold"))
dev.off()

# Collections per collector by month
png(filename = "./figs/collectors_tempcollection2.png", width = 1000, height = 710, res = 120)
b %>%
  filter(COLLECTOR %in% as.character(collect_table$collectors[1:12])) %>%
  ggplot(aes(x = factor(COLL_MONTH, levels = 1:12, ordered = T), y = Records, group = COLLECTOR)) +
  geom_col() +
  facet_wrap(~COLLECTOR) +
  labs(y = "Records",
       x = "Collection date") +
  scale_x_discrete(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
        strip.background = element_rect(colour = "black"),
        strip.text = element_text(face = "bold"))
dev.off()

