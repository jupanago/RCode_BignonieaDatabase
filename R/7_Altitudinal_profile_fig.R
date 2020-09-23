## Altitudinal profile of the Bignonieae Database (BD)

# This script contains only the figure produce to show the altitudinal profile.
# To see the steps taken to produce the elevational data, please go to 
# ./R/0_Altitudinal_profile_BD.R

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