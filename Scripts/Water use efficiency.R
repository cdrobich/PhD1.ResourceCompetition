library(tidyverse)
library(ggpubr)


ciras.17 <- read.csv("Data/CIRAS_carbon_WUE_2017.csv")
ciras.16 <- read.csv("Data/CIRAS_2016.csv") 

ciras <- full_join(ciras.16, ciras.17)

write.csv(ciras, "Data/CIRAS_WUE_bothyears.csv")

ciras <- read.csv("Data/CIRAS_WUE_bothyears.csv")

ciras <- ciras %>% 
  unite("spp_trt", Phytometer, Treatment, remove = FALSE)

str(ciras)


ciras <- ciras %>% 
  separate(spp_trt, c("Phytometer", NA, NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Neighbour", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, NA, "Treatment"), remove = FALSE)


(sum <- ciras %>% group_by(spp_trt, light) %>% 
  summarise(avg = mean(WUE),
            sd = sd(WUE),
            N = length(WUE),
            str = (sd/(sqrt(N)))))

sum <- sum %>% 
  separate(spp_trt, c("Phytometer", NA, NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Neighbour", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, NA, "Treatment"), remove = FALSE)


WUE.phrag <- sum %>% filter(Phytometer == "Phragmites")

WUE.phrag <- WUE.phrag %>% 
  unite("neigh_trt", Neighbour, Treatment, remove = FALSE)



target <- c("Carex", "Calamagrostis", "Typha")

WUE.res <- sum %>% filter(Phytometer %in% target)

WUE.res <- WUE.res %>% 
  unite("phy_trt", Phytometer, Treatment, remove = FALSE)


#  WUE figures --------------------------------------------------------------------

res.WUE <- ggplot(WUE.res, aes(x = light, y = avg,
                    group = phy_trt,
                    shape = Treatment,
                    colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Phytometer") +
  geom_point(alpha = 0.7,
             size = 5) +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme_classic() +
  labs(y = expression(paste("WUE"," ", " (", "CO"[2], " mmol ",  s^-1, " ", m^-2,"H"[2],"O", sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "\u00B5mol  ",  s^-1, " ", m^-2, sep=")"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  ylim(-1, 4) +
  guides(colour = "none") +
  theme(legend.position = c(0.9, 0.2))

phr.WUE <- ggplot(WUE.phrag, aes(x = light, y = avg,
                                 group = neigh_trt,
                                 shape = Treatment,
                                 colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Neighbour") +
  geom_point(alpha = 0.7,
             size = 5) +
  scale_colour_manual(values = c("#454ADE", "#440C53")) +
  theme_classic() +
  labs(y = expression(paste("WUE"," ", " (", "CO"[2], " mmol ",  s^-1, " ", m^-2,"H"[2],"O", sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "\u00B5mol  ",  s^-1, " ", m^-2, sep=")"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  ylim(-1, 4) +
  guides(colour = "none") +
  theme(legend.position = c(0.9, 0.2))

wue.panel <- ggarrange(res.WUE, phr.WUE,
          labels = "AUTO",
          hjust = c(-5, -5),
          vjust = 1.75,
          ncol = 1,
          nrow = 2)

ggsave("Figures/panel_WUE.TIFF", wue.panel, dpi = 300)
