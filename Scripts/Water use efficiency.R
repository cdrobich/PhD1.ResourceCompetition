library(tidyverse)
library(ggpubr)


ciras.17 <- read.csv("Data/CIRAS_carbon_WUE_2017.csv")
ciras.16 <- read.csv("Data/CIRAS_2016.csv") 

ciras <- full_join(ciras.16, ciras.17)


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



plot.res<- ggplot(WUE.res, aes(x = light, y = avg,
                                 group = phy_trt,
                                 shape = Treatment,
                                 colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Phytometer") +
  geom_point(size = 5) +
  theme_classic() +
  labs(y = expression(paste("Water Use Efficiency")),
       x = "") + 
  scale_colour_manual(values = c("black", "grey")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-1, 5) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = "none")

plot.res


plot.phrag <- ggplot(WUE.phrag, aes(x = light, y = avg,
                               group = neigh_trt,
                               shape = Treatment,
                               colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Neighbour") +
  geom_point(size = 5) +
  theme_classic() +
  labs(y = expression(paste("Water Use Efficiency")),
       x = "") + 
  scale_colour_manual(values = c("black", "grey")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-1, 5) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = c(0.9, 0.2)) 

plot.phrag

panel <- ggarrange(plot.res, plot.phrag,
                   labels = "AUTO",
                   hjust = c(-5, -5),
                   vjust = 1.75,
                   ncol = 1,
                   nrow = 2)

panel


ggsave("Figures/panel_WUE.TIFF", panel, dpi = 300)
