library(tidyverse)
library(ggpubr)

ciras.17 <- read.csv("Data/CIRAS_long_2017.csv")

ciras.16 <- read.csv("Data/CIRAS_2016.csv") 


#### 2016 data play around #####

ciras.16 <- ciras.16 %>% 
  unite("spp_trt", Phytometer, Treatment, remove = FALSE)

ciras.16$light <- as.factor(ciras.16$light)
str(ciras.16)

ciras.17 <- ciras.17 %>% 
  unite("spp_trt", Phytometer, Treatment, remove = FALSE)

ciras.17$light <- as.factor(ciras.17$light)
str(ciras.17)


ciras <- full_join(ciras.16, ciras.17)
ciras <- ciras[,1:10]


sum <- ciras %>% group_by(spp_trt, light) %>% 
  summarise(avg = mean(carbon),
            sd = sd(carbon),
            N = length(carbon),
            str = (sd/(sqrt(N))))

ciras.sum <- read.csv("Data/CIRAS_summary.csv")
colnames(ciras.sum)


ggplot(ciras.sum, aes(x = light, y = avg, color = Phytometer,
                      shape = Treatment,
                      group = spp_trt)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str), width = 0.1) +
  geom_line() +
  geom_point(size = 3) +
  theme_classic() +
  labs(y = "Carbon Assimilation ",
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  #scale_color_manual(values = c("#9ebcda","#8856a7")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))



ciras.phrag <- ciras.sum %>% filter(Phytometer == "Phragmites")

ciras.phrag <- ciras.phrag %>% 
  unite("neigh_trt", Neighbour, Treatment, remove = FALSE)


phr.ciras <- ggplot(ciras.phrag, aes(x = light, y = avg,
                      shape = neigh_trt,
                      group = neigh_trt)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  geom_point(colour = "#737373", size = 5) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  scale_shape_manual(values = c(15, 0, 16, 1, 17, 2)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
  scale_x_continuous(breaks=c(0, 50, 100, 200, 500, 1000, 1500)) +
  labs(shape = "Neighbour & Treatment") +
  theme(legend.position = c(0.75, 0.2))


phr.ciras

ggsave("Figures/phrag_CIRAS.TIFF", phr.ciras,
       width = 14, height = 6.8, units = "in",
       dpi = 300)


target <- c("Carex", "Calamagrostis", "Typha")

ciras.res <- ciras.sum %>% filter(Phytometer %in% target)


ciras.res <- ciras.res %>% 
  unite("phy_trt", Phytometer, Treatment, remove = FALSE)

res.ciras <- ggplot(ciras.res, aes(x = light, y = avg,
                        shape = phy_trt,
                        group = phy_trt)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  geom_point(colour = "#00441b", size = 5) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  scale_color_manual(values = c("#1b7837","#4d9221", "#01665e")) +
  scale_shape_manual(values = c(15, 0, 16, 1, 17, 2)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
  labs(shape = "Phytometer & Treatment") +
  scale_x_continuous(breaks=c(0, 50, 100, 200, 500, 1000, 1500)) +
  theme(legend.position = c(0.75, 0.2)) 
  
res.ciras

ggsave("Figures/res_CIRAS.TIFF", res.ciras,
       width = 14, height = 6.8, units = "in",
       dpi = 300)


panel <- ggarrange(res.ciras, phr.ciras,
                   labels = "AUTO",
                   hjust = c(-6.5, -6.5),
                   vjust = 2,
                   ncol = 1,
                   nrow = 2)

panel


ggsave("Figures/pane_CIRAS.TIFF", panel,
       width = 25, height = 6.74, units = "in",
       dpi = 300)
# size 17.8 x 6.74 in

### facet wrap #####
str(ciras.res)

plot.res<- ggplot(ciras.res, aes(x = light, y = avg,
                                    group = phy_trt,
                                    shape = Treatment,
                                    colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Phytometer") +
  geom_point(size = 5) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = "") + 
  scale_colour_manual(values = c("black", "grey")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = )

plot.res


ciras.phrag <- ciras.phrag %>% 
  unite("phy_trt", Phytometer, Neighbour, remove = FALSE)

ciras.phrag <- ciras.phrag  %>% 
  separate(neigh_trt, c(NA,"Treatment"), remove = FALSE) 

colnames(ciras.phrag)


plot.phr <- ggplot(ciras.phrag, aes(x = light, y = avg,
                         group = neigh_trt,
                         shape = Treatment,
                         colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("phy_trt") +
  geom_point(size = 5) +
  scale_colour_manual(values = c("black", "grey")) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = c(0.9, 0.2))

plot.phr


carbon.assim.panel <- ggarrange(plot.res, plot.phr,
          nrow = 2)

ggsave("Figures/Carbon_facet.jpeg", carbon.assim.panel)


######## Comparison at 1500 ########

light1500 <- ciras %>% filter(light == "1500")

target <- c("Calamagrostis", "Carex", "Typha")

light1500.res <- light1500 %>% filter(Species %in% target)
colnames(light1500.res)


sum.1500 <- light1500.res %>% 
  group_by(Species, Treatment) %>% 
  summarise(median = median(carbon))

light1500.res %>% 
  group_by(Species, Treatment) %>% 
  mutate(Cmean = mean(carbon),
         Cmedian = median(carbon)) -> light1500.res

res1500 <- ggplot(light1500.res, aes(x = carbon, fill = as.factor(Treatment))) +
  geom_density(alpha = 0.6, size = 1) +
  facet_wrap("Species") + 
  scale_fill_manual(values = c("black", "grey")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Density") +
  xlim(0, 40) +
  labs(fill = "Competition") +
  theme(legend.position = c(0.85, 0.8)) +
  geom_vline(aes(xintercept = Cmean,
                 colour = Treatment),
             size = 1,
             show.legend = FALSE) +
  scale_colour_manual(values = c("black", "grey")) +
  theme(legend.position = "none")

res1500




light1500.phrag <- light1500 %>% filter(Species == "Phragmites")

light1500.phrag %>% 
  group_by(Neighbour, Treatment) %>% 
  mutate(Cmean = mean(carbon),
         Cmedian = median(carbon)) -> light1500.phrag


phrag1500 <- ggplot(light1500.phrag, aes(x = carbon, fill = as.factor(Treatment))) +
  geom_density(alpha = 0.6, size = 1) +
  facet_wrap("Phytometer") + 
  scale_fill_manual(values = c("black", "grey")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2], "umol  ",  s^-1, " ", m^-2, sep=")")),
       y = "Density") +
  xlim(0, 40) +
  labs(fill = "Competition") +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = Cmean,
                 colour = Treatment),
             size = 1,
             show.legend = FALSE) +
  scale_colour_manual(values = c("black", "grey")) +
  theme(legend.position = c(0.7, 0.85))

phrag1500

carbon.1500 <- ggarrange(res1500, phrag1500,
          nrow = 2)


ggsave("Figures/Carbon Assimilation 1500 panel.JPEG", carbon.1500)


all.panel <- ggarrange(carbon.assim.panel, carbon.1500,
          labels = "AUTO")



ggsave("Figures/All_Carbon_Assimilation.JPEG", all.panel)

