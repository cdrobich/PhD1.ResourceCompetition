
library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)


RCI.res <- read.csv("Data/RCI_residents.csv")
str(RCI.res) 
RCI.res$Year <- as.factor(RCI.res$Year)

sum <- RCI.res %>% 
  group_by(Phytometer, Year) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#  Phytometer    Year  average   std     N     SE
#1 Calamagrostis 2016  -1.37   3.50     10 1.11  
#2 Calamagrostis 2017   0.163  0.420     9 0.140 
#3 Carex         2016   0.365  0.244     5 0.109 
#4 Carex         2017   0.0611 0.244    11 0.0735
#5 Typha         2016   0.188  0.383     6 0.156 
#6 Typha         2017  -0.192  0.699     6 0.285 

sum.res <- RCI.res %>% 
  group_by(Phytometer) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#Phytometer     average   std     N     SE
#1 Calamagrostis -0.646   2.61     19 0.600 
#2 Carex          0.156   0.277    16 0.0692
#3 Typha         -0.00193 0.573    12 0.165

##### Resident species ##############

colours = c("Calamagrostis" = "#084594", 
            "Typha" = "#6e016b", 
            "Carex" = "#9ecae1", 
            "Phragmites" = "#fb6a4a")

shapes = c("Calamagrostis" = 22, 
           "Typha" = 23, 
           "Carex" = 21, 
           "Phragmites" = 24)

RCI.plot <- ggplot(RCI.res, 
                   aes(x = Phytometer, y = RCI, 
                       fill = Phytometer, shape = Phytometer)) + 
  geom_jitter(aes(stroke = 1.5),
              width = 0.15,
              height = 0.15,
             size = 5) +
  theme_classic() +
  labs(x = " ",
       y = expression(paste("Relative Competition Index (RCI)"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  coord_flip() +
  scale_fill_manual(values = colours) +
  scale_shape_manual(values = shapes) +
  scale_y_continuous(breaks = c(-12, -10, -8, -6, -4, -2, 0, 2)) 

ggsave("Figures/RCI_residents.jpeg")

###### Phragmites ###############

RCI.phrag <- read.csv("Data/RCI.phrag.csv")
str(RCI.phrag) 
RCI.phrag$Year <- as.factor(RCI.phrag$Year)

RCI.phrag <- RCI.phrag %>% 
  unite("Phytometer", Phytometer,Neighbours, remove = FALSE)


ph.sum <- RCI.phrag %>% 
  group_by(Phytometer, Year) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#   spp_n                    Year  average   std     N    SE
#1 Phragmites_Calamagrostis 2016  -0.123  0.544     6 0.222
#2 Phragmites_Calamagrostis 2017  -0.373  0.958     4 0.479
#3 Phragmites_Carex         2016  -0.675  2.05      5 0.915
#4 Phragmites_Carex         2017   0.104  0.728     4 0.364
#5 Phragmites_Typha         2016  -0.0544 0.373     5 0.167
#6 Phragmites_Typha         2017  -0.361  0.670     7 0.253


sum.phr <- RCI.phrag %>% 
  group_by(Phytometer) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#Phytometer               average   std     N    SE
#<chr>                      <dbl> <dbl> <int> <dbl>
#1 Phragmites_Calamagrostis  -0.223 0.698    10 0.221
#2 Phragmites_Carex          -0.329 1.57      9 0.523
#3 Phragmites_Typha          -0.233 0.566    12 0.163

##### Resident species ##############

ph.sum <- ph.sum %>% 
  mutate(Year = fct_relevel(Year, "2017", "2016"))

RCI.plot2 <- ggplot(RCI.phrag, 
                    aes(x = Neighbours, y = RCI, 
                        fill = Phytometer, shape = Neighbours)) + 
  geom_jitter(aes(stroke = 1.5),
              width = 0.15,
              height = 0.15,
              size = 5) +
  theme_classic() +
  labs(x = " ",
       y = expression(paste("Relative Competition Index (RCI)"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  coord_flip() +
  scale_fill_manual(values = colours) +
  scale_shape_manual(values = shapes) +
  scale_y_continuous(breaks = c(-12, -10, -8, -6, -4, -2, 0, 2)) 


RCI.panel <- RCI.plot + RCI.plot2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A')

ggsave("Figures/RCI_panel.TIFF", RCI.panel,
       dpi = 150, units = "in")



summed <- full_join(sum, ph.sum)

summed <- summed %>% 
  mutate(Year = fct_relevel(Year, "2017", "2016"))

summed <- summed %>% 
  mutate(Phytometer = fct_relevel(Phytometer, 
                                  "Typha", 
                                  "Carex", 
                                  "Calamagrostis",
                                  "Phragmites_Typha", 
                                  "Phragmites_Carex", 
                                  "Phragmites_Calamagrostis"))


RCI.plot.all <- ggplot(summed, aes(x = Phytometer, y = average, colour = Year, shape = Year)) + 
  geom_point(position = position_dodge(0.6),
             size = 5) +
  geom_errorbar(aes(ymin = average - SE, ymax = average + SE),
                width = 0.3, position = position_dodge(0.6),
                color = "black",
                size = 1) +
  theme_classic() +
  scale_colour_manual(values = c("#00441b", "#08519c")) +
  labs(x = " ",
       y = expression(paste("Relative Competition Index (RCI)"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             size = 1) +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 30))

######### No Years Apart ###############

sum.phrag <- RCI.phrag %>% 
  group_by(Phytometer) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#  Phytometer               average   std     N    SE
#1 Phragmites_Calamagrostis  -0.223 0.698    10 0.221
#2 Phragmites_Carex          -0.329 1.57      9 0.523
#3 Phragmites_Typha          -0.233 0.566    12 0.163

write.csv(sum.phrag, "Data/phrag.RCI.sum.csv")

RCI.plot.phr <- ggplot(sum.phrag, 
                       aes(x = Phytometer, 
                           y = average, 
                           shape = Phytometer)) + 
  geom_errorbar(aes(ymin = average - SE, ymax = average + SE),
                width = 0.3, 
                color = "black",
                size = 1) +
  geom_point(size = 5,
             colour = "#737373") +
  theme_classic(base_size = 14) +
  ylab("Relative Competition Index (RCI)") + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed",
             size = 1) +
  coord_flip() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size = 11))

RCI.plot.phr

ggsave("Figures/RCI_phrag.TIFF", RCI.plot.phr,
       dpi = 300)


sum.res <- RCI.res %>% 
  group_by(Phytometer) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

#Phytometer     average   std     N     SE
#1 Calamagrostis -0.651   2.53     21 0.552 
#2 Carex          0.156   0.277    16 0.0692
#3 Typha         -0.00193 0.573    12 0.165 

RCI.plot.res <- ggplot(sum.res, aes(x = Phytometer, y = average, shape = Phytometer)) + 
    geom_errorbar(aes(ymin = average - SE, ymax = average + SE),
                width = 0.3, 
                color = "black",
                size = 1) +
  geom_point(size = 5,
             colour = "#00441b") +
  theme_classic(base_size = 14) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.border = element_rect(fill = NA)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             size = 1) +
  ylab("Relative Competition Index (RCI)") +
  coord_flip() +
  theme(legend.position = c(0.15, 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size=11))

RCI.plot.res


ggsave("Figures/RCI_residents.TIFF", RCI.plot.phr,
       dpi = 300)



panel.RCI <- ggarrange(RCI.plot.res, RCI.plot.phr,
                       nrow = 2,
                   labels = "AUTO",
                   hjust = c(-2, -2),
                   vjust = 2)

panel.RCI

ggsave("Figures/RCI_panel.TIFF", panel.RCI,
       dpi = 300)

######## RCI Density Plots ##########
RCI.res <- read.csv("Data/RCI_residents.csv")
RCI.res

rci.res.den <- ggplot(RCI.res, aes(x = RCI, fill = as.factor(Phytometer))) +
  geom_density(alpha = 0.5, size = 1) +
  facet_wrap("Phytometer") + 
  scale_fill_manual(values = c("grey", "grey", "grey")) +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Density") +
  theme(legend.position = "none",
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  xlim(-15, 2) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=2)

rci.res.den 

ggsave("Figures/RCI_density.TIFF", rci.res.den,
       dpi= 300)




RCI.phrag <- read.csv("Data/RCI.phrag.csv")
RCI.phrag

RCI.phrag <- RCI.phrag %>% 
  unite("Species", Phytometer,Neighbours, remove = FALSE)

rci.phr.den <- ggplot(RCI.phrag, aes(x = RCI, fill = as.factor(Phytometer))) +
  geom_density(alpha = 0.5, size = 1) +
  facet_wrap("Species") + 
  scale_fill_manual(values = c("grey")) +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = "Relative Competition Index (RCI)",
       y = "Density") +
  theme(legend.position = "none",
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  xlim(-5, 2) +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size= 2)

rci.phr.den 

ggsave("Figures/RCI_phrag_density.TIFF", rci.phr.den,
       dpi= 300)


RCI.panel <- ggarrange(rci.res.den, rci.phr.den,
          nrow = 2,
          labels = "AUTO",
          hjust = -2,
          vjust = 2)

ggsave("Figures/RCI_panel.jpeg", RCI.panel,
       height = 6.68,
       width = 9,
       dpi = 300)

############ Raw Biomass ##########

weights16 <- read.csv("Data/weights_2016.csv")
weights17 <- read.csv("Data/weights_2017.csv")

weights <- full_join(weights16, weights17)


weights <- na.omit(weights)

weights <- weights %>% 
  unite("spp_trt", Species, Competition, remove = FALSE)

weights <- weights %>% #rename the factors
  mutate(Competition = fct_recode(Competition,
                                  "No competition" = "No",
                                  "Competition" = "Yes"))


target <- c("Carex", "Calamagrostis", "Typha")

weight.res <- weights %>% filter(Species %in% target)

res.den <- ggdensity(weight.res, x = "Total",
                     fill = "spp_trt", combine = TRUE)
res.den

weights$Competition <- factor(weights$Competition, 
                            levels = c("Competition",
                                       "No competition"))

weights %>% 
  group_by(Species, Competition) %>% 
  mutate(mean = mean(Total),
         median = median(Total)) -> weights


weights %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Total),
         median = median(Total),
         std = sd(Total),
         N = length(Total),
         sterr = std/sqrt(N))

#  Species        mean median   std   N   sterr
#1 Calamagrostis  1.87   1.59  1.91   43  0.291
#2 Carex          2.09   1.98  1.10   33  0.191
#3 Phragmites     21.0   20.3  11.8   66  1.45 
#4 Typha          37.5   37.1  10.4   24  2.13 



library(viridis)

density <- ggplot(weights, aes(x = Competition, 
                               y = Total)) +
  geom_jitter(aes(colour = Competition,
                  shape = Competition),
              width = 0.1,
              height = 1,
              size = 2.5,
              alpha = 0.7) +
  facet_wrap("Species") + 
  theme_classic(base_size = 12) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  labs(x = "",
       y = "Total Biomass (g)") +
  labs(fill = "Competition") +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Competition, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60))

density

ggsave("Figures/biomass_density.TIFF", density,
       dpi = 300)


##### Light Penetration #########
phrag.light.pair <- read.csv("Data/Light_phrag.csv")
light.res.pair <- read.csv("Data/light_resident.csv")

light <- full_join(phrag.light.pair, light.res.pair)

light <- light %>% #rename the factors
  mutate(Competition = fct_recode(Competition,
                                "No competition" = "no",
                                "Competition" = "yes"))

light$Competition <- factor(light$Competition, 
                                   levels = c("Competition",
                                              "No competition"))



light %>% 
  group_by(Phytometer, Competition) %>% 
  mutate(mean = mean(Incident),
         median = median(Incident)) -> light



light.den <- ggplot(light, aes(x = Competition,
                               y = Incident)) +
  geom_jitter(aes(colour = Competition,
                  shape = Competition),
              width = 0.07,
              height = 1,
              size = 2.5,
              alpha = 0.7) +
  facet_wrap("Phytometer") + 
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme_classic(base_size = 12) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Photosynthetically Active Radiation "," ", " ( ", "\u00B5mol  ",  s^-1, " ", m^-2, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Competition, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1) 

light.den

ggsave("Figures/Light_densityplot.TIFF", light.den,
       dpi = 300)


light.biomass.jitter <- ggarrange(light.den, density,
                           ncol = 2,
                           labels = "AUTO",
                           hjust = -2)


ggsave("Figures/PAR_biomass_jitter.jpeg", light.biomass.jitter,
       dpi = 300)









Light.Biomass <- ggarrange(light.den, density,
                           rci.res.den, rci.phr.den, 
                       ncol = 2,
                       nrow = 2,
                       labels = "AUTO",
                       hjust = c(-3.5, -2.5, -3, -3),
                       vjust = 2)

Light.Biomass

ggsave("Figures/Light_Biomass.TIFF", Light.Biomass,
       height = 9.3, width = 14, units = "in",
       dpi = 300)

####### Height ###########

height6 <- read.csv("Data/Height_long_2016.csv")
height6$height <- as.numeric(height6$height)

date6 <- height6$date
dates <- dmy(date6)

height6$dates <- dates
height6

height7 <- read.csv("Data/Height_long_2017.csv")

height7 <- na.omit(height7)
str(height7)

height7$height <- as.numeric(height7$height)

date <- height7$date
dates <- dmy(date)

height7$dates <- dates

height7

heights <- full_join(height6, height7)

heights <- heights %>% #rename the factors
  mutate(Species = fct_recode(Species,
                              "Calamagrostis" = "Calamagrositis"))


heights <- heights %>% 
  unite("spp_trt", Species, Competition, remove = FALSE)

heights <- heights %>% 
  separate(dates, c(NA,"Month", NA), remove = FALSE) 

unique(heights$Species)


height.sum.month <- heights %>% group_by(spp_trt, Month) %>% 
  summarise(avg = mean(height),
            sd = sd(height),
            N = length(height),
            str = (sd/(sqrt(N))))



height.sum.month <- height.sum.month %>% 
  separate(spp_trt, c("Species", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Competition"), remove = FALSE) 


height.sum.month <- height.sum.month %>% #rename the factors
  mutate(Month = fct_recode(Month,
                              "May" = "05",
                            "June" = "06",
                            "July" = "07"))



plot.month <- ggplot(height.sum.month, aes(x = Month, y = avg,
                                shape = Species,
                                group = spp_trt,
                                color = Competition)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str), width = 0.1) +
  geom_line(color = "black") +
  geom_point(size = 5) + 
  #scale_x_date(date_labels = "%d-%b-%y",
    #           date_breaks = "1 week") +
  theme_classic() +
  labs(y = "Height (cm)",
       x = "") + 
  scale_color_manual(values = c("grey","black")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14)) +
  labs(shape = "Phytometer") +
  ylim(50, 250) 

plot.month

ggsave("Figures/Height_2016_2017.JPEG", plot.month)




height.sum <- heights %>% group_by(spp_trt, dates) %>% 
  summarise(avg = mean(height),
            sd = sd(height),
            N = length(height),
            str = (sd/(sqrt(N))))


height.sum <- height.sum %>% 
  separate(spp_trt, c("Species", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Competition"), remove = FALSE) %>% 
  separate(dates, c("Year", NA, NA), remove = FALSE)

colnames(height.sum)


height.16 <- height.sum %>% filter(Year == "2016")
height.17 <- height.sum %>% filter(Year == "2017")

plot16 <- ggplot(height.16, aes(x = dates, y = avg,
                           shape = Competition,
                           group = spp_trt,
                           colour = Competition)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str), width = 0.7) +
  geom_line(color = "black") +
  geom_point(size = 5) + 
  facet_wrap("Species") +
  scale_x_date(date_labels = "%d-%b-%y",
                    date_breaks = "2 week") +
  theme_classic() +
  labs(y = "Height (cm)",
       x = "") + 
  scale_color_manual(values = c("grey","black")) +
  #scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14)) +
  labs(shape = "Competition") 

plot16


plot17 <- ggplot(height.17, aes(x = dates, y = avg,
                                shape = Competition,
                                group = spp_trt,
                                colour = Competition)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str), width = 0.7) +
  geom_line(color = "black") +
  geom_point(size = 5) + 
  facet_wrap("Species") +
  scale_x_date(date_labels = "%d-%b-%y",
               date_breaks = "2 week") +
  theme_classic() +
  labs(y = "Height (cm)",
       x = "") + 
  scale_color_manual(values = c("grey","black")) +
  #scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14)) +
  labs(shape = "Treatment") 

plot17



panel <- ggarrange(plot16, plot17,
          common.legend = TRUE,
          legend = "bottom",
          labels = "AUTO")

ggsave("Figures/Height_panel.JPEG", panel)


# Starting height ---------------------------------------------------------

heights.start <- heights %>% 
  filter(Month == "05")

unique(heights.start$dates)

heights.start <- heights.start %>% 
  filter(date != c("270517"))

colnames(heights.start)


heights.start <- heights.start %>% 
  separate(dates, c("Year", NA, NA), remove = FALSE) %>% 
  separate(dates, c(NA, "Month", NA), remove = FALSE) 




phy.height <- ggplot(heights.start, 
       aes(x = Year, y = height,
           shape = Competition,
           colour = Competition)) +
  geom_jitter(aes(shape = Competition, 
                  color = Competition), 
    position = position_jitterdodge(jitter.width = 0.15,
                                    dodge.width = 0.7),
    size = 4,
    alpha = 0.55) + 
  facet_wrap("Species") +
  theme_classic() +
  labs(y = "Height (cm)",
       x = "") + 
  scale_color_manual(values = c("#24908C", "#3A518B")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100, 120)) +
  stat_summary(aes(shape = Competition, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  theme(legend.position = c(0.6,0.07),
        legend.background = element_rect(linetype = 1, 
                                         size = 0.5, colour = 1))

phy.height 

ggsave("Figures/phytometer_starting_height.jpeg", phy.height)
