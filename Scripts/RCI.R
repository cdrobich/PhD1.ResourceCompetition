
library(tidyverse)
library(ggpubr)

RCI.res <- read.csv("Data/RCI_residents.csv")
str(RCI.res) 
RCI.res$Year <- as.factor(RCI.res$Year)

sum <- RCI.res %>% 
  group_by(Phytometer, Year) %>% 
  summarise(average = mean(RCI),
            std = sd(RCI),
            N = length(RCI),
            SE = (std/(sqrt(N))))

# A tibble: 6 x 6
# Groups:   Phytometer [3]
#Phytometer      Year   average   std     N     SE
#1 Calamagrostis 2016  -1.37    3.50     10  1.11  
#2 Calamagrostis 2017   0.00591 0.862    11  0.260 
#3 Carex         2016   0.365   0.244     5  0.109 
#4 Carex         2017   0.0611  0.244    11  0.0735
#5 Typha         2016   0.188   0.383     6  0.156 
#6 Typha         2017  -0.192   0.699     6  0.285 


##### Resident species ##############

RCI.plot <- ggplot(sum, aes(x = Phytometer, y = average, colour = Year, shape = Year)) + 
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

##### Resident species ##############

ph.sum <- ph.sum %>% 
  mutate(Year = fct_relevel(Year, "2017", "2016"))

RCI.plot2 <- ggplot(ph.sum, aes(x = Phytometer, y = average, colour = Year, shape = Year)) + 
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

ggsave("Figures/RCI_phragmites.jpeg")



RCI.panel <- ggarrange(RCI.plot, RCI.plot2,
                       ncol = 2, common.legend = TRUE,
                       legend = "bottom",
                       labels = "AUTO",
                       hjust = c(-13, -20), vjust = 1.75)

ggsave("Figures/RCI_panel.TIFF", RCI.panel,
       dpi = 150, units = "in")



summed <- full_join(sum, ph.sum)

summmed <- summed %>% 
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

# Phytometer               average   std     N    SE
#1 Phragmites_Calamagrostis  -0.223 0.698    10 0.221
#2 Phragmites_Carex          -0.329 1.57      9 0.523
#3 Phragmites_Typha          -0.233 0.566    12 0.163


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




summed.yr <- full_join(sum.res, sum.phrag)

summed.yr <- summed.yr %>% 
  mutate(Phytometer = fct_relevel(Phytometer, 
                                  "Typha", 
                                  "Carex", 
                                  "Calamagrostis",
                                  "Phragmites_Typha", 
                                  "Phragmites_Carex", 
                                  "Phragmites_Calamagrostis"))


RCI.plot.all.yr <- ggplot(summed.yr, aes(x = Phytometer, y = average)) + 
  geom_point(position = position_dodge(0.6),
             size = 5) +
  geom_errorbar(aes(ymin = average - SE, ymax = average + SE),
                width = 0.3, position = position_dodge(0.6),
                color = "black",
                size = 1) +
  theme_classic() +
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

ggsave("Figures/RCI_panel_combinedyears.JPEG", RCI.plot.all.yr)
