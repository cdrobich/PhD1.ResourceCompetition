
library(tidyverse)
library(ggpubr)

weights16 <- read.csv("Data/weights_2016.csv")
weights16


weights16 <- weights16 %>% 
  unite("spp_n", Species:Neighbour, remove = FALSE)

weights16 <- rename(weights16, Competition = Treatment) # rename "treatment" to competition
weights16 <- rename(weights16, Neighbours = Neighbour)

weights16 %>% group_by(spp_n, Competition) %>% 
  summarise(Total.avg = mean(Total, na.rm = TRUE),
            Total.sd = sd(Total, na.rm = TRUE),
            n = n())

#spp_n                     Competition Total.avg Total.sd     n
#1 Calamagrostis_Phragmites No            2.97     3.67     10
#2 Calamagrostis_Phragmites Yes           1.73     0.856    10
#3 Carex_Phragmites         No            1.38     0.717     5
#4 Carex_Phragmites         Yes           0.764    0.322     5
#5 Phragmites_Calamagrostis No           24.7     11.4       6
#6 Phragmites_Calamagrostis Yes          24.0      9.56      6
#7 Phragmites_Carex         No           17.8      8.48      5
#8 Phragmites_Carex         Yes          17.7     11.5       5
#9 Phragmites_Typha         No           27.5     10.3       5
#10 Phragmites_Typha         Yes          26.5      8.12      5
#11 Typha_Phragmites         No           40.8      9.00      6
#12 Typha_Phragmites         Yes          32.9     13.7       6


weight2016 <- ggplot(weights16, aes(x = spp_n, y = Total, shape = Species, colour = Species)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))

weight2016


# only resident species, with or without competition

unique(weights16$Species)

res16 <- weights16 %>% 
  filter(Species %in% c("Carex", "Calamagrostis","Typha"))
  
resident_2016 <- ggplot(res16, aes(x = Species, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  ylim(0, 60) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4"))

resident_2016

# only phragmites and pals

unique(weights16$Neighbour)

phragmites16 <- weights16 %>% 
  filter(Species %in% c("Phragmites"))

phrag_2016 <- ggplot(phragmites16, aes(x = Neighbours, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  ylim(0, 60) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#b2182b", "#ef8a62"))

phrag_2016


### 2017 weights ####

weights17 <- read.csv("Data/weights_2017.csv")
weights17


weights17 <- weights17 %>% 
  unite("spp_n", Species:Neighbours, remove = FALSE)



weights17 %>% group_by(spp_n, Competition) %>% 
  summarise(Total.avg = mean(Total, na.rm = TRUE),
            Total.sd = sd(Total, na.rm = TRUE),
            n = n())

#spp_n                    Competition  Total.avg  Total.sd     n

#1 Calamagrostis_Phragmites No               1.64    0.635    12
#2 Calamagrostis_Phragmites Yes              1.24    0.595    12
#3 Carex_Phragmites         No               2.56    0.769    12
#4 Carex_Phragmites         Yes              2.51    1.13     12
#5 Phragmites_Calamagrostis No              27.1    20.3       4
#6 Phragmites_Calamagrostis Yes             33.4    22.7       4
#7 Phragmites_Carex         No              15.4     3.90      4
#8 Phragmites_Carex         Yes             12.2     6.64      4
#9 Phragmites_Typha         No              17.0    10.3      10
#10 Phragmites_Typha         Yes             16.1     7.48     10
#11 Typha_Phragmites         No              36.8     7.71      6
#12 Typha_Phragmites         Yes             39.6    11.3       6


weight2017 <- ggplot(weights17, aes(x = spp_n, y = Total, shape = Species, colour = Species)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))

weight2017

## resident species 

res17 <- weights17 %>% 
  filter(Species %in% c("Carex", "Calamagrostis","Typha"))


resident_2017 <- ggplot(res17, aes(x = Species, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4"))

resident_2017

# resident species both years
resident <- ggarrange(resident_2016, resident_2017,
          labels = c("Resident Species in 2016","Resident Species in 2017"),
          hjust = (-0.3),
          common.legend = TRUE,
          legend = "bottom")

# phragmites


phragmites17 <- weights17 %>% 
  filter(Species %in% c("Phragmites"))

phrag_2017 <- ggplot(phragmites17, aes(x = Neighbours, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#b2182b", "#ef8a62"))

phrag_2017

# phragmites both years
phragmites <- ggarrange(phrag_2016, phrag_2017,
          labels = c("Phragmites with resident competition in 2016","Phragmites with resident competition in 2017"),
          hjust = (-0.2),
          common.legend = TRUE,
          legend = "bottom")

total.biomass.1617 <- ggarrange(resident, phragmites,
          nrow = 2)

ggsave("Figures/Total_Biomass_2016_2017.jpeg", total.biomass.1617)

############## put it together ##########3

bothyears <- rbind(weights16, weights17)


## resident species 
unique(bothyears$Species)

res.both <- bothyears %>% 
  filter(Species %in% c("Carex", "Calamagrostis","Typha"))


resident.bothyears <- ggplot(res.both, aes(x = Species, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4"))

resident.bothyears


# just phragmites 

phrag.both <- bothyears %>% 
  filter(Species %in% c("Phragmites"))

phrag.bothyears <- ggplot(phrag.both, aes(x = Neighbours, y = Total, shape = Competition, colour = Competition)) +
  geom_point(position = position_dodge(0.6), size = 2) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total Biomass (g)") +
  scale_colour_manual(values = c("#b2182b", "#ef8a62"))

phrag.bothyears

ggarrange(resident.bothyears, phrag.bothyears,
          legend = "bottom")
