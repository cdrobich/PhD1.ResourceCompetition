
# Load packages -----------------------------------------------------------

library(car)
library(agricolae)
library(tidyverse)


# Load data ---------------------------------------------------------------

light.1500 <- read.csv("Data/1500.carbonassimilation.csv")

light.1500$Year <- as.factor(light.1500$Year)

ggplot(light.1500, aes(x=carbon)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


light.1500.res <- light.1500 %>% 
  filter(Species %in% c("Calamagrostis", "Carex", "Typha"))


test <- lm(carbon ~ Species * Treatment, data = light.1500)
Anova(test, type = 3)

#Response: carbon
#                   Sum Sq  Df  F value    Pr(>F)    
#(Intercept)       1802.67   1 114.7024 < 2.2e-16 ***
#Species           2023.36   3  42.9149 < 2.2e-16 ***
#Treatment          228.24   1  14.5229  0.000198 ***
#Species:Treatment  106.34   3   2.2555  0.084065 .  
#Residuals         2483.13 158 


spp <- HSD.test(test, "Species")

#                carbon      std  r Min  Max   Q25  Q50    Q75
#Calamagrostis 10.77333 3.593960 45 5.0 18.4  8.10 10.8 13.700
#Carex         11.79643 4.871191 28 0.7 24.9  9.95 12.6 14.125
#Phragmites    19.57681 4.196651 69 5.7 28.7 16.40 19.6 23.100
#Typha         19.17500 5.247215 24 6.7 28.1 16.55 19.8 22.375

#carbon groups
#Phragmites    19.57681      a
#Typha         19.17500      a
#Carex         11.79643      b
#Calamagrostis 10.77333      b

trt <- HSD.test(test, "Treatment")

#                 carbon      std  r Min  Max   Q25  Q50   Q75
#Competition    14.18218 6.024162 87 0.7 26.3  9.45 13.8 19.20
#No competition 17.62342 5.397007 79 5.5 28.7 13.95 16.5 22.35

#    carbon groups
#No competition 17.62342      a
#Competition    14.18218      b

light.1500 %>% group_by(Species) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))

#  Species       C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Calamagrostis  10.8  3.59    45 0.536    2.05  0.443  0.0661
#2 Carex          11.8  4.87    28 0.921    2.12  0.646  0.122 
#3 Phragmites     19.6  4.20    69 0.505    3.04  0.447  0.0538
#4 Typha          19.2  5.25    24 1.07     2.38  0.554  0.113

light.1500 %>% group_by(Species, Treatment) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))

#  Species       Treatment      C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Calamagrostis Competition     8.67  2.49    24 0.508    1.92  0.438  0.0895
#2 Calamagrostis No competition 13.2   3.14    21 0.686    2.20  0.410  0.0895
#3 Carex         Competition     9.15  4.43    15 1.14     1.85  0.730  0.188 
#4 Carex         No competition 14.9   3.41    13 0.945    2.44  0.340  0.0943
#5 Phragmites    Competition    18.8   3.65    36 0.608    2.94  0.420  0.0700
#6 Phragmites    No competition 20.4   4.63    33 0.807    3.14  0.456  0.0793
#7 Typha         Competition    17.6   4.42    12 1.28     2.35  0.409  0.118 
#8 Typha         No competition 20.7   5.73    12 1.65     2.42  0.688  0.199 


## Resident species Carbon

light.1500.res %>% group_by(Species, Treatment) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))


#  Species       Treatment      C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Calamagrostis Competition     8.67  2.49    24 0.508    1.92  0.438  0.0895
#2 Calamagrostis No competition 13.2   3.14    21 0.686    2.20  0.410  0.0895
#3 Carex         Competition     9.15  4.43    15 1.14     1.85  0.730  0.188 
#4 Carex         No competition 14.9   3.41    13 0.945    2.44  0.340  0.0943
#5 Typha         Competition    17.6   4.42    12 1.28     2.35  0.409  0.118 
#6 Typha         No competition 20.7   5.73    12 1.65     2.42  0.688  0.199 

light.1500.res %>% group_by(Treatment) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))

#Treatment        C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Competition     10.9  5.18    51 0.725    2.00  0.560  0.0784
#2 No competition  15.6  5.04    46 0.743    2.32  0.484  0.0714


light.1500.res %>% group_by(Species) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))

#  Species       C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Calamagrostis  10.8  3.59    45 0.536    2.05  0.443  0.0661
#2 Carex          11.8  4.87    28 0.921    2.12  0.646  0.122 
#3 Typha          19.2  5.25    24 1.07     2.38  0.554  0.113 

light.1500.phrag %>% group_by(Neighbour, Treatment) %>% 
  summarise(C.avg = mean(carbon),
            C.sd = sd(carbon),
            C.n = length(carbon),
            C.str = (C.sd/sqrt(C.n)),
            wue.avg = mean(WUE),
            wue.sd = sd(WUE),
            wue.str = wue.sd/sqrt(C.n))

#  Neighbour     Treatment      C.avg  C.sd   C.n C.str wue.avg wue.sd wue.str
#1 Calamagrostis Competition     18.3  3.67    12 1.06     2.94  0.428  0.123 
#2 Calamagrostis No competition  18.8  3.16    12 0.911    3.11  0.538  0.155 
#3 Carex         Competition     18.3  3.57    12 1.03     2.82  0.272  0.0784
#4 Carex         No competition  21.4  3.84    11 1.16     3.02  0.332  0.100 
#5 Typha         Competition     19.9  3.78    12 1.09     3.05  0.525  0.152 
#6 Typha         No competition  21.3  6.49    10 2.05     3.32  0.457  0.144 



# Two-way ANOVAS ----------------------------------------------------------

# Resident Species, Carbon ------------------------------------------------

light.1500.test <- lm(carbon ~ Species * Treatment, data = light.1500.res)
Anova(light.1500.test, type = 3)

#Anova Table (Type III tests)

#Response: carbon
#                   Sum Sq Df  F value    Pr(>F)    
#(Intercept)       1802.67  1 123.3275 < 2.2e-16 ***
#Species            709.86  2  24.2820 3.541e-09 ***
#Treatment          228.24  1  15.6149 0.0001531 ***
#Species:Treatment   22.19  2   0.7591 0.4710075    
#Residuals         1330.14 91  


Anova(light.1500.test, type = 2)

#Anova Table (Type II tests)

#Response: carbon
#                   Sum Sq Df F value    Pr(>F)    
#Species           1131.94  2 38.7201 6.809e-13 ***
#Treatment          489.93  1 33.5181 9.945e-08 ***
#Species:Treatment   22.19  2  0.7591     0.471    
#Residuals         1330.14 91 

carbon.hsd.res <- HSD.test(light.1500.test, "Treatment")

#$means
#                 carbon      std  r Min  Max    Q25  Q50   Q75
#Competition    10.91765 5.179796 51 0.7 22.6  7.800 10.1 13.45
#No competition 15.61957 5.039295 46 5.5 28.1 13.425 14.6 17.30


#$groups
#carbon groups
#No competition 15.61957      a
#Competition    10.91765      b


carbon.hsd.res.spp <- HSD.test(light.1500.test, "Species")

#$means
#carbon      std  r Min  Max   Q25  Q50    Q75
#Calamagrostis 10.77333 3.593960 45 5.0 18.4  8.10 10.8 13.700
#Carex         11.79643 4.871191 28 0.7 24.9  9.95 12.6 14.125
#Typha         19.17500 5.247215 24 6.7 28.1 16.55 19.8 22.375

#$groups
#carbon groups
#Typha         19.17500      a
#Carex         11.79643      b
#Calamagrostis 10.77333      b

plot(light.1500.test)


# Resident species, WUE ---------------------------------------------------

ggplot(light.1500, aes(x=WUE)) + 
  geom_histogram(binwidth = 0.5,
                 color="black", fill="white")


light.1500.WUE <- lm(WUE ~ Species * Treatment, data = light.1500.res)
Anova(light.1500.WUE, type = 3)

#Anova Table (Type III tests)

#Response: WUE
#                  Sum Sq Df  F value  Pr(>F)    
#(Intercept)       88.512  1 340.2048 < 2e-16 ***
#Species            1.939  2   3.7256 0.02785 *  
#Treatment          0.864  1   3.3193 0.07176 .  
#Species:Treatment  0.904  2   1.7372 0.18180    
#Residuals         23.676 91 

Anova(light.1500.WUE, type = 2)

#Anova Table (Type II tests)

#Response: WUE
#                   Sum Sq Df F value   Pr(>F)   
#Species            1.6371  2  3.1462 0.047728 * 
#Treatment          2.3986  1  9.2194 0.003123 **
#Species:Treatment  0.9039  2  1.7372 0.181801   
#Residuals         23.6757 91 


res.hsd.wue.trt <- HSD.test(light.1500.WUE, "Treatment")

#$means
#WUE       std  r  Min  Max    Q25  Q50    Q75
#Competition    2.000196 0.5595551 51 0.26 3.04 1.6950 1.99 2.4350
#No competition 2.322609 0.4844627 46 1.27 3.83 2.0625 2.22 2.6025

#$groups
#      WUE groups
#No competition 2.322609      a
#Competition    2.000196      b

res.hsd.wue.spp <- HSD.test(light.1500.WUE, "Species")

#$means
#WUE       std  r  Min  Max    Q25   Q50   Q75
#Calamagrostis 2.050000 0.4434524 45 1.21 3.04 1.7900 2.030 2.410
#Carex         2.123214 0.6456926 28 0.26 3.05 2.0400 2.225 2.520
#Typha         2.381250 0.5543842 24 1.47 3.83 2.0075 2.195 2.725


#$groups
#WUE groups
#Typha         2.381250      a
#Carex         2.123214     ab
#Calamagrostis 2.050000      b

plot(light.1500.WUE)



# Phragmites, carbon assimilation -----------------------------------------

light.1500.phrag <- light.1500 %>% 
  filter(Species == "Phragmites")

ggplot(light.1500.phrag, aes(x = carbon)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")


# carbon use anovas

phrag.1500.test <- lm(carbon ~ Neighbour * Treatment, data = light.1500.phrag)
Anova(phrag.1500.test, type = 3)

#Anova Table (Type III tests)

#Response: carbon
#                    Sum Sq Df  F value Pr(>F)    
#(Intercept)         4005.9  1 233.2946 <2e-16 ***
#Neighbour             20.5  2   0.5979 0.5530    
#Treatment              1.6  1   0.0903 0.7648    
#Neighbour:Treatment   21.1  2   0.6150 0.5439    
#Residuals           1081.8 63  


Anova(phrag.1500.test, type = 2)

#Anova Table (Type II tests)

#Response: carbon
#                     Sum Sq Df F value  Pr(>F)  
#Neighbour             50.11  2  1.4591 0.24019  
#Treatment             48.07  1  2.7998 0.09924 .
#Neighbour:Treatment   21.12  2  0.6150 0.54388  
#Residuals           1081.77 63 

plot(phrag.1500.test)



# Phragmites, WUE ---------------------------------------------------------

phrag.1500.wue <- lm(WUE ~ Neighbour * Treatment, data = light.1500.phrag)
Anova(phrag.1500.wue, type = 3)

#Anova Table (Type III tests)

#Response: WUE
#                     Sum Sq Df  F value Pr(>F)    
#(Intercept)         103.664  1 543.1787 <2e-16 ***
#Neighbour             0.320  2   0.8379 0.4374    
#Treatment             0.184  1   0.9628 0.3302    
#Neighbour:Treatment   0.025  2   0.0644 0.9377    
#Residuals            12.023 63 


Anova(phrag.1500.wue, type = 2)

#Anova Table (Type II tests)

#Response: WUE
#                     Sum Sq Df F value  Pr(>F)  
#Neighbour            0.7734  2  2.0261 0.14035  
#Treatment            0.7702  1  4.0355 0.04884 *
#Neighbour:Treatment  0.0246  2  0.0644 0.93768  
#Residuals           12.0234 63


phrag.hsd.trt <- HSD.test(phrag.1500.wue, "Treatment")


#                    WUE       std  r  Min  Max    Q25  Q50    Q75
#Competition    2.936667 0.4201088 36 2.24 4.38 2.7175 2.86 3.0825
#No competition 3.143636 0.4556645 33 2.23 4.25 2.9100 3.12 3.3400


#                    WUE groups
#No competition 3.143636      a
#Competition    2.936667      a


# Figures -----------------------------------------------------------------

ciras.sum <- read.csv("Data/CIRAS_summary.csv")

ciras.sum <- ciras.sum  %>% 
  separate(spp_trt, c("Phytometer", NA, NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Neighbour", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, NA, "Treatment"), remove = FALSE)

ciras.sum <- ciras.sum %>% #rename the factors
  mutate(Treatment = fct_recode(Treatment,
                                  "No competition" = "No"))


ciras.res <- ciras.sum %>% 
  filter(Phytometer %in% c("Carex", "Calamagrostis","Typha"))

ciras.res <- ciras.res %>% 
  unite("phy_trt", Phytometer, Treatment, remove = FALSE)


ciras.phrag <- ciras.sum %>% 
  filter(Phytometer == "Phragmites")

ciras.phrag <- ciras.phrag %>% 
  unite("neigh_trt", Neighbour, Treatment, remove = FALSE)


ciras.phrag <- ciras.phrag %>% 
  unite("phy_trt", Phytometer, Neighbour, remove = FALSE)


ciras.phrag <- ciras.phrag  %>% 
  separate(neigh_trt, c(NA,"Treatment"), remove = FALSE) 


# Line plots --------------------------------------------------------------

# Resident species
plot.res <- ggplot(ciras.res, aes(x = light, y = avg,
                                 group = phy_trt,
                                 shape = Treatment,
                                 colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("Phytometer") +
  geom_point(alpha = 0.7,
             size = 3) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "\u00B5mol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = "") + 
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = "none")

plot.res


# Phragmites line plot

plot.phr <- ggplot(ciras.phrag, aes(x = light, y = avg,
                                    group = neigh_trt,
                                    shape = Treatment,
                                    colour = Treatment)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str)) +
  geom_line() +
  facet_wrap("phy_trt") +
  geom_point(alpha = 0.7,
             size = 3) +
  scale_colour_manual(values = c("#454ADE", "#440C53")) +
  theme_classic() +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "\u00B5mol CO"[2],  s^-1, " ", m^-2, sep=")")),
       x = expression(paste("Photosynthetically Active Radiation"," ", " (", "\u00B5mol  ",  s^-1, " ", m^-2, sep=")"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  guides(colour = "none") +
  theme(legend.position = c(0.9, 0.2))

plot.phr


(carbon.assim.panel <- ggarrange(plot.res, plot.phr,
                                 nrow = 2,
                                 labels = c("A","B")))


# Jitter plots ------------------------------------------------------------

# Resident Species, Carbon
colnames(light.1500.res)

carbon.res <- ggplot(light.1500.res, aes(x = Species, y = carbon)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 3,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = expression(paste("Carbon Assimilation"," ", " (", "\u00B5mol CO"[2],  s^-1, " ", m^-2, sep=")"))) +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))


wue.res <- ggplot(light.1500.res, aes(x = Species, y = WUE)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 3,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = expression(paste("Water Use Efficiency"," ", " (", "mmol CO"[2],  mol^-1, " ", "H"[2],"O)"))) +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(0, 5)

c.wue.res <- ggarrange(carbon.res, wue.res,
                       labels = c("C","D"))

# Phragmites, Carbon

colnames(light.1500.phrag)

light.1500.phrag1 <- light.1500.phrag %>% #rename the factors
  mutate(Phytometer = fct_recode(Phytometer,
                                "P_Calmagrostis" = "Phragmites_Calamagrostis",
                                "P_Carex" = "Phragmites_Carex",
                                "P_Typha" = "Phragmites_Typha"))


carbon.phrag <- ggplot(light.1500.phrag1, 
                       aes(x = Phytometer, y = carbon)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 3,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12, angle = -10, hjust = 0.5, vjust = -0.5),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = expression(paste("Carbon Assimilation"," ", " (", "\u00B5mol CO"[2],  s^-1, " ", m^-2, sep=")"))) +
  scale_colour_manual(values = c("#454ADE", "#440C53")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))


# Phragmites, WUE

wue.phrag <- ggplot(light.1500.phrag1,
                    aes(x = Phytometer, y = WUE)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 3,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12, angle = -10, hjust = 0.5, vjust = -0.5),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = expression(paste("Water Use Efficiency"," ", " (", "mmol CO"[2],  mol^-1, " ", "H"[2],"O)"))) +
  scale_colour_manual(values = c("#454ADE", "#440C53")) +
  theme(legend.position = c(0.8,0.2)) +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(0, 5)

c.wue.phrag <- ggarrange(carbon.phrag, wue.phrag,
                         labels = c("E","F"))


c.wue.panel <- ggarrange(c.wue.res, c.wue.phrag,
          nrow = 2)


carbon.wue.panel <- ggarrange(carbon.assim.panel, c.wue.panel)


ggsave("Figures/PI_C_WUE.jpeg", carbon.wue.panel,
       height = 9.24,
       width = 16.9,
       units = "in")

