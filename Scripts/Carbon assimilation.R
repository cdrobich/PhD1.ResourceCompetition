library(tidyverse)
library(ggpubr)

ciras.17 <- read.csv("Data/CIRAS_carbon_WUE_2017.csv")

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

(sum <- ciras %>% group_by(spp_trt, light) %>% 
  summarise(avg = mean(carbon),
            sd = sd(carbon),
            N = length(carbon),
            str = (sd/(sqrt(N)))))

write.csv(sum, "Data/CIRAS_summary.csv")


ciras.sum <- read.csv("Data/CIRAS_summary.csv")
colnames(ciras.sum)

ciras.sum <- ciras.sum  %>% 
  separate(spp_trt, c("Phytometer", NA, NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, "Neighbour", NA), remove = FALSE) %>% 
  separate(spp_trt, c(NA, NA, "Treatment"), remove = FALSE)



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


(phr.ciras <- ggplot(ciras.phrag, aes(x = light, y = avg,
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
  theme(legend.position = c(0.75, 0.2)))



ggsave("Figures/phrag_CIRAS.TIFF", phr.ciras,
       width = 14, height = 6.8, units = "in",
       dpi = 300)


target <- c("Carex", "Calamagrostis", "Typha")

ciras.res <- ciras.sum %>% filter(Phytometer %in% target)


ciras.res <- ciras.res %>% 
    unite("phy_trt", Phytometer, Treatment, remove = FALSE)

(res.ciras <- ggplot(ciras.res, aes(x = light, y = avg,
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
  theme(legend.position = c(0.75, 0.2))) 
  


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
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  ylim(-2, 30) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = "none")

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
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size=12)) +
  ylim(-2, 30) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = c(0.9, 0.2))

plot.phr


(carbon.assim.panel <- ggarrange(plot.res, plot.phr,
          nrow = 2,
          labels = c("A","B")))

ggsave("Figures/Carbon_facet.jpeg", carbon.assim.panel)


######## Comparison at 1500 ########

light1500 <- ciras %>% filter(light == "1500")

write.csv(light1500,"Data/1500.carbonassimilation.csv")

target <- c("Calamagrostis", "Carex", "Typha")

light1500.res <- light1500 %>% filter(Species %in% target)

colnames(light1500.res)


sum.1500 <- light1500.res %>% 
  group_by(Species, Treatment) %>% 
  summarise(median = median(carbon),
            mean = mean(carbon),
            C.sd = sd(carbon),
            N = length(carbon),
            sterr = (C.sd/sqrt(N)))


light1500.res %>% 
  group_by(Species, Treatment) %>% 
  mutate(Cmean = mean(carbon),
         Cmedian = median(carbon)) -> light1500.res

# change order
light1500.res$Treatment <- as.factor(light1500.res$Treatment)

light1500.res$Treatment<- factor(light1500.res$Treatment, levels = c("No competition",
                                                                     "Competition"))


res1500 <- ggplot(light1500.res, aes(x = Treatment,
                                     y = carbon)) +
  geom_jitter(aes(colour = Treatment,
                  shape = Treatment),
              size = 2,
              width = 0.08) +
  facet_wrap("Species") + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = expression(paste
                      ("Carbon Assimilation"," ", " (", "umol CO"[2],
                        s^-1, " ", m^-2, sep=")"))) +
  ylim(0, 40) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("#762a83", "#1b7837")) +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1) 

res1500




light1500.phrag <- light1500 %>% filter(Species == "Phragmites")

light1500.phrag %>% 
  group_by(Neighbour, Treatment) %>% 
  mutate(Cmean = mean(carbon),
         Cmedian = median(carbon)) -> light1500.phrag


light1500.phrag$Treatment <- as.factor(light1500.phrag$Treatment)

light1500.phrag$Treatment<- factor(light1500.phrag$Treatment, 
                                   levels = c("No competition",
                                              "Competition"))



phrag1500 <- ggplot(light1500.phrag, aes(x = Treatment,
                                         y = carbon)) +
  geom_jitter(aes(colour = Treatment,
                  shape = Treatment),
              size = 2,
              width = 0.08) +
  facet_wrap("Phytometer") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Carbon Assimilation"," ", " (", "umol CO"[2], "umol  ",  s^-1, " ", m^-2, sep=")")),
       x = " ") +
  ylim(0, 40) +
  scale_colour_manual(values = c("#762a83", "#1b7837")) +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1) +
  theme(legend.position = c(0.9, 0.85))

phrag1500

(carbon.1500 <- ggarrange(res1500, phrag1500,
          nrow = 2,
          labels = c("C","D")))


ggsave("Figures/Carbon Assimilation 1500 panel.JPEG", carbon.1500)


(all.panel <- ggarrange(carbon.assim.panel, carbon.1500))


ggsave("Figures/All_Carbon_Assimilation.JPEG", all.panel)


########## GLMM wih pair and year as random ##########

library(lme4)
library(MuMIn)

##### Resident Species ############
light.1500 <- read.csv("Data/1500.carbonassimilation.pair.csv")
light.1500$Year <- as.factor(light.1500$Year)


ggplot(light.1500, aes(x=carbon)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white") # this actually looks good

carex.A <- light.1500 %>% filter(Species == "Carex")
cala.A <- light.1500 %>% filter(Species == "Calamagrostis")
typha.A <- light.1500 %>% filter(Species == "Typha")

colnames(light.1500)

#### Carex GLMM ####

carex1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), data = carex.A, REML = TRUE)
summary(carex1500)

#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: carex.A

# REML criterion at convergence: 142.3

# Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
# -1.17382 -0.60067 -0.03122  0.51606  2.25761 

# Random effects:
# Groups   Name        Variance Std.Dev.
# Pair     (Intercept)  5.449   2.334   
# Year     (Intercept) 12.338   3.513   
# Residual              5.915   2.432   
# Number of obs: 28, groups:  Pair, 18; Year, 2

# Fixed effects:
# Estimate Std. Error t value
# (Intercept)                8.557      2.634   3.249
# TreatmentNo competition    4.567      0.995   4.589

# Correlation of Fixed Effects:
#  (Intr)
# TrtmntNcmpt -0.164


r.squaredGLMM(carex1500) # marginal and conditional r2 for model 1

#         R2m       R2c
#[1,] 0.184963 0.7966059


# examine the residuals 
plot(carex1500)
qqnorm(resid(carex1500))
qqline(resid(carex1500))

## check out model 1 coefficients

coef(carex1500)



###  Cala 1500 GLMM ####

cala1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), data = cala.A, REML = TRUE)
summary(cala1500)

#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: cala.A

#REML criterion at convergence: 216.8

# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.8601 -0.6941  0.1529  0.7756  1.7835 

# Random effects:
#  Groups   Name        Variance Std.Dev.
# Pair     (Intercept) 0.0000   0.0000  
# Year     (Intercept) 0.3691   0.6075  
# Residual             7.7177   2.7781  
# Number of obs: 45, groups:  Pair, 24; Year, 2

# Fixed effects:
# Estimate Std. Error t value
# (Intercept)               8.6667     0.7114  12.182
# TreatmentNo competition   4.4702     0.8312   5.378

# Correlation of Fixed Effects:
#  (Intr)
# TrtmntNcmpt -0.544
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular
# r.squaredGLMM(cala1500) # marginal and conditional r2 for model 1

r.squaredGLMM(cala1500)

#           R2m       R2c
# [1,] 0.3861236 0.4141413
 

# examine the residuals 
plot(cala1500)
qqnorm(resid(cala1500))
qqline(resid(cala1500))

## check out model 1 coefficients

coef(cala1500)


###  Typha 1500 GLMM ####

typ1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), data = typha.A, REML = TRUE)
summary(typ1500)

# Linear mixed model fit by REML ['lmerMod']
# Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
# Data: typha.A

# REML criterion at convergence: 130.4

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.4838 -0.6877  0.0482  0.4930  1.1329 

#Random effects:
#  Groups   Name        Variance  Std.Dev. 
# Pair     (Intercept) 1.948e+01 4.4134441
# Year     (Intercept) 2.642e-09 0.0000514
# Residual             6.713e+00 2.5910391
# Number of obs: 24, groups:  Pair, 12; Year, 2

# Fixed effects:
#  Estimate Std. Error t value
# (Intercept)               17.633      1.477  11.936
# TreatmentNo competition    3.083      1.058   2.915

# Correlation of Fixed Effects:
#  (Intr)
# TrtmntNcmpt -0.358
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular


r.squaredGLMM(typ1500) # marginal and conditional r2 for model 1

#         R2m       R2c
#[1,] 0.08649793 0.7658526

# examine the residuals 
plot(typ1500)
qqnorm(resid(typ1500))
qqline(resid(typ1500))

## check out model 1 coefficients

coef(typ1500)

######## Phragmites phytometers #######
colnames(light.1500)

phragmites.phyt <- light.1500 %>% filter(Species == "Phragmites")

phragmites.phyt <- phragmites.phyt %>% 
  unite("neigh_trt", Neighbour, Treatment, remove = FALSE)


carex.phr <- phragmites.phyt %>% filter(Neighbour == "Carex")
cala.phr <- phragmites.phyt %>% filter(Neighbour == "Calamagrostis")
typha.phr <- phragmites.phyt %>% filter(Neighbour == "Typha")


###  Phrag and Carex ####

phr.car1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), 
                    data = carex.phr)

summary(phr.car1500)


#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: carex.phr

#REML criterion at convergence: 119.4

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.7931 -0.6715  0.4773  0.6540  1.2470 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  0.0000  0.0000  
#Year     (Intercept)  0.2239  0.4732  
#Residual             13.5724  3.6841  
#Number of obs: 23, groups:  Pair, 12; Year, 2

#Fixed effects:
#                         Estimate Std. Error t value
#(Intercept)               18.275      1.115  16.392
#TreatmentNo competition    3.164      1.538   2.057

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.660
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phr.car1500) # marginal and conditional r2 for model 1

#           R2m       R2c
#[1,] 0.1591981 0.1728443


# examine the residuals 
plot(phr.car1500)
qqnorm(resid(phr.car1500))
qqline(resid(phr.car1500))

## check out model 1 coefficients

coef(phr.car1500)


###  Phrag and Cala ####

phr.cal1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), 
                    data = cala.phr, REML = TRUE)

summary(phr.cal1500)

#REML criterion at convergence: 121.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.1885 -0.7396 -0.2993  0.3218  2.3442 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  0.00    0.000   
#Year     (Intercept)  0.00    0.000   
#Residual             11.73    3.425   
#Number of obs: 24, groups:  Pair, 12; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)              18.2708     0.9888  18.478
#TreatmentNo competition   0.5083     1.3983   0.364

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.707
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phr.cal1500) # marginal and conditional r2 for model 1

#            R2m        R2c
#[1,] 0.005713046 0.005713046


# examine the residuals 
plot(phr.cal1500)
qqnorm(resid(phr.cal1500))
qqline(resid(phr.cal1500))

## check out model 1 coefficients

coef(phr.cal1500)


###  Phrag and Typha ####

phr.typ1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), 
                    data = typha.phr, REML = TRUE)

summary(phr.typ1500)

#REML criterion at convergence: 126.2

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.6999 -0.4148  0.2861  0.5720  1.1562 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  7.9017  2.8110  
#Year     (Intercept)  0.7526  0.8675  
#Residual             18.3074  4.2787  
#Number of obs: 22, groups:  Pair, 12; Year, 2

#Fixed effects:
#                      Estimate Std. Error t value
#(Intercept)               19.875      1.600  12.421
#TreatmentNo competition    1.555      1.859   0.837

# Correlation of Fixed Effects:
#  (Intr)
# TrtmntNcmpt -0.513

r.squaredGLMM(phr.typ1500) # marginal and conditional r2 for model 1

#          R2m       R2c
#[1,] 0.02276981 0.3364449


# examine the residuals 
plot(phr.typ1500)
qqnorm(resid(phr.typ1500))
qqline(resid(phr.typ1500))

## check out model 1 coefficients

coef(phr.typ1500)

