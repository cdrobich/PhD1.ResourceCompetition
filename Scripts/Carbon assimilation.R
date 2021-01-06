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
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
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
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16)) +
  ylim(-5, 30) +
  scale_x_continuous(breaks=c(0, 200, 500, 1000, 1500)) +
  theme(legend.position = c(0.9, 0.2))

plot.phr


(carbon.assim.panel <- ggarrange(plot.res, plot.phr,
          nrow = 2))

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

(carbon.1500 <- ggarrange(res1500, phrag1500,
          nrow = 2))


ggsave("Figures/Carbon Assimilation 1500 panel.JPEG", carbon.1500)


(all.panel <- ggarrange(carbon.assim.panel, carbon.1500,
          labels = "AUTO"))


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

#REML criterion at convergence: 143.9

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.61492 -0.68856 -0.07908  0.44919  2.43037 

# Random effects:
# Groups   Name        Variance Std.Dev.
# Pair     (Intercept)  1.801   1.342   
# Year     (Intercept) 11.151   3.339   
# Residual              9.445   3.073   
# Number of obs: 28, groups:  Pair, 11; Year, 2

#Fixed effects:
#                        Estimate  Std. Error  t value
#(Intercept)                8.502      2.539   3.349
#Treatment No competition    5.008      1.206   4.153

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.208


r.squaredGLMM(carex1500) # marginal and conditional r2 for model 1

#      R2m       R2c
#[1,] 0.2240789 0.6727968


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

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.8456 -0.6725  0.1624  0.6967  1.7385 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept) 0.2407   0.4907  
#Year     (Intercept) 0.3626   0.6022  
#Residual             7.4879   2.7364  
#Number of obs: 45, groups:  Pair, 12; Year, 2

#Fixed effects:
#                          Estimate  Std. Error  t value
#(Intercept)                8.665       0.717    12.084
# TreatmentNo competition    4.484      0.820    5.469

# Correlation of Fixed Effects:
#  (Intr)
# TrtmntNcmpt -0.532

r.squaredGLMM(cala1500) # marginal and conditional r2 for model 1

#          R2m      R2c
#[1,] 0.387503 0.433176


# examine the residuals 
plot(cala1500)
qqnorm(resid(cala1500))
qqline(resid(cala1500))

## check out model 1 coefficients

coef(cala1500)



###  Typha 1500 GLMM ####

typ1500 <- lmer(carbon ~ Treatment + (1|Pair) + (1|Year), data = typha.A, REML = TRUE)
summary(typ1500)


#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: typha.A

#REML criterion at convergence: 139.2

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7388 -0.4282  0.0635  0.7221  1.4427 

#Random effects:
#  Groups   Name        Variance  Std.Dev. 
#Pair     (Intercept) 0.000e+00 0.000e+00
#Year     (Intercept) 3.192e-14 1.787e-07
#Residual             2.619e+01 5.118e+00
#Number of obs: 24, groups:  Pair, 6; Year, 2

#Fixed effects:
#                      Estimate Std. Error t value
#(Intercept)               17.633      1.477  11.936
#TreatmentNo competition    3.083      2.089   1.476

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.707
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular


r.squaredGLMM(typ1500) # marginal and conditional r2 for model 1

#         R2m        R2c
#[1,] 0.08649794 0.08649794

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
                    data = carex.phr, REML = TRUE)
summary(phr.car1500)


#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: carex.phr

#REML criterion at convergence: 119.4

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.7281 -0.6296  0.4076  0.6574  1.2145 

#Random effects:

#Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  0.4923  0.7016  
#Year     (Intercept)  0.2621  0.5120  
#Residual             13.1017  3.6196  
#Number of obs: 23, groups:  Pair, 6; Year, 2

#Fixed effects:
#                        Estimate  Std. Error  t value
#(Intercept)               18.275      1.142   15.998
#TreatmentNo competition    3.164      1.512   2.092

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.632

r.squaredGLMM(phr.car1500) # marginal and conditional r2 for model 1

#           R2m       R2c
#[1,] 0.1585793 0.2043916


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

#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: cala.phr

#REML criterion at convergence: 121.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.1434 -0.8113 -0.2591  0.3306  2.2935 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  0.3622  0.6018  
#Year     (Intercept)  0.0000  0.0000  
#Residual             11.4025  3.3768  
#Number of obs: 24, groups:  Pair, 6; Year, 2

# Fixed effects:
#                       Estimate  Std. Error  t value
#(Intercept)              18.2708     1.0053  18.175
#TreatmentNo competition   0.5083     1.3786   0.369

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.686
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phr.cal1500) # marginal and conditional r2 for model 1

#            R2m        R2c
#[1,] 0.005697145 0.03631021


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

#Linear mixed model fit by REML ['lmerMod']
#Formula: carbon ~ Treatment + (1 | Pair) + (1 | Year)
#Data: typha.phr

#REML criterion at convergence: 126.2

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7932 -0.3898  0.2746  0.5476  1.1180 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  5.904   2.430   
#Year     (Intercept)  1.088   1.043   
#Residual             20.760   4.556   
#Number of obs: 22, groups:  Pair, 6; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)               19.875      1.805  11.011
#TreatmentNo competition    1.237      1.966   0.629

#Correlation of Fixed Effects:
#  (Intr)
#TrtmntNcmpt -0.488

r.squaredGLMM(phr.typ1500) # marginal and conditional r2 for model 1

#          R2m       R2c
#[1,] 0.01412186 0.2625179


# examine the residuals 
plot(phr.typ1500)
qqnorm(resid(phr.typ1500))
qqline(resid(phr.typ1500))

## check out model 1 coefficients

coef(phr.typ1500)

