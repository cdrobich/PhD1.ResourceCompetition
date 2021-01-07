
########## GLMM wih pair and year as random ##########
library(tidyverse)
library(lme4)
library(MuMIn)

##### Resident Species ############
light.res.pair <- read.csv("Data/light_resident.csv")
light.res.pair$Year <- as.factor(light.res.pair$Year)
light.res.pair$Pair <- as.factor(light.res.pair$Pair)

ggplot(light.res.pair, aes(x=Incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


light.res.pair <- light.res.pair %>% mutate(log.inciden = log(Incident)) # much worse


light.res.pair <- light.res.pair %>% 
  unite("compyear", Competition:Year, remove = FALSE)

carex.light <- light.res.pair %>% filter(Phytometer == "Carex")
cala.light <- light.res.pair %>% filter(Phytometer == "Calamagrostis")
typha.light <- light.res.pair %>% filter(Phytometer == "Typha")



# Carex

test.carex <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = carex.light, REML = FALSE)
summary(test.carex)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: carex.light

#REML criterion at convergence: 426.9

#           Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.3936 -0.6833 -0.2722  0.7527  2.2935 

#              Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  59.1     7.688  
#Year     (Intercept) 129.2    11.367  
#Residual             746.3    27.319  
#Number of obs: 46, groups:  Pair, 23; Year, 2

#Fixed effects:
#              Estimate Std. Error t value
#(Intercept)      50.264      9.983   5.035
#Competitionyes   -8.522      8.056  -1.058

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.403

VarCorr(test.carex)

#Groups   Name        Std.Dev.
#Pair     (Intercept)  7.6877 
#Year     (Intercept) 11.3674 
#Residual             27.3187 

r.squaredGLMM(test.carex) # marginal and conditional r2 for model 1

#       R2m       R2c
#[1,] 0.01946985 0.2170353


# examine the residuals 
plot(test.carex)
qqnorm(resid(test.carex))
qqline(resid(test.carex))

## check out model 1 coefficients

coef(test.carex)

## Calamagrostis

test.cala <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = cala.light)
summary(test.cala)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: cala.light

#REML criterion at convergence: 411.8

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -1.9141 -0.6920 -0.1274  0.7652  2.1376 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  73.27    8.56   
#Year     (Intercept) 150.81   12.28   
#Residual             814.55   28.54   
#Number of obs: 44, groups:  Pair, 22; Year, 2

#Fixed effects:
#              Estimate Std. Error  t value
#(Intercept)      67.694     10.766   6.288
#Competitionyes  -26.636      8.605  -3.095

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.400

r.squaredGLMM(test.cala) 

#       R2m       R2c
#[1,] 0.148753 0.3324101



### Typha ####

test.typha <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = typha.light)
summary(test.typha)


#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: typha.light

#REML criterion at convergence: 190.4

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-2.59889 -0.44558 -0.01338  0.48103  1.43819 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  54.88    7.408  
#Year     (Intercept) 128.89   11.353  
#Residual             198.65   14.094  
#Number of obs: 24, groups:  Pair, 12; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
# (Intercept)      90.000      9.250   9.729
# Competitionyes  -14.750      5.754  -2.563

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.311

r.squaredGLMM(test.typha) 

#       R2m       R2c
#[1,] 0.1292334 0.5476748


######## Phragmites ###############


phrag.light.pair <- read.csv("Data/Light_phrag.csv")

phrag.light.pair$Year <- as.factor(phrag.light.pair$Year)


ggplot(phrag.light.pair, aes(x=Incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


phrag.light.pair <- phrag.light.pair %>% mutate(sqrt.incident = sqrt(Incident - 1),
                                                log.incident = log(Incident +1))

phrag.light.pair <- phrag.light.pair %>% 
  unite("compyear", Competition:Year, remove = FALSE)


p.carex.light <- phrag.light.pair %>% filter(Neighbour == "Carex")
p.cala.light <- phrag.light.pair %>% filter(Neighbour == "Calamagrostis")
p.typha.light <- phrag.light.pair %>% filter(Neighbour == "Typha")


## GLMM 

phrag.carex <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = p.carex.light)
summary(phrag.carex)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: p.carex.light

#REML criterion at convergence: 180.7

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -3.7303 -0.1215  0.2060  0.5334  0.9534 

#Random effects:
#Groups   Name        Variance  Std.Dev. 
#Pair     (Intercept) 2.239e-11 4.732e-06
#Year     (Intercept) 0.000e+00 0.000e+00
#Residual             3.858e+02 1.964e+01
#Number of obs: 22, groups:  Pair, 11; Year, 2

#Fixed effects:
#               Estimate Std. Error t value
#(Intercept)      90.636      5.923  15.304
#Competitionyes   -9.364      8.376  -1.118

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.707
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phrag.carex) 

#       R2m        R2c
#[1,] 0.05617228 0.05617228

phrag.cala <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = p.cala.light)
summary(phrag.cala)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: p.cala.light

#REML criterion at convergence: 184.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.8237 -0.1080  0.2077  0.6006  1.0100 

#Random effects:
#Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  51.06    7.146  
#Year     (Intercept)  10.92    3.305  
#Residual             414.95   20.370  
#Number of obs: 22, groups:  Pair, 11; Year, 2

#Fixed effects:
#Estimate Std. Error t value
#(Intercept)      91.544      6.918  13.232
#Competitionyes  -16.091      8.686  -1.853

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.628

r.squaredGLMM(phrag.cala)

#       R2m       R2c
#[1,] 0.1244844 0.2382694


# variance for mountain ranges is 339.7 - explain a lot of variation
# Take mountain range variance and divide by total variance

339.7/(339.7 + 223.8)  # ~60 %




phrag.typha <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = p.typha.light)
summary(phrag.typha)


#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: p.typha.light

#REML criterion at convergence: 116.4

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.0268 -0.2256  0.1934  0.4662  1.0824 

#Random effects:
#  Groups   Name        Variance Std.Dev.
# Pair     (Intercept) 13.79    3.713   
#Year     (Intercept) 10.28    3.206   
#Residual             16.78    4.096   
#Number of obs: 20, groups:  Pair, 10; Year, 2

#Fixed effects:
#Estimate Std. Error t value
#(Intercept)      94.023      2.874  32.718
#Competitionyes   -1.000      1.832  -0.546

#Correlation of Fixed Effects:
#(Intr)
#Competitnys -0.319


r.squaredGLMM(phrag.typha)

#        R2m       R2c
#[1,] 0.006402295 0.5918182



light <- light %>% #rename the factors
  mutate(Competition = fct_recode(Competition,
                                  "No" = "no",
                                  "Yes" = "yes"))

light %>% 
  group_by(Phytometer, Competition) %>% 
  mutate(mean = mean(Incident),
         median = median(Incident)) -> light

colnames(phrag.light.pair)

(phrag.light <- ggplot(phrag.light.pair, aes(x = Incident, fill = as.factor(Competition))) +
  geom_density(alpha = 0.6, size = 1) +
  facet_wrap("Neighbour") + 
  scale_fill_manual(values = c("#fee08b", "black")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = expression(paste("Photosynthetically Active Radiation"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")")),
       y = "Density") +
  xlim(0, 150) +
  labs(fill = "Competition") +
  theme(legend.position = c(0.75, 0.9)) +
  #geom_vline(aes(xintercept = mean,
        #         colour = Competition),
         #    size = 1,
          #   show.legend = FALSE) +
  scale_colour_manual(values = c("#fee08b", "black"))) 

ggsave("Figures/Phragmites_neighbours_lightdensity.TIFF", phrag.light,
       dpi = 300)
