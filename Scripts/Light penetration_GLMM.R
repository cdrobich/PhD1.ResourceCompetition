
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

test.carex <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = carex.light, REML = TRUE)
summary(test.carex)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: carex.light

#REML criterion at convergence: 426.9

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.3936 -0.6833 -0.2722  0.7527  2.2935 

#Random effects:
#Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  59.1     7.688  
#Year     (Intercept) 129.2    11.367  
#Residual             746.3    27.319  
#Number of obs: 46, groups:  Pair, 23; Year, 2

#Fixed effects:
#Estimate Std. Error t value
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

#REML criterion at convergence: 430.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.8933 -0.6931 -0.1603  0.8116  2.1346 

#Random effects:
#Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  66.54    8.157  
#Year     (Intercept) 191.66   13.844  
#Residual             801.92   28.318  
#Number of obs: 46, groups:  Pair, 24; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)      67.939     11.636   5.839
#Competitionyes  -25.064      8.379  -2.991

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.377

r.squaredGLMM(test.cala) 

#       R2m       R2c
#[1,] 0.1313019 0.3428793



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
#Groups   Name        Variance Std.Dev.
#Pair     (Intercept)  54.88    7.408  
#Year     (Intercept) 128.89   11.353  
#Residual             198.65   14.094  
#Number of obs: 24, groups:  Pair, 12; Year, 2

#Fixed effects:
#Estimate Std. Error t value
#(Intercept)      90.000      9.250   9.729
#Competitionyes  -14.750      5.754  -2.563

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.311

r.squaredGLMM(test.typha) 

#       R2m       R2c
#[1,] 0.1292334 0.547674


######## Phragmites ###############


phrag.light.pair <- read.csv("Data/Light_phrag.csv")

phrag.light.pair$Year <- as.factor(phrag.light.pair$Year)

colnames(phrag.light.pair)

phrag.light.pair %>% 
  group_by(Neighbour, Competition) %>% 
  summarise(Light.avg = mean(Incident),
            Light.sd = sd(Incident),
            N = length(Incident),
            st.err = (Light.sd)/(sqrt(N)))
  
#  Neighbour     Competition Light.avg Light.sd     N st.err
#1 Calamagrostis no               91.6     7.80    11   2.35
#2 Calamagrostis yes              77.3    29.0     12   8.37
#3 Carex         no               90.6     6.62    11   2.00
#4 Carex         yes              82.6    26.1     12   7.54
#5 Typha         no               94.4     5.89    10   1.86
#6 Typha         yes              93.7     5.63    12   1.63







ggplot(phrag.light.pair, aes(x=Incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


phrag.light.pair <- phrag.light.pair %>% mutate(sqrt.incident = sqrt(Incident - 1),
                                                log.incident = log(Incident +1))




ggplot(phrag.light.pair, aes(x=sqrt.incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")



ggplot(phrag.light.pair, aes(x=log.incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white") # going to go wtih raw


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

#REML criterion at convergence: 189.1

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.8348 -0.1098  0.2758  0.4842  0.8955 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept)   0.0     0.00   
#Year     (Intercept)   0.0     0.00   
#Residual             378.3    19.45   
#Number of obs: 23, groups:  Pair, 12; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)      90.636      5.864  15.456
#Competitionyes   -8.053      8.118  -0.992

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.722
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phrag.carex) 

#       R2m        R2c
#[1,] 0.04281042 0.04281042

phrag.cala <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = p.cala.light)
summary(phrag.cala)

#Linear mixed model fit by REML ['lmerMod']
#Formula: 
#  Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: p.cala.light

#REML criterion at convergence: 193.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.9248 -0.1254  0.2699  0.5916  0.9578 

#Random effects:
#  Groups   Name        Variance  Std.Dev. 
#Pair     (Intercept) 5.645e+01 7.513e+00
#Year     (Intercept) 7.207e-08 2.685e-04
#Residual             4.131e+02 2.032e+01
#Number of obs: 23, groups:  Pair, 12; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)      91.851      6.530  14.067
#Competitionyes  -14.518      8.506  -1.707

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.683
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

r.squaredGLMM(phrag.cala)

#       R2m       R2c
#[1,] 0.1048243 0.2124382


phrag.typha <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = p.typha.light)
summary(phrag.typha)

#Linear mixed model fit by REML ['lmerMod']
#Formula: 
#  Incident ~ Competition + (1 | Pair) + (1 | Year)
#Data: p.typha.light

#REML criterion at convergence: 128.1

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.0245 -0.1534  0.2065  0.4997  1.0239 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Pair     (Intercept) 13.295   3.646   
#Year     (Intercept)  5.092   2.257   
#Residual             16.225   4.028   
#Number of obs: 22, groups:  Pair, 12; Year, 2

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)     94.3872     2.3263  40.575
#Competitionyes  -0.7205     1.7634  -0.409

#Correlation of Fixed Effects:
#  (Intr)
#Competitnys -0.428


r.squaredGLMM(phrag.typha)

#        R2m       R2c
#[1,] 0.003880565 0.5330511

phrag.light.pair <- phrag.light.pair %>% #rename the factors
  mutate(Competition = fct_recode(Competition,
                                  "No" = "no",
                                  "Yes" = "yes"))

phrag.light.pair %>% 
  group_by(Neighbour, Competition) %>% 
  mutate(mean = mean(Incident),
         median = median(Incident)) -> phrag.light.pair

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
  theme(legend.position = c(0.75, 0.8)) +
  geom_vline(aes(xintercept = mean,
              colour = Competition),
           size = 1,
           show.legend = FALSE) +
  scale_colour_manual(values = c("#fee08b", "black"))) 

ggsave("Figures/Phragmites_neighbours_lightdensity.TIFF", phrag.light,
       dpi = 300)


