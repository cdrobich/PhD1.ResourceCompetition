
library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(car)# logit transformation for proportions

# Light assimilation ------------------------------------------------------

# Load data
ciras.17 <- read.csv("Data/CIRAS_carbon_WUE_2017.csv")
ciras.16 <- read.csv("Data/CIRAS_2016.csv") 

# Organize and put it together
ciras.16 <- ciras.16 %>% 
  unite("spp_trt", Phytometer, Treatment, remove = FALSE)

ciras.16$light <- as.factor(ciras.16$light)
str(ciras.16)

ciras.17 <- ciras.17 %>% 
  unite("spp_trt", Phytometer, Treatment, remove = FALSE)

ciras.17$light <- as.factor(ciras.17$light)
str(ciras.17)

ciras <- full_join(ciras.16, ciras.17)

# Are they different between years?

ciras.1500 <- ciras %>% filter(light == "1500")

ggplot(ciras.1500, aes(x = carbon)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


year.carbon.test <- lm(carbon ~ Year, data = ciras.1500)
anova(year.carbon.test)
summary(year.carbon.test)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
#(Intercept) -107.05947 1881.54397  -0.057    0.955
#Year           0.06094    0.93305   0.065    0.948

plot(year.carbon.test)

# Light Penetration -------------------------------------------------------

light16 <- read.csv("Data/light_2016.csv")
light17 <- read.csv("Data/light_2017.csv")

light <- full_join(light16, light17)
light$Year <- as.factor(light$Year)

ggplot(light, aes(x = Incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white") 


year.light.test <- lm(Incident ~ Year, data = light)
summary(year.light.test)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   74.714      3.244  23.034  < 2e-16 ***
#  Year2017     -12.342      4.551  -2.712  0.00732 **

plot(year.light.test) # curving all around that q-q line


# convert percentages to proportions
light$incident.prop <- (light$Incident)/100

ggplot(light, aes(x = incident.prop)) + 
  geom_histogram(binwidth = 0.01,
                 color="black", fill="white") 

# logit transformation: log[p/(1-p)]
# remapped to 0.25 - 0.975 for 0 and 1 values

light$prop.logt <- logit(light$incident.prop, percents = FALSE) 

ggplot(light, aes(x = prop.logt)) + 
  geom_histogram(binwidth = 0.1,
                 color="black", fill="white")


year.prop.test <- lm(prop.logt ~ Year, data = light)
summary(year.prop.test)

#Coefficients:
#       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.4265     0.1795   7.948 1.89e-13 ***
#Year2017     -0.6523     0.2518  -2.590   0.0104 * 

plot(year.prop.test) # still curvy, not great
