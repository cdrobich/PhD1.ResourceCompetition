
library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(car)# logit transformation for proportions
library(agricolae)

# Light assimilation ------------------------------------------------------

# Load data
ciras.17 <- read.csv("Data/CIRAS_carbon_WUE_2017.csv")
ciras.16 <- read.csv("Data/CIRAS_2016.csv") 

str(ciras.16)

# Organize and put it together
ciras.16$light <- as.factor(ciras.16$light)
ciras.17$light <- as.factor(ciras.17$light)

ciras <- full_join(ciras.16, ciras.17)

# Are they different between years?

ciras.1500 <- ciras %>% filter(light == "1500")

ggplot(ciras.1500, aes(x = carbon)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")


year.carbon.test <- lm(carbon ~ Year, data = ciras.1500)
anova(year.carbon.test)
summary(year.carbon.test)

#Response: carbon
#            Df Sum Sq Mean Sq F value Pr(>F)
#Year        1    0.2   0.153  0.0043  0.948
#Residuals 164 5883.1  35.873 

plot(year.carbon.test)

# Light Penetration -------------------------------------------------------

light16 <- read.csv("Data/light_2016.csv")
light17 <- read.csv("Data/light_2017.csv")


light17 <- light17 %>% #rename the factors
  mutate(Treatment = fct_recode(Treatment,
                                "no_competition" = "No",
                                "competition" = "Yes"))


light <- full_join(light16, light17)
light$Year <- as.factor(light$Year)

light %>% group_by(Treatment) %>% 
  summarise(light.avg = mean(Incident),
            light.sd = sd(Incident),
            N = length(Incident),
            light.str = light.sd/sqrt(N))

#Treatment      light.avg light.sd     N light.str
#1 competition         62.6     34.2    96      3.49
#2 no_competition      74.7     27.1    89      2.87

ggplot(light, aes(x = Incident)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white") 


year.light.test <- lm(Incident ~ Year, data = light)
summary(year.light.test)
anova(year.light.test)

#Response: Incident
#           Df Sum Sq Mean Sq F value   Pr(>F)   
#Year        1   7043  7043.1  7.3561 0.007321 **
#Residuals 183 175215   957.5 

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

colnames(light)

year.prop.test <- lm(prop.logt ~ Treatment * Year, data = light)
summary(year.prop.test)
Anova(year.prop.test, type = 2)

#Anova Table (Type II tests)

#Response: prop.logt
#               Sum Sq  Df F value  Pr(>F)   
#Treatment       15.81   1  5.4956 0.02015 * 
#Year            20.26   1  7.0442 0.00866 **
#Treatment:Year   0.02   1  0.0084 0.92693   
#Residuals      520.70 181

plot(year.prop.test) # still curvy, not great

years.light <- HSD.test(year.prop.test, "Year")

#      prop.logt groups
#2016 1.4265457      a
#2017 0.7742809      b

trt.light <- HSD.test(year.prop.test, "Treatment")

#               prop.logt groups
#no_competition 1.3929874      a
#competition    0.8189811      b

# Biomass -----------------------------------------------------------------

weights16 <- read.csv("Data/weights_2016.csv")
weights17 <- read.csv("Data/weights_2017.csv")

weights <- full_join(weights16, weights17)
weights <- na.omit(weights)

weights$Year <- as.factor(weights$Year)

ggplot(weights, aes(x = Total)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")

weights$total.log <- log(weights$Total + 1)

ggplot(weights, aes(x = total.log)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")

weight.test <- lm(Total ~ Year, data = weights)
summary(weight.test )
anova(weight.test)

#Response: Total
#           Df Sum Sq Mean Sq F value Pr(>F)
#Year        1    565  565.05  2.3991 0.1233
#Residuals 164  38627  235.53

plot(weight.test ) # still curvy, not great


