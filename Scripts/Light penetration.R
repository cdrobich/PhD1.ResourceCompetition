library(tidyverse)
library(ggpubr)
library(car)
library(performance)


light16 <- read.csv("Data/light_2016.csv")
light17 <- read.csv("Data/light_2017.csv")

str(light16)

unique(light16$Phytometer)
unique(light17$Phytometer)

light17 <- light17 %>% #rename the factors
  mutate(Treatment = fct_recode(Treatment,
                           "no_competition" = "No",
                           "competition" = "Yes"))


light16$Incident <- as.numeric(light16$Incident)
light17$Incident <- as.numeric(light17$Incident)




##### t-test for differences in incident light ####

carex16 <- light16 %>% filter(Phytometer == "Carex")
cala16 <- light16 %>% filter(Phytometer == "Calamagrostis")
typha16 <- light16 %>% filter(Phytometer == "Typha")
phrag16 <- light16 %>% filter(Phytometer == "Phragmites")



carex16.t <- lm(Incident ~ Treatment, data = carex16)
summary(carex16.t)

check_heteroscedasticity(carex16.t)
#OK: Error variance appears to be homoscedastic (p = 0.168).
check_normality(carex16.t)
#OK: residuals appear as normally distributed (p = 0.669).



#Welch Two Sample t-test

#data:  Incident by Treatment
#t = -1.2536, df = 18.973, p-value = 0.2252
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -39.86392  10.00028
#sample estimates:
#  mean in group competition     mean in group no_competition 
#      50.25000                     65.18182 


cala16.t <- lm(Incident ~ Treatment, data = cala16)

#Welch Two Sample t-test

#data:  Incident by Treatment
#t = -1.6824, df = 18.911, p-value = 0.1089
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -37.033811   4.033811
#sample estimates:
#  mean in group competition mean in group no_competition 
#           57.5                         74.0 

check_heteroscedasticity(cala16.t)
#OK: Error variance appears to be homoscedastic (p = 0.177).
check_normality(cala16.t)
#OK: residuals appear as normally distributed (p = 0.741).



typha16.t <- lm(Incident ~ Treatment, data = typha16)

#Welch Two Sample t-test

# data:  Incident by Treatment
# t = -1.1235, df = 6.8212, p-value = 0.2992
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -20.25361   7.25361
# sample estimates:
#  mean in group competition mean in group no_competition 
#       88.16667                     94.6666

check_heteroscedasticity(typha16.t)
#OK: Error variance appears to be homoscedastic (p = 0.094).
check_normality(typha16.t)
#OK: residuals appear as normally distributed (p = 0.111).


phrag16.t <- lm(Incident ~ Treatment, data = phrag16)

#Welch Two Sample t-test

#data:  Incident by Treatment
#t = -1.3778, df = 18.336, p-value = 0.1849
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -24.633033   5.105256
#sample estimates:
#  mean in group competition mean in group no_competition 
#82.61111                     92.37500 

check_heteroscedasticity(phrag16.t)
#OK: heteroskedastic (p = 0.000).
check_normality(phrag16.t)
#OK: non normal (p < 0.001)



######## Incident Light in 2017 ############

carex17 <- light17 %>% filter(Phytometer == "Carex")
cala17 <- light17 %>% filter(Phytometer == "Calamagrostis")
typha17 <- light17 %>% filter(Phytometer == "Typha")
phrag17 <- light17 %>% filter(Phytometer == "Phragmites")



carex17.t <- lm(Incident ~ Treatment, data = carex17)
summary(carex17.t)

check_heteroscedasticity(carex17.t)
#Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.012).
check_normality(carex17.t)
#Warning: Non-normality of residuals detected (p < .001).

#Welch Two Sample t-test

# data:  Incident by Treatment
# t = -0.57732, df = 14.368, p-value = 0.5727
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -47.06158  27.06158
# sample estimates:
#  mean in group no_competition    mean in group competition 
#          36                           46 



cala17.t <- t.test(Incident ~ Treatment, data = cala17)

#Welch Two Sample t-test

#data:  Incident by Treatment
#t = 2.384, df = 20.631, p-value = 0.02682
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  4.171906 61.661428
#sample estimates:
#  mean in group no_competition    mean in group competition 
#      61.16667                     28.25000 


typha17.t <- t.test(Incident ~ Treatment, data = typha17)

#Welch Two Sample t-test

#data:  Incident by Treatment
#t = 2.0285, df = 5.4922, p-value = 0.09322
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -5.377507 51.377507
#sample estimates:
#  mean in group no_competition    mean in group competition 
#      85.33333                     62.3333


phrag17.t <- t.test(Incident ~ Treatment, data = phrag17)

#Welch Two Sample t-test

#data:  Incident by Treatment
#t = 1.3993, df = 26.436, p-value = 0.1734
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.699686 14.241353
#sample estimates:
#  mean in group no_competition    mean in group competition 
#    91.93750                     86.16667 





light17 %>% 
  group_by(Phytometer, Treatment) %>% 
  summarise(average = mean(Incident),
            std = sd(Incident),
            sample = length(Incident),
            st.err = std/sqrt(sample))

#  Phytometer    Treatment      average   std sample st.err
#1 Calamagrostis no_competition    61.2 37.9      12  10.9 
#2 Calamagrostis competition       28.2 29.1      12   8.41
#3 Carex         no_competition    36   22.1      12   6.38
#4 Carex         competition       46   55.8      12  16.1 
#5 Phragmites    no_competition    91.9  8.08     16   2.02
#6 Phragmites    competition       86.2 15.3      18   3.60
#7 Typha         no_competition    85.3  6.02      6   2.46
#8 Typha         competition       62.3 27.1       6  11.1 



##### Figures ##########

light16 <- light16 %>% 
  mutate(Phytometer = fct_relevel(Phytometer,
                                 "Carex",
                                 "Calamagrostis",
                                 "Typha",
                                 "Phragmites"))


light17 <- light17 %>% 
  mutate(Phytometer = fct_relevel(Phytometer,
                                  "Carex",
                                  "Calamagrostis",
                                  "Typha",
                                  "Phragmites"))

light17 <- light17 %>% 
  mutate(Treatment = fct_relevel(Treatment,
                                 "competition",
                                 "no_competition"))

lights16 <- ggplot(light16, aes(x = Phytometer, y = Incident)) + 
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
    size = 4) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_se", fun.args = list(mult = 1),
    geom = "pointrange", size = 1,
    position = position_dodge(0.6)
  ) +
  labs(x = " ",
       y = expression(paste("Percent Incident Light"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  scale_color_manual(values = c("#9ebcda","#8856a7")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  ylim(0, 100)







panel <- ggarrange(lights16, lights17,
                         common.legend = TRUE, 
                         legend = "bottom",
                         labels = c("A", "B"), 
                         hjust = c(-7, -7),
                         vjust = 2.5)
panel

ggsave("Figures/light_figure.jpeg")


########## Paired T.Tests with Year as Random ##########

library(lme4)

##### Resident Species ############

light.res.pair <- read.csv("Data/light_resident.csv")
light.res.pair$Year <- as.factor(light.res.pair$Year)

light.res.pair <- light.res.pair %>% 
  unite("compyear", Competition:Year, remove = FALSE)



carex.light <- light.res.pair %>% filter(Phytometer == "Carex")
cala.light <- light.res.pair %>% filter(Phytometer == "Calamagrostis")
typha.light <- light.res.pair %>% filter(Phytometer == "Typha")


test.carex <- lmer(Incident ~ Competition + (1|Pair) + (1|Year), data = carex.light)
summary(test.carex)

library(MuMIn)

#### the marginal and conditional r2 for model 1 ####

r.squaredGLMM(test.carex)

# examine the residuals 
plot(test.carex)
qqnorm(resid(test.carex))
qqline(resid(test.carex))

## check out model 1 coefficients

coef(test.carex)

### paired t test 
colnames(carex.light)

carex.16 <- carex.light %>% filter(Year == 2016)
carex.17 <- carex.light %>% filter(Year == 2017)


carex.pair.test <- t.test(Incident ~ Competition, paired = TRUE, data = carex.light)

#Paired t-test

# data:  Incident by Competition
# t = 1.04, df = 22, p-value = 0.3097
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -8.472095 25.515573
# sample estimates:
#  mean of the differences 
# 8.521739 



carex.pair.test1 <- t.test(Incident ~ Competition, paired = TRUE, data = carex.16)

#Paired t-test

#data:  Incident by Competition
#t = 1.8581, df = 10, p-value = 0.09281
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -3.965109 43.783291
#sample estimates:
#  mean of the differences 
# 19.90909

carex.pair.test2 <- t.test(Incident ~ Competition, paired = TRUE, data = carex.17)

#Paired t-test

#data:  Incident by Competition
#t = -0.16141, df = 11, p-value = 0.8747
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -28.05256  24.21923
#sample estimates:
#  mean of the differences 
# -1.916667 

cala.pair.test <- t.test(Incident ~ Competition, paired = TRUE, data = cala.light)

#Paired t-test

# data:  Incident by Competition
# t = 3.1414, df = 21, p-value = 0.004929
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  9.003233 44.269495
# sample estimates:
#  mean of the differences 
# 26.63636

cala.16 <- cala.light %>% filter(Year == 2016)
cala.17 <- cala.light %>% filter(Year == 2017)

cala.pair.test1 <- t.test(Incident ~ Competition, paired = TRUE, data = cala.16)

#Paired t-test

# data:  Incident by Competition
# t = 2.4794, df = 9, p-value = 0.03503
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  1.673557 36.526443
# sample estimates:
#  mean of the differences 
#   19.1


cala.pair.test2 <- t.test(Incident ~ Competition, paired = TRUE, data = cala.17)

#Paired t-test

# data:  Incident by Competition
#  = 2.3087, df = 11, p-value = 0.04139
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  1.535861 64.297472
# sample estimates:
#  mean of the differences 
#  32.91667 


typha.pair.test <- t.test(Incident ~ Competition, paired = TRUE, data = typha.light)

#Paired t-test

#data:  Incident by Competition
#t = 2.5635, df = 11, p-value = 0.02636
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  2.085639 27.414361
#sample estimates:
#  mean of the differences 
#14.75 

typh.16 <- typha.light %>% filter(Year == 2016)
typh.17 <- typha.light %>% filter(Year == 2017)

typha.pair.test1 <- t.test(Incident ~ Competition, paired = TRUE, data = typh.16)

#Paired t-test

#data:  Incident by Competition
#t = 1.9495, df = 5, p-value = 0.1087
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.070748 15.070748
#sample estimates:
#  mean of the differences 
#  6.5 


typha.pair.test2 <- t.test(Incident ~ Competition, paired = TRUE, data = typh.17)

#Paired t-test

#data:  Incident by Competition
#t = 2.22, df = 5, p-value = 0.07712
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -3.631691 49.631691
#sample estimates:
#  mean of the differences 
#23 



light.res.pair %>% 
  group_by(Phytometer, Competition) %>% 
  summarise(average = mean(Incident),
            std = sd(Incident),
            sample = length(Incident),
            st.err = std/sqrt(sample))


# Phytometer    Competition   average std   sample  st.err
#1 Calamagrostis no             67   30.5      22   6.51
#2 Calamagrostis yes            40.4 31.6      22   6.74
#3 Carex         no             50.0 26.2      23   5.47
#4 Carex         yes            41.4 32.5      23   6.78
#5 Typha         no             90    7.40     12   2.14
#6 Typha         yes            75.2 24.4      12   7.03



lights.res.plot <- ggplot(light.res.pair, aes(x = Phytometer, y = Incident)) + 
  geom_jitter(
    aes(shape = Competition, color = Competition), 
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
    size = 4) +
  theme_classic() +
  stat_summary(
    aes(shape = Competition),
    fun.data = "mean_se", fun.args = list(mult = 1),
    geom = "pointrange", size = 1,
    position = position_dodge(0.6)
  ) +
  labs(x = " ",
       y = expression(paste("Percent Incident Light"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")"))) + 
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  scale_color_manual(values = c("#9ebcda","#8856a7")) +
  ylim(0, 100)

lights.res.plot

ggsave("Figures/IncidentLightResidents.jpeg")

######### Phragmites Paired T.Test #############

phrag.light.pair <- read.csv("Data/Light_phrag.csv")

light.res.pair$Year <- as.factor(light.res.pair$Year)

phrag.light.pair <- phrag.light.pair %>% 
  unite("compyear", Competition:Year, remove = FALSE)



p.carex.light <- phrag.light.pair %>% filter(Neighbour == "Carex")
p.cala.light <- phrag.light.pair %>% filter(Neighbour == "Calamagrostis")
p.typha.light <- phrag.light.pair %>% filter(Neighbour == "Typha")

 
