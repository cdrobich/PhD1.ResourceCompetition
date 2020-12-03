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




light16 %>% 
  group_by(Phytometer, Treatment) %>% 
  summarise(average = mean(Incident),
            std = sd(Incident),
            sample = length(Incident),
            st.err = std/sqrt(sample))


#  Phytometer    Treatment      average   std sample st.err
#1 Calamagrostis competition       57.5 27.8      12   8.02
#2 Calamagrostis no_competition    74   17.8      10   5.64
#3 Carex         competition       50.2 34.2      12   9.87
#4 Carex         no_competition    65.2 22.1      11   6.66
#5 Phragmites    competition       82.6 29.5      18   6.95
#6 Phragmites    no_competition    92.4  5.52     16   1.38
#7 Typha         competition       88.2 13.0       6   5.31
#8 Typha         no_competition    94.7  5.65      6   2.30

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




lights17 <- ggplot(light17, aes(x = Phytometer, y = Incident)) + 
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
