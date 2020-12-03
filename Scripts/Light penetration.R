library(tidyverse)

light16 <- read.csv("Data/light_2016.csv")
light17 <- read.csv("Data/light_2017.csv")

str(light16)

unique(light16$Phytometer)
unique(light17$Phytometer)

light17 <- light17 %>% #rename the factors
  mutate(Treatment = fct_recode(Treatment,
                           "no_competition" = "No",
                           "competition" = "Yes"))

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
       y = expression(paste("Incident Light to Phytometer"))) + 
  scale_color_manual(values = c("#fc8d62","#35978f")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  ylim(0, 100)


light17 <- light17 %>% 
  mutate(Treatment = fct_relevel(Treatment,
                            "competition",
                            "no_competition"))

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
       y = expression(paste("Incident Light to Phytometer"))) + 
  scale_color_manual(values = c("#fc8d62","#35978f")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  ylim(0, 100)