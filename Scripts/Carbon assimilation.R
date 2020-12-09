library(tidyverse)
library(ggpubr)

ciras.17 <- read.csv("Data/CIRAS_long_2017.csv")

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
ciras <- ciras[,1:10]


sum <- ciras %>% group_by(spp_trt, light) %>% 
  summarise(avg = mean(carbon),
            sd = sd(carbon),
            N = length(carbon),
            str = (sd/(sqrt(N))))


ggplot(sum, aes(x = light, y = avg, color = spp_trt, group = spp_trt)) +
  geom_errorbar(aes(ymin = avg - str, ymax = avg + str), width = 0.1) +
  geom_line() +
  geom_point() +
  theme_classic()
  



