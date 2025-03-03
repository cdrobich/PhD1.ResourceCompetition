library(tidyverse)


ciras.17 <- read.csv("Data/Ciras_2017.csv")
ciras.17
colnames(ciras.17)



ciras_long <- gather(ciras.17, light, carbon, X1500:X0, factor_key = TRUE)



ciras.long <- ciras_long %>% #rename the factors
  mutate(light = fct_recode(light,
                            "1500" = "X1500",
                            "1000" = "X1000",
                            "500" = "X500",
                            "200" = "X200",
                            "100" = "X100",
                            "50" = "X50",
                            "0" = "X0"))

ciras.long <- ciras.long %>% arrange(ciras.long, Site)


write.csv(ciras.long, "Data/CIRAS_long_2017.csv")

###### Height 2016 ###########

height6 <- read.csv("Data/height_2016.csv")
height6 <- na.omit(height6)

str(height6)

colnames(height6)

height_long6 <- gather(height6, date, height, X18.May.16:X12.Jul.16)
height_long6 <- height_long %>% arrange(height_long, Site.ID)



height_long6 <- height_long6 %>% #rename the factors
  mutate(date = fct_recode(date,
                           "180516" = "X18.May.16",
                           "030616" = "X03.Jun.16",
                           "120616" = "X12.Jun.16",
                           "220616" = "X22.Jun.16",
                           "020716" = "X02.Jul.16",
                           "120716" = "X12.Jul.16"))


write.csv(height_long6, "Data/Height_long_2016.csv")



### Height 2017 ######

height <- read.csv("Data/Height_2017.csv")
height

colnames(height)

height_long <- gather(height, date, height, X10.May.17:X28.Jun.17)
height_long <- height_long %>% arrange(height_long, Site.ID)



height_long <- height_long %>% #rename the factors
  mutate(date = fct_recode(date,
                           "100517" = "X10.May.17",
                            "270517" = "X27.May.17",
                            "070617" = "X07.Jun.17",
                            "160617" = "X16.Jun.17",
                            "280617" = "X28.Jun.17"))


write.csv(height_long, "Data/Height_long_2017.csv")
