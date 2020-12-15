library(tidyverse)


ciras.17 <- read.csv("Data/Ciras_2017.csv")
ciras.17
colnames(ciras.17)


?gather
ciras_long <- gather(ciras.17, light, carbon, X1500:X0, factor_key = TRUE)

test <- ciras_long %>% arrange(ciras_long, Site)

write.csv(test, "Data/CIRAS_2017_long.csv")

ciras.17 <- ciras.17 %>% #rename the factors
  mutate(light = fct_recode(light,
                            "1500" = "X1500",
                            "1000" = "X1000",
                            "500" = "X500",
                            "200" = "X200",
                            "100" = "X100",
                            "50" = "X50",
                            "0" = "X0"))


write.csv(ciras.17, "Data/CIRAS_long_2017.csv")


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
