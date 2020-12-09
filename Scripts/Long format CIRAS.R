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