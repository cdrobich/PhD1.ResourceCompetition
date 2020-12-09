library(tidyverse)


ciras.17 <- read.csv("Data/Ciras_2017.csv")
ciras.17
colnames(ciras.17)


?gather
ciras_long <- gather(ciras.17, light, carbon, X1500:X0, factor_key = TRUE)

test <- ciras_long %>% arrange(ciras_long, Site)

write.csv(test, "Data/CIRAS_2017_long.csv")
