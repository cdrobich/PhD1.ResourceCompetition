library(tidyverse)

ciras <- read.csv("data/CIRAS_sum_bothyears.csv", header = TRUE)

comp_lrc <- ciras %>% 
  filter(Treatment == "Competition") %>% 
  select(ID_yr, light, carbon) %>% 
  pivot_wider(names_from = ID_yr, values_from = carbon)

com_lrc <- comp_lrc[,-1]


PARlrc <- comp_lrc$light


dataframe <- data.frame(matrix(nrow = length(colnames(comp_lrc)),
                               ncol = 3))

names <- c("Site", "LCPT", "LSP")
colnames(dataframe) <- names

site <- colnames(comp_lrc) 
dataframe$Site <- site
dataframe[-1,]

results <- dataframe[-1,]




for (i in 1:87){
  photo <- unlist(com_lrc[,i])
  curve.nlslrc = nls(photo ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))
                     -Rd,start=list(Am=(max(photo)-min(photo)),
                                    AQY=0.05,Rd=-min(photo),theta=1))
  results[i,2] <- uniroot(x,c(0,50), extendInt = "yes")$root
  results[i,3] <- uniroot(y,c(0,1000), extendInt = "yes")$root )}
