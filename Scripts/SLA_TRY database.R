
# Load package ------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)


# Load database -----------------------------------------------------------

SLA <- read.csv("Data/TRY_SLA_traits.csv")
head(SLA)
colnames(SLA)

# use StdValue and UnitName which have been standardized
unique(SLA$UnitName) # all mm2 mg-1


SLA %>% group_by(TraitName, SpeciesName) %>% 
  summarise(N = length(StdValue),
            average = mean(StdValue),
            st.dev = sd(StdValue),
            st.err = (st.dev/sqrt(N)))

#  TraitName     SpeciesName                  N average st.dev st.err
#1 SLA.nopetiole Calamagrostis canadensis    15   29.1   7.54   1.95 
#2 SLA.nopetiole Carex aquatilis             29   14.7   3.24   0.601
#3 SLA.nopetiole Phragmites australis        34   15.7   3.72   0.638
#4 SLA.nopetiole Typha angustifolia          19   7.62  0.945   0.217
#5 SLA.nopetiole Typha latifolia             26   11.0   2.89   0.567

#6 SLA.petiole   Calamagrostis canadensis    29   27.3   14.1    2.61 
#7 SLA.petiole   Carex aquatilis              6   14.1   1.68   0.688
#8 SLA.petiole   Phragmites australis        20   12.2   2.09   0.467
#9 SLA.petiole   Typha angustifolia           2   6.31   0      0    
#10 SLA.petiole  Typha latifolia             24   11.1   3.21   0.655

#11 SLA.unsure    Calamagrostis canadensis   17   17.4   11.2    2.71 
#12 SLA.unsure    Carex aquatilis           524   15.0   4.51   0.197
#13 SLA.unsure    Phragmites australis       44   15.1   4.10   0.618
#14 SLA.unsure    Typha angustifolia         11   8.71   1.73   0.521
#15 SLA.unsure    Typha latifolia            18   10.9   2.50   0.589

