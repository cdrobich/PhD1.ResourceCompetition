library(tidyverse)
library(ggpubr)
library(car)
library(agricolae)

Isotopes <- read.csv("Data/Isotopes.csv")

colnames(Isotopes)


## Delta C ANOVA

target <- c("Carex", "Calamagrostis", "Typha")
  
residents.isotopes <- Isotopes %>% filter(Species %in% target)

ggplot(Isotopes, aes(x = DeltaC)) + 
  geom_histogram(binwidth = 3,
                 color="black", fill="white")

deltaC <- lm(DeltaC ~ Species * Treatment, data = residents.isotopes)
Anova(deltaC, type = 2)

#Anova Table (Type II tests)

#Response: DeltaC
#                    Sum Sq Df F value    Pr(>F)    
#Species           20.6373  2  15.873 4.054e-05 ***
#Treatment          7.9567  1  12.240  0.001849 ** 
#Species:Treatment  0.2250  2   0.173  0.842154    
#Residuals         15.6016 24                      

spp <- HSD.test(deltaC, "Species")

#              DeltaC    groups
#Calamagrostis -27.637      a
#Carex         -28.297      a
#Typha         -29.631      b


yr <- HSD.test(deltaC, "Treatment")

#$means
#                  DeltaC      std  r    Min    Max     Q25    Q50    Q75
#Competition    -29.03667 1.142826 15 -30.79 -26.83 -29.705 -28.93 -28.43
#No_competition -28.00667 1.139522 15 -30.14 -25.83 -28.630 -27.84 -27.42

#                  DeltaC groups
#No_competition -28.00667      a
#Competition    -29.03667      b

deltaN <- lm(DeltaN ~ Species * Treatment, data = residents.isotopes)
Anova(deltaN, type = 2) # no interaction so using type II

#Response: DeltaN
#                   Sum Sq Df F value  Pr(>F)  
#Species            5.387  2  2.0036 0.15678  
#Treatment          4.784  1  3.5586 0.07139 .
#Species:Treatment  1.301  2  0.4837 0.62238  
#Residuals         32.264 24                               


aovC <- lm(C ~ Species * Treatment, data = residents.isotopes)
Anova(aovC, type = 2)

#Response: C
#                  Sum Sq Df F value   Pr(>F)   
#Species           26.837  2  7.8323 0.002408 **
#Treatment          0.006  1  0.0035 0.953108   
#Species:Treatment  3.841  2  1.1211 0.342387   
#Residuals         41.117 24   

sppC <- HSD.test(aovC, "Species")

#C groups
#Typha         48.3701      a
#Calamagrostis 47.2696     ab
#Carex         46.0543      b



aovN <- lm(N ~ Species * Treatment, data = residents.isotopes)
Anova(aovN, type = 3)


#Response: N
#                   Sum Sq Df F value    Pr(>F)    
# (Intercept)       4.4840  1 26.2079 3.073e-05 ***
#  Species           4.1837  2 12.2262 0.0002182 ***
#  Treatment         1.6565  1  9.6817 0.0047539 ** 
#  Species:Treatment 1.6491  2  4.8194 0.0173964 *  
#  Residuals         4.1063 24  

phrag.isotopes <- Isotopes %>% filter(Species == "Phragmites")

deltN.phr <- lm(DeltaN ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(deltN.phr , type = 2)

#Response: DeltaN
#                      Sum Sq Df F value Pr(>F)
#Neighbours           2.4797  2  2.4511 0.1281
#Treatment            0.5796  1  1.1459 0.3055
#Neighbours:Treatment 2.2621  2  2.2360 0.1495
#Residuals            6.0699 12 


deltC.phr  <- lm(DeltaC ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(deltC.phr, type = 2)

#Response: DeltaC
#                      Sum Sq Df F value Pr(>F)
#Neighbours           0.7757  2  0.5365 0.5982
#Treatment            1.0368  1  1.4342 0.2542
#Neighbours:Treatment 0.0692  2  0.0479 0.9534
#Residuals            8.6747 12 



aovN.ph <- lm(N ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(aovN.ph, type = 2)


#Response: N
#                     Sum Sq  Df F value   Pr(>F)   
#Neighbours           0.10430  2  0.3819 0.690543   
#Treatment            2.37838  1 17.4196 0.001291 **
#Neighbours:Treatment 0.00093  2  0.0034 0.996612   
#Residuals            1.63842 12                    

ph.N.trt <- HSD.test(aovN.ph, "Treatment")

#          N groups
#No_competition 2.474667      a
#Competition    1.747667      b

aovC.ph <- lm(C ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(aovC.ph, type = 2)

#                       Sum Sq Df F value  Pr(>F)  
#Neighbours            0.9945  2  0.4526 0.64639  
#Treatment             3.9069  1  3.5561 0.08376 .
#Neighbours:Treatment  1.4949  2  0.6803 0.52495  
#Residuals            13.1838 12 




nutrient.sum <- Isotopes %>% group_by(Type) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))


#Type                         N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#1 Calamagrostis_Competition    0.947 0.348        5 0.156   46.9 1.66         5 0.741
#2 Calamagrostis_No competition 1.76  0.206        5 0.0920  47.7 1.57         5 0.701
#3 Carex_Competition            0.838 0.225        5 0.101   46.0 0.348        5 0.156
#4 Carex_No competition         1.08  0.349        5 0.156   46.1 1.24         5 0.556
#5 Phragmites_Competition       1.75  0.284        9 0.0947  47.4 0.748        9 0.249
#6 Phragmites_No competition    2.47  0.370        9 0.123   46.4 1.18         9 0.394
#7 Typha_Competition            2.01  0.548        5 0.245   48.8 1.24         5 0.552
#8 Typha_No competition         1.67  0.625        5 0.280   47.9 1.37         5 0.614


isotope.sum <- Isotopes %>% group_by(Type) %>% 
  summarise(dN.avg = mean(DeltaN),
            dN.sd = sd(DeltaN),
            dN.length = length(DeltaN),
            dN.err = (dN.sd/(sqrt(dN.length))),
            dC.avg = mean(DeltaC),
            dC.sd= sd(DeltaC),
            dC.length = length(DeltaC),
            dC.err = (dC.sd/(sqrt(dC.length))))

#Type                        dN.avg dN.sd dN.length dN.err dC.avg dC.sd dC.length dC.err
#1 Calamagrostis_Competition   1.53 1.50          5  0.673  -28.3 1.06          5  0.476
#2 Calamagrostis_No competi~   2.69 1.84          5  0.824  -27.0 0.879         5  0.393
#3 Carex_Competition           2.40 0.648         5  0.290  -28.8 0.851         5  0.380
#4 Carex_No competition        3.41 0.756         5  0.338  -27.8 0.494         5  0.221
#5 Phragmites_Competition      3.85 0.865         9  0.288  -26.8 0.634         9  0.211
#6 Phragmites_No competition   4.21 0.776         9  0.259  -26.3 0.888         9  0.296
#7 Typha_Competition           1.82 0.787         5  0.352  -30.0 0.795         5  0.355
#8 Typha_No competition        2.04 0.893         5  0.399  -29.2 0.629         5  0.281

nutrient.summary <- full_join(nutrient.sum, isotope.sum)
write.csv(nutrient.summary, "Data/Nutrient_Isotope_sum.csv")

colnames(residents.isotopes)

centroids <- aggregate(cbind(DeltaC,DeltaN)~Type,Isotopes,mean)
f         <- function(z)sd(z)/sqrt(length(z)) # function to calculate std.err
se        <- aggregate(cbind(se.x=DeltaC,se.y=DeltaN)~Type,Isotopes,f)
centroids <- merge(centroids,se, by="Type")

write.csv(centroids, "Data/isotope_isotopes.csv")

isotopes <- ggplot(Isotopes, aes(x = DeltaC, y = DeltaN, shape = Type, colour = Type)) +
  geom_point(size = 3, stroke = 1.5) +
  theme_classic(base_size = 16) + 
  theme(panel.border = element_rect(fill = NA)) +
  ylim(-1, 6) +
  labs(x = expression(paste(delta^{13}, "C")),
       y = expression(paste(delta^{15}, "N"))) +
  scale_shape_manual(values = c(0,15,1, 16,2, 17, 5,18)) +
  scale_colour_manual(values = c("black","#bdbdbd",
                                 "#084594","#9ecae1",
                                 "#99000d","#fb6a4a",
                                 "#8c96c6","#6e016b")) +
  theme(legend.position = "none") 

legend <- get_legend(isotopes)
legends <- as_ggplot(legend)


iso.eror <- isotopes + 
  geom_errorbar(data = centroids,
                aes(ymin = DeltaN - se.y, ymax = DeltaN + se.y), 
                width = 0.1, size = 1)+
  geom_errorbarh(data = centroids, aes(xmin = DeltaC - se.x, xmax = DeltaC + se.x),
                 height = 0.1, size = 1) +
  geom_point(data = centroids, size = 6, stroke = 1.5)
  




nutrient <- ggplot(Isotopes, aes(x = C, y = N, shape = Type, colour = Type)) +
  geom_point(size = 5, stroke = 1.5) +
  theme_classic(base_size = 16) + 
  theme(panel.border = element_rect(fill = NA)) +
  xlab("Total % Carbon") +
  ylab("Total % Nitrogen") +
  scale_shape_manual(values = c(0,15,1, 16,2, 17, 5,18)) +
  scale_colour_manual(values = c("black","#bdbdbd",
                                 "#084594","#9ecae1",
                                 "#99000d","#fb6a4a",
                                 "#8c96c6","#6e016b")) 
nutrient



leaf.nutrient <- ggarrange(isotopes, nutrient,
          common.legend = TRUE,
          legend = "right",
          labels = "AUTO",
          hjust = c(-5, -4.5),
          vjust = 2.5)

ggsave("Figures/Plant_Nutrients.TIFF", leaf.nutrient,
       dpi = 300)


Isotopes %>% 
  group_by(Species, Treatment) %>% 
  mutate(Cmean = mean(C),
         Cmedian = median(C),
         Nmean = mean(N),
         Nmedian = median(N)) -> Isotopes
  
  
  
nut.den <- ggplot(Isotopes, aes(x = N, fill = as.factor(Treatment))) +
  geom_density(alpha = 0.6, size = 1) +
    facet_wrap("Species") + 
  scale_fill_manual(values = c("#fb6a4a", "white")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = "Total % Nitrogen",
       y = "Density") +
  xlim(0, 4) +
  labs(fill = "Competition") +
  geom_vline(aes(xintercept = Nmean, colour = Treatment),
             size = 1) +
  scale_colour_manual(values = c("#fb6a4a", "#737373")) +
  theme(legend.position = "none") 

nut.den
  

nut.cden <- ggplot(Isotopes, aes(x = C, fill = as.factor(Treatment))) +
  geom_density(alpha = 0.6, size = 1) +
  facet_wrap("Species") + 
  scale_fill_manual(values = c("#8c96c6", "white")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = "Total % Carbon",
       y = "Density") +
  xlim(43, 52) +
  labs(fill = "Competition") +
  geom_vline(aes(xintercept = Cmean,
                 colour = Treatment),
             size = 1,
             show.legend = FALSE) +
  scale_colour_manual(values = c("#8c96c6", "#737373")) +
  theme(legend.position = c(0.85, 0.8))

nut.cden


(isotope.nut.den <- ggarrange(iso.eror, nut.den,
          legends, nut.cden,
          labels = c("A","B"," ","C")))

ggsave("Figures/Isotopes_Nutrient_density.TIFF", isotope.nut.den,
       dpi = 300)






