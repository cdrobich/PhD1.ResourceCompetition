library(tidyverse)
library(ggpubr)
library(car)
library(agricolae)

Isotopes <- read.csv("Data/Isotopes.csv")

colnames(Isotopes)


# Summary of data ---------------------------------------------------------

library(plotrix) # STerr

library(EnvStats) # CV



sum <- Isotopes %>% group_by(Species) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, SE = std.error, CV = cv), na.rm = TRUE,
    .names = "{col}_{fn}"
  ))

sum <- sum %>% t %>% as.data.frame

write.csv(sum, "Data/isotopes_leafnutrients_sum.csv")

#Species     Calamagrostis       Carex  Phragmites       Typha

#C_Mean           47.48411    46.05430    46.93667    48.37010
#C_SD            1.5097954   0.8619747   0.9453844   1.3291585
#C_SE            0.5032651   0.2725803   0.2228292   0.4203168
#C_CV           0.03179580  0.01871649  0.02014170  0.02747893

#N_Mean           1.458333    0.959800    2.120056    1.841300
#N_SD            0.4079062   0.3052161   0.4980093   0.5814530
#N_SE           0.13596875  0.09651779  0.11738191  0.18387158
#N_CV            0.2797071   0.3179996   0.2349039   0.3157839

#DeltaC_Mean     -27.45000   -28.29700   -26.57722   -29.63100
#DeltaC_SD       1.0219956   0.8486074   0.7803454   0.8027100
#DeltaC_SE       0.3406652   0.2683532   0.1839292   0.2538392
#DeltaC_CV     -0.03723117 -0.02998930 -0.02936143 -0.02709021

#DeltaN_Mean      2.398889    2.906000    4.040000    1.930000
#DeltaN_SD       1.5251922   0.8528931   0.8298547   0.8017342
#DeltaN_SE       0.5083974   0.2697085   0.1955986   0.2535306
#DeltaN_CV       0.6357911   0.2934938   0.2054096   0.4154063




sum2 <- Isotopes %>% group_by(Type) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, SE = std.error, CV = cv), na.rm = TRUE,
    .names = "{col}_{fn}"
  ))

sum2 <- sum2 %>% t %>% as.data.frame


write.csv(sum2, "Data/isotopes_leafnutrients_sum_treatments.csv")






## Delta C ANOVA

target <- c("Carex", "Calamagrostis", "Typha")
  
residents.isotopes <- Isotopes %>% filter(Species %in% target)

ggplot(Isotopes, aes(x = DeltaN)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")

ggplot(Isotopes, aes(x = C)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")

ggplot(Isotopes, aes(x = N)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")

ggplot(Isotopes, aes(x = DeltaC)) + 
  geom_histogram(binwidth = 1,
                 color="black", fill="white")
# res species two-way ANOVA -----------------------------------------------------------

deltaC <- lm(DeltaC ~ Species * Treatment, data = residents.isotopes)
Anova(deltaC, type = 2)

#Anova Table (Type II tests)

#Response: DeltaC
#                   Sum Sq Df F value    Pr(>F)    
#Species           22.1254  2 17.9221 2.034e-05 ***
#Treatment          6.3836  1 10.3418  0.003831 ** 
#Species:Treatment  0.0554  2  0.0448  0.956235    
#Residuals         14.1971 23 


spp <- HSD.test(deltaC, "Species")

#$means
#DeltaC       std  r    Min    Max    Q25     Q50    Q75
#Calamagrostis -27.450 1.0219956  9 -28.93 -25.83 -27.88 -27.450 -26.83
#Carex         -28.297 0.8486074 10 -29.90 -27.26 -28.71 -27.995 -27.72
#Typha         -29.631 0.8027100 10 -30.79 -28.61 -30.29 -29.460 -29.02

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

plot(deltaC)




deltaN <- lm(DeltaN ~ Species * Treatment, data = residents.isotopes)
Anova(deltaN, type = 2) # no interaction so using type II

#Response: DeltaN
#                   Sum Sq Df F value Pr(>F)
#Species            4.7810  2  2.0166 0.1560
#Treatment          2.8735  1  2.4241 0.1331
#Species:Treatment  0.8035  2  0.3389 0.7160
#Residuals         27.2645 23                              

plot(deltaN)






aovC <- lm(C ~ Species * Treatment, data = residents.isotopes)
Anova(aovC, type = 2)

#Anova Table (Type II tests)

#Response: C
#                   Sum Sq Df F value   Pr(>F)   
#Species           27.306  2  8.2265 0.002018 **
#Treatment          0.229  1  0.1379 0.713799   
#Species:Treatment  2.422  2  0.7296 0.492920   
#Residuals         38.172 23 


sppC <- HSD.test(aovC, "Species")

#$means

#C                            std  r   Min    Max      Q25   Q50    Q75
#Calamagrostis 47.48411 1.5097954  9 44.93 49.910 46.81000 47.83 48.340
#Carex         46.05430 0.8619747 10 44.75 47.700 45.64475 46.08 46.495
#Typha         48.37010 1.3291585 10 46.58 51.026 47.67375 48.30 49.085

#C groups
#Typha         48.3701      a
#Calamagrostis 47.2696     ab
#Carex         46.0543      b

plot(aovC)




aovN <- lm(N ~ Species * Treatment, data = residents.isotopes)
Anova(aovN, type = 3)

#Anova Table (Type III tests)

#Response: N
#                   Sum Sq Df F value    Pr(>F)    
#(Intercept)       4.6656  1 28.5966 1.978e-05 ***
#Species           3.7644  2 11.5366  0.000339 ***
#Treatment         1.0306  1  6.3167  0.019415 *  
#Species:Treatment 1.2302  2  3.7700  0.038360 *  
#Residuals         3.7525 23                      

plot(aovN)


residents.isotopes %>% group_by(Species, Treatment) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.n = length(N),
            N.str = N.sd/sqrt(N.n))

#  Species       Treatment      N.avg  N.sd   N.n  N.str
#1 Calamagrostis Competition    1.08  0.209     4 0.105 
#2 Calamagrostis No_competition 1.76  0.206     5 0.0920
#3 Carex         Competition    0.838 0.225     5 0.101 
#4 Carex         No_competition 1.08  0.349     5 0.156 
#5 Typha         Competition    2.01  0.548     5 0.245 
#6 Typha         No_competition 1.67  0.625     5 0.280 


residents.isotopes %>% group_by(Species) %>% 
  summarise(avg = mean(C),
            sd = sd(C),
            n = length(C),
            str = sd/sqrt(n))

# Carbon content
# Species         avg    sd     n   str
#1 Calamagrostis  47.5 1.51      9 0.503
#2 Carex          46.1 0.862    10 0.273
#3 Typha          48.4 1.33     10 0.420




residents.isotopes %>% group_by(Species) %>% 
  summarise(dN.avg = mean(DeltaN),
          dN.sd = sd(DeltaN),
          dN.length = length(DeltaN),
          dN.err = (dN.sd/(sqrt(dN.length))),
          dC.avg = mean(DeltaC),
          dC.sd= sd(DeltaC),
          dC.length = length(DeltaC),
          dC.err = (dC.sd/(sqrt(dC.length))))

#  Species       dN.avg dN.sd dN.length dN.err dC.avg dC.sd dC.length dC.err
#1 Calamagrostis   2.40 1.53          9  0.508  -27.4 1.02          9  0.341
#2 Carex           2.91 0.853        10  0.270  -28.3 0.849        10  0.268
#3 Typha           1.93 0.802        10  0.254  -29.6 0.803        10  0.254




residents.isotopes %>% group_by(Treatment) %>% 
  summarise(dN.avg = mean(DeltaN),
            dN.sd = sd(DeltaN),
            dN.length = length(DeltaN),
            dN.err = (dN.sd/(sqrt(dN.length))),
            dC.avg = mean(DeltaC),
            dC.sd= sd(DeltaC),
            dC.length = length(DeltaC),
            dC.err = (dC.sd/(sqrt(dC.length))))

#Treatment      dN.avg dN.sd dN.length dN.err dC.avg dC.sd dC.length dC.err
#<chr>           <dbl> <dbl>     <int>  <dbl>  <dbl> <dbl>     <int>  <dbl>
#1 Competition      2.09 0.835        14  0.223  -29.0  1.18        14  0.316
#2 No_competition   2.72 1.30         15  0.337  -28.0  1.14        15  0.294


residents.isotopes %>% group_by(Species) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))

#  Species       N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#1 Calamagrostis 1.46  0.408        9 0.136   47.5 1.51         9 0.503
#2 Carex         0.960 0.305       10 0.0965  46.1 0.862       10 0.273
#3 Typha         1.84  0.581       10 0.184   48.4 1.33        10 0.420


residents.isotopes %>% group_by(Species, Treatment) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))


#Species       Treatment      N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#<chr>         <chr>          <dbl> <dbl>    <int>  <dbl> <dbl> <dbl>    <int> <dbl>
#1 Calamagrostis Competition    1.08  0.209        4 0.105   47.3 1.64         4 0.818
#2 Calamagrostis No_competition 1.76  0.206        5 0.0920  47.7 1.57         5 0.701
#3 Carex         Competition    0.838 0.225        5 0.101   46.0 0.348        5 0.156
#4 Carex         No_competition 1.08  0.349        5 0.156   46.1 1.24         5 0.556
#5 Typha         Competition    2.01  0.548        5 0.245   48.8 1.24         5 0.552
#6 Typha         No_competition 1.67  0.625        5 0.280   47.9 1.37         5 0.614

# phrag two-way anovas ----------------------------------------------------

phrag.isotopes <- Isotopes %>% filter(Species == "Phragmites")

deltN.phr <- lm(DeltaN ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(deltN.phr , type = 2)

#Anova Table (Type II tests)

#Response: DeltaN
#                     Sum Sq Df F value Pr(>F)
#Neighbours           2.2444  2  2.1364 0.1608
#Treatment            0.6728  1  1.2809 0.2798
#Neighbours:Treatment 2.4868  2  2.3672 0.1360
#Residuals            6.3032 12 

deltC.phr  <- lm(DeltaC ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(deltC.phr, type = 2)

#Anova Table (Type II tests)

#Response: DeltaC
#                     Sum Sq Df F value Pr(>F)
#Neighbours           0.7995  2  0.5635 0.5836
#Treatment            0.9847  1  1.3879 0.2616
#Neighbours:Treatment 0.0544  2  0.0384 0.9625
#Residuals            8.5133 12 



aovN.ph <- lm(N ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(aovN.ph, type = 2)

#Anova Table (Type II tests)

#Response: N
#                      Sum Sq Df F value    Pr(>F)    
#Neighbours           0.12277  2  0.4619 0.6408272    
#Treatment            2.49612  1 18.7833 0.0009719 ***
#Neighbours:Treatment 0.00265  2  0.0100 0.9900817    
#Residuals            1.59468 12                 

ph.N.trt <- HSD.test(aovN.ph, "Treatment")

#$means
#                      N       std r  Min   Max  Q25  Q50  Q75
#Competition    1.747667 0.2841549 9 1.37 2.349 1.57 1.74 1.80
#No_competition 2.492444 0.3664270 9 1.91 3.060 2.35 2.49 2.69


#          N groups
#No_competition 2.474667      a
#Competition    1.747667      b

aovC.ph <- lm(C ~ Neighbours * Treatment, data = phrag.isotopes)
Anova(aovC.ph, type = 2)

#Anova Table (Type II tests)

#Response: C
#                       Sum Sq Df F value  Pr(>F)  
#Neighbours            0.5653  2  0.3229 0.73013  
#Treatment             3.1300  1  3.5762 0.08299 .
#Neighbours:Treatment  0.9958  2  0.5689 0.58070  
#Residuals            10.5027 12  

phrag.isotopes %>% group_by(Neighbours, Treatment) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))

#  Neighbours    Treatment      N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#1 Calamagrostis Competition     1.65 0.240        3 0.139   47.0 0.851        3 0.491
#2 Calamagrostis No_competition  2.36 0.324        3 0.187   46.8 0.405        3 0.234
#3 Carex         Competition     1.8  0.104        3 0.0600  47.4 0.811        3 0.468
#4 Carex         No_competition  2.57 0.227        3 0.131   46.1 1.07         3 0.618
#5 Typha         Competition     1.80 0.481        3 0.278   47.7 0.711        3 0.411
#6 Typha         No_competition  2.54 0.584        3 0.337   46.7 1.43         3 0.827


phrag.isotopes %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))

#   N.avg      N.sd N.length     N.err    C.avg      C.sd C.length     C.err
#1 2.120056 0.4980093       18 0.1173819 46.93667 0.9453844       18 0.2228292


phrag.isotopes %>% group_by(Treatment) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))


#  Treatment      N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#1 Competition     1.75 0.284        9 0.0947  47.4 0.748        9 0.249
#2 No_competition  2.49 0.366        9 0.122   46.5 0.974        9 0.325



phrag.isotopes  %>% group_by(Neighbours, Treatment) %>% 
  summarise(dN.avg = mean(DeltaN),
            dN.sd = sd(DeltaN),
            dN.length = length(DeltaN),
            dN.err = (dN.sd/(sqrt(dN.length))),
            dC.avg = mean(DeltaC),
            dC.sd= sd(DeltaC),
            dC.length = length(DeltaC),
            dC.err = (dC.sd/(sqrt(dC.length))))


nutrient.sum <- Isotopes %>% group_by(Type) %>% 
  summarise(N.avg = mean(N),
            N.sd = sd(N),
            N.length = length(N),
            N.err = (N.sd/(sqrt(N.length))),
            C.avg = mean(C),
            C.sd= sd(C),
            C.length = length(C),
            C.err = (C.sd/(sqrt(C.length))))


#  Type                      N.avg  N.sd N.length  N.err C.avg  C.sd C.length C.err
#1 Calamagrostis_Competition 1.08  0.209        4 0.105   47.3 1.64         4 0.818
#2 Calamagrostis_No competi~ 1.76  0.206        5 0.0920  47.7 1.57         5 0.701
#3 Carex_Competition         0.838 0.225        5 0.101   46.0 0.348        5 0.156
#4 Carex_No competition      1.08  0.349        5 0.156   46.1 1.24         5 0.556
#5 Phragmites_Competition    1.75  0.284        9 0.0947  47.4 0.748        9 0.249
#6 Phragmites_No competition 2.49  0.366        9 0.122   46.5 0.974        9 0.325
#7 Typha_Competition         2.01  0.548        5 0.245   48.8 1.24         5 0.552
#8 Typha_No competition      1.67  0.625        5 0.280   47.9 1.37         5 0.614



isotope.sum <- Isotopes %>% group_by(Type) %>% 
  summarise(dN.avg = mean(DeltaN),
            dN.sd = sd(DeltaN),
            dN.length = length(DeltaN),
            dN.err = (dN.sd/(sqrt(dN.length))),
            dC.avg = mean(DeltaC),
            dC.sd= sd(DeltaC),
            dC.length = length(DeltaC),
            dC.err = (dC.sd/(sqrt(dC.length))))

#  Type                         dN.avg dN.sd dN.length dN.err dC.avg dC.sd dC.length dC.err
#1 Calamagrostis_Competition      2.03 1.16          4  0.581  -28.0 1.02          4  0.511
#2 Calamagrostis_No competition   2.69 1.84          5  0.824  -27.0 0.879         5  0.393
#3 Carex_Competition              2.40 0.648         5  0.290  -28.8 0.851         5  0.380
#4 Carex_No competition           3.41 0.756         5  0.338  -27.8 0.494         5  0.221
#5 Phragmites_Competition         3.85 0.865         9  0.288  -26.8 0.634         9  0.211
#6 Phragmites_No competition      4.23 0.794         9  0.265  -26.3 0.877         9  0.292
#7 Typha_Competition              1.82 0.787         5  0.352  -30.0 0.795         5  0.355
#8 Typha_No competition           2.04 0.893         5  0.399  -29.2 0.629         5  0.281


nutrient.summary <- full_join(nutrient.sum, isotope.sum)

write.csv(nutrient.summary, "Data/Nutrient_Isotope_sum.csv")


# Figures -----------------------------------------------------------------

library(patchwork)

unique(Isotopes$Type)
colours = c("Calamagrostis_Competition" = "#084594",
            "Calamagrostis_Without" = "#084594",
            "Typha_Without" = "#6e016b",
            "Typha_Competition" = "#6e016b",
            "Carex_Without" = "#9ecae1", 
            "Carex_Competition" = "#9ecae1",
            "Phragmites_Without" = "#fb6a4a",
            "Phragmites_Competition" = "#fb6a4a")

colnames(residents.isotopes)

centroids <- aggregate(cbind(DeltaC,DeltaN)~Type,Isotopes,mean)
f         <- function(z)sd(z)/sqrt(length(z)) # function to calculate std.err
se        <- aggregate(cbind(se.x=DeltaC,se.y=DeltaN)~Type,Isotopes,f)
centroids <- merge(centroids,se, by="Type")

write.csv(centroids, "Data/isotope_isotopes.csv")

isotopes <- ggplot(Isotopes, aes(x = DeltaC, y = DeltaN, 
                                 shape = Type, colour = Type)) +
  geom_point(size = 4, stroke = 1.5) +
  theme_minimal(base_size = 16) + 
  theme(panel.border = element_rect(fill = NA)) +
  ylim(-1, 6) +
  labs(x = expression(paste(delta^{13}, "C")),
       y = expression(paste(delta^{15}, "N"))) +
  scale_shape_manual(name = "Species & Treatment",
                     labels = c("Calamagrostis with competition",
                                "Calamagrostis without competition",
                                "Carex with competition",
                                "Carex without competition",
                                "Phragmites with competition",
                                "Phragmites without competition",
                                "Typha with competition",
                                "Typha without competition"),
                     values = c(0,15,1, 16,2, 17, 5,18)) +
  scale_colour_manual(name = "Species & Treatment",
                      labels = c("Calamagrostis with competition",
                                 "Calamagrostis without competition",
                                 "Carex with competition",
                                 "Carex without competition",
                                 "Phragmites with competition",
                                 "Phragmites without competition",
                                 "Typha with competition",
                                 "Typha without competition"),
                      values = colours) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) 

isotopes

iso.eror <- isotopes + 
  geom_errorbar(data = centroids,
                aes(ymin = DeltaN - se.y, ymax = DeltaN + se.y,
                    colour = Type), 
                width = 0.1, size = 1,
                show.legend = F)+
  geom_errorbarh(data = centroids, 
                 aes(xmin = DeltaC - se.x, xmax = DeltaC + se.x,
                     colour = Type),
                 height = 0.1, size = 1,
                 show.legend = F) +
  geom_point(data = centroids, aes(colour = Type),
             size = 6, stroke = 1.5,
             show.legend = F) 

iso.eror 
  
colnames(Isotopes)

cent <- aggregate(cbind(C,N)~Type,Isotopes,mean)
f         <- function(z)sd(z)/sqrt(length(z)) # function to calculate std.err
ser        <- aggregate(cbind(se.x=C,se.y=N)~Type,Isotopes,f)
cent.nut <- merge(cent,ser, by="Type")


nutrient <- ggplot(Isotopes, aes(x = C, y = N, shape = Type, 
                                 colour = Type)) +
  geom_point(size = 5, stroke = 1.5) +
  theme_minimal(base_size = 16) + 
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none") +
  xlab("Total % Carbon") +
  ylab("Total % Nitrogen") +
  scale_shape_manual(values = c(0,15,1, 16,2, 17, 5,18)) +
  scale_colour_manual(values = colours) 

nutrient.eror <- nutrient +
  geom_errorbar(data = cent.nut,
                aes(ymin = N - se.y, ymax = N + se.y,
                    colour = Type), 
                width = 0.1, size = 1)+
  geom_errorbarh(data = cent.nut, 
                 aes(xmin = C - se.x, xmax = C + se.x,
                     colour = Type),
                 height = 0.1, size = 1) +
  geom_point(data = cent.nut, aes(colour = Type),
             size = 6, stroke = 1.5)

nutrient.eror

nutiso <- nutrient.eror + iso.eror +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')


ggsave("Figures/Plant_Nutrients.TIFF", nutiso,
       height = 6.67,
       width = 15.5,
       dpi = 300) #15.5 x 6.67


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



# Jitter plot and error ---------------------------------------------------

nut.den.jit <- ggplot(Isotopes, 
                      aes(x = Treatment, y = N,
                          colour = Treatment,
                          shape = Treatment)) +
  geom_jitter(width = 0.05,
              size = 2) +
  facet_wrap("Species") + 
  scale_fill_manual(values = c("#fb6a4a", "white")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total % Nitrogen") +
  scale_colour_manual(values = c("#762a83", "#1b7837")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black") 

nut.den.jit


nut.c.jit <- ggplot(Isotopes, 
                      aes(x = Treatment, y = C,
                          colour = Treatment,
                          shape = Treatment)) +
  geom_jitter(width = 0.05,
              size = 2) +
  facet_wrap("Species") + 
  scale_fill_manual(values = c("#fb6a4a", "white")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Total % Carbon") +
  scale_colour_manual(values = c("#762a83", "#1b7837")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black") 

nut.c.jit




(isotope.nut.jit <- ggarrange(iso.eror, nut.den.jit,
                              legends, nut.c.jit,
                              labels = c("A","B"," ","C")))


ggsave("Figures/Isotopes_Nutrient_jitter.TIFF", isotope.nut.jit,
       dpi = 300)



# no facet wrap -----------------------------------------------------------

iso.resident <- Isotopes %>% filter(Species %in% c("Calamagrostis", "Carex","Typha"))
iso.phrag <- Isotopes %>% filter(Species == "Phragmites")

# Resident species

N.resident <- ggplot(iso.resident, 
       aes(x = Species, y = N)) +
  geom_jitter(
         aes(shape = Treatment, color = Treatment), 
         position = position_jitterdodge(jitter.width = 0.2,
                                         dodge.width = 0.8),
         size = 4,
         alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Nitrogen (%)") +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(0,4)


C.resident <- ggplot(iso.resident, 
       aes(x = Species, y = C)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 4,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Carbon (%)") +
  scale_colour_manual(values = c("#24908C", "#3A518B")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(43, 53)


resident.CN <- ggarrange(C.resident, N.resident,
                         labels = c("C","D"))

resident.CN.label <- annotate_figure(resident.CN,
                top = text_grob("Resident phytometers"))


### Phragmites

N.phrag <- ggplot(iso.phrag, 
       aes(x = Neighbours, y = N)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 5,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA),
        legend.position = c(0.6, 0.2)) +
  labs(x = " ",
       y = "Nitrogen (%)") +
  scale_colour_manual(name = "Treatment",
                      labels = c("With competition",
                                 "Without competition"),
                                 values = c("#454ADE", "#440C53")) +
  scale_shape_manual(name = "Treatment",
                      labels = c("With competition",
                                 "Without competition"),
                      values = c(16, 17)) +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(0, 4) 
  



C.phrag <- ggplot(iso.phrag, 
       aes(x = Neighbours, y = C)) +
  geom_jitter(
    aes(shape = Treatment, color = Treatment), 
    position = position_jitterdodge(jitter.width = 0.2,
                                    dodge.width = 0.8),
    size = 5,
    alpha = 0.7)  +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        panel.border = element_rect(fill = NA)) +
  labs(x = " ",
       y = "Carbon (%)") +
  scale_colour_manual(values = c("#454ADE", "#440C53")) +
  theme(legend.position = "none") +
  stat_summary(aes(shape = Treatment, size = 0.5),
               fun.data = "mean_se", fun.args = list(mult = 1), 
               geom = "pointrange", size = 1,
               colour = "black",
               position = position_dodge(0.8)) +
  ylim(43, 53)

phrag.CN <- ggarrange(C.phrag, N.phrag,
                         labels = c("E","F"))

phrag.CN.label <- annotate_figure(phrag.CN,
                top = text_grob("Phragmites phytometers and neighbours"))


CN.panel <- ggarrange(resident.CN.label, phrag.CN.label,
          ncol=2)

ggsave("Figures/nutrient_panels.jpeg", CN.panel,
       width = 9.6,
       height = 8.6,
       dpi = 300,
       units = "in")


figures <- ggarrange(resident.CN.label, iso.eror, 
          phrag.CN.label, legends,
          labels = c("","E","",""),
          vjust = 3,
          hjust = -6)

ggsave("Figures/panels_nutrient_good.jpeg",
       width = 13.1,
       height = 8.65,
       units = "in",
       dpi = 300)


scatt.jitt <- ggarrange(leaf.nutrient,CN.panel,
          nrow = 2)

scatt.jitt


ggsave("Figures/isotope_nutrients_scatter_jitter.jpeg", 
       scatt.jitt,
       width = 13.8,
       height = 9.24,
       units = "in") #13.8 x 9.24 in



# Patchwork  --------------------------------------------------------------

library(patchwork)

p1 <- nutrient.eror 
p2 <- iso.eror
l <- legends

p3 <- C.resident <- C.resident + ggtitle("Resident species")
p4 <- N.resident <- N.resident + ggtitle("Resident species")
p5 <- C.phrag <- C.phrag + ggtitle("Phragmites with neighbours")
p6 <- N.phrag <- N.phrag + ggtitle("Phragmites with neighbours")



p2l <- p2 + inset_element(l, left = 0, bottom = 0.6, right = 0.4, top = 1, align_to = 'full')



plot <- p1 + p3 + p4 + p2 + p5 + p6 +
  plot_layout(widths = c(1, 0.5, 0.5))


plot 

ggsave("Figures/Isotope_Nutrients_patch.jpeg",plot,
       dpi = 300)

ggsave("Figures/Isotope_Nutrients_legend.jpeg", legends,
       dpi = 300)