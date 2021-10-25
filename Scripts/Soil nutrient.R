
# Load packages -----------------------------------------------------------

library(tidyverse)
library(vegan)
library(ggrepel)
library(Hmisc)
library(ggpubr)


# Load data ---------------------------------------------------------------

soil <- read.csv("Data/soil_nutrients.csv")
dim(soil)
colnames(soil)

soils <- soil %>% select(Moisture:Light)# just the soil nutrient data
env <- soil %>% select(Vegetation:Species) # species is the group factor

hist.data.frame(soils)


# Data sum ----------------------------------------------------------------
install.packages("plotrix")
library(plotrix)

?std.error

sum <- soil %>% group_by(Species) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, SE = std.error), na.rm = TRUE,
    .names = "{col}_{fn}"
  ))

sum <- sum %>% t %>% as.data.frame
write.csv(sum, "Data/soil_nutrients_sum.csv")


#Species       Calamagrostis      Carex Phragmites      Typha

#Moisture_Mean      736466.7   791600.0   782266.7   819533.3
#Moisture_SD        59532.54   35936.05  148920.14   49816.62
#Moisture_SE       15371.237   9278.649  38451.016  12862.595

#Cu_Mean            3.083333   2.238733   2.206667   4.872667
#Cu_SD              1.108452   1.787405   1.077886   2.482653
#Cu_SE             0.2862012  0.4615060  0.2783090  0.6410181

#Fe_Mean            107.0280   130.2933   167.8400   398.9333
#Fe_SD              41.06576   57.69327   75.23168  146.35547
#Fe_SE              10.60313   14.89634   19.42474   37.78882

#Mn_Mean            14.48800   35.34000   47.63133   50.16667
#Mn_SD              3.150778  18.904225  50.186376  23.594511
#Mn_SE             0.8135274  4.8810498 12.9580665  6.0920766

#P_Mean             9.201333  14.733333  18.806667  35.600000
#P_SD               2.905775   3.326660  10.539278  20.282998
#P_SE              0.7502678  0.8589399  2.7212299  5.2370475

#Mg_Mean            85.68667   96.20000  179.86667  424.00000
#Mg_SD              24.92747   29.24576   83.53431  194.96520
#Mg_SE              6.436244   7.551222  21.568466  50.339798

#K_Mean             34.15800   45.93333   46.66667  128.60000
#K_SD               11.79645   17.61277   25.03902   68.80386
#K_SE               3.045830   4.547597   6.465046  17.765081

#pH_Mean            7.400000   7.026667   6.940000   6.580000
#pH_SD             0.1362770  0.2404361  0.3065942  0.4246007
#pH_SE            0.03518658 0.06208034 0.07916228 0.10963142

#Zn_Mean            2.282000   4.904667   5.328000  11.134667
#Zn_SD             0.7180052  1.4208693  4.3714744  5.9171625
#Zn_SE             0.1853881  0.3668669  1.1287098  1.5278048

#C_Mean             42440.00   55866.67   78520.00  153060.00
#C_SD               12919.96   24593.95   39287.46   49581.60
#C_SE               3335.920   6350.131  10143.979  12801.915

#TN_Mean            1940.000   2860.000   4666.667  10806.667
#TN_SD              1159.310   1948.919   2945.133   4705.842
#TN_SE              299.3326   503.2088   760.4301  1215.0433

#Na_Mean            17.70533   39.68000   44.57333   84.90667
#Na_SD              2.757721  19.118846  11.990917  55.979658
#Na_SE             0.7120406  4.9364649  3.0960415 14.4538855

#Ca_Mean            2981.544   4338.200   5061.600   7767.333
#Ca_SD              412.0649   594.9528  1421.9763  2410.5545
#Ca_SE              106.3947   153.6162   367.1527   622.4025

#S_Mean             362.0000   635.3333  1136.6667  2586.6667
#S_SD               83.76839  223.92176  542.75312 1394.82035
#S_SE               21.62890   57.81635  140.13825  360.14107

#Light_Mean         8.086667  16.060000   2.746667   6.193333
#Light_SD           8.352319  22.527279   3.224874   5.998865
#Light_SE          2.1565596  5.8165183  0.8326588  1.5489003


soil %>% group_by(Species) %>% 
  summarise(CV = cv(Moisture))

# Log transformation ------------------------------------------------------

soils.log <- soils %>% 
  mutate(Moist.log = log(Moisture + 1),
         Cu.log = log(Cu+1),
         Fe.log = log(Fe+1),
         Mn.log = log(Mn+1),
         P.log = log(P+1),
         Mg.log = log(Mg+1),
         K.log = log(K+1),
         Zn.log = log(Zn+1),
         C.log = log(C+1),
         TN.log = log(TN+1),
         Na.log = log(Na+1),
         Ca.log = log(Ca+1),
         S.log = log(S+1),
         Light.log= log(Light+1))

soils.log <- soils.log %>% select(Moist.log:Light.log)

soils.log$pH <- soil$pH
colnames(soils.log)
dim(soils.log)

hist.data.frame(soils.log) # looks better go with this one
dim(soils.log)

write.csv(soils.log,"Data/soil_nutrients_logtransform.csv")


# PCA ---------------------------------------------------------------------
soils.log <- read.csv("Data/soil_nutrients_logtransform.csv")

pca <- rda(soils.log, scale = TRUE) 
head(summary(pca))

#Partitioning of correlations:
#  Inertia Proportion
#Total              15          1
#Unconstrained      15          1

#Eigenvalues, and their contribution to the correlations 

#Importance of components:
#                      PC1     PC2     PC3     PC4     PC5
#Eigenvalue            9.720 1.28635 1.05100 0.90016 0.55455
#Proportion Explained  0.648 0.08576 0.07007 0.06001 0.03697
#Cumulative Proportion 0.648 0.73377 0.80383 0.86384 0.90081



summary(eigenvals(pca))

#Importance of components:
#                       PC1     PC2     PC3     PC4     PC5     PC6     PC7    PC8      PC9     PC10
#Eigenvalue            9.720 1.28635 1.05100 0.90016 0.55455 0.38612 0.30406 0.2355 0.146933 0.142682
#Proportion Explained  0.648 0.08576 0.07007 0.06001 0.03697 0.02574 0.02027 0.0157 0.009796 0.009512
#Cumulative Proportion 0.648 0.73377 0.80383 0.86384 0.90081 0.92655 0.94683 0.9625 0.972323 0.981836

par(1,1)
screeplot(pca, bstick = TRUE, type = "l",
          main = NULL)

biplot(pca) 
summary(pca)

pca.scores.sites <- as.data.frame(scores(pca, choices = c(1,2,3), display = "sites")) # coordinates for points
pca.scores.sites$Species <- as.factor(env$Species) # add group factor

write.csv(pca.scores.sites,"Data/PCA scores_123.csv") #use this with NicheRover


pca.scores.spp <- as.data.frame(scores(pca, choices = c(1,2,3), display = c("species"))) # vectors
pca.scores.spp$labels <- row.names(pca.scores.spp) # adding in as the label

write.csv(pca.scores.spp, "Data/PCA scores_spp.csv")



### Just the Figure ####

pca.spp <- read.csv("Data/PCA scores_spp.csv")

pca.sites <- read.csv("Data/PCA scores_123.csv")


colours = c("Calamagrostis" = "#084594", 
            "Typha" = "#6e016b", 
            "Carex" = "#9ecae1", 
            "Phragmites" = "#fb6a4a")


p <-
  pca.sites %>%
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, 
                 fill = Species,
                 shape = Species), 
             size = 6, stroke = 1.5,
             alpha = 0.8) +
    stat_ellipse(data = pca.sites,
               aes(x = PC1, y = PC2, 
                   colour = Species),
               level = 0.9) +
  geom_segment(data = pca.spp, 
               aes(x = 0, xend = PC1, 
                   y = 0, yend = PC2), 
               arrow = arrow(length = unit(0.35, "cm"), # adjust the arrow head size here
                             type="closed"), # can change to open 
               color = "black") + 
  geom_label_repel(data = pca.spp, 
                 aes(x = PC1, y = PC2, label = labels1),
                 color = "black",
                 size = 7,
                 force = 2) +
  ylim(-2, 2.2) +
  xlim(-2, 2.2) +
  theme_classic() +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  labs(x = "PC1 (64.8%)",
       y = "PC2 (8.6%)") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 13))
p


ggsave("Figures/soils_PCA_axis12.jpeg", p)



library(ggbiplot)

unique(env$Species)

soil.pca <- prcomp(soils.log, scale. = TRUE)

PCA.biplot <- ggbiplot(soil.pca, 
         groups = env$Species, ellipse = TRUE,
         varname.size = 5) +
  theme(legend.direction = 'vertical', legend.position = 'right') +
  geom_point(aes(colour = env$Species, shape = env$Species), size = 5) +
  theme_classic(base_size = 12) +
  scale_shape_manual(name = " ",
                     labels = c("Calamagrostis",
                                "Carex",
                                "Phragmites",
                                "Typha"),
                     values = c(15, 16, 17, 18)) +
  scale_colour_manual(name = " ",
                     labels = c("Calamagrostis",
                                "Carex",
                                "Phragmites",
                                "Typha"),
                     values = colours) 

ggsave("Figures/soils_biplot.jpeg", PCA.biplot)
