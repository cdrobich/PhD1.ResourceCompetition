library(tidyverse)
library(vegan)
library(ggrepel)
library(Hmisc)
library(ggpubr)

soil <- read.csv("Data/soil_nutrients.csv")
dim(soil)
colnames(soil)

soils <- soil %>% select(Moisture:Light)# just the soil nutrient data
env <- soil %>% select(Vegetation:Species) # species is the group factor

hist.data.frame(soils)

# CHECK THEM AND LOG + 10 TRANSFORM

soils.log <- soils %>% 
  mutate(Moist.log = log(Moisture -1),
         Cu.log = log(Cu+1),
         Fe.log = log(Fe+1),
         Mn.log = log(Mn+1),
         P.log = log(P+1),
         Mg.log = log(Mg+1),
         K.log = log(K+1),
         N.log = log(N+1),
         Zn.log = log(Zn+1),
         PC.log = log(PC+1),
         TN.log = log(TN+1),
         Na.log = log(Na+1),
         Ca.log = log(Ca+1),
         S.log = log(S+1),
         Light.log= log(Light+1))


soils.log <- soils.log %>% select(Moist.log:Light.log)

soils.log$pH <- soil$pH

hist.data.frame(soils.log) # looks better go with this one


## Tidy keeps freaking out when i try to transform so I did it in excel

soils.tr <- read.csv("Data/soil_nutrients_transform.csv")
soils.log <- soils.tr %>% select(Moisture:pH)# just the soil nutrient data
env <- soils.tr %>% select(Vegetation:Species)

pca <- rda(soils.log, scale = TRUE) 
summary(pca)

#Call:
#  rda(X = soils.log, scale = TRUE) 

#Partitioning of correlations:
#                 Inertia Proportion
#Total              16          1
#Unconstrained      16          1

#Eigenvalues, and their contribution to the correlations 

#Importance of components:
#                        PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8      PC9     PC10     PC11   PC12     PC13
#Eigenvalue            10.6775 1.28773 1.05527 0.90061 0.55561 0.38778 0.30447 0.24715 0.148847 0.146857 0.087963 0.0800 0.062438
#Proportion Explained   0.6673 0.08048 0.06595 0.05629 0.03473 0.02424 0.01903 0.01545 0.009303 0.009179 0.005498 0.0050 0.003902
#Cumulative Proportion  0.6673 0.74783 0.81378 0.87007 0.90480 0.92903 0.94806 0.96351 0.972812 0.981990 0.987488 0.9925 0.996390

#PC14     PC15      PC16
#Eigenvalue            0.025712 0.017380 0.0146676
#Proportion Explained  0.001607 0.001086 0.0009167
#Cumulative Proportion 0.997997 0.999083 1.0000000

#Scaling 2 for species and site scores
#* Species are scaled proportional to eigenvalues
#* Sites are unscaled: weighted dispersion equal on all dimensions
#* General scaling constant of scores:  5.542976 


summary(eigenvals(pca))

#Importance of components:

#                        PC1     PC2     PC3     PC4     PC5     PC6     PC7
#Eigenvalue            10.6775 1.28773 1.05527 0.90061 0.55561 0.38778 0.30447
#Proportion Explained   0.6673 0.08048 0.06595 0.05629 0.03473 0.02424 0.01903
#Cumulative Proportion  0.6673 0.74783 0.81378 0.87007 0.90480 0.92903 0.94806

#PC8      PC9     PC10     PC11   PC12     PC13     PC14
#Eigenvalue            0.24715 0.148847 0.146857 0.087963 0.0800 0.062438 0.025712
#Proportion Explained  0.01545 0.009303 0.009179 0.005498 0.0050 0.003902 0.001607
#Cumulative Proportion 0.96351 0.972812 0.981990 0.987488 0.9925 0.996390 0.997997

#PC15      PC16
#Eigenvalue            0.017380 0.0146676
#Proportion Explained  0.001086 0.0009167
#Cumulative Proportion 0.999083 1.0000000

par(1,1)
screeplot(pca, bstick = TRUE, type = "l",
          main = NULL)

biplot(pca) 


pca.scores.sites <- as.data.frame(scores(pca, choices = c(1,2,3), display = "sites")) # coordinates for points
pca.scores.sites$Species <- as.factor(env$Species) # add group factor

write.csv(pca.scores.sites,"Data/PCA scores_123.csv") #use this with NicheRover


pca.scores.spp <- as.data.frame(scores(pca, choices = c(1,2,3), display = c("species"))) # vectors
pca.scores.spp$labels <- row.names(pca.scores.spp) # adding in as the label

write.csv(pca.scores.spp, "Data/PCA scores_spp.csv")



### Just the Figure ####

pca.spp <- read.csv("Data/PCA scores_spp.csv")

pca.sites <- read.csv("Data/PCA scores_123.csv")


p <-
  pca.sites %>%
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, 
                 colour = Species,
                 shape = Species), size = 5, stroke = 1.5) +
    stat_ellipse(data = pca.sites,
               aes(x = PC1, y = PC2, 
                   colour = Species, 
                   linetype = Species)) +
  geom_segment(data = pca.spp, 
               aes(x = 0, xend = PC1, 
                   y = 0, yend = PC2), 
               arrow = arrow(length = unit(0.35, "cm"), # adjust the arrow head size here
                             type="closed"), # can change to open 
               color = "black") + 
  geom_text_repel(data = pca.spp, 
                 aes(x = PC1, y = PC2, label = labels),
                 color="black",
                 size = 6) +
  #ylim(-2, 2.2) +
  #xlim(-2, 2.2) +
  theme_classic() +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  scale_colour_manual(values = c("#525252","#9ecae1",
                                 "#fb6a4a","#6a51a3")) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed() +
  labs(x = "PC1 (0.667)",
       y = "PC2 (0.080)") +
  theme(legend.position = c(0.95,0.9),
        legend.background = element_rect(linetype = 2, 
                                         size = 0.1, colour = 1))

p


ggsave("Figures/soils_PCA_axis12.jpeg", p)


p3 <-
  pca.sites %>%
  ggplot()+
  geom_point(aes(x = PC1, y = PC3, 
                 colour = Species,
                 shape = Species), size = 5, stroke = 1.5) +
  stat_ellipse(data = pca.sites,
               aes(x = PC1, y = PC3, 
                   colour = Species, 
                   linetype = Species)) +
  geom_segment(data = pca.spp, 
               aes(x = 0, xend = PC1, 
                   y = 0, yend = PC3), 
               arrow = arrow(length = unit(0.35, "cm"), # adjust the arrow head size here
                             type="closed"), # can change to open 
               color = "black") + 
  geom_text_repel(data = pca.spp, 
  aes(x = PC1, y = PC3, label = labels),
  color="black",
  size = 6) +
  #ylim(-2, 2) +
  #xlim(-2, 2.2) +
  theme_classic() +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  scale_colour_manual(values = c("#525252","#9ecae1",
                                 "#fb6a4a","#6a51a3")) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed() +
  labs(x = "PC1 (0.667)",
       y = "PC3 (0.066)") 

p3


PCA <- ggarrange(p,p3,
          common.legend = TRUE,
          labels = "AUTO",
          legend = "bottom",
          align = c("hv"))


ggsave("Figures/PCA_soil_axes.TIFF", PCA,
       dpi = 300)
