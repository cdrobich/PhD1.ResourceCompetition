
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

write.csv(soils.log,"Data/soil_nutrients_logtransform.csv")


# PCA ---------------------------------------------------------------------

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
  scale_colour_manual(values = colours) +
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
