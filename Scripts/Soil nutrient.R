library(tidyverse)
library(vegan)
library(ggrepel)
library(Hmisc)

soil <- read.csv("Data/soil_nutrients.csv")
dim(soil)
colnames(soil)

soils <- soil[,3:17] # just the soil nutrient data
env <- soil[,1:2] # species is the group factor

hist.data.frame(soils)

# CHECK THEM AND LOG + 10 TRANSFORM

soils.log <- soils %>% 
  mutate(Cu.log = log(Cu+1),
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


soils.log <- soils.log[,16:29]

soils.log$pH <- soil$pH

hist.data.frame(soils.log)

pca <- rda(soils.log, scale = TRUE) 
summary(pca)

summary(eigenvals(pca))



#Importance of components:
#                        PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8
#Eigenvalue            10.6224 1.23140 1.01816 0.56270 0.39867 0.30950 0.26292 0.15434
#Proportion Explained   0.7082 0.08209 0.06788 0.03751 0.02658 0.02063 0.01753 0.01029
#Cumulative Proportion  0.7082 0.79025 0.85813 0.89564 0.92222 0.94285 0.96038 0.97067

#                       PC9    PC10     PC11     PC12     PC13     PC14      PC15
#Eigenvalue            0.148458 0.08805 0.080003 0.062670 0.028124 0.017946 0.0146728
#Proportion Explained  0.009897 0.00587 0.005334 0.004178 0.001875 0.001196 0.0009782
#Cumulative Proportion 0.980569 0.98644 0.991772 0.995950 0.997825 0.999022 1.0000000



screeplot(pca, bstick = TRUE, type = "l",
          main = NULL)

biplot(pca) 


pca.scores.sites <- as.data.frame(scores(pca, choices = c(1,2,3), display = "sites")) # coordinates for points
pca.scores.sites$Species <- as.factor(env$Species) # add group factor

write.csv(pca.scores.sites,"Data/PCA scores_123.csv") #use this with NicheRover


pca.scores.spp <- as.data.frame(scores(pca, display = c("species"))) # vectors
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
  #ylim(-2, 2) +
  xlim(-2, 2.2) +
  theme_classic() +
  scale_shape_manual(values = c(0, 1, 2, 5)) +
  scale_colour_manual(values = c("#525252","#9ecae1",
                                 "#fb6a4a","#6a51a3")) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed() +
  labs(x = "PC1 (0.713)",
       y = "PC2 (0.087)") 

p


ggsave("Figures/PCA_soil.TIFF", p,
       dpi = 300)



