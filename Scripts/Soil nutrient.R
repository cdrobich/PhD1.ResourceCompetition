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
         Light.log= log(Light+1),
         pH = pH)


soils.log <- soils.log[,16:29]

hist.data.frame(soils.log)

soils.log$pH <- soil$pH

pca <- rda(soils.log, scale = TRUE) 
summary(pca)

summary(eigenvals(pca))

biplot(pca) 


screeplot(pca, bstick = TRUE, type = "l",
          main = NULL)


pca.scores.sites <- as.data.frame(scores(pca, display = "sites")) # coordinates for points
pca.scores.sites$Species <- as.factor(env$Species) # add group factor


pca.scores.spp <- as.data.frame(scores(pca, display = c("species"))) # vectors
pca.scores.spp$labels <- row.names(pca.scores.spp) # adding in as the label

p <-
  pca.scores.sites %>%
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, 
                 colour = Species,
                 shape = Species), size = 5, stroke = 1.5) +
    stat_ellipse(data = pca.scores.sites,
               aes(x = PC1, y = PC2, 
                   colour = Species, 
                   linetype = Species)) +
  geom_segment(data = pca.scores.spp, 
               aes(x = 0, xend = PC1, 
                   y = 0, yend = PC2), 
               arrow = arrow(length = unit(0.35, "cm"), # adjust the arrow head size here
                             type="closed"), # can change to open 
               color = "black") + 
  geom_text_repel(data = pca.scores.spp, 
                 aes(x = PC1, y = PC2, label = labels),
                 color="black",
                 size = 6) +
  ylim(-2.5, 2.5) +
  xlim(-2.5, 2.5) +
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



