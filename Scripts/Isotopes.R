library(tidyverse)
library(ggpubr)

Isotopes <- read.csv("Data/Isotopes.csv")

colnames(Isotopes)




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

isotopes <- ggplot(Isotopes, aes(x = DeltaC, y = DeltaN, shape = Type, colour = Type)) +
  geom_point(size = 5, stroke = 1.5) +
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

isotopes

legend <- get_legend(isotopes)
legends <- as_ggplot(legend)

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
  scale_fill_manual(values = c("#8c96c6", "white")) +
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
  scale_colour_manual(values = c("#8c96c6", "grey")) +
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
  scale_colour_manual(values = c("#8c96c6", "grey")) +
  theme(legend.position = c(0.85, 0.8))

nut.cden


isotope.nut.den <- ggarrange(isotopes, nut.den,
          legends, nut.cden,
          labels = c("A","B"," ","C"))

ggsave("Figures/Isotopes_Nutrient_density.TIFF", isotope.nut.den,
       dpi = 300)
