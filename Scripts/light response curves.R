library(tidyverse)

ciras <- read.csv("data/CIRAS_sum_bothyears.csv", header = TRUE)

#### Light Response Curves from Heberling #####
# Heberling JM, Brouwer NL, Kalisz S. 2017. Effects of deer on the photosynthetic performance of invasive and native forest
# herbs. AoB PLANTS 9: plx011; doi:10.1093/aobpla/plx011

# 'leaf level photosynthesis' https://sites.google.com/site/fridleylab/home/protocols for code"

lrc <- read.csv("data/sample_lrc.txt",sep="",skip=16)

PARlrc <- lrc$PARi #PAR (aka PPFD or Q)
photolrc <- lrc$Photo #net photosynthetic rate (Anet)

curvelrc <- data.frame(PARlrc,photolrc)
curvelrc # *inspect raw data and check notebook (data reasonable or need edited/discarded?)

par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=1.5)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2.5,cex=1.5)

curve.nlslrc = nls(photolrc ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))-Rd,
                   start=list(Am=(max(photolrc)-min(photolrc)),
                              AQY=0.05,
                              Rd=-min(photolrc), theta=1)) 


par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=2)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2,cex=2)
curve((1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1],lwd=2,col="blue",add=T)

# ---Solve for light compensation point (LCPT), PPFD where Anet=0 ---
x <-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1]}

uniroot(x,c(0,50),extendInt = "yes")$root #Light compensation point

# ---Solve for light saturation point (LSP), PPFD where 75% of Amax is achieved (75% is arbitrary - cutoff could be changed)
y <-function(y) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*y+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*y+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*y))-summary(curve.nlslrc)$coef[3,1]-(0.75*summary(curve.nlslrc)$coef[1,1])+0.75*(summary(curve.nlslrc)$coef[3,1])}

uniroot(y,c(0,1000),extendInt = "yes")$root #Light saturation point




# My data -----------------------------------------------------------------

comp_lrc <- ciras %>% 
  select(ID_yr, light, carbon) %>% 
  pivot_wider(names_from = ID_yr, values_from = carbon)

PARlrc <- comp_lrc$light

com_lrc <- comp_lrc[,-1]

write.csv(com_lrc, "Data/organized_ciras_lrc.csv")

glimpse(com_lrc)



# Trouble shooting --------------------------------------------------------

#CAP9CU_2017 LSP of 10,000

photolrc <- com_lrc$CAP9CU_2017 #net photosynthetic rate (Anet)

curvelrc <- data.frame(PARlrc,photolrc)
curvelrc # *inspect raw data and check notebook (data reasonable or need edited/discarded?)

par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=1.5)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2.5,cex=1.5)

curve.nlslrc = nls(photolrc ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))-Rd,start=list(Am=(max(photolrc)-min(photolrc)),AQY=0.05,Rd=-min(photolrc),theta=1)) 


par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=2)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2,cex=2)
curve((1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1],lwd=2,col="blue",add=T)






# Jody code ---------------------------------------------------------------


results <-
  do.call(rbind,
          map(.x = colnames(com_lrc), ~{
            
            photo <- com_lrc%>% select(!!sym(.x))%>% purrr::reduce(c)
            
            model <-
              tryCatch(nls(photo ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))-Rd,start=list(Am=(max(photo)-min(photo)),
                                                                                                                             AQY=0.05,Rd=-min(photo),theta=1)), error = function(e){
                                                                                                                               NULL
                                                                                                                             })
            
            LCPT <-function(x)
            {
              (1/(2*summary(model)$coef[4,1]))*(summary(model)$coef[2,1]*x+summary(model)$coef[1,1]-sqrt((summary(model)$coef[2,1]*x+summary(model)$coef[1,1])^2-4*summary(model)$coef[2,1]*summary(model)$coef[4,1]*summary(model)$coef[1,1]*x))-summary(model)$coef[3,1]}
            
            
            LSP <-function(y) {(1/(2*summary(model)$coef[4,1]))*(summary(model)$coef[2,1]*y+summary(model)$coef[1,1]-sqrt((summary(model)$coef[2,1]*y+summary(model)$coef[1,1])^2-4*summary(model)$coef[2,1]*summary(model)$coef[4,1]*summary(model)$coef[1,1]*y))-summary(model)$coef[3,1]-(0.75*summary(model)$coef[1,1])+0.75*(summary(model)$coef[3,1])}
            
            
            if(is.null(model)){
              
              results <- data.frame(Variable = .x,
                                    LCPT = "NA",
                                    LSP = "NA")
              
            }
            
            
            else{
              
              results <- data.frame(Variable = .x,
                                    LCPT = uniroot(LCPT,c(0,50), extendInt = "yes")$root,
                                    LSP = uniroot(LSP,c(0,1000), extendInt = "yes")$root)
              
              
            }
            
            
          }
          )
  )


results


write.csv(results, "Data/lrc_results.csv")



# Import LRC values -------------------------------------------------------

library(patchwork)

lrc_data <- read.csv("Data/lrc_results_env.csv")

lrc_data <- lrc_data %>% 
  unite("type", Species,Neighbour, remove = FALSE)

res <- c("Carex", "Calamagrostis", "Typha")
resident <- lrc_data %>% filter(Species %in% res)
phrag <- lrc_data %>% filter(Species == "Phragmites")


sum.LCPT <- lrc_data %>% 
  group_by(type, Treatment) %>% 
  summarise(median = median(LCPT, na.rm = TRUE),
            mean = mean(LCPT, na.rm = TRUE),
            LCPT.sd = sd(LCPT, na.rm = TRUE),
            N = length(LCPT),
            sterr = (LCPT.sd/sqrt(N)))

#  type                    Treatment      median   mean  LCPT.sd     N sterr

#1 Calamagrostis_Phragmites Competition      19.9  23.6     24.2    24  4.95
#2 Calamagrostis_Phragmites No competition   22.9  23.7     18.5    21  4.03

#3 Carex_Phragmites         Competition      31.6  29.7     20.3    15  5.25
#4 Carex_Phragmites         No competition   46.1  38.0     24.6    13  6.83

#5 Phragmites_Calamagrostis Competition      30.6  17.6     46.9    12 13.5 
#6 Phragmites_Calamagrostis No competition   35.4  35.9     25.8    12  7.44

#7 Phragmites_Carex         Competition      38.4  31.5     23.8    12  6.87
#8 Phragmites_Carex         No competition   33.5  35.8     17.2    11  5.18

#9 Phragmites_Typha         Competition      11.1  -9.83    63.9    12 18.5 
#10 Phragmites_Typha        No competition   29.5  18.2     23.8    10  7.52

#11 Typha_Phragmites         Competition      38.0  25.0     32.1    12  9.27
#12 Typha_Phragmites         No competition   28.8 -10.7     94.7    12 27.3 

phrag %>% 
  group_by(type, Treatment) %>% 
  summarise(median = median(LCPT, na.rm = TRUE),
            mean = mean(LCPT, na.rm = TRUE),
            LCPT.sd = sd(LCPT, na.rm = TRUE),
            N = length(LCPT),
            sterr = (LCPT.sd/sqrt(N)))

##  type                     Treatment      median  mean LCPT.sd     N sterr
##1 Phragmites_Calamagrostis Competition      30.6 17.6     46.9    12 13.5 
##2 Phragmites_Calamagrostis No competition   35.4 35.9     25.8    12  7.44
##3 Phragmites_Carex         Competition      38.4 31.5     23.8    12  6.87
##4 Phragmites_Carex         No competition   33.5 35.8     17.2    11  5.18
##5 Phragmites_Typha         Competition      11.1 -9.83    63.9    12 18.5 
##6 Phragmites_Typha         No competition   29.5 18.2     23.8    10  7.52


sum.LSP <- lrc_data %>% 
  group_by(type, Treatment) %>% 
  summarise(median = median(LSP, na.rm = TRUE),
            mean = mean(LSP, na.rm = TRUE),
            LSP.sd = sd(LSP, na.rm = TRUE),
            N = length(LSP),
            sterr = (LSP.sd/sqrt(N)))

#type                       Treatment      median  mean LSP.sd     N sterr
#1 Calamagrostis_Phragmites Competition      521.  712.  721.     24 147. 
#2 Calamagrostis_Phragmites No competition   623.  683.  292.     21  63.6

#3 Carex_Phragmites         Competition      577.  495.  194.     15  50.1
#4 Carex_Phragmites         No competition   651.  699.  169.     13  46.7

#5 Phragmites_Calamagrostis Competition      870.  861.  300.     12  86.5
#6 Phragmites_Calamagrostis No competition   827.  888.  277.     12  80.1

#7 Phragmites_Carex         Competition      766.  748.  139.     12  40.2
#8 Phragmites_Carex         No competition   886.  855.  172.     11  51.9

#9 Phragmites_Typha         Competition      870.  835.  332.     12  95.8
#10 Phragmites_Typha         No competition  1068.  980.  308.     10  97.3

#11 Typha_Phragmites         Competition      564.  556.   94.7    12  27.3
#12 Typha_Phragmites         No competition   595.  832.  539.     12 156. 





# plot --------------------------------------------------------------------


LCPT <- ggplot(resident, aes(x = Treatment,
                          y = LCPT)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.5) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  facet_wrap("type", scales = "free") + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Compensation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24))
  



LSP <- ggplot(resident, aes(x = Treatment,
                     y = LSP)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.5) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  facet_wrap("type", scales = "free") + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Saturation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24))


review.plot <- LCPT + LCPT_phrag + LSP + LSP_phrag +
  plot_annotation(tag_levels = "A")

ggsave("Figures/LSP_LCPT.jpeg", dpi = 300)





LSP_phrag <- ggplot(phrag, aes(x = Treatment,
                            y = LSP)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.5) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  facet_wrap("type", scales = "free") + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Saturation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24))


LCPT_phrag <- ggplot(phrag, aes(x = Treatment,
                             y = LCPT)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.5) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  facet_wrap("type", scales = "free") + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Compensation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24))


layout <- "
AAABBB
CCCDDD
DDEEFF"


anova <- LCPT + LCPT_phrag +
  LSP + LSP_phrag 

# anova -------------------------------------------------------------------

library(car)
library(agricolae)
library(performance)


lrc_data

#do a bunch of t-tests (3 - just by neighbours without competition included)

all_aov <- lm(LCPT ~ type, data = lrc_data)
Anova(all_aov)

#Anova Table (Type II tests)

#Response: LCPT
#                Sum Sq  Df F value  Pr(>F)  
#type            17605   5  2.5978 0.02782 *
#Treatment         786   1  0.5797 0.44768  
#type:Treatment  11958   5  1.7645 0.12381  
#Residuals      195174 144   


typeall <- HSD.test(all_aov, "type")



LCPT_aovres <- lm(LCPT ~ Species * Treatment, data = resident, na.rm = TRUE)

Anova(LCPT_aovres , type = 3)

#Anova Table (Type III tests)
#
#Response: LCPT
#                   Sum Sq Df F value  Pr(>F)   
#(Intercept)        13334  1 10.3409 0.00187 **
#Species              323  2  0.1253 0.88244   
#Treatment              0  1  0.0002 0.98816   
#Species:Treatment   5762  2  2.2342 0.11364   
#Residuals         104447 81            




LSP_aovres <- lm(LSP ~ Species * Treatment, data = resident, na.rm = TRUE)

Anova(LSP_aovres , type = 3)

#Anova Table (Type III tests)

#Response: LSP
#                    Sum Sq Df F value    Pr(>F)    
#(Intercept)       12181494  1 60.2101 2.232e-11 ***
#Species             453753  2  1.1214    0.3308    
#Treatment             9360  1  0.0463    0.8302    
#Species:Treatment   389893  2  0.9636    0.3859    
#Residuals         16387636 81  


# Phragmites
LCPT_aovph <- lm(LCPT ~ type * Treatment, data = phrag, na.rm = TRUE)

Anova(LCPT_aovph , type = 3)

#Response: LCPT
#Sum Sq Df F value  Pr(>F)  
#(Intercept)      3719  1  2.5822 0.11307  
#type            10629  2  3.6904 0.03051 *
#Treatment        2002  1  1.3904 0.24277  
#type:Treatment   1607  2  0.5578 0.57528  
#Residuals       90727 63  

type <- HSD.test(LCPT_aovph, "type")

#LCPT groups
#Phragmites_Carex         33.552141      a
#Phragmites_Calamagrostis 26.737704     ab
#Phragmites_Typha          2.907755      b


LSP_aovph <- lm(LSP ~ type * Treatment, data = phrag, na.rm = TRUE)

Anova(LSP_aovph , type = 3)

#response: LSP
#               Sum Sq Df  F value Pr(>F)    
#(Intercept)    8901834  1 127.2261 <2e-16 ***
#type             84561  2   0.6043 0.5496    
#Treatment         4295  1   0.0614 0.8051    
#type:Treatment   42054  2   0.3005 0.7415    
#Residuals      4408024 63


# t tests -----------------------------------------------------------------

unique(lrc_data$type)

cala <- c("Calamagrostis_Phragmites","Phragmites_Calamagrostis")
carex <- c("Carex_Phragmites","Phragmites_Carex")
typha <- c("Typha_Phragmites","Phragmites_Typha")

cala.data <- lrc_data %>% filter(type %in% cala)
carex.data <- lrc_data %>% filter(type %in% carex)
typha.data <- lrc_data %>% filter(type %in% typha)

cala.LCPT.x <- lrc_data %>% 
  filter(type == "Calamagrostis_Phragmites") %>% 
  select(LCPT)

cala.LCPT.y <- lrc_data %>% 
  filter(type == "Phragmites_Calamagrostis") %>% 
  select(LCPT)

cala.ttest <- t.test(cala.LCPT.x, cala.LCPT.y)

#Welch Two Sample t-test
#
#data:  cala.LCPT.x and cala.LCPT.y
#t = -0.36579, df = 31.447, p-value = 0.717
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -20.33614  14.14788
#sample estimates:
#  mean of x mean of y 
#23.64357  26.73770 

calamagrostis <- ggplot(cala.data, aes(x = Species,
                  y = LCPT)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12),
        legend.position = "none") +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Compensation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = -0.366, df = 31.45, p = 0.717",
           size = 4)

  
carex.LCPT.x <- lrc_data %>% 
  filter(type == "Carex_Phragmites") %>% 
  select(LCPT)

carex.LCPT.y <- lrc_data %>% 
  filter(type == "Phragmites_Carex") %>% 
  select(LCPT)

carex.ttest <- t.test(carex.LCPT.x, carex.LCPT.y)

#Welch Two Sample t-test
#
#data:  carex.LCPT.x and carex.LCPT.y
#t = 0.019981, df = 46, p-value = 0.9841
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -12.36088  12.60873

carex <- ggplot(carex.data, aes(x = Species,
                                       y = LCPT)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12),
        legend.position = "none") +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Compensation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = 0.0199, df = 46, p = 0.984",
           size = 4)


typha.LCPT.x <- lrc_data %>% 
  filter(type == "Typha_Phragmites") %>% 
  select(LCPT)

typha.LCPT.y <- lrc_data %>% 
  filter(type == "Phragmites_Typha") %>% 
  select(LCPT)

typha.ttest <- t.test(typha.LCPT.x, typha.LCPT.y)

#data:  typha.LCPT.x and typha.LCPT.y
#t = 0.37836, df = 33.537, p-value = 0.7075
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -30.90744  45.04004

typha.data$Species <- as.factor(typha.data$Species)
typha.data$Species <- factor(typha.data$Species, levels=c("Typha", "Phragmites"))

typha <- ggplot(typha.data, aes(x = Species,
                                y = LCPT)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Compensation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = 0.378, df = 33.54, p = 0.708",
           size = 4)



cala.LSP.x <- lrc_data %>% 
  filter(type == "Calamagrostis_Phragmites") %>% 
  select(LSP)

cala.LSP.y <- lrc_data %>% 
  filter(type == "Phragmites_Calamagrostis") %>% 
  select(LSP)

cala.ttest2 <- t.test(cala.LSP.x, cala.LSP.y)



#data:  cala.LSP.x and cala.LSP.y
#t = -1.6865, df = 64.411, p-value = 0.09654
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -383.05507   32.33535


cala.lsp <- ggplot(cala.data, aes(x = Species,
                  y = LSP)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Saturation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = 1.687, df = 64.41, p = 0.097",
           size = 4)




carex.LSP.x <- lrc_data %>% 
  filter(type == "Carex_Phragmites") %>% 
  select(LSP)

carex.LSP.y <- lrc_data %>% 
  filter(type == "Phragmites_Carex") %>% 
  select(LSP)

carex.ttest2 <- t.test(carex.LSP.x, carex.LSP.y)

#data:  carex.LSP.x and carex.LSP.y
#t = -3.8658, df = 44.891, p-value = 0.000354
#alternative hypothesis: true difference in means is not equal to 0


carex.lsp <- ggplot(carex.data, aes(x = Species,
                                  y = LSP)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Saturation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = -3.866, df = 44.89, p < 0.001",
           size = 4)



Typha.LSP.x <- lrc_data %>% 
  filter(type == "Typha_Phragmites") %>% 
  select(LSP)

Typha.LSP.y <- lrc_data %>% 
  filter(type == "Phragmites_Typha") %>% 
  select(LSP)

Typha.ttest2 <- t.test(Typha.LSP.x, Typha.LSP.y)

#data:  Typha.LSP.x and Typha.LSP.y
#t = -2.0863, df = 35.991, p-value = 0.04409
#alternative hypothesis: true difference in means is not equal to 0


typha.data$Species <- as.factor(typha.data$Species)
typha.data$Species <- factor(typha.data$Species, levels=c("Typha", "Phragmites"))

typha.lsp <- ggplot(typha.data, aes(x = Species,
                                    y = LSP)) +
  geom_boxplot(alpha = 0.7, lwd = 1, width = 0.25) +
  geom_jitter(aes(fill = Treatment,
                  shape = Treatment),
              size = 5,
              width = 0.08,
              alpha = 0.7) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=9),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12)) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(y = expression(paste("Light Saturation Point"," ", " (", "\u00B5mol photons ", " ", m^-2," ", s^-1, sep=")")),
       x = " ") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#24908C", "#3A518B")) +
  scale_shape_manual(values = c(21,24)) +
  annotate("text", x = 1.5, y = -100, label = "t = -2.086, df = 35.99, p = 0.044",
           size = 4)


t.tests <- calamagrostis + carex + typha +
  cala.lsp + carex.lsp + typha.lsp + 
  plot_annotation(tag_level = "A")

ggsave("Figures/t.test_LCPT.jpeg", t.tests)
