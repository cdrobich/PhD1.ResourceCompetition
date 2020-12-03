
library(tidyverse)
library(ggpubr)

RCI.res <- read.csv("Data/RCI_residents.csv")
str(RCI.res) 
RCI.res$Year <- as.factor(RCI.res$Year)

RCI.plot <- ggplot(RCI.res, aes(x = Phytometer, y = RCI)) + 
  geom_jitter(
    aes(shape = Year, color = Year), 
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
    size = 4) +
  theme_classic() +
  stat_summary(
    aes(shape = Year),
    fun.data = "mean_se", fun.args = list(mult = 1),
    geom = "pointrange", size = 1,
    position = position_dodge(0.6)
  ) +
  labs(x = " ",
       y = expression(paste("Relative Competition Index (RCI)"))) + 
  scale_color_manual(values = c("#9ebcda","#8856a7")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15)) +
  ylim(-5, 5) +
  geom_hline(yintercept = 1, linetype = "dashed",
             size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             size = 1)