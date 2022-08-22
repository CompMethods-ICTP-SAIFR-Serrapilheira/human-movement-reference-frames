# ------------------------------------------------------------------------------
# Verification of the referential used by motor control in reaching movements
#
# Description: This code will plot all the figures needed for the report of this
# project
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 21/08/2022
# ------------------------------------------------------------------------------


# Necessary packages -----------------------------------------------------------

library(ggplot2)

# Reading necessary tables ---------- ------------------------------------------

indices <- read.csv("./output/mean_indices_MED.csv")

# Plotting the boxplot of the paired difference --------------------------------

data_plt <- data.frame(text = c(rep("W", length(indices$W_mean_diff)),
                                rep("R²", length(indices$R2_mean_diff))),
                       value = c(indices$W_mean_diff, indices$R2_mean_diff))

pdf("./figures/paired_difference.pdf", width=6, height=4)

p <- data_plt %>%
  ggplot(aes(x = text, y = value, fill =  text)) +
  geom_violin(width = 0.8, size = 0, alpha = 0.8, color = NA) +
  geom_boxplot(width=0.15, fill="white", color = "black", alpha = 0.3, lwd=.15) +
  geom_hline(yintercept = 0, size = .3)+
  theme(
    legend.position="none",
    text = element_text(size = 15),
    axis.text.y = element_text(color = "black", size = 20),
    axis.text.x = element_text(color = "black", size = 15),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 0.3)
  )+
  coord_flip() +
  xlab("") +
  ylab("Paired difference (inertial - non-inertial)")

p + scale_fill_manual(values = c("#299AE5", "#EB6F39"))

dev.off()


