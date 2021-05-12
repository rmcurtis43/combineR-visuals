#devtools::install_github("rmcurtis43/combineR")
library(combineR)
library(tidyverse)
library(corrplot)

# pull combineR data
data <- pull_combine_data()

####################################

cor_data <- data %>%
  select(height=height_in, weight=weight_lbs, vertical=vertical_in, `broad jump`=broad_jump_in, bench, `3cone`=x3cone, shuttle, `40yd`=x40yd) %>%
  cor(use = "pairwise.complete.obs")


corrplot.mixed(cor_data)



png(height=1000, width=1000, file="images/corrplot.png", type = "cairo")
corrplot.mixed(cor_data)
dev.off()