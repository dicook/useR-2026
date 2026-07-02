# Load libraries used everywhere
library(tidyverse)
library(tidymodels)
library(conflicted)
library(colorspace)
library(patchwork)
library(MASS)
library(rpart)
library(randomForest)
library(patchwork)
library(GGally)
library(geozoo)
library(mulgar)
library(mvtnorm)
library(rpart.plot)
library(discrim)
library(tourr)
library(detourr)
library(classifly)
library(ggthemes)
library(ggrepel)
library(xgboost)
library(plotly)
library(keras)
library(forcats)
library(ggbeeswarm)
library(DALEXtra)
library(kernelshap)
library(shapviz)
library(lime)
# pak::pak("dandls/counterfactuals")
library(iml)
library(counterfactuals)
library(kernlab)
library(kableExtra)
library(vip)
library(colorspace)
library(scales)
# pak::pak("janithwanni/sillysplines")
library(sillysplines)
# pak::pak("janithwanni/kumquat")
library(kumquat)
# pak::pak("janithwanni/kultarr")
library(kultarr)
#pak::pak("ropenscilabs/ochRe")
library(ochRe)

# Set up chunk for all slides
knitr::opts_chunk$set(
  fig.path = "images/",
  fig.align = "center",
  code.line.numbers = FALSE,
  fig.retina = 4,
  echo = TRUE,
  eval = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  dev.args = list(pointsize = 11)
)
options(
  digits = 2,
  width = 60)
theme_weedy <- function() {
  theme_void() + 
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = "black", linewidth = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5))
}

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(viridis::viridis_pal)
conflicts_prefer(ggplot2::margin)

ochre_clrs <- c("#EAC024", "#435d42")

# Utility function
find_closest <- function(pt, data) {
  pt <- as.matrix(pt)
  data <- as.matrix(data)
  dst <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      dst[i] <- dst[i] + (data[i,j]-pt[j])^2
    }
    dst[i] <- sqrt(dst[i])
  }
  return(which.min(dst))
}  


