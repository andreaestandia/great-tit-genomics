# great-tit-genomics_source.R
# 
# Copyright (c) Andrea Estandia, 2024, except where indicated
# Date Created: 2024-01-11


# --------------------------------------------------------------------------
# REQUIRES
# --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidybayes)
  library(brms)
  library(devtools)
  library(janitor)
  library(ggraph)
  library(igraph)
  library(dplyr)
  library(qvalue)
  library(lfmm)
  library(wesanderson)
  library(gghighlight)
  library(kinship2)
  library(lubridate)
  library(geosphere)
  library(SNPRelate)
  library(sf)
  library(vegan)
  library(patchwork)
  library(gghighlight)
  library(viridis)
  library(adegenet)
  library(spdep)
  library(ade4)
  library(randomForest)
  library(dbscan)
  library(showtext)

})

text_size = 11
# --------------------------------------------------------------------------
# PATHS
# --------------------------------------------------------------------------

data_path <- file.path(getwd(), "data")
reports_path <- file.path(getwd(), "reports")
figures_path <- file.path(getwd(), "reports", "plots")

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

if (!dir.exists(figures_path)) {
  dir.create(figures_path, recursive = TRUE)
}

if (!dir.exists(reports_path)) {
  dir.create(reports_path, recursive = TRUE)
}

font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto() 


'%!in%' <- function(x,y)!('%in%'(x,y))
CalculateEuclideanDistance <- function(vect1, vect2) sqrt(sum((vect1 - vect2)^2)) 
