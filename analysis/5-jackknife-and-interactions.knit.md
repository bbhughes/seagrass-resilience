---
output:
  html_document: default
---

# Jackknife test of leverage 






```r
library(tidyverse)
library(rstanarm)
library(RColorBrewer)
library(assertthat)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")
```

Let's leave out each single data point from each model fit and collect all the predictions and plot them. 


```r
d_long <- readRDS("data/generated/d_long.rds")
labels <- readRDS("data/generated/labels.rds")
```










