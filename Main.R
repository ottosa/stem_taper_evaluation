

# Empty R environment 

rm(list = ls())


# librarys needed
library(data.table)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(scales)
library(ggpmisc)
library(reshape2)
library(Metrics)
library(lme4)
library(rsq)
library(agricolae)

# Scripts containing functions
source("re_functions.R")

# 1.Data_process.R = script to read, synchronise and prepare teh data
source("Data_process.R")

# 2.re-parametrize.R = script to parametrize Laasasenaho (1982) stem curve function
source("re-parametrize.R")

# 3.volume_calculations.R = script to calculate volumes with each parametrization for each sample tree
source("volume_calculations.R")

# 4. variable_calculations.R = script to calculate attributes for each sample tree
source("variable_calculations.R")

unique(data_attr$stand)

stands <- unique(data_attr$stand[data_attr$treat == "CCF"])
stands <- stands[!stands == "CCF 4" & !stands == "CCF 5"]


stand_med <- c()
for (i in levels(as.factor(stands))) {
  stand_med <- rbind(stand_med,median(data_attr$formQ[data_attr$stand == i]))
}
min(stand_med)
max(stand_med)

median(data_attr$formQ[data_attr$stand == "EAF 4"])


median(data_attr$fh_tree[data_attr$CC == "1 CC CCF"])



