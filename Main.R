

# Empty R environment 

rm(list = ls())


# needed libraryes
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

# 1.Data_process.R = script to read, synchronize and prepare the data
source("Data_process.R")

# 2.re-parametrize.R = script to parametrize Laasasenaho (1982) stem curve function
source("re-parametrize.R")

# 3.volume_calculations.R = script to calculate volumes with each parametrization for each sample tree
source("volume_calculations.R")

# 4. variable_calculations.R = script to calculate attributes for each sample tree
source("variable_calculations.R")


