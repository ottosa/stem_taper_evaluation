

#### Variable calculation script ####


# For loop calculates attributes of interest based on volume and tree measurements 
# Attributes and abbreviates are:
#   Relative volume = rel_vol
#   Relative bottom volume = rel_bott
#   Relative upper volume = rel_upper
#   Form Factor = ff
#   Form factor at 50% height = ff50
#   Form Height = fh
#   Slenderness = slend
#   Form Quotient = formQ
#   Taper = taper
#   Relative taper = rel_taper



# loop goes through each tree by id and calculates each attribute
# Attributes are calculated with volumes from:
#   treatment re-parametrization
#   Stand re-parametrization
#   Canopy Class re-parametrization
#   Tree re-parametrization
#   Laasasenaho (1982) original parameters
# 

for (i in levels(as.factor(data_attr$id))) {
  tree <- data_attr[data_attr$id == i,] # separate tree (i) to own row
  
  # Relative whole, bottom and upper tree volumes
  # Only for volumes calculated with single tree re-parametrizations
  data_attr$rel_vol[data_attr$id==i] <- tree$volume_upp_tree/tree$volume_bott_tree
  data_attr$rel_bott[data_attr$id==i] <- tree$volume_bott_tree/tree$volume_tree
  data_attr$rel_upper[data_attr$id==i] <- tree$volume_upp_tree/tree$volume_tree
  
  # Form Factor 
  # (ppa unit transformed corresponding to volume unit)
  data_attr$ff_treat[data_attr$id==i] <- (tree$volume_treat)/((tree$ppa)*tree$h)
  data_attr$ff_stand[data_attr$id==i] <- (tree$volume_stand)/((tree$ppa)*tree$h)
  data_attr$ff_CC[data_attr$id==i] <- (tree$volume_CC)/((tree$ppa)*tree$h)
  data_attr$ff_tree[data_attr$id==i] <- (tree$volume_tree)/((tree$ppa)*tree$h)
  data_attr$ff_Laasasenaho[data_attr$id==i] <- (tree$volume_Laasasenaho)/((tree$ppa)*tree$h)
  
  # Form Factor at 50% 
  # (diameter at 50 % height (d50) unit transformed corresponding to volume unit)
  data_attr$ff50_treat[data_attr$id==i] <- (tree$volume_bott_treat)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  data_attr$ff50_stand[data_attr$id==i] <- (tree$volume_bott_stand)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  data_attr$ff50_CC[data_attr$id==i] <- (tree$volume_bott_CC)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  data_attr$ff50_tree[data_attr$id==i] <- (tree$volume_bott_tree)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  data_attr$ff50_Laasasenaho[data_attr$id==i] <- (tree$volume_bott_Laasasenaho)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  
  # Form height
  # (ppa unit transformed corresponding to volume unit)
  data_attr$fh_treat[data_attr$id==i] <- (tree$volume_treat*1000)/(tree$ppa*10000)
  data_attr$fh_stand[data_attr$id==i] <- (tree$volume_stand*1000)/(tree$ppa*10000)
  data_attr$fh_CC[data_attr$id==i] <- (tree$volume_CC*1000)/(tree$ppa*10000)
  data_attr$fh_tree[data_attr$id==i] <- (tree$volume_tree*1000)/(tree$ppa*10000)
  data_attr$fh_Laasasenaho[data_attr$id==i] <- (tree$volume_Laasasenaho*1000)/(tree$ppa*10000)
  
  data_attr$slend[data_attr$id==i] <- tree$h/tree$dbh # slenderness 
  data_attr$formQ[data_attr$id==i] <- tree$d50/tree$dbh # Form Quotient
  data_attr$taper[data_attr$id==i] <- tree$dbh - tree$d6m # taper
  
  data_attr$rel_taper[data_attr$id==i] <- (tree$dbh - tree$d6m)/tree$dbh # relative taper
  
}

head(data_attr)

# Change stand and canopy class names to more easily understandable 

data_attr <- data_attr %>% mutate(stand = recode(stand,"1" = "RF 1", "120" = "RF 2", "136" = "RF 3", 
                                                 "140" = "RF 4", "141" = "RF 5", "151" = "RF 6", 
                                                 "158" = "RF 7", "17" = "RF 8","2" = "RF 9", "20" = "RF 10", 
                                                 "3" = "RF 11", "315" = "RF 12","5" = "RF 13", "86" = "RF 14",
                                                 "EVO2" = "CCF 1", "EVO3" = "CCF 2", "EVO4" = "CCF 3",
                                                 "VEP2" = "CCF 4", "VEP4" = "CCF 5", "VES16" = "CCF 6","VES2" = "CCF 7"))

data_attr <- data_attr %>% mutate(CC = recode(CC, "CCF_1" = "1 CC CCF", "CCF_2" = "2 CC CCF", "CCF_3" = "3 CC CCF",
                                              "RF_1" = "1 CC RF", "RF_2" = "2 CC RF", "RF_3" = "3 CC RF"))
### End of the attribute calculation part ###




### Summary tables ###
# Contains:
#   number of observations = n
#   mean diameter at 1.3m = meanDBH
#   min diameter at 1.3m = minDBH
#   max diameter at 1.3m = maxDBH
#   mean height = meanH
#   min height = minH
#   max height = maxH

# Calculated by treatments, stands and canopy classes


# data.frame to store the summary results
data_summary <- data.frame("strata" = character(), "n (trees)" = numeric(), "mean dbh (cm)" = numeric(), "SD dbh" = numeric(), "min dbh (cm)" = numeric(), 
                           "max dbh (cm)" = numeric(), "mean h (m)" = numeric(), "SD h" = numeric(), "min h (m)" = numeric(), "max h (m)" = numeric())

# Treatment calculations
for (i in levels(as.factor(data_attr$treat))) {
  sr <- unique(data_attr$treat[data_attr$treat == i]) # stratum
  n <- length(data_attr$treat[data_attr$treat == i]) # number of observations
  
  meanDBH <- mean(data_attr$dbh[data_attr$treat == i]) # mean DBH
  SDDBH <- sd(data_attr$dbh[data_attr$treat == i]) # SD DBH
  minDBH <- min(data_attr$dbh[data_attr$treat == i]) # min DBH
  maxDBH <- max(data_attr$dbh[data_attr$treat == i]) # max DBH
  
  meanH <- mean(data_attr$h[data_attr$treat == i]) # mean height
  SDH <- sd(data_attr$h[data_attr$treat == i]) # SD height
  minH <- min(data_attr$h[data_attr$treat == i]) # min height
  maxH <- max(data_attr$h[data_attr$treat == i]) # max height
  
  # Store the data
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH,
                                   "SD dbh" = SDDBH,
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH,
                                   "SD h" = SDH,
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

# Stand calculations
for (i in levels(as.factor(data_attr$stand))) {
  sr <- unique(data_attr$stand[data_attr$stand == i])
  n <- length(data_attr$stand[data_attr$stand == i])
  
  meanDBH <- mean(data_attr$dbh[data_attr$stand == i])
  SDDBH <- sd(data_attr$dbh[data_attr$stand == i])
  minDBH <- min(data_attr$dbh[data_attr$stand == i])
  maxDBH <- max(data_attr$dbh[data_attr$stand == i])
  
  meanH <- mean(data_attr$h[data_attr$stand == i])
  SDH <- sd(data_attr$h[data_attr$stand == i])
  minH <- min(data_attr$h[data_attr$stand == i])
  maxH <- max(data_attr$h[data_attr$stand == i])
  
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH, 
                                   "SD dbh" = SDDBH,
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH, 
                                   "SD h" = SDH,
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

# Canopy Class calculations
for (i in levels(as.factor(data_attr$CC))) {
  sr <- unique(data_attr$CC[data_attr$CC == i])
  n <- length(data_attr$CC[data_attr$CC == i])
  
  meanDBH <- mean(data_attr$dbh[data_attr$CC == i])
  SDDBH <- sd(data_attr$dbh[data_attr$CC == i])
  minDBH <- min(data_attr$dbh[data_attr$CC == i])
  maxDBH <- max(data_attr$dbh[data_attr$CC == i])
  
  meanH <- mean(data_attr$h[data_attr$CC == i])
  SDH <- sd(data_attr$h[data_attr$CC == i])
  minH <- min(data_attr$h[data_attr$CC == i])
  maxH <- max(data_attr$h[data_attr$CC == i])
  
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH,
                                   "SD dbh" = SDDBH,
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH,
                                   "SD h" = SDH,
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

data_summary


### End of the code ###