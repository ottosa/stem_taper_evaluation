

#### Variable calculation script ####

# Calculates variables:
# ff
# ....

#run re-parametrization script
source("E:/MMM_UNITE/Stemcurves/GitHub/volume_calculations.R")




# Check the data
colnames(data_vol)
head(data_vol)


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


# Empty data.frame where the attributes are stored
data_attr <- data.frame()

# loop goes through each tree by id and calculates each attribute
# Attributes are calculated with volumes from:
#   treatment re-parametrization
#   Stand re-parametrization
#   Canopy Class re-parametrization
#   Tree re-parametrization
#   Laasasenaho (1982) original parameters
# 
# NOTE! all the attributes are not calculated to all the paramters

for (i in levels(as.factor(data_vol$id))) {
  tree <- data_vol[data_vol$id == i,] # separate tree (i) to own row
  
  # Relative whole, bottom and upper tree volumes
  # Only for volumes calculated with single tree re-parametrizations
  tree$rel_vol <- tree$volume_upp_tree/tree$volume_bott_tree
  tree$rel_bott <- tree$volume_bott_tree/tree$volume_tree
  tree$rel_upper <- tree$volume_upp_tree/tree$volume_tree
  
  # Form Factor 
  # (ppa unit transformed corresponding to volume unit)
  tree$ff_treat <- (tree$volume_treat)/((tree$ppa)*tree$h)
  tree$ff_stand <- (tree$volume_stand)/((tree$ppa)*tree$h)
  tree$ff_CC <- (tree$volume_CC)/((tree$ppa)*tree$h)
  tree$ff_tree <- (tree$volume_tree)/((tree$ppa)*tree$h)
  tree$ff_laasis <- (tree$volume_laasis)/((tree$ppa)*tree$h)
  
  # Form Factor at 50% 
  # (diameter at 50 % height (d50) unit transformed corresponding to volume unit)
  tree$ff50_treat <- (tree$volume_bott_treat)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  tree$ff50_stand <- (tree$volume_bott_stand)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  tree$ff50_CC <- (tree$volume_bott_CC)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  tree$ff50_tree <- (tree$volume_bott_tree)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  tree$ff50_laasis <- (tree$volume_bott_laasis)/((pi*((tree$d50/100)/2)^2)*(tree$h/2))
  
  # Form height
  # (ppa unit transformed corresponding to volume unit)
  tree$fh_treat <- (tree$volume_treat*1000)/(tree$ppa*10000)
  tree$fh_stand <- (tree$volume_stand*1000)/(tree$ppa*10000)
  tree$fh_CC <- (tree$volume_CC*1000)/(tree$ppa*10000)
  tree$fh_tree <- (tree$volume_tree*1000)/(tree$ppa*10000)
  tree$fh_laasis <- (tree$volume_laasis*1000)/(tree$ppa*10000)
  
  tree$slend <- tree$h/tree$dbh # slenderness 
  tree$formQ <- tree$d50/tree$dbh # Form Quotient
  tree$taper <- tree$dbh - tree$d6m # taper
  
  tree$rel_taper <- (tree$dbh - tree$d6m)/tree$dbh # relative taper
  
  # Sotore results to data_attr data.frame
  data_attr <- rbind(data_attr, tree)
  
}

head(data_attr)

# Save data as csv file
write.csv(data_attr, "E:/Väikkäri_Otto/Artikkelit/Artikkeli 1/data_table.csv")


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
data_summary <- data.frame("strata" = character(), "n (trees)" = numeric(), "mean dbh (cm)" = numeric(), "min dbh (cm)" = numeric(), 
                           "max dbh (cm)" = numeric(), "mean h (m)" = numeric(), "min h (m)" = numeric(), "max h (m)" = numeric())

# Treatment calculations
for (i in levels(as.factor(data_attr$treat))) {
  sr <- unique(data_attr$treat[data_attr$treat == i]) # stratum
  n <- length(data_attr$treat[data_attr$treat == i]) # number of observations
  
  meanDBH <- mean(data_attr$dbh[data_attr$treat == i]) # mean DBH
  minDBH <- min(data_attr$dbh[data_attr$treat == i]) # min DBH
  maxDBH <- max(data_attr$dbh[data_attr$treat == i]) # max DBH
  
  meanH <- mean(data_attr$h[data_attr$treat == i]) # mean height
  minH <- min(data_attr$h[data_attr$treat == i]) # min height
  maxH <- max(data_attr$h[data_attr$treat == i]) # max height
  
  # Store the data
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH, 
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH, 
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

# Stand calculations
for (i in levels(as.factor(data_attr$stand))) {
  sr <- unique(data_attr$stand[data_attr$stand == i])
  n <- length(data_attr$stand[data_attr$stand == i])
  
  meanDBH <- mean(data_attr$dbh[data_attr$stand == i])
  minDBH <- min(data_attr$dbh[data_attr$stand == i])
  maxDBH <- max(data_attr$dbh[data_attr$stand == i])
  
  meanH <- mean(data_attr$h[data_attr$stand == i])
  minH <- min(data_attr$h[data_attr$stand == i])
  maxH <- max(data_attr$h[data_attr$stand == i])
  
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH, 
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH, 
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

# Canopy Class calculations
for (i in levels(as.factor(data_attr$CC))) {
  sr <- unique(data_attr$CC[data_attr$CC == i])
  n <- length(data_attr$CC[data_attr$CC == i])
  
  meanDBH <- mean(data_attr$dbh[data_attr$CC == i])
  minDBH <- min(data_attr$dbh[data_attr$CC == i])
  maxDBH <- max(data_attr$dbh[data_attr$CC == i])
  
  meanH <- mean(data_attr$h[data_attr$CC == i])
  minH <- min(data_attr$h[data_attr$CC == i])
  maxH <- max(data_attr$h[data_attr$CC == i])
  
  data_summary <- rbind(data_summary, 
                        data.frame("strata" = sr, 
                                   "n (trees)" = n, 
                                   "mean dbh (cm)" = meanDBH, 
                                   "min dbh (cm)" = minDBH, 
                                   "max dbh (cm)" = maxDBH, 
                                   "mean h (m)" = meanH, 
                                   "min h (m)" = minH,
                                   "max h (m)" = maxH))
}

data_summary


### End of the code ###