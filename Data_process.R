
# Data processing for stem taper function evaluation

## Data from two treatments: CCF (continuous-cover forest) and EAF (even-aged forest)

## Workflow:
## 1. Define canopy class and diameter class of each tree
##    - This is done based on field measurements of the trees
##    - Diameter classes are at 5 cm interval from 0-5 cm, 5-10 cm ... 30-35 cm and over 39 cm.
## 2. Reading in single-tree diameter measurements
##  Each tree measurements is at own .txt file
##    - Data contains: tree id, X and Y coordinates, Z = diameter and stem height at the Z diameter
##     

# 1. Defining  canopy class and diameter class of each tree

# Reading field measurements of the trees
data_CCF <- as.data.table(read.delim("Data/field_CCF.txt", dec = ','))
data_CCF$DBH <- (data_CCF$DBH1+data_CCF$DBH2)/2
data_CCF$Treat <- "CCF"
data_CCF <- data_CCF[, .(ID, Stand = Plot, Treat, DBH, H)]
colnames(data_CCF)

data_EAF <- as.data.table(read.delim("Data/field_EAF.txt"))
data_EAF$Treat <- "EAF"
data_EAF <- data_EAF[, .(ID = TreeID, Stand, Treat, DBH = dbh_field, H = h_field)]
colnames(data_EAF)

data_comb <- as.data.frame(rbind(data_CCF,data_EAF))

str(data_comb)

# Defining stands tallest tree
k_max <- data_comb %>% group_by(Stand) %>% 
  summarise(max = max(H))


# Defining Canopy Class of each tree
for (i in levels(as.factor(data_comb$Stand))) {
  
  maxH <- k_max[k_max$Stand == i, "max"]
  
  for (j in levels(as.factor(data_comb[data_comb$Stand == i, "ID"]))) {
    
    tree_h <- (data_comb$H[data_comb$ID == j]/maxH)
    
    if (tree_h >= 0.9){
      data_comb$CC[data_comb$ID == j] <- 1 # Canopy Class 1 (Height 90% of tallest tree in stand)
      
    }else if(tree_h < 0.9 & tree_h >= 0.8){
      data_comb$CC[data_comb$ID == j] <- 2 # Canopy Class 2 (Height 90% - 80% of tallest tree in stand) 
      
    }else{
      data_comb$CC[data_comb$ID == j] <- 3 # Canopy Class 3 (under 80% of tallest tree in stand) 
    }
  }
}

# Define diameter class of each tree


for (i in levels(as.factor(data_comb$ID))){
  
  dbh <- data_comb$DBH[data_comb$ID == i]
  
  if (dbh > 0 & dbh < 5){
    data_comb$dbh_c[data_comb$ID == i] <- "0-5 cm"
    
  }else if (dbh >= 5 & dbh < 10){
    data_comb$dbh_c[data_comb$ID == i] <- "5-10 cm"
    
  }else if (dbh >= 10 & dbh < 15){
    data_comb$dbh_c[data_comb$ID == i] <- "10-15 cm"
    
  }else if (dbh >= 15 & dbh < 20){
    data_comb$dbh_c[data_comb$ID == i] <- "15-20 cm"
    
  }else if (dbh >= 20 & dbh < 25){
    data_comb$dbh_c[data_comb$ID == i] <- "20-25 cm"
    
  }else if (dbh >= 25 & dbh < 30){
    data_comb$dbh_c[data_comb$ID == i] <- "25-30 cm"
    
  }else if (dbh >= 30 & dbh < 35){
    data_comb$dbh_c[data_comb$ID == i] <- "30-35 cm"
    
  }else if (dbh >= 35 & dbh < 40){
    data_comb$dbh_c[data_comb$ID == i] <- "35-39 cm"
    
  }else{
    data_comb$dbh_c[data_comb$ID == i] <- "over 39 cm"
  }
}




# 2. Reading in single-tree diameter measurements

# List of tree files at the directory
files_CCF <- list.files("Data/TXT_CCF") # CCF data
files_EAF <- list.files("Data/TXT_EAF") #EAF data


# Read CCF and EAF single tree TLS measurement data with read_trees() function 
tree_data_CCF <- read_trees(files_CCF,"Data/TXT_CCF/", data_comb) 
tree_data_EAF <- read_trees(files_EAF,"Data/TXT_EAF/", data_comb)


# Adding canopy Class to tree data
tree_data_CCF$CC <- data_comb$CC[match(tree_data_CCF$id,data_comb$ID)]
tree_data_EAF$CC <- data_comb$CC[match(tree_data_EAF$id,data_comb$ID)]

# Adding dbh class to tree data
tree_data_CCF$dbh_c <- data_comb$dbh_c[match(tree_data_CCF$id,data_comb$ID)]
tree_data_EAF$dbh_c <- data_comb$dbh_c[match(tree_data_EAF$id,data_comb$ID)]

# Data table ready
summary(tree_data_CCF)
summary(tree_data_EAF)

head(tree_data_CCF)
head(tree_data_EAF)










