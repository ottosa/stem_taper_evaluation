

### Volume calculations with each re-parametrization ###
# Workflow: 
# 1. function of each re-parametrization.
# 2. Form factor calculation with integral with each function. 
# 3. Calculate volume with form factors
#   -> v = f * g * h, f = form factor at 20 % height, g = basal area at 20 % height ja h = tree height 

# directory to functions 


# data.frame to store calculations of each tree
data <- data.frame(id = unique(tree_data_CCF$id), treat = "CCF")
data <- rbind(data, data.frame(id = unique(tree_data_RF$id), treat = "RF"))


# loop to calculate attributes from stem curve

for (i in levels(as.factor(data$id))){
  
  # CCF 
  if (data$treat[data$id == i] == "CCF"){
    
    spl = smooth.spline(tree_data_CCF$Z[tree_data_CCF$id == i], tree_data_CCF$D[tree_data_CCF$id == i]) # Fit splin
    h50 <- max(tree_data_CCF$Z[tree_data_CCF$id == i])*0.5 # Height at 50 % 
    
    data$stand[data$id == i] <- unique(pros_CCF$stand[pros_CCF$id == i]) # Stand 
    data$CC[data$id == i] <- unique(pros_CCF$CC[pros_CCF$id == i]) # Canopy Class
    data$dbh[data$id == i] <- data_CCF$DBH[data_CCF$ID == i] # dbh (at 1.3m) 
    data$d20[data$id == i] <- pros_CCF$d[pros_CCF$inter == 0.2 & pros_CCF$id == i]*100 # diameter at 20 % converted to cm  
    data$d50[data$id == i] <- predict(spl,h50)$y * 100 # diameter at 50 % height from splin and converted to cm
    data$d6m[data$id == i] <- predict(spl,6)$y * 100 # diameter at 6 m height converted to cm
    # Check if field measurement is line with splin
    if(data$dbh[data$id == i] < data$d6m[data$id == i]){
      data$dbh[data$id == i] <- predict(spl,1.3)$y # field dbh is smaller than d6m from splin = dbh taken from splin
    }
    data$h[data$id == i] <- pros_CCF$h[pros_CCF$inter== 1 & pros_CCF$id == i] # tree height 
    data$ppa[data$id == i] <- pi * ((data$dbh[data$id == i]/100)/2)^2 # basal area at 1.3m  
    
    
    
    
  # RF (same parameters)  
  } else{
    
    spl = smooth.spline(tree_data_RF$Z[tree_data_RF$id == i], tree_data_RF$D[tree_data_RF$id == i])
    h50 <- max(tree_data_RF$Z[tree_data_RF$id == i])*0.5
    
    data$stand[data$id == i] <- unique(pros_RF$stand[pros_RF$id == i])
    data$CC[data$id == i] <- unique(pros_RF$CC[pros_RF$id == i])
    data$dbh[data$id == i] <- data_RF$DBH[data_RF$ID == i]
    data$d20[data$id == i] <- pros_RF$d[pros_RF$inter == 0.2 & pros_RF$id == i]*100
    data$d50[data$id == i] <- predict(spl,h50)$y * 100
    data$d6m[data$id == i] <- predict(spl,6)$y * 100
    if(data$dbh[data$id == i] < data$d6m[data$id == i]){
      data$dbh[data$id == i] <- predict(spl,1.3)$y * 100
    }
    data$h[data$id == i] <- pros_RF$h[pros_RF$inter== 1 & pros_RF$id == i]
    data$ppa[data$id == i] <- pi * ((data$dbh[data$id == i]/100)/2)^2
    
    
  }
  
}


# Volume calculation

# data.frame to store form factors
form <- data.frame()

for (i in 1:length(models)) {
  
  # model name
  mod_name <- names(models)[i]
  
  # Integral is using stemC_fun as stem curve function and coefs to function is set from model (i) 
  int <- integrate(stemC_fun, model_coefs = models[[i]], lower = 0, upper = 1)$value # integral of whole stem
  int_upper <- integrate(stemC_fun, model_coefs = models[[i]], lower = 0.5, upper = 1)$value # integral of upper 50 % of stem 
  int_bott <- integrate(stemC_fun, model_coefs = models[[i]], lower = 0, upper = 0.5)$value # integral of bottom 50 % of stem
  form <- rbind(form, data.frame(mod_name, int, int_upper, int_bott)) #store integrals to data.frame
  
}

# data frame to store results
data_attr <- data.frame() 

# 1. loop goes through all trees one by one based on the tree id
for (i in levels(as.factor(data$id))) {
  
  tree <- data[data$id == i,] # tree separated as own row
  
  # 2. loop goes through all re-parametrizations and calculates volumes 
  for (j in levels(as.factor(unique(form$mod_name)))) {
    
    # if the model (j) is equal with the tree (i) treatment, stand or CC is volume calculated 
    if (tree$treat == j | tree$stand == j | tree$CC == j){
    
      volume <- (pi*((tree$d20/100)/2)^2) * tree$h * (pi/4) * form$int[form$mod_name == j] # whole trunk volume
      volume_upp <- (pi*((tree$d20/100)/2)^2) * tree$h * (pi/4) * form$int_upper[form$mod_name == j] # upper (50%) trunk volume
      volume_bott <- (pi*((tree$d20/100)/2)^2) * tree$h * (pi/4) * form$int_bott[form$mod_name == j] # bottom (50%) trun volume
      
      # elif stores volume to correct column according to model (j) name
      if (j == "CCF" | j == "RF"){
        
        tree$volume_treat <- volume
        tree$volume_upp_treat <- volume_upp
        tree$volume_bott_treat <- volume_bott
        
      } else if(substring(j,1,4) == "CCF_" | substring(j,1,3) == "RF_"){
        
        tree$volume_CC <- volume
        tree$volume_upp_CC <- volume_upp
        tree$volume_bott_CC <- volume_bott
        
      } else {
        
        tree$volume_stand <- volume
        tree$volume_upp_stand <- volume_upp
        tree$volume_bott_stand <- volume_bott
      }
    }
  }
  data_attr <- rbind(data_attr,tree)
}




# Volume with original Laasasenahon (1982) parameters

int <- integrate(f,lower = 0, upper = 1)$value
int_upper <- integrate(f, lower = 0.5, upper = 1)$value
int_bott <- integrate(f, lower = 0, upper = 0.5)$value 

data_attr$volume_Laasasenaho <- (pi*((data_attr$d20/100)/2)^2 * data_attr$h * (pi/4) * int)
data_attr$volume_upp_Laasasenaho <- (pi*((data_attr$d20/100)/2)^2 * data_attr$h * (pi/4) * int_upper)
data_attr$volume_bott_Laasasenaho <- (pi*((data_attr$d20/100)/2)^2 * data_attr$h * (pi/4) * int_bott)

#############################################################
#### Volume calculations by each tree re-parametrization ####
#############################################################

# Loop goes through all the trees and calculates volume based on re-parametrized models of each tree 
for (i in levels(as.factor(data_attr$id))) {
  
  # Integral is using stemC_fun as stem curve function and coefs to function is set from model_trees (i)
  int <- integrate(stemC_fun, model_coefs = models_trees[[i]], 
                   lower = 0, upper = 1)$value # whole tree trunk integral
  int_upp <- integrate(stemC_fun, model_coefs = models_trees[[i]], 
                       lower = 0.5, upper = 1)$value # upper (50%) tree trunk integral
  int_bott <- integrate(stemC_fun, model_coefs = models_trees[[i]], 
                        lower = 0, upper = 0.5)$value # bottom (50%) tree trunk integral
  
  data_attr$volume_tree[data_attr$id == i] <- (pi*((data_attr$d20[data_attr$id == i]/100)/2)^2) * data_attr$h[data_attr$id == i] * (pi/4) * int # whole tree trunk volume
  data_attr$volume_upp_tree[data_attr$id == i] <- (pi*((data_attr$d20[data_attr$id == i]/100)/2)^2) * data_attr$h[data_attr$id == i] * (pi/4) * int_upp # upper (50%) tree trunk volume
  data_attr$volume_bott_tree[data_attr$id == i] <- (pi*((data_attr$d20[data_attr$id == i]/100)/2)^2) * data_attr$h[data_attr$id == i] * (pi/4) * int_bott# bottom (50%) tree trunk volume
}


### End of the code ###

