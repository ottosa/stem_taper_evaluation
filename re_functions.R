
### This script contains all the functions used in the stem curve analysis ###


### Function to read TLS data ###

# f = list of files
# dir_path = folder where the files are
# d_frame = data frame of field data  

read_trees <- function(f, dir_path, d_frame){
  
  # Empty data.frame() to store the data
  data <- data.frame()
  
  # loop reads files one by one from f
  for (i in 1:length(f)){
    
    # read in .txt file from dir_path
    f_name <- paste0(dir_path,f[i]) # f_name = directory path
    d <- read.delim(f_name, sep = " ", head = F) # reading the .txt file in
    names(d) <- c("X","Y","Z","D") # rename columns
    
    # Setting height mesurements starting from 0
    d$Z = d$Z - min(d$Z) # measurement height = 0
    
    id <- substr(f[i],1,nchar(f[i])-4) # specify tree id
    stand <- unique(d_frame$Stand[d_frame$ID == id]) # specify tree stand
    
    # Tree height from field measurements
    tree <- d_frame[d_frame$ID == id,] 
    h <- tree$H # Tree height
    d <- rbind(d,c(0,0,h,0)) #adding tree height to measurement height 0
    
    d$id <- id # adding tree id
    d$stand <- stand # adding tree stand 
    data <- rbind(data,d) # adding tree data data to data.frame
  }
  
  data <- data[, c(5,6,1,2,3,4)] # re-organize colums  
  
  # re-organize by id and height  
  data <- data %>% arrange(id,Z)
  
  # return data.frame
  return(data)
}
################################################################################

### Laasasenaho (1982) stem curve function ###

f <- function(x) (2.3366*(1-x)^1 + -3.2684*(1-x)^2 + 3.6513*(1-x)^3 + 
                    -2.2608*(1-x)^5 + 0.0*(1-x)^8 + 2.1501*(1-x)^13 + 
                    -2.7412*(1-x)^21 + 1.8876*(1-x)^34)^2
################################################################################

### Function for creating interval data frame ###

intervals <- function(f, treat){
  
  #intervals
  interval <- data.frame(inter = c (0.01,0.025,0.05,0.075,0.10,0.15,0.20,0.25,0.30,0.35,0.40,
                                    0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95))
  # data frame for ... 
  pros <- data.frame()
  
  for (i in levels(as.factor(f$id))){
    interval$id <- i 
    interval$stand <- unique(f$stand[f$id == i]) 
    interval$treat <- treat
    pros <- rbind(pros,interval)
  }
  return(pros)
  
}
################################################################################

### Function for calculate splin and relative values for each tree ###
# Returns list containing two data.frames: treat_f and pros_f

rel_splin <- function(treat_f, pros_f, treat){
  
  # lists for forloop 
  trees = list()
  interval = list()
  
  for (i in levels(as.factor(treat_f$id))) {
    
    # Tree id to lists: trees and interval
    trees[[i]] <- treat_f[treat_f$id == unique(treat_f$id[treat_f$id == i]),]
    interval[[i]] <- pros_f[pros_f$id == unique(pros_f$id[pros_f$id == i]),]
    
    # Fit smooth.spline() for each tree, and spar set to 0.55
    spl <- smooth.spline(treat_f$Z[treat_f$id == i], treat_f$D[treat_f$id == i], spar = 0.55)
    
    h20 <- max(treat_f$Z[treat_f$id == i])*0.2 # tree height at 20 % 
    D20 <- predict(spl,h20)$y # dbh at 20 % height
    CC <- unique(treat_f$CC[treat_f$id == i]) # Canopy Class from tree data
    
    trees[[i]]$relD <- trees[[i]]$D/D20 # relative dbh for each interval point by 20 % dbh  
    
    interval[[i]]$h <- max(treat_f$Z[treat_f$id == i]) * interval[[i]]$inter # Tree height at different intervals
    interval[[i]]$d <- predict(spl, interval[[i]]$h)$y # Tree dbh at different intervals
    
    interval[[i]]$relD <- interval[[i]]$d/D20 # Relative dbh by 20 % dbh
    interval[[i]]$relH <- interval[[i]]$inter # Relative heights by 20 % height
    interval[[i]]$CC <- paste0(treat,"_",CC) # Canopy Class
    
    # tree top (height at 100 %) as own row 
    top <- data.frame(as.double(1.00), unique(treat_f$id[treat_f$id == i]), unique(treat_f$stand[treat_f$id == i]),
                      treat, as.double(max(treat_f$Z[treat_f$id == i])),
                      as.double(0), as.double(0), as.double(1), paste0(treat,"_",CC))
    names(top) <- c('inter','id','stand','treat','h','d','relD','relH','CC') # Set names for tree top  
    interval[[i]] <- rbind(interval[[i]],top) # Bind to interval list
  }
  
  treat_f <- rbindlist(trees)
  pros_f <- rbindlist(interval)
  
  return(list(treat_f,pros_f))
  
}
################################################################################

### Function to calculate average dbh for each relative height ###
# and standard deviation (SD) for relD and relH

avg_fun <- function(x, column) {
  
  avg <- data.frame()
  
  for (i in levels(as.factor(x[[column]]))) {
    
    f <- x[x[[column]] == i,]
    
    for(j in as.numeric(levels(as.factor(f$relH)))){
      
      c <- i
      relD <- mean(as.numeric(f$relD[f$relH == j]))
      
      relH <- j
      
      dbh <- mean(as.numeric(f$d[f$relH == j]))
      
      sd_relD <- sd(as.numeric(f$relD[f$relH == j]))
      
      sd_dbh <- sd(as.numeric(f$d[f$relH == j]))
      
      avg <- rbind(avg, data.frame(c,relH, relD, dbh, sd_relD, sd_dbh))
    }
    
  }
  
  return(avg)
  
}
################################################################################

### Re-parametrize stem curve function ###

# Stem curve function need model to get the coefficients 
# Model is developed by Laasasenaho (1982)
stemC_fun <- function(x, model_coefs)(as.numeric(coef(model_coefs)[[1]])+
                                        as.numeric(coef(model_coefs)[[2]])*(1-x)^1+
                                        as.numeric(coef(model_coefs)[[3]])*(1-x)^2+
                                        as.numeric(coef(model_coefs)[[4]])*(1-x)^3+
                                        as.numeric(coef(model_coefs)[[5]])*(1-x)^5+
                                        as.numeric(coef(model_coefs)[[6]])*(1-x)^8+
                                        as.numeric(coef(model_coefs)[[7]])*(1-x)^13+
                                        as.numeric(coef(model_coefs)[[8]])*(1-x)^21+
                                        as.numeric(coef(model_coefs)[[9]])*(1-x)^34)^2
################################################################################




