
###############################################################
#### re-parametrize Laasasenaho (1982) stem curve function ####
###############################################################

# Runs Data_process.r script

#  data.frame contains:
#   tree id = id
#   tree stand  = stand
#   Measurement X coordinate = X
#   Measurement Y coordinate = Y
#   Measurement height = Z
#   Diameter at Z = D
#   Tree Canopy Class = CC
#   Tree diamter class = dbh_c


# Re-parametrizing workflow
# 1. Fitting splin curve to each tree diameter and height measurement points
# 2. Relative height and diameters calculations
# 3. Determine relative height and diameters for each attribute
# 4. Re-parametrize stem curve function for 1. each attribute 2. for each individual tree


#########################
##### Phase 1 and 2 #####
#########################


# create data.frames to store:
#   1. tree id = id
#   2. tree stand = stand
#   3. treatment = treat
#   4. Relative height intervals from 0.01 to 0.95 = inter

# Using functions from re_functions.R script to create:
# 1. pros_CCF/EAF data.frame containing relative and absolute height and dbh values
# 2. updating tree_data_CCF/EAF data.frame to contain relative dbh

# CCF trees
pros_CCF <- intervals(tree_data_CCF, "CCF")

CCF_list <- rel_splin(tree_data_CCF, pros_CCF, "CCF")

tree_data_CCF <- as.data.frame(CCF_list[[1]])
pros_CCF <- as.data.frame(CCF_list[[2]])

# EAF trees
pros_EAF <- intervals(tree_data_EAF, "EAF")

EAF_list <- rel_splin(tree_data_EAF, pros_EAF, "EAF")

tree_data_EAF <- as.data.frame(EAF_list[[1]])
pros_EAF <- as.data.frame(EAF_list[[2]])

###############################
#### re-parametrize model  ####
###############################

# re-parametrizing for: treatments, stands and canopy class

pros_all <- rbind(pros_CCF,pros_EAF) # combining CCF and EAF data
plot_lists <- list() # list for plotting
models <- list() # list for re-parametrizing

# Summary table containing: R2, RMSE, and bias for each re-parametrizing
sum_table = data.frame("Model"=character(), 
                       "R2"=numeric(),
                       "RMSE"=numeric(),
                       "BIAS"=numeric())

# Loop to re-parametrize treatment, stand and canopy class factors
for (i in levels(as.factor(c("treat", "stand", "CC")))) {
  
  avg_data <- avg_fun(pros_all,i) # Function calculates the statistics for R2, RMSE and BIAS. 
  
  for(j in levels(as.factor(unique(avg_data$c)))){
    # re-parametrizing part
    models[[j]] <- lm(relD ~ I((1-relH)^1) + I((1-relH)^2) + I((1-relH)^3) + I((1-relH)^5) + 
                        I((1-relH)^8) + I((1-relH)^13) + I((1-relH)^21) + I((1-relH)^34), 
                      data = avg_data[avg_data$c == j,]) 
    
    # statistic calculations
    name <- names(models[j])
    bias <- mean((avg_data$relD[avg_data$c == j] - predict(models[[j]],avg_data[avg_data$c == j,]))/mean(avg_data$relD[avg_data$c == j]))
    rmse <- sqrt(mean(models[[j]]$residuals^2))
    r2 <- summary(models[[j]])$r.squared
    # store statistics to summary table
    sum_table <- rbind(sum_table, data.frame("Model" = name,
                                             "R2" = r2,
                                             "RMSE" = rmse,
                                             "BIAS" = bias))
    ### This part is for plotting purposes ###
    # store plots to list from each re-parametrizing
    plot_data <- avg_data[avg_data$c == j,]
    plot_data$pred_sD <- predict(models[[j]],avg_data[avg_data$c == j,])
    plot_data$pred_xmin <- plot_data$pred_sD - plot_data$sd_relD
    plot_data$pred_xmax <- plot_data$pred_sD + plot_data$sd_relD
    
    plot_lists[[j]] <- ggplot(data = plot_data)+
      geom_errorbar(aes(x = pred_sD, y = relH, xmin = pred_xmin, xmax = pred_xmax), size = 1) +
      geom_line(aes(x = pred_sD, y = relH), colour = "red", size = 1) + xlim(-0.02,1.8) + ylim(-0.02,1.1) +  
      labs(x = "Relative d", y = "Relative h", title = j) + theme_classic()
    
  }
  
}
# Check the summary table
sum_table

# Re-parametrizing for each tree 

# list to store each re-parametrizing
models_trees <- list()

# Loop to separately re-parametrize each tree
for (i in levels(as.factor(pros_all$id))) {
  models_trees[[i]] <- lm(relD ~ I((1-relH)^1) +
                            I((1-relH)^2) +
                            I((1-relH)^3) +
                            I((1-relH)^5) +
                            I((1-relH)^8) +
                            I((1-relH)^13) +
                            I((1-relH)^21) +
                            I((1-relH)^34), data = pros_all[pros_all$id == i,])
  
}

# End of the functional code #





