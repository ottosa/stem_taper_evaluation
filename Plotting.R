
######################
#### Result plots ####
######################

# load library's

library(ggpubr)
library(ggplot2)
library(scales)
library(ggpmisc)
library(dplyr)
library(reshape2)
library(tidyverse)
library(Metrics)


### Note! ###

# Run first scripts:
#   - Data_process.R
#   - re-parametrize.R
#   - volume_calculations.R
#   - variable_calculations.R

# set working directory
setwd("E:/MMM_UNITE/Stemcurves/GitHub")
source("re_functions.R") # load functions

###########################
### Plotting stemcurves ###
###########################

# First plot all the stem curves with different stratum 
plot_data <- data.frame(relH = c(0.0,0.01,0.025,0.05,0.075,0.10,0.15,0.20,0.25,
                                 0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,
                                 0.75,0.80,0.85,0.90,0.95,1.0))


plot_data$pred_SD <- f(plot_data$relH)

plot_lists[["Laasis"]] <- ggplot(data = plot_data, aes(x = pred_SD, y = relH))+
  geom_line( colour = "red", size = 1) + xlim(-0.02,1.8) + ylim(-0.02,1.1) +  
  labs(x = "Relative d", y = "Relative h", title = "Laasasenaho") + theme_classic()

do.call("grid.arrange", c(plot_lists, ncol = 5, nrow = 6))


#### Plot treatment and Laasasenaho stem curves to same plot ####

avg_data <- avg_fun(pros_all, "treat")

plot_data <- data.frame()

for (i in levels(as.factor(avg_data$c))){
  
  plot_data <- avg_data[avg_data$c == i,]
  plot_data$pred_sD <- predict(models[[i]],avg_data[avg_data$c == i,])
  plot_data$pred_xmin <- plot_data$pred_sD - plot_data$sd_relD
  plot_data$pred_xmax <- plot_data$pred_sD + plot_data$sd_relD
  
  plot_data <- rbind(plot_data,plot_data)
}

plot_data <- data.frame(c = "Laasasenaho", relH = c(0.0,0.01,0.025,0.05,0.075,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.0))

plot_data <- plot_data %>% add_column(relD = NA, 
                                      dbh = NA,
                                      sd_relD = NA,
                                      sd_dbh = NA,
                                      pred_sD = f(plot_data$relH),
                                      pred_xmin = NA,
                                      pred_xmax = NA)

colnames(plot_data)
colnames(plot_data)
plot_data <- rbind(plot_data, plot_data)

plot_treat <- ggplot(data = plot_data, aes(x = pred_sD, y = relH, color = c)) +
  geom_line(size = 1.2, alpha = 0.75) 

plot_treat +
  geom_errorbar(aes(x = pred_sD, y = relH, xmin = pred_xmin, xmax = pred_xmax), size = 1.2, alpha = 0.75)+ 
  labs(x = "Relative diameter", y = "Relative height")+
  theme_classic() +
  scale_color_manual(values = c('orange', 'red', 'gray65' ), alpha(0.2)) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.length=unit(.25, "cm"),
        legend.position = c(0.83, 0.85))




###################################################
########## Stem taper attribute plots ############# 
###################################################

# Use data_attr data.frame
colnames(data_attr)

# Split data to CCF and EAF data sets
data_attr_CCF <- data_attr[data_attr$treat == "CCF",]
data_attr_EAF <- data_attr[data_attr$treat == "EAF",]
data_attr <- as.data.table(data_attr)

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('volume_treat', 'volume_CC', 'volume_stand', 'volume_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_treat', 
                                'Canopy Class' = 'volume_CC', 
                                'Stand' = 'volume_stand', 
                                'Laasasenaho' = 'volume_laasis')

p_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_color_brewer(palette = "BrBG") + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(x = '', y = expression(vol~(m^3)),color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20)) 



plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('volume_upp_treat', 'volume_upp_CC', 'volume_upp_stand', 'volume_upp_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_upp_treat', 
                                'Canopy Class' = 'volume_upp_CC', 
                                'Stand' = 'volume_upp_stand', 
                                'laasasenaho' = 'volume_upp_laasis')


p_u_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") + 
  labs(x = '', y = expression(vol_top~(m^3)), color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('volume_bott_treat', 'volume_bott_CC', 'volume_bott_stand', 'volume_bott_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_bott_treat', 
                                'Canopy Class' = 'volume_bott_CC', 
                                'Stand' = 'volume_bott_stand', 
                                'Laasasenaho' = 'volume_bott_laasis')

p_b_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = expression(vol_bottom~(m^3)), color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('ff_treat', 'ff_CC', 'ff_stand', 'ff_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'ff_treat', 
                                'Canopy Class' = 'ff_CC', 
                                'Stand' = 'ff_stand', 
                                'Laasasenaho' = 'ff_laasis')

p_ff <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = 'ff', color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('ff50_treat', 'ff50_CC', 'ff50_stand', 'ff50_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'ff50_treat', 
                                'Canopy Class' = 'ff50_CC', 
                                'Stand' = 'ff50_stand', 
                                'Laasasenaho' = 'ff50_laasis')

p_ff50 <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = 'ff_50%', color = '') + ylim(c(0.95,2.5)) + 
  theme_classic() +
  theme(text = element_text(size = 20))


plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('fh_treat', 'fh_CC', 'fh_stand', 'fh_laasis'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'fh_treat', 
                                'Canopy Class' = 'fh_CC', 
                                'Stand' = 'fh_stand', 
                                'Laasasenaho' = 'fh_laasis')

p_fh <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = 'fh', color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))




ggarrange(p_vol, p_u_vol, p_b_vol, p_ff, p_ff50, p_fh, common.legend = TRUE, legend = "bottom", align = "v")



####################################################
########## Single tree attribute plots ############# 
####################################################

colnames(data_attr)

#### Treatment ####
plot_data <- data.frame(attribute = c('volume_tree', 'volume_bott_tree', 'volume_upp_tree', 'rel_vol', 'rel_bott', 
                                      'rel_upper', 'ff_tree', 'ff50_tree', 'fh_tree', 'rel_taper', 'taper', 'slend', 'formQ'),
                        ylim1 = c(0.0,0.0,0.0,0.0,0.75,0.0,0.0,0.0,0.0,0.0,0.0,0.6,0.0),
                        ylim2 = c(2.0,1.6,0.35,0.35,1.0,0.25,0.8,1.5,1.8,0.5,18,1.1,1.02),
                        titles = c("vol m^3", 'vol_bottom m^3', 'vol_top m^3', 'r_vol', 'r_vol_bottom', 
                                   'r_vol_top', 'ff', 'ff_50%', 'fh', 'r_taper', 'taper', 
                                   'slend', 'formQ'))
plot_list_treat <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr), id.vars = 'treat',
                      measure.vars = c(i))
  
  lim1 <- plot_data$ylim1[plot_data$attribute == i]
  lim2 <- plot_data$ylim2[plot_data$attribute == i]
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = treat, y = value, color = treat), lwd = 1, fatten = 4) +
    scale_color_brewer(palette = "Set2") +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 20)) + 
    ylim(lim1,lim2)
  
  plot_list_treat[[i]] <- treat_plot
  
}
plot_list_treat$fh_tree


#### Canopy Class plot ####

plot_list_cc <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr), id.vars = 'CC',
                      measure.vars = c(i))
  
  lim1 <- plot_data$ylim1[plot_data$attribute == i]
  lim2 <- plot_data$ylim2[plot_data$attribute == i]
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = CC, y = value, color = CC), lwd = 1, fatten = 3) +
    scale_color_brewer(palette = "Set2") +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 20)) + 
    scale_x_discrete(labels=c("CCF 1", "CCF 2", "CCF 3", "EAF 1", "EAF 2", "EAF 3")) +
    ylim(lim1,lim2)
  
  plot_list_cc[[i]] <- treat_plot
  
}
plot_list_cc$ff_tree


#### EAF Stands ####

plot_list_EAF <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr_EAF), id.vars = 'stand',
                      measure.vars = c(i))
  
  lim1 <- plot_data$ylim1[plot_data$attribute == i]
  lim2 <- plot_data$ylim2[plot_data$attribute == i]
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = stand, y = value, color = stand), lwd = 1, fatten = 3) +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 20)) + 
    ylim(lim1,lim2)
  
  plot_list_EAF[[i]] <- treat_plot
  
}
plot_list_EAF$ff_tree


#### CCF Plots #### 

plot_list_CCF <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr_CCF), id.vars = 'stand',
                      measure.vars = c(i))
  
  lim1 <- plot_data$ylim1[plot_data$attribute == i]
  lim2 <- plot_data$ylim2[plot_data$attribute == i]
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = stand, y = value, color = stand), lwd = 1, fatten = 3) + 
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 20)) + 
    ylim(lim1,lim2)
  
  plot_list_CCF[[i]] <- treat_plot
  
}
plot_list_CCF$ff_tree

ggarrange(plot_list_treat$rel_vol + 
            labs(y = "r_vol") + 
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$rel_bott +
            labs(y = "r_vol_bottom") +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$rel_upper +
            labs(y = "r_vol_top") +
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$fh_tree + 
            labs(y = expression(fh~(dm^3/cm^2))) + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$ff_tree + 
            labs(y = "ff") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()), 
          plot_list_treat$ff50_tree + 
            labs(y = "ff50") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$formQ + 
            labs(y = "formQ") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$slend + 
            labs(y = "slend") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$slend + 
            labs(y = "slend") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$taper + 
            labs(y = "taper") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$volume_tree + 
            labs(y = "vol") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$volume_bott_tree + 
            labs(y = "vol_bottom") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),
          plot_list_treat$volume_upp_tree + 
            labs(y = "vol_top") + 
            theme(axis.title.x = element_blank(),
                  plot.title = element_blank()),align = "v")


treat_plots <- ggarrange(plot_list_treat$rel_vol + 
                           labs(y = "r_vol") + 
                           theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 plot.title = element_blank()),
                         
                         plot_list_treat$rel_bott +
                           labs(y = "r_vol_bottom") +
                           theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 plot.title = element_blank()),
                         
                         plot_list_treat$rel_upper +
                           labs(y = "r_vol_top") +
                           theme(axis.title.x = element_blank(),
                                 plot.title = element_blank()),
                         
                         plot_list_treat$fh + 
                           labs(y = expression(fh~(dm^3/cm^2))) + 
                           theme(axis.title.x = element_blank(),
                                 plot.title = element_blank()), align = "v")


treat_plots


cc_plot <- ggarrange(plot_list_cc$volume_tree + 
                       labs(y = expression(vol~(m^3)), title = "Canopy class") + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             plot.title = element_text(face = "bold", hjust = 0.5)), 
                     
                     plot_list_cc$volume_upp_tree + 
                       labs(y = expression(vol_top~(m^3))) + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             plot.title = element_blank()), 
                     
                     plot_list_cc$volume_bott_tree + 
                       labs(y = expression(vol_bottom~(m^3))) + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             plot.title = element_blank()), 
                     
                     plot_list_cc$fh_tree + 
                       labs(y = expression(fh~(dm^3/cm^2))) + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.x = element_blank(),
                             plot.title = element_blank()),
                     
                     plot_list_cc$slend + 
                       labs(y = "slend") + 
                       theme(axis.title.x = element_blank(),
                             plot.title = element_blank()),
                     
                     ncol = 1, nrow = 5, align = "v")

EAF_plot <- ggarrange(plot_list_EAF$volume_tree +
                        labs(title = "EAF stands") +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_text(face = "bold", hjust = 0.5)), 
                      
                      plot_list_EAF$volume_upp_tree +
                        labs(y = expression(vol_top~(m^3))) +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_EAF$volume_bott_tree +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_EAF$fh_tree +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_EAF$slend +
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.title = element_blank(),
                              plot.title = element_blank()),
                      
                      ncol = 1, nrow = 5, align = "v")

CCF_plot <- ggarrange(plot_list_CCF$volume_tree + 
                        labs(title = "CCF stands") +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_text(face = "bold", hjust = 0.5)), 
                      
                      plot_list_CCF$volume_upp_tree + 
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_CCF$volume_bott_tree + 
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_CCF$fh_tree + 
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              plot.title = element_blank()), 
                      
                      plot_list_CCF$slend +  
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.title = element_blank(),
                              plot.title = element_blank()),
                      ncol = 1, nrow = 5, align = "v")

ggarrange(cc_plot, EAF_plot, CCF_plot, ncol = 3, align = "v")

