
######################
#### Result plots ####
######################

# set working directory

###########################
### Plotting stemcurves ###
###########################



# First plot all the stem curves with different stratum 
plot_data <- data.frame(relH = c(0.01,0.025,0.05,0.075,0.10,0.15,0.20,0.25,
                                 0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,
                                 0.75,0.80,0.85,0.90,0.95,1.0))

plot_data$pred_SD <- f(plot_data$relH)

plot_lists[["Laasasenaho"]] <- ggplot(data = plot_data, aes(x = pred_SD, y = relH))+
  geom_line( colour = "red", linewidth = 1) + xlim(-0.02,1.8) + ylim(-0.02,1.1) +  
  labs(x = "Relative d", y = "Relative h", title = "Laasasenaho") + theme_classic()

do.call("grid.arrange", c(plot_lists, ncol = 5, nrow = 6))


#### Plot treatment and Laasasenaho stem curves to same plot ####

avg_data <- avg_fun(pros_all, "treat")

plot_data <- data.frame()

for (i in levels(as.factor(avg_data$c))){
  
  plot_data_2 <- avg_data[avg_data$c == i,]
  plot_data_2$pred_sD <- predict(models[[i]],avg_data[avg_data$c == i,])
  plot_data_2$pred_xmin <- plot_data_2$pred_sD - plot_data_2$sd_relD
  plot_data_2$pred_xmax <- plot_data_2$pred_sD + plot_data_2$sd_relD
  
  plot_data <- rbind(plot_data,plot_data_2)
}

plot_data_2 <- data.frame(c = "Laasasenaho", relH = c(0.01,0.025,0.05,0.075,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.0))

plot_data_2 <- plot_data_2 %>% add_column(relD = NA, 
                                      dbh = NA,
                                      sd_relD = NA,
                                      sd_dbh = NA,
                                      pred_sD = f(plot_data_2$relH),
                                      pred_xmin = NA,
                                      pred_xmax = NA)

plot_data <- rbind(plot_data, plot_data_2)

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
                 measure.vars = c('volume_treat', 'volume_CC', 'volume_stand', 'volume_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_treat', 
                                'Canopy Class' = 'volume_CC', 
                                'Stand' = 'volume_stand', 
                                'Laasasenaho' = 'volume_Laasasenaho')

p_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_color_brewer(palette = "BrBG") + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(x = '', y = expression(vol~(m^3)),color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20)) 



plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('volume_upp_treat', 'volume_upp_CC', 'volume_upp_stand', 'volume_upp_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_upp_treat', 
                                'Canopy Class' = 'volume_upp_CC', 
                                'Stand' = 'volume_upp_stand', 
                                'laasasenaho' = 'volume_upp_Laasasenaho')


p_u_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") + 
  labs(x = '', y = expression(vol_top~(m^3)), color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('volume_bott_treat', 'volume_bott_CC', 'volume_bott_stand', 'volume_bott_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'volume_bott_treat', 
                                'Canopy Class' = 'volume_bott_CC', 
                                'Stand' = 'volume_bott_stand', 
                                'Laasasenaho' = 'volume_bott_Laasasenaho')

p_b_vol <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = expression(vol_bottom~(m^3)), color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('ff_treat', 'ff_CC', 'ff_stand', 'ff_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'ff_treat', 
                                'Canopy Class' = 'ff_CC', 
                                'Stand' = 'ff_stand', 
                                'Laasasenaho' = 'ff_Laasasenaho')

p_ff <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = 'ff', color = '') + 
  theme_classic() +
  theme(text = element_text(size = 20))

plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('ff50_treat', 'ff50_CC', 'ff50_stand', 'ff50_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'ff50_treat', 
                                'Canopy Class' = 'ff50_CC', 
                                'Stand' = 'ff50_stand', 
                                'Laasasenaho' = 'ff50_Laasasenaho')

p_ff50 <- ggplot(plot_data) +
  geom_boxplot(aes(x = treat, y = value, color = variable), lwd = 1, fatten=4,
               position=position_dodge(0.89)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_brewer(palette = "BrBG") +  
  labs(x = '', y = 'ff_50%', color = '') + ylim(c(0.95,2.5)) + 
  theme_classic() +
  theme(text = element_text(size = 20))


plot_data <- melt(data_attr, id.vars = 'treat',
                 measure.vars = c('fh_treat', 'fh_CC', 'fh_stand', 'fh_Laasasenaho'))
plot_data_2 <- plot_data
plot_data_2$treat <- 'CCF & EAF'
plot_data <- rbind(plot_data,plot_data_2)
plot_data$variable <- fct_recode(plot_data$variable,
                                'Treatment' = 'fh_treat', 
                                'Canopy Class' = 'fh_CC', 
                                'Stand' = 'fh_stand', 
                                'Laasasenaho' = 'fh_Laasasenaho')

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
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = treat, y = value, color = treat), lwd = 1, fatten = 4) +
    scale_color_brewer(palette = "Set2") +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 20)) 
  
  plot_list_treat[[i]] <- treat_plot
  
}


#### Canopy Class plot ####

plot_list_cc <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr), id.vars = 'CC',
                      measure.vars = c(i))
  data_single$CC <- factor(data_single$CC, levels = c("1 CC CCF", "2 CC CCF", "3 CC CCF",
                                                      "1 CC EAF", "2 CC EAF", "3 CC EAF"))
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = CC, y = value, color = CC), lwd = 1, fatten = 3) +
    scale_color_brewer(palette = "Set2") +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  plot_list_cc[[i]] <- treat_plot
  
}

#### CCF Plots #### 

plot_list_CCF <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr_CCF), id.vars = 'stand',
                      measure.vars = c(i))
  
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = stand, y = value, color = stand), lwd = 1, fatten = 3) + 
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  plot_list_CCF[[i]] <- treat_plot
  
}

#### EAF Stands ####

plot_list_EAF <- list()

for (i in levels(as.factor(plot_data$attribute))) {
  data_single <- melt(as.data.table(data_attr_EAF), id.vars = 'stand',
                      measure.vars = c(i))
  
  treat_plot <- ggplot(data_single) +
    geom_boxplot(aes(x = stand, y = value, color = stand), lwd = 1, fatten = 3) +
    ggtitle(plot_data$titles[plot_data$attribute==i]) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  plot_list_EAF[[i]] <- treat_plot
  
}


# Labs for the plots
labs_plots <- list("ff","ff_50%",expression(fh~(dm^3/cm^2)),"formQ","r_vol_bottom","r_taper","r_vol_top",
                  "r_vol","slend",expression(taper~(cm)),expression(vol_bottom~(m^3)),expression(vol~(m^3)),
                  expression(vol_top~(m^3)))

## treat
for (i in 1:length(labs_plots)) {
  plot_list_treat[[i]]<- plot_list_treat[[i]] +
    labs(title = labs_plots[[i]]) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
}

plot_1 <- ggarrange(plot_list_treat$rel_vol,
                    plot_list_treat$rel_bott,
                    plot_list_treat$rel_upper, ncol = 3)
plot_2 <- ggarrange(plot_list_treat$volume_tree,
                    plot_list_treat$volume_upp_tree,
                    plot_list_treat$volume_bott_tree,
                    plot_list_treat$fh_tree,
                    plot_list_treat$ff_tree, ncol = 5)

plot_list_treat$ff50_tree <- plot_list_treat$ff50_tree + labs(color = " ")
plot_3 <- ggarrange(plot_list_treat$ff50_tree,
                    plot_list_treat$formQ,
                    plot_list_treat$slend,
                    plot_list_treat$taper,
                    plot_list_treat$rel_taper, ncol = 5,common.legend = TRUE, legend = "bottom")


ggarrange(plot_1,plot_2,plot_3,
          align = "hv",ncol = 1, nrow = 3)



# CC
for (i in 1:length(labs_plots)) {
  plot_list_cc[[i]]<- plot_list_cc[[i]] +
    labs(title = labs_plots[[i]]) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
}


plot_1 <- ggarrange(plot_list_cc$rel_vol,
                    plot_list_cc$rel_bott,
                    plot_list_cc$rel_upper, ncol = 3)
plot_2 <- ggarrange(plot_list_cc$volume_tree,
                    plot_list_cc$volume_upp_tree,
                    plot_list_cc$volume_bott_tree,
                    plot_list_cc$fh_tree,
                    plot_list_cc$ff_tree, ncol = 5)

plot_list_cc$ff50_tree <- plot_list_cc$ff50_tree + labs(color = " ")
plot_3 <- ggarrange(plot_list_cc$ff50_tree,
                    plot_list_cc$formQ,
                    plot_list_cc$slend,
                    plot_list_cc$taper,
                    plot_list_cc$rel_taper, ncol = 5,common.legend = TRUE, legend = "bottom")


CC_plot <- ggarrange(plot_1,plot_2,plot_3,
                      align = "hv",ncol = 1, nrow = 3)


## CCF
for (i in 1:length(labs_plots)) {
  plot_list_CCF[[i]]<- plot_list_CCF[[i]] +
    labs(title = labs_plots[[i]]) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
}

plot_1 <- ggarrange(plot_list_CCF$rel_vol,
                    plot_list_CCF$rel_bott,
                    plot_list_CCF$rel_upper, ncol = 3)
plot_2 <- ggarrange(plot_list_CCF$volume_tree,
                    plot_list_CCF$volume_upp_tree,
                    plot_list_CCF$volume_bott_tree,
                    plot_list_CCF$fh_tree,
                    plot_list_CCF$ff_tree, ncol = 5)

plot_list_CCF$ff50_tree <- plot_list_CCF$ff50_tree + labs(color = " ")
plot_3 <- ggarrange(plot_list_CCF$ff50_tree,
                    plot_list_CCF$formQ,
                    plot_list_CCF$slend,
                    plot_list_CCF$taper,
                    plot_list_CCF$rel_taper, ncol = 5,common.legend = TRUE, legend = "bottom")


CCF_plot <- ggarrange(plot_1,plot_2,plot_3,
                     align = "hv",ncol = 1, nrow = 3)

## EAF
for (i in 1:length(labs_plots)) {
  plot_list_EAF[[i]]<- plot_list_EAF[[i]] +
    labs(title = labs_plots[[i]]) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
}

plot_1 <- ggarrange(plot_list_EAF$rel_vol,
                    plot_list_EAF$rel_bott,
                    plot_list_EAF$rel_upper, ncol = 3)
plot_2 <- ggarrange(plot_list_EAF$volume_tree,
                    plot_list_EAF$volume_upp_tree,
                    plot_list_EAF$volume_bott_tree,
                    plot_list_EAF$fh_tree,
                    plot_list_EAF$ff_tree, ncol = 5)

plot_list_EAF$ff50_tree <- plot_list_EAF$ff50_tree + labs(color = " ")
plot_3 <- ggarrange(plot_list_EAF$ff50_tree,
                    plot_list_EAF$formQ,
                    plot_list_EAF$slend,
                    plot_list_EAF$taper,
                    plot_list_EAF$rel_taper, ncol = 5,common.legend = TRUE, legend = "bottom")


EAF_plot<- ggarrange(plot_1,plot_2,plot_3,
                     align = "hv",ncol = 1, nrow = 3)

CC_plot
CCF_plot
EAF_plot

