

# Paired t-test 


# data.frame to store the results
tableY=data.frame("Variable"=character(), 
                  "Stratum"=character(),
                  "Factor"=character(), 
                  "R2"=numeric(),
                  "bias"=numeric(),
                  "RMSE"=numeric(),
                  "t-value"=numeric(), 
                  "p"=numeric(),
                  "95_low"=numeric(),
                  "95_upp"=numeric())

resp=c("volume", "volume_upp", "volume_bott", "ff", "ff50", "fh")

# CCF and RF combined dataset
for(r in resp){
  
  x=paste0(r, "_tree")
  
  y=paste0(r, "_Laasasenaho")
  bias= mean(data_attr[,x] - data_attr[,y])/mean(data_attr[,x])
  # Relative RMSE
  rmse = sqrt(mean((data_attr[,x] - data_attr[,y])^2))/mean(data_attr[,x])
  rmse(data_attr[,x], data_attr[,y])
  sqrt(mean((data_attr[,x] - data_attr[,y])^2))
  t=t.test(data_attr[,x],data_attr[,y], paired = TRUE)
  s=summary(lm(data_attr[,x]~data_attr[,y]))
  plot(data_attr[,x], data_attr[,y], xlab=r, ylab="predicted")
  abline(0,1)
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                    "Stratum"="ALL",
                    "Factor"="laasasenaho",
                    "R2"=s$r.squared, 
                    "bias"=bias,
                    "RMSE"=rmse,
                    "t-value"=t$statistic, 
                    "p"=t$p.value,
                    "95_low"=t$conf.int[1],
                    "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_treat")
  bias= mean(data_attr[,x] - data_attr[,y])/mean(data_attr[,x])
  rmse= sqrt(mean((data_attr[,x] - data_attr[,y])^2))/mean(data_attr[,x])
  t=t.test(data_attr[,x],data_attr[,y], paired = TRUE)
  s=summary(lm(data_attr[,x]~data_attr[,y]))
  points(data_attr[,x], data_attr[,y], col="red")
  tableY=rbind(tableY, 
               data.frame("Variable"=r, 
                    "Stratum"="ALL",
                    "Factor"="treatment", 
                    "R2"=s$r.squared, 
                    "bias"=bias,
                    "RMSE"=rmse,
                    "t-value"=t$statistic, 
                    "p"=t$p.value,
                    "95_low"=t$conf.int[1],
                    "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_stand")
  bias= mean(data_attr[,x] - data_attr[,y])/mean(data_attr[,x])
  rmse= sqrt(mean((data_attr[,x] - data_attr[,y])^2))/mean(data_attr[,x])
  t=t.test(data_attr[,x],data_attr[,y], paired = TRUE)
  s=summary(lm(data_attr[,x]~data_attr[,y]))
  points(data_attr[,x], data_attr[,y], col="blue")
  tableY=rbind(tableY, 
               data.frame("Variable"=r, 
                    "Stratum"="ALL",
                    "Factor"="stand", 
                    "R2"=s$r.squared, 
                    "bias"=bias,
                    "RMSE"=rmse,
                    "t-value"=t$statistic, 
                    "p"=t$p.value,
                    "95_low"=t$conf.int[1],
                    "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_CC")
  bias= mean(data_attr[,x] - data_attr[,y])/mean(data_attr[,x])
  rmse= sqrt(mean((data_attr[,x] - data_attr[,y])^2))/mean(data_attr[,x])
  t=t.test(data_attr[,x],data_attr[,y], paired = TRUE)
  s=summary(lm(data_attr[,x]~data_attr[,y]))
  points(data_attr[,x], data_attr[,y], col="green")
  tableY=rbind(tableY, 
               data.frame("Variable"=r, 
                    "Stratum"="ALL",
                    "Factor"="crownClass", 
                    "R2"=s$r.squared, 
                    "bias"=bias,
                    "RMSE"=rmse,
                    "t-value"=t$statistic, 
                    "p"=t$p.value,
                    "95_low"=t$conf.int[1],
                    "95_upp"=t$conf.int[2]))
  
}
tableY
data_attr_CCF <- data_attr[data_attr$treat == "CCF",]

# CCF dataset
for(r in resp){
  
  x=paste0(r, "_tree")
  
  y=paste0(r, "_Laasasenaho")
  bias= mean(data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])/mean(data_attr[data_attr$treat=="CCF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])^2))/mean(data_attr[data_attr$treat=="CCF",x])
  t=t.test(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="CCF",x]~data_attr[data_attr$treat=="CCF",y]))
  plot(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], xlab=r, ylab="predicted")
  abline(0,1)
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="CCF",
                          "Factor"="laasasenaho",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_treat")
  bias= mean(data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])/mean(data_attr[data_attr$treat=="CCF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])^2))/mean(data_attr[data_attr$treat=="CCF",x])
  t=t.test(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="CCF",x]~data_attr[data_attr$treat=="CCF",y]))
  points(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], col="red")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="CCF",
                          "Factor"="Treatment",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_stand")
  bias= mean(data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])/mean(data_attr[data_attr$treat=="CCF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])^2))/mean(data_attr[data_attr$treat=="CCF",x])
  t=t.test(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="CCF",x]~data_attr[data_attr$treat=="CCF",y]))
  points(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], col="blue")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="CCF",
                          "Factor"="Stand",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_CC")
  bias= mean(data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])/mean(data_attr[data_attr$treat=="CCF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="CCF",x] - data_attr[data_attr$treat=="CCF",y])^2))/mean(data_attr[data_attr$treat=="CCF",x])
  t=t.test(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="CCF",x]~data_attr[data_attr$treat=="CCF",y]))
  points(data_attr[data_attr$treat=="CCF",x], data_attr[data_attr$treat=="CCF",y], col="green")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="CCF",
                          "Factor"="crownClass",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
}

# RF dataset
for(r in resp){
  
  x=paste0(r, "_tree")
  
  y=paste0(r, "_Laasasenaho")
  bias= mean(data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])/mean(data_attr[data_attr$treat=="RF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])^2))/mean(data_attr[data_attr$treat=="RF",x])
  t=t.test(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="RF",x]~data_attr[data_attr$treat=="RF",y]))
  plot(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], xlab=r, ylab="predicted")
  abline(0,1)
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="RF",
                          "Factor"="laasasenaho",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_treat")
  bias= mean(data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])/mean(data_attr[data_attr$treat=="RF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])^2))/mean(data_attr[data_attr$treat=="RF",x])
  t=t.test(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="RF",x]~data_attr[data_attr$treat=="RF",y]))
  points(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], col="red")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="RF",
                          "Factor"="Treatment",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_stand")
  bias= mean(data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])/mean(data_attr[data_attr$treat=="RF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])^2))/mean(data_attr[data_attr$treat=="RF",x])
  t=t.test(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="RF",x]~data_attr[data_attr$treat=="RF",y]))
  points(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], col="blue")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="RF",
                          "Factor"="Stand",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
  y=paste0(r, "_CC")
  bias= mean(data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])/mean(data_attr[data_attr$treat=="RF",x])
  rmse= sqrt(mean((data_attr[data_attr$treat=="RF",x] - data_attr[data_attr$treat=="RF",y])^2))/mean(data_attr[data_attr$treat=="RF",x])
  t=t.test(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], paired = TRUE)
  s=summary(lm(data_attr[data_attr$treat=="RF",x]~data_attr[data_attr$treat=="RF",y]))
  points(data_attr[data_attr$treat=="RF",x], data_attr[data_attr$treat=="RF",y], col="green")
  tableY=rbind(tableY, 
               data.frame("Variable"=r,
                          "Stratum"="RF",
                          "Factor"="crownClass",
                          "R2"=s$r.squared, 
                          "bias"=bias,
                          "RMSE"=rmse,
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
}

tableY


# Single tree unpaired t-test
# between CCF and RF

# Vector of examined attributes
resp=c("ff_tree","ff50_tree","fh_tree","volume_tree","volume_upp_tree","volume_bott_tree",
       "rel_vol","rel_bott","rel_upper","taper","rel_taper","slend","formQ")

# First examine normal distribution
# -> normal distribution is examined separately by both treatments

# create plot panel containing four panels to plot hist and Q-Q plot from both CCF and RF treatments 
par(mfrow = c(2,2))

# Loop makes Shapiro Wilk test, plots histogram and Q-Q plot for all atributes 
# -> separately to CCF and RF treatment data-sets
# -> loop stops after each iteration (Esc = stops the loop)
for (r in resp){
  
  # CCF 
  print(shapiro.test(data_attr[data_attr$treat == "CCF",r])) # Shapiro Wilk test 
  hist(data_attr[data_attr$treat == "CCF",r], main = paste(r,"_CCF",sep=""), xlab = r) # Histogram plot
  qqnorm(data_attr[data_attr$treat == "CCF",r], main = paste(r,"_CCF",sep="")) # Q-Q plot
  qqline(data_attr[data_attr$treat == "CCF",r], col ="red") # Q-Q plot line
  
  # RF
  print(shapiro.test(data_attr[data_attr$treat == "RF",r]))
  hist(data_attr[data_attr$treat == "RF",r], main = paste(r,"_RF",sep=""), xlab = r)
  qqnorm(data_attr[data_attr$treat == "RF",r], main = paste(r,"_RF",sep=""),)
  qqline(data_attr[data_attr$treat == "RF",r], col ="red")
  
  a <- readline("Press any button to continue")
}



t.test(data_attr[data_attr$treat=="CCF","rel_vol"], data_attr[data_attr$treat=="RF","rel_vol"])

tableX = data.frame("Variable"=character(), 
                    "Factor"=character(),
                    "t-value"=numeric(), 
                    "p"=numeric(),
                    "95_low"=numeric(),
                    "95_upp"=numeric())
for(r in resp){
  t=t.test(data_attr[data_attr$treat=="CCF",r], data_attr[data_attr$treat=="RF",r])
  tableX=rbind(tableX, 
               data.frame("Variable"=r,
                          "Factor"="Treatment",
                          "t-value"=t$statistic, 
                          "p"=t$p.value,
                          "95_low"=t$conf.int[1],
                          "95_upp"=t$conf.int[2]))
  
}

tableX

