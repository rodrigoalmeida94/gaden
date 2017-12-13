setwd("~/catkin_ws/src/gaden/orchard_demo/demo")
set.seed(12345678)
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results.RData")
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results_01.RData")
library(RColorBrewer)
require(tikzDevice)
library(fields)
library(MASS)
library(xtable)
path_to_sims <- '/Users/rodrigoalmeida/Dropbox/Rodrigo/Thesis/Simulations/figures/'
# Making zones
main_volume <- list(25:174,25:130,1:32)
# trees + 2.5 m buffer around it
in_rows <- list(c((50-4):(50+4),(100-4):(100+4),(150-4):(150+4)),(50-4):(116+4),1:32)
# trees + 0.4 cm on each side (0.8 m per row)
inbetween_rows <- list(c((50+5):(100-5),(100+5):(150-5)),(50-4):(116+4),1:32)
#w <- 3.7
#h <- 3.7


X <- 1:199
Y <- 1:164
ii_prec <- cut(emission_prec*1000, breaks = seq(min(emission_prec*1000), max(emission_prec*1000), len = 5), include.lowest = TRUE)
ii_entc <- cut(emission_entc*1000, breaks = seq(min(emission_entc*1000), max(emission_entc*1000), len = 5), include.lowest = TRUE)
ii_c <- cut(emission_c*1000, breaks = seq(min(emission_c*1000), max(emission_c*1000), len = 5), include.lowest = TRUE)
ii <- list(ii_prec,ii_entc,ii_c)

sims <- list(sim_emission_c_0ms_s, sim_emission_c_2msX_s,sim_emission_c_2msY_s,sim_emission_c_5msX_s,sim_emission_c_5msY_s,sim_emission_entc_0ms_s,sim_emission_entc_2msX_s,sim_emission_entc_2msY_s,sim_emission_entc_5msX_s,sim_emission_entc_5msY_s,sim_emission_prec_0ms_s,sim_emission_prec_2msX_s,sim_emission_prec_2msY_s,sim_emission_prec_5msX_s,sim_emission_prec_5msY_s)

names(sims) <- c('sim_emission_c_0ms_s',"sim_emission_c_2msX_s","sim_emission_c_2msY_s","sim_emission_c_5msX_s","sim_emission_c_5msY_s",'sim_emission_entc_0ms_s',"sim_emission_entc_2msX_s","sim_emission_entc_2msY_s","sim_emission_entc_5msX_s","sim_emission_entc_5msY_s",'sim_emission_prec_0ms_s',"sim_emission_prec_2msX_s","sim_emission_prec_2msY_s","sim_emission_prec_5msX_s","sim_emission_prec_5msY_s")

#sims <- list(sim_emission_c_01ms_s, sim_emission_c_2msX_s,sim_emission_c_2msY_s,sim_emission_c_5msX_s,sim_emission_c_5msY_s,sim_emission_entc_0ms_s,sim_emission_entc_2msX_s,sim_emission_entc_2msY_s,sim_emission_entc_5msX_s,sim_emission_entc_5msY_s,sim_emission_prec_0ms_s,sim_emission_prec_2msX_s,sim_emission_prec_2msY_s,sim_emission_prec_5msX_s,sim_emission_prec_5msY_s)

#names(sims) <- c('sim_emission_c_01ms_s',"sim_emission_c_2msX_s","sim_emission_c_2msY_s","sim_emission_c_5msX_s","sim_emission_c_5msY_s",'sim_emission_entc_0ms_s',"sim_emission_entc_2msX_s","sim_emission_entc_2msY_s","sim_emission_entc_5msX_s","sim_emission_entc_5msY_s",'sim_emission_prec_0ms_s',"sim_emission_prec_2msX_s","sim_emission_prec_2msY_s","sim_emission_prec_5msX_s","sim_emission_prec_5msY_s")

sim_avg <- list()
sim_std <- list()
sim_var <- list()
sim_max <- list()
sim_n <- list()

sim_avg_main <- list()
sim_std_main <- list()
sim_var_main <- list()
sim_max_main <- list()
sim_n_main <- list()

sim_avg_in <- list()
sim_std_in <- list()
sim_var_in <- list()
sim_max_in <- list()
sim_n_in <- list()

sim_avg_inbet <- list()
sim_std_inbet <- list()
sim_var_inbet <- list()
sim_max_inbet <- list()
sim_n_inbet <- list()

# Makes 0 <- NA
for(i in 1:length(sims)){
  is.na(sims[[i]]) <- !sims[[i]]
}
rm(i)

for(sim in sims){
  #is.na(sim) <- !sim
  
  sim_avg <- c(sim_avg,list(apply(sim,4,mean, na.rm=T)))
  sim_avg_main <- c(sim_avg_main,list(apply(sim[main_volume[[1]],main_volume[[2]],main_volume[[3]],],4,mean, na.rm=T)))
  sim_avg_in <- c(sim_avg_in,list(apply(sim[in_rows[[1]],in_rows[[2]],in_rows[[3]],],4,mean, na.rm=T)))
  sim_avg_inbet <- c(sim_avg_inbet,list(apply(sim[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],],4,mean, na.rm=T)))
  
  sim_std <- c(sim_std,list(apply(sim,4,sd, na.rm=T)))
  sim_std_main <- c(sim_std_main,list(apply(sim[main_volume[[1]],main_volume[[2]],main_volume[[3]],],4,sd, na.rm=T)))
  sim_std_in <- c(sim_std_in,list(apply(sim[in_rows[[1]],in_rows[[2]],in_rows[[3]],],4,sd, na.rm=T)))
  sim_std_inbet <- c(sim_std_inbet,list(apply(sim[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],],4,sd, na.rm=T)))
  
  sim_var <- c(sim_var,list(apply(sim,4,var, na.rm=T)))
  sim_var_main <- c(sim_var_main,list(apply(sim[main_volume[[1]],main_volume[[2]],main_volume[[3]],],4,var, na.rm=T)))
  sim_var_in <- c(sim_var_in,list(apply(sim[in_rows[[1]],in_rows[[2]],in_rows[[3]],],4,var, na.rm=T)))
  sim_var_inbet <- c(sim_var_inbet,list(apply(sim[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],],4,var, na.rm=T)))
  
  sim_max <- c(sim_max,list(apply(sim,4,max, na.rm=T)))
  sim_max_main <- c(sim_max_main,list(apply(sim[main_volume[[1]],main_volume[[2]],main_volume[[3]],],4,max, na.rm=T)))
  sim_max_in <- c(sim_max_in,list(apply(sim[in_rows[[1]],in_rows[[2]],in_rows[[3]],],4,max, na.rm=T)))
  sim_max_inbet <- c(sim_max_inbet,list(apply(sim[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],],4,max, na.rm=T)))
  
  #is.na(sim) <- !sim
  sim_n <- c(sim_n,list(apply(sim,4,function(x) length(which(!is.na(x))))))
  sim_n_main <- c(sim_n_main,list(apply(sim[main_volume[[1]],main_volume[[2]],main_volume[[3]],],4,function(x) length(which(!is.na(x))))))
  sim_n_in <- c(sim_n_in,list(apply(sim[in_rows[[1]],in_rows[[2]],in_rows[[3]],],4,function(x) length(which(!is.na(x))))))
  sim_n_inbet <- c(sim_n_inbet,list(apply(sim[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],],4,function(x) length(which(!is.na(x))))))
}

rm(sim)

names(sim_avg) <- names(sims)
names(sim_std) <- names(sims)
names(sim_var) <- names(sims)
names(sim_max) <- names(sims)
names(sim_n) <- names(sims)

names(sim_avg_in) <- names(sims)
names(sim_std_in) <- names(sims)
names(sim_var_in) <- names(sims)
names(sim_max_in) <- names(sims)
names(sim_n_in) <- names(sims)

names(sim_avg_inbet) <- names(sims)
names(sim_std_inbet) <- names(sims)
names(sim_var_inbet) <- names(sims)
names(sim_max_inbet) <- names(sims)
names(sim_n_inbet) <- names(sims)

names(sim_avg_main) <- names(sims)
names(sim_std_main) <- names(sims)
names(sim_var_main) <- names(sims)
names(sim_max_main) <- names(sims)
names(sim_n_main) <- names(sims)

wind_pch <- c(1,2,3)
emission_col <- brewer.pal(3,'Reds')
zones <- c('Environment','Main volume','In rows','In-between rows')
zones_files <- c('Environment','Main_volume','In_rows','Inbetween_rows')

# Plotting
# Average Concentration XY maps ----
#breaks_stages <- list(seq(0,12,1.2),seq(0,35,3.5),seq(0,50,5))

for(n in 1:15){
  xy_plane <- apply(sims[[n]],c(1,2),max)
  print(paste('range XY',range(xy_plane)[1], '-', range(xy_plane)[2]))
  if(n %in% 1:5){
    stage <- 3
  }
  if(n %in% 6:10){
    stage <- 2
  }
  if(n %in% 11:15){
    stage <- 1
  }
  
  pdf.options(family='Helvetica-Narrow')
  pdf(paste0(path_to_sims,names(sims)[[n]],'_XY.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image.plot(X,Y,xy_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,174),ylim=c(25,141), ann=FALSE, legend.lab = 'Ethylene concentration ($ppb$)')
  image(X,Y,trees_xy, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
  points(coords$e_x*10,coords$e_y*10,cex=1,pch=23,col='black',bg=colorRampPalette(c("white", "red"))(4)[ii[[stage]]])
  dev.off()
}
rm(s)
#breaks = breaks_stages[[stage]]

tikz(file = paste0(path_to_sims,'legend_c_XY.tex'), height = 0.7)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('top',levels(ii_c), pch=23,pt.bg = colorRampPalette(c("white", "red"))(4), xpd = TRUE, horiz = TRUE, bty = "n", title = '$E_3$ ($Ls^{-1}$)'  )
dev.off()

tikz(file = paste0(path_to_sims,'legend_prec_XY.tex'), height = 0.7)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('top',levels(ii_prec), pch=23,pt.bg = colorRampPalette(c("white", "red"))(4), xpd = TRUE, horiz = TRUE, bty = "n", title = '$E_1$ ($Ls^{-1}$)'  )
dev.off()

tikz(file = paste0(path_to_sims,'legend_entc_XY.tex'), height = 0.7)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('top',levels(ii_entc), pch=23,pt.bg = colorRampPalette(c("white", "red"))(4), xpd = TRUE, horiz = TRUE, bty = "n", title = '$E_2$ ($Ls^{-1}$)'  )
dev.off()

# Average Concentration ----
z <- 1
for(s in list(sim_avg,sim_avg_main,sim_avg_in,sim_avg_inbet)){
  tikz(file = paste0(path_to_sims,'Avg_',zones_files[z],'.tex'))
plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Average ethylene concentration ($ppb$)', main = zones[z], lwd=3)
z =z+ 1
for(i in 2:length(sims)){
  if(i %in% 2:5){
    p_t <- emission_col[3]
  }
  if(i %in% 6:10){
    p_t <- emission_col[2]
  }
  if(i %in% 11:15){
    p_t <- emission_col[1]
  }
  
  if(i %in% c(6,11)){
    p_ch <- wind_pch[1]
  }
  if(i %in% c(2,3,7,8,12,13)){
    p_ch <- wind_pch[2]
  }
  if(i %in% c(4,5,9,10,14,15)){
    p_ch <- wind_pch[3]
  }
  points(timesteps[1:length(s[[i]])],s[[i]], col=p_t,pch=p_ch, type = 'b',lwd=3)
}
dev.off()
}

# Variance ----
z <- 1
for(s in list(sim_var,sim_var_main,sim_var_in,sim_var_inbet)){
  tikz(file = paste0(path_to_sims,'Var_',zones_files[z],'.tex'))
plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s), xlim=range(timesteps),col=emission_col[3], type = 'b',lwd=3, pch=wind_pch[1], xlab = 'Time ($s$)',ylab = 'Variance of ethylene concentration',main = zones[z])
z =z+ 1
for(i in 2:length(sims)){
  if(i %in% 2:5){
    p_t <- emission_col[3]
  }
  if(i %in% 6:10){
    p_t <- emission_col[2]
  }
  if(i %in% 11:15){
    p_t <- emission_col[1]
  }
  if(i %in% c(6,11)){
    p_ch <- wind_pch[1]
  }
  if(i %in% c(2,3,7,8,12,13)){
    p_ch <- wind_pch[2]
  }
  if(i %in% c(4,5,9,10,14,15)){
    p_ch <- wind_pch[3]
  }
  points(timesteps[1:length(s[[i]])],s[[i]], col=p_t, pch=p_ch,type = 'b',lwd=3)
}
dev.off()
}

# Maximum ----
z <- 1
for(s in list(sim_max,sim_max_main,sim_max_in,sim_max_inbet)){
  tikz(file = paste0(path_to_sims,'Max_',zones_files[z],'.tex'))
plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s), xlim=range(timesteps), col=emission_col[3], type = 'b', pch=wind_pch[1], lwd=3,xlab = 'Time ($s$)',ylab = 'Maximum ethylene concentration ($ppb$)', main = zones[z])
z =z+ 1
for(i in 2:length(sims)){
  if(i %in% 2:5){
    p_t <- emission_col[3]
  }
  if(i %in% 6:10){
    p_t <- emission_col[2]
  }
  if(i %in% 11:15){
    p_t <- emission_col[1]
  }
  if(i %in% c(6,11)){
    p_ch <- wind_pch[1]
  }
  if(i %in% c(2,3,7,8,12,13)){
    p_ch <- wind_pch[2]
  }
  if(i %in% c(4,5,9,10,14,15)){
    p_ch <- wind_pch[3]
  }
  points(timesteps[1:length(s[[i]])],s[[i]], col=p_t,type = 'b',pch=p_ch, lwd=3)
}
dev.off()
}

# N ----
z <- 1
for(s in list(sim_n,sim_n_main,sim_n_in,sim_n_inbet)){
  tikz(file = paste0(path_to_sims,'N_',zones_files[z],'.tex'))
  plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s), col=emission_col[3],  xlim=range(timesteps), type = 'b', pch=wind_pch[1], xlab = 'Time ($s$)',ylab = '$n$', main = zones[z], lwd=3)
z =z+ 1
for(i in 2:length(sims)){
  if(i %in% 2:5){
    p_t <- emission_col[3]
  }
  if(i %in% 6:10){
    p_t <- emission_col[2]
  }
  if(i %in% 10:15){
    p_t <- emission_col[1]
  }
  if(i %in% c(6,11)){
    p_ch <- wind_pch[1]
  }
  if(i %in% c(2,3,7,8,12,13)){
    p_ch <- wind_pch[2]
  }
  if(i %in% c(4,5,9,10,14,15)){
    p_ch <- wind_pch[3]
  }
  points(timesteps[1:length(s[[i]])],s[[i]], col=p_t,type = 'b',pch=p_ch, lwd=3)
}
dev.off()
}

# Standard deviation ----
z <- 1
for(s in list(sim_std,sim_std_main,sim_std_in,sim_std_inbet)){
  tikz(file = paste0(path_to_sims,'Sd_',zones_files[z],'.tex'))
  plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s), col=emission_col[3],  xlim=range(timesteps), type = 'b', pch=wind_pch[1], xlab = 'Time ($s$)',ylab = 'Standard deviation of ethylene concentration ($ppb$)', main = zones[z], lwd=3)
  z =z+ 1
  for(i in 2:length(sims)){
    if(i %in% 2:5){
      p_t <- emission_col[3]
    }
    if(i %in% 6:10){
      p_t <- emission_col[2]
    }
    if(i %in% 10:15){
      p_t <- emission_col[1]
    }
    if(i %in% c(6,11)){
      p_ch <- wind_pch[1]
    }
    if(i %in% c(2,3,7,8,12,13)){
      p_ch <- wind_pch[2]
    }
    if(i %in% c(4,5,9,10,14,15)){
      p_ch <- wind_pch[3]
    }
    points(timesteps[1:length(s[[i]])],s[[i]], col=p_t,type = 'b',pch=p_ch, lwd=3)
  }
  dev.off()
}

rm(z,s,i,p_ch,p_t)

# Testing random sampling ----

z.test = function(a, mu, sd){
  zeta = (mean(a) - mu) / (sd / sqrt(length(a)))
  return(abs(zeta))
}

my.t.test.p.value <- function(...) {
  obj<-try(t.test(...), silent=TRUE)
  if (is(obj, "try-error")) return(1) else return(obj$p.value)
}

ihs <- function(x) { transformed <- log(x + sqrt((x^2)+1)); return(transformed) }

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
  
  boxcoxTrans <- function(x, lam1, lam2 = NULL) {
    
    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)
    
    if (lam1 == 0L) {
      log(y + lam2)
    } else {
      (((y + lam2)^lam1) - 1) / lam1
    }
  }
  
  switch(method
         , boxcox = boxcoxTrans(y, lambda1, lambda2)
         , tukey = y^lambda1
  )
}


# Construct results table
random_sampling <- array(NA,dim = c(length(sims),4*20))
row.names(random_sampling) <- names(sims)
colnames(random_sampling) <- sprintf(c('%d.all','%d.main','%d.in','%d.inbet'), rep(1:20,each=4))
random_sampling <- as.data.frame(random_sampling)
# Tests assume normal distribution, so transform data
random_sampling_na <- array(NA,dim = c(length(sims),4*20))
row.names(random_sampling_na) <- names(sims)
colnames(random_sampling_na) <- sprintf(c('%d.all','%d.main','%d.in','%d.inbet'), rep(1:20,each=4))
random_sampling_na <- as.data.frame(random_sampling_na)

# test_na <- sim_emission_c_2msX_s
# is.na(test_na) <- !test_na
# hist(test_na)
# hist(log(test_na))
# mean(sample(test_na,4))

hist(sims[[1]][,,,1])
boxcox(sims[[15]][,,,1]~1)
hist(log(sims[[1]][main_volume[[1]],main_volume[[2]],main_volume[[3]],1]))
hist(log(sims[[1]][in_rows[[1]],in_rows[[2]],in_rows[[3]],1]))
hist(log(sims[[1]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],1]))

sim_num <- 1

for(s in sims){
  n <- 0
  for(time in 1:dim(s)[4]){
    bc <- boxcox(s[,,,time]~1, plotit=F)
    lambda <- bc$x[which.max(bc$y)]

    #pop <- log(na.omit(as.vector(s[,,,time])))
    #pop_main <- log(na.omit(as.vector(s[main_volume[[1]],main_volume[[2]],main_volume[[3]],time])))
    #pop_in <- log(na.omit(as.vector(s[in_rows[[1]],in_rows[[2]],in_rows[[3]],time])))
    #pop_inbet <- log(na.omit(as.vector(s[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],time])))
    
    pop <- powerTransform(na.omit(as.vector(s[,,,time])), lambda)
    pop_main <- powerTransform(na.omit(as.vector(s[main_volume[[1]],main_volume[[2]],main_volume[[3]],time])), lambda)
    m_pop <- mean(pop_main)
    sd_pop <- sd(pop_main)
    pop_in <- powerTransform(na.omit(as.vector(s[in_rows[[1]],in_rows[[2]],in_rows[[3]],time])),lambda)
    pop_inbet <- powerTransform(na.omit(as.vector(s[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],time])), lambda)
    
    pop_na <- !is.na(s[,,,time])
    pop_main_na <- !is.na(s[main_volume[[1]],main_volume[[2]],main_volume[[3]],time])
    pop_in_na <- !is.na(s[in_rows[[1]],in_rows[[2]],in_rows[[3]],time])
    pop_inbet_na <- !is.na(s[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],time])
    #Select 4 random samples, 1000 times, check how many times mean samples is sign diff than population.
    #z <- c()
    #z_main <- c()
    t <- c()
    t_main <- c()
    t_in <- c()
    t_inbet <- c()
    
    t_na <- c()
    t_main_na <- c()
    t_in_na <- c()
    t_inbet_na <- c()
    for(i in 1:100){
      s_all <- sample(pop,4)
      s_main <- sample(pop_main,4)
      s_in <- sample(pop_in,4)
      s_inbet <- sample(pop_inbet,4)
      
      s_all_na <- sample(pop_na,4)
      s_main_na <- sample(pop_main_na,4)
      s_in_na <- sample(pop_in_na,4)
      s_inbet_na <- sample(pop_inbet_na,4)
      # To find a critical value for a two-tailed 95% confidence interval:
      #z <- c(z, z.test(s,mu=log10(sim_avg$sim_emission_c_2msX_s[1]),var=log10(sim_var$sim_emission_c_2msX_s[1])) > qnorm(1-.05/2))
      # If TRUE z.score > 1.959964 then the mean of the sample is signif. diff from the population.
      #z_main <- c(z_main, z.test(s,mu=log10(sim_avg_main$sim_emission_c_2msX_s[1]),var=log10(sim_var_main$sim_emission_c_2msX_s[1])) > qnorm(1-.05/2))
      
      # With a significance level of 95%, remember this rule: If p-value is greater than 0.05 then we accept the null hypothesis H0; if p-value is less than 0.05 then we reject the null hypothesis H0, in favor of the alternative hypothesis H1.
      # If p < 0.05 then means are not equal (significantly different)
      # Is it equal? TRUE, FALSE
      
      # Using t.tes
      #t <- c(t,!(my.t.test.p.value(s_all,mu=log(sim_avg_main[[sim_num]][time])) < 0.05))
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=log(sim_avg_main[[sim_num]][time])) < 0.05))
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=log(sim_avg_main[[sim_num]][time])) < 0.05))
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=log(sim_avg_main[[sim_num]][time])) < 0.05))
      
      # Effect size
      #print(paste('All:',mean(s_all)-powerTransform(sim_avg_main[[sim_num]][time],lambda)/powerTransform(sim_std_main[[sim_num]][time],lambda)))
      #print(paste('Main:',mean(s_main)-powerTransform(sim_avg_main[[sim_num]][time],lambda)/powerTransform(sim_std_main[[sim_num]][time],lambda)))
      #print(paste('In:',mean(s_in)-powerTransform(sim_avg_main[[sim_num]][time],lambda)/powerTransform(sim_std_main[[sim_num]][time],lambda)))
      #print(paste('Inbet:',mean(s_inbet)-powerTransform(sim_avg_main[[sim_num]][time],lambda)/powerTransform(sim_std_main[[sim_num]][time],lambda)))
      
      #t <- c(t,!(my.t.test.p.value(s_all,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda)) < 0.05))
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda)) < 0.05))
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda)) < 0.05))
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda)) < 0.05))
      
      t_na <- c(t_na,any(s_all_na))
      t_main_na <- c(t_main_na,any(s_main_na))
      t_in_na <- c(t_in_na,any(s_in_na))
      t_inbet_na <- c(t_inbet_na,any(s_inbet_na))
      
      # Using the Z.test
      #t <- c(t,!(z.test(s_all,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda),sd=powerTransform(sim_std_main[[sim_num]][time],lambda)) > qnorm(1-.05/2)))
      #t_main <- c(t_main,!(z.test(s_main,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda),sd=powerTransform(sim_std_main[[sim_num]][time],lambda)) > qnorm(1-.05/2)))
      #t_in <- c(t_in,!(z.test(s_in,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda),sd=powerTransform(sim_std_main[[sim_num]][time],lambda)) > qnorm(1-.05/2)))
      #t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=powerTransform(sim_avg_main[[sim_num]][time],lambda),sd=powerTransform(sim_std_main[[sim_num]][time],lambda)) > qnorm(1-.05/2)))
      
      t <- c(t,!(z.test(s_all,mu=m_pop,sd=sd_pop) > qnorm(1-.05/2)))
      t_main <- c(t_main,!(z.test(s_main,mu=m_pop,sd=sd_pop) > qnorm(1-.05/2)))
      t_in <- c(t_in,!(z.test(s_in,mu=m_pop,sd=sd_pop) > qnorm(1-.05/2)))
      t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=m_pop,sd=sd_pop) > qnorm(1-.05/2)))
    }
    random_sampling[sim_num,(1:4)+n] <- c(sum(t),sum(t_main),sum(t_in),sum(t_inbet))
    random_sampling_na[sim_num,(1:4)+n] <- c(sum(t_na),sum(t_main_na),sum(t_in_na),sum(t_inbet_na))
    n <- n + 4
  }
  
  sim_num <- sim_num + 1
}

rm(s,n,sim_num,t,t_main,t_in,t_inbet,s_all,s_main, s_in, s_inbet,pop,pop_main,pop_in,pop_inbet, time, t,t_in_na,t_inbet_na,t_main_na,t_na, s_all_na,s_in_na,s_inbet_na,s_main_na, pop_in_na, pop_inbet_na,pop_main_na,pop_na, lambda, bc, m_pop,sd_pop)

random_sampling.all <- random_sampling[,grepl( "*.all" , names(random_sampling))]
random_sampling.main <- random_sampling[,grepl( "*.main" , names(random_sampling))]
random_sampling.in <- random_sampling[,grepl( "\\.in$" , names(random_sampling))]
random_sampling.inbet <- random_sampling[,grepl( "*.inbet" , names(random_sampling))]

random_sampling_na.all <- random_sampling_na[,grepl( "*.all" , names(random_sampling_na))]
random_sampling_na.main <- random_sampling_na[,grepl( "*.main" , names(random_sampling_na))]
random_sampling_na.in <- random_sampling_na[,grepl( "\\.in$" , names(random_sampling_na))]
random_sampling_na.inbet <- random_sampling_na[,grepl( "*.inbet" , names(random_sampling_na))]

# Plot results of random sampling
z <- 1
for(s in list(random_sampling.all,random_sampling.main,random_sampling.in,random_sampling.inbet)){
  tikz(file = paste0(path_to_sims,'RandomSampling_',zones_files[z],'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Confidence level of a 4 set random sample', main = zones[z], lwd=3)
  z =z+ 1
  for(i in 2:length(s)){
    if(i %in% 2:5){
      p_t <- emission_col[3]
    }
    if(i %in% 6:10){
      p_t <- emission_col[2]
    }
    if(i %in% 11:15){
      p_t <- emission_col[1]
    }
    
    if(i %in% c(6,11)){
      p_ch <- wind_pch[1]
    }
    if(i %in% c(2,3,7,8,12,13)){
      p_ch <- wind_pch[2]
    }
    if(i %in% c(4,5,9,10,14,15)){
      p_ch <- wind_pch[3]
    }
    points(timesteps,s[i,], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  dev.off()
}

z <- 1
for(s in list(random_sampling_na.all,random_sampling_na.main,random_sampling_na.in,random_sampling_na.inbet)){
  tikz(file = paste0(path_to_sims,'RandomSamplingNA_',zones_files[z],'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Probability of ethylene existing in a 4 set random sample', main = zones[z], lwd=3)
  z =z+ 1
  for(i in 2:length(s)){
    if(i %in% 2:5){
      p_t <- emission_col[3]
    }
    if(i %in% 6:10){
      p_t <- emission_col[2]
    }
    if(i %in% 11:15){
      p_t <- emission_col[1]
    }
    
    if(i %in% c(6,11)){
      p_ch <- wind_pch[1]
    }
    if(i %in% c(2,3,7,8,12,13)){
      p_ch <- wind_pch[2]
    }
    if(i %in% c(4,5,9,10,14,15)){
      p_ch <- wind_pch[3]
    }
    points(timesteps,s[i,], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  dev.off()
}

z <- 1
for(s in list(random_sampling_na.all*random_sampling.all*0.01,random_sampling_na.main*random_sampling.main*0.01,random_sampling_na.in*random_sampling.in*0.01,random_sampling_na.inbet*random_sampling.inbet*0.01)){
  tikz(file = paste0(path_to_sims,'RandomSamplingComp_',zones_files[z],'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Composite confidence level of a 4 set random sample', main = zones[z], lwd=3)
  z =z+ 1
  for(i in 2:length(s)){
    if(i %in% 2:5){
      p_t <- emission_col[3]
    }
    if(i %in% 6:10){
      p_t <- emission_col[2]
    }
    if(i %in% 11:15){
      p_t <- emission_col[1]
    }
    
    if(i %in% c(6,11)){
      p_ch <- wind_pch[1]
    }
    if(i %in% c(2,3,7,8,12,13)){
      p_ch <- wind_pch[2]
    }
    if(i %in% c(4,5,9,10,14,15)){
      p_ch <- wind_pch[3]
    }
    points(timesteps,s[i,], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  dev.off()
}

rm(z, s,i)

# Make table with range
random_sampling.all$min <- apply(random_sampling.all,1,min, na.rm=T)
random_sampling.all$max <- apply(random_sampling.all,1,max, na.rm=T)
random_sampling.main$min <- apply(random_sampling.main,1,min, na.rm=T)
random_sampling.main$max <- apply(random_sampling.main,1,max, na.rm=T)
random_sampling.in$min <- apply(random_sampling.in,1,min, na.rm=T)
random_sampling.in$max <- apply(random_sampling.in,1,max, na.rm=T)
random_sampling.inbet$min <- apply(random_sampling.in,1,min, na.rm=T)
random_sampling.inbet$max <- apply(random_sampling.in,1,max, na.rm=T)

# tikz(file = paste0(path_to_sims,'Legend_sims.tex'),height = 0.7)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("top", c("Pre-climacteric", "Entering climacteric", "Climacteric"), xpd = TRUE, horiz = TRUE, bty = "n", lty = 1, lwd=3, col = emission_col)
# legend('top',inset=c(0,0.6), c("0 $ms^{-1}$", "2 $ms^{-1}$", "5 $ms^{-1}$"), xpd = TRUE, horiz = TRUE, bty = "n", pch=wind_pch)
# dev.off()

# Check if samples are significantly different from each other, if stage is also different

sim_avg_all <- lapply(sim_avg_main,mean, na.rm=T)
sim_std_all <- lapply(sim_std_main,mean, na.rm=T)
# Take out 0 velocity since it varies stupidely over time

# Construct comparison table
avg_sampling <- array(NA,dim = c(16,16))
row.names(avg_sampling) <- c(1:15,'Mean')
colnames(avg_sampling) <- c(1:15,'Mean')
avg_sampling <- as.data.frame(avg_sampling)

avg_sampling.all <- avg_sampling
avg_sampling.main <- avg_sampling
avg_sampling.in <- avg_sampling
avg_sampling.inbet <- avg_sampling
rm(avg_sampling)

sims_avg <- lapply(sims,function(x) apply(x,c(1,2,3),mean, na.rm=T ))

for(master in c(1:15)){
  for(slave in c(1:15)){
    t_all <- c()
    t_main <- c()
    t_in <- c()
    t_inbet <- c()
    
    bc <- boxcox(sims_avg[[slave]]~1, plotit=F)
    lambda <- bc$x[which.max(bc$y)]
    transformed_all <- powerTransform(na.omit(as.vector(sims_avg[[slave]])), lambda)
    transformed_main <- powerTransform(na.omit(as.vector(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]])), lambda)
    transformed_in <- powerTransform(na.omit(as.vector(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]])), lambda)
    transformed_inbet <- powerTransform(na.omit(as.vector(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]])), lambda)
    
    m_master <- mean(powerTransform(sims_avg[[master]],lambda), na.rm = T)
    sd_master <- sd(powerTransform(sims_avg[[master]],lambda), na.rm = T)
    for(i in 1:100){
      
      #s_all <- sample(transformed,4)
      #t_all <- c(t_all,!(my.t.test.p.value(s_all,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_all <- log(sample(sims_avg[[slave]],4))
      #t_all <- c(t_all,!(z.test(s_all,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_all <- sample(transformed_all,4)
      #t_all <- c(t_all,!(my.t.test.p.value(s_all,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_main <- log1p(sample(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]],4))
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_main <- log(sample(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]],4))
      #t_main <- c(t_main,!(z.test(s_main,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_main <- sample(transformed_main,4)
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_in <- log1p(sample(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]],4))
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_in <- log(sample(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]],4))
      #t_in <- c(t_in,!(z.test(s_in,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_in <- sample(transformed_in,4)
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_inbet <- log1p(sample(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]],4))
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=log1p(sim_avg_all [[master]]))$p.value < 0.05))
      #s_inbet <- log(sample(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]],4))
      #t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_inbet <- sample(transformed_inbet,4)
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      t_all <- c(t_all,!(z.test(s_all,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_main <- c(t_main,!(z.test(s_main,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_in <- c(t_in,!(z.test(s_in,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      
    }
    avg_sampling.all[master,slave] <- as.numeric(sum(t_all))
    avg_sampling.main[master,slave] <- as.numeric(sum(t_main))
    avg_sampling.inbet[master,slave] <- as.numeric(sum(t_inbet))
    avg_sampling.in[master,slave] <- as.numeric(sum(t_in))
  }
}
rm(master,slave,t_all,t_main,t_in,t_inbet,s_in,s_inbet,s_main,s_all, transformed, transformed_all,transformed_in, transformed_inbet, m_master, sd_master)

avg_sampling.all[,16]<-round(as.vector(apply(as.matrix(avg_sampling.all),1,mean,na.rm=T)))
avg_sampling.all[16,16] <- NA
avg_sampling.all[16,]<-round(as.vector(apply(as.matrix(avg_sampling.all),2,mean,na.rm=T)))
avg_sampling.all[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling.all))),na.rm = T))
avg_sampling.main[,16]<-round(as.vector(apply(as.matrix(avg_sampling.main),1,mean,na.rm=T)))
avg_sampling.main[16,16] <- NA
avg_sampling.main[16,]<-round(as.vector(apply(as.matrix(avg_sampling.main),2,mean,na.rm=T)))
avg_sampling.main[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling.main))),na.rm = T))
avg_sampling.in[,16]<-round(as.vector(apply(as.matrix(avg_sampling.in),1,mean,na.rm=T)))
avg_sampling.in[16,16] <- NA
avg_sampling.in[16,]<-round(as.vector(apply(as.matrix(avg_sampling.in),2,mean,na.rm=T)))
avg_sampling.in[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling.in))),na.rm = T))
avg_sampling.inbet[,16]<-round(as.vector(apply(as.matrix(avg_sampling.inbet),1,mean,na.rm=T)))
avg_sampling.inbet[16,16] <- NA
avg_sampling.inbet[16,]<-round(as.vector(apply(as.matrix(avg_sampling.inbet),2,mean,na.rm=T)))
avg_sampling.inbet[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling.inbet))),na.rm = T))

# Plot the result
z <- 1
for(s in list(avg_sampling.all,avg_sampling.main,avg_sampling.in,avg_sampling.inbet)){
tikz(file=paste0(path_to_sims,'AvgComp_',zones_files[z],'.tex'))
image(1:16, 1:16, t(s),
      col = brewer.pal(8,'Greens'),
      main=zones[z],
      breaks = seq(0,100,12.5),
      xaxt = 'n', 
      yaxt = 'n', 
      xlab = '', 
      ylab = 'Simulation Number',
      ylim = c(16 + 0.5, 1 - 0.5)
)
centers <- expand.grid(1:16,1:16)
text(centers[,2], centers[,1], c(as.matrix(s)), col= "black")
for(i in 1:16){text(i, i, s[i,i], col= 'black',font=2)}

mtext(names(avg_sampling.all), at=1:ncol(s), padj = -0.2)
mtext(names(avg_sampling.all), at=1:nrow(s), side = 2, las = 1, adj = 1.2)
#add black lines
abline(h=1:15 + 0.5)
abline(v=1:15 + 0.5)
abline(h=c(5,10,15)+0.5,lwd=6)
abline(v=c(5,10,15)+0.5,lwd=6)
dev.off()
z <- z +1
}
rm(centers,s,z,i)

avg_sampling_summary_total <- list('','','','')
n <- 1
for(s in list(avg_sampling.all,avg_sampling.main,avg_sampling.in,avg_sampling.inbet)){
  avg_sampling_summary <- array(NA,dim = c(3,3))
  avg_sampling_summary[1,1] <- round(mean(as.matrix(s[1:5,1:5])))
  avg_sampling_summary[1,2] <- round(mean(as.matrix(s[1:5,5:10])))
  avg_sampling_summary[1,3] <- round(mean(as.matrix(s[1:5,10:15])))
  avg_sampling_summary[2,1] <- round(mean(as.matrix(s[5:10,1:5])))
  avg_sampling_summary[3,1] <- round(mean(as.matrix(s[10:15,1:5])))
  avg_sampling_summary[2,2] <- round(mean(as.matrix(s[5:10,5:10])))
  avg_sampling_summary[3,3] <- round(mean(as.matrix(s[10:15,10:15])))
  avg_sampling_summary[2,3] <- round(mean(as.matrix(s[5:10,10:15])))
  avg_sampling_summary[3,2] <- round(mean(as.matrix(s[10:15,5:10])))
  avg_sampling_summary_total[[n]] <- avg_sampling_summary
  n <- n+1
}

avg_sampling_summary.all <- avg_sampling_summary_total[[1]]
avg_sampling_summary.main <- avg_sampling_summary_total[[2]]
avg_sampling_summary.in <- avg_sampling_summary_total[[3]]
avg_sampling_summary.inbet <- avg_sampling_summary_total[[4]]
rm(s,n,avg_sampling_summary_total,avg_sampling_summary)

# n <- 1
# for(avg_s in list(avg_sampling_summary.all, avg_sampling_summary.main, avg_sampling_summary.in, avg_sampling_summary.inbet)){
# 
# colnames(avg_s) <- c('Climacteric','Entering climacteric', 'Pre-climacteric')
# rownames(avg_s) <- c('Climacteric','Entering climacteric', 'Pre-climacteric')
# 
# print(xtable(avg_s, type = "latex", caption = zones[n], digits = 0), file = paste0(path_to_sims,"AvgCompTable_",zones_files[n],".tex"))
# n <- n + 1
# }

# Make average table ----
table_avg <- data.frame(as.array(lapply(sim_avg,mean)),as.array(sim_avg_all), as.array(lapply(sim_avg_in,mean)), as.array(lapply(sim_avg_inbet,mean)), row.names = 1:15)
colnames(table_avg) <- zones

print(xtable(table_avg, type = "latex", caption = 'Average concentration of ethylene ($ppb$) in the different simulations and zones.', digits = 3, label='tbl:avg_concentration'), file = paste0(path_to_sims,"MeanConcentration.tex"))

# Make histograms
tikz(file = paste0(path_to_sims,'hist_avg.tex'))
hist(sims_avg[[1]]+sims_avg[[2]]+sims_avg[[3]]+sims_avg[[4]]+sims_avg[[5]], col=emission_col[3], xlab="Ethylene concentration ($ppb$)", main='')
hist(sims_avg[[6]]+sims_avg[[7]]+sims_avg[[8]]+sims_avg[[9]]+sims_avg[[10]], col=emission_col[2], add=T)
hist(sims_avg[[11]]+sims_avg[[12]]+sims_avg[[13]]+sims_avg[[14]]+sims_avg[[15]], col=emission_col[1], add=T)
legend('topright', c("Pre-climacteric", "Entering climacteric", "Climacteric"),bty = "n",fill = emission_col)
dev.off()

tikz(file = paste0(path_to_sims,'hist_logavg.tex'))
hist(powerTransform(sims_avg[[1]]+sims_avg[[2]]+sims_avg[[3]]+sims_avg[[4]]+sims_avg[[5]], 0), col=adjustcolor(emission_col[3], alpha.f = 0.7), xlab="Ethylene concentration ($log(ppb)$)", main='')
hist(powerTransform(sims_avg[[6]]+sims_avg[[7]]+sims_avg[[8]]+sims_avg[[9]]+sims_avg[[10]], 0), col=adjustcolor(emission_col[2], alpha.f = 0.7), add=T)
hist(powerTransform(sims_avg[[11]]+sims_avg[[12]]+sims_avg[[13]]+sims_avg[[14]]+sims_avg[[15]], 0), col=adjustcolor(emission_col[1], alpha.f = 0.7), add=T)
dev.off()

# Make regular grid points (1,4,16) ----
s_1p <- c(mean(main_volume[[1]]),mean(main_volume[[2]]),mean(main_volume[[3]]))
s_1p <- round(s_1p)

s_4p <- list(c((s_1p[1]-min(main_volume[[1]]))/2 + s_1p[1],
               (s_1p[2]-min(main_volume[[2]]))/2 + s_1p[2],
               s_1p[3]),
             c((s_1p[1]-min(main_volume[[1]]))/2 + s_1p[1],
               -(s_1p[2]-min(main_volume[[2]]))/2 + s_1p[2],
               s_1p[3]),
             c(-(s_1p[1]-min(main_volume[[1]]))/2 + s_1p[1],
               -(s_1p[2]-min(main_volume[[2]]))/2 + s_1p[2],
               s_1p[3]),
             c(-(s_1p[1]-min(main_volume[[1]]))/2 + s_1p[1],
               (s_1p[2]-min(main_volume[[2]]))/2 + s_1p[2],
               s_1p[3])
)
s_4p <- lapply(s_4p, round)

s_16p <- list(c((s_4p[[1]][1]-s_1p[1])/2 + s_4p[[1]][1],
               (s_4p[[1]][2]-s_1p[2])/2 + s_4p[[1]][2],
               s_1p[3]),
             c((s_4p[[1]][1]-s_1p[1])/2 + s_4p[[1]][1],
               -(s_4p[[1]][2]-s_1p[2])/2 + s_4p[[1]][2],
               s_1p[3]),
             c(-(s_4p[[1]][1]-s_1p[1])/2 + s_4p[[1]][1],
               -(s_4p[[1]][2]-s_1p[2])/2 + s_4p[[1]][2],
               s_1p[3]),
             c(-(s_4p[[1]][1]-s_1p[1])/2 + s_4p[[1]][1],
               (s_4p[[1]][2]-s_1p[2])/2 + s_4p[[1]][2],
               s_1p[3]),
             ##
             c((s_4p[[2]][1]-s_1p[1])/2 + s_4p[[2]][1],
               (s_4p[[2]][2]-s_1p[2])/2 + s_4p[[2]][2],
               s_1p[3]),
             c((s_4p[[2]][1]-s_1p[1])/2 + s_4p[[2]][1],
               -(s_4p[[2]][2]-s_1p[2])/2 + s_4p[[2]][2],
               s_1p[3]),
             c(-(s_4p[[2]][1]-s_1p[1])/2 + s_4p[[2]][1],
               -(s_4p[[2]][2]-s_1p[2])/2 + s_4p[[2]][2],
               s_1p[3]),
             c(-(s_4p[[2]][1]-s_1p[1])/2 + s_4p[[2]][1],
               (s_4p[[2]][2]-s_1p[2])/2 + s_4p[[2]][2],
               s_1p[3]),
             ##
             c((s_4p[[3]][1]-s_1p[1])/2 + s_4p[[3]][1],
               (s_4p[[3]][2]-s_1p[2])/2 + s_4p[[3]][2],
               s_1p[3]),
             c((s_4p[[3]][1]-s_1p[1])/2 + s_4p[[3]][1],
               -(s_4p[[3]][2]-s_1p[2])/2 + s_4p[[3]][2],
               s_1p[3]),
             c(-(s_4p[[3]][1]-s_1p[1])/2 + s_4p[[3]][1],
               -(s_4p[[3]][2]-s_1p[2])/2 + s_4p[[3]][2],
               s_1p[3]),
             c(-(s_4p[[3]][1]-s_1p[1])/2 + s_4p[[3]][1],
               (s_4p[[3]][2]-s_1p[2])/2 + s_4p[[3]][2],
               s_1p[3]),
             ##
             c((s_4p[[4]][1]-s_1p[1])/2 + s_4p[[4]][1],
               (s_4p[[4]][2]-s_1p[2])/2 + s_4p[[4]][2],
               s_1p[3]),
             c((s_4p[[4]][1]-s_1p[1])/2 + s_4p[[4]][1],
               -(s_4p[[4]][2]-s_1p[2])/2 + s_4p[[4]][2],
               s_1p[3]),
             c(-(s_4p[[4]][1]-s_1p[1])/2 + s_4p[[4]][1],
               -(s_4p[[4]][2]-s_1p[2])/2 + s_4p[[4]][2],
               s_1p[3]),
             c(-(s_4p[[4]][1]-s_1p[1])/2 + s_4p[[4]][1],
               (s_4p[[4]][2]-s_1p[2])/2 + s_4p[[4]][2],
               s_1p[3])
)
s_16p <- lapply(s_16p, round)         

# plot the points in the orchard
tikz(file = paste0(path_to_sims,'points_to_sample.tex'))
plot(s_1p[1],s_1p[2], xlim= range(main_volume[[1]]),ylim=range(main_volume[[2]]),pch=1,xlab = '',ylab = '')
points(s_4p[[1]][1],s_4p[[1]][2],pch=2)
points(s_4p[[2]][1],s_4p[[2]][2],pch=2)
points(s_4p[[3]][1],s_4p[[3]][2],pch=2)
points(s_4p[[4]][1],s_4p[[4]][2],pch=2)
points(s_4p[[4]][1],s_4p[[4]][2],pch=2)
for(i in 1:16){
  points(s_16p[[i]][1],s_16p[[i]][2],pch=4)
}
points(coords$x*10,coords$y*10,pch=16, cex=2)
dev.off()

measures_s_1p <- list()
measures_s_4p <- list(list(),list(),list(),list())
measures_s_16p <- list(list(),list(),list(),list(),list(),list(),list(),list(),list(),list(),list(),list(), list(),list(),list(),list())

# Get the measurement values
for(s in sims){
  # 1 point
  measures_s_1p <- c(measures_s_1p,list(s[s_1p[1], s_1p[1], s_1p[3],]))
  
  # 4 points
  for(i in 1:4){
    measures_s_4p[[i]] <- c(measures_s_4p[[i]],list(s[s_4p[[i]][1], s_4p[[i]][1], s_4p[[i]][3],]))
  }
  
  # 16 points
  for(i in 1:16){
    measures_s_16p[[i]] <- c(measures_s_16p[[i]],list(s[s_16p[[i]][1], s_16p[[i]][1], s_16p[[i]][3],]))
  }
}

rm(s,i)
