setwd("~/catkin_ws/src/gaden/orchard_demo/demo")
set.seed(12345678)
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results.RData")
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results_01.RData")
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results_drone.RData")
library(RColorBrewer)
require(tikzDevice)
library(fields)
library(MASS)
library(xtable)
library(randomForest)

path_to_sims <- '/Users/rodrigoalmeida/Dropbox/Rodrigo/Thesis/Simulations/figures/'
# Making zones
main_volume <- list(25:174,25:130,1:32)
# trees + 2.5 m buffer around it
in_rows <- list(c((50-4):(50+4),(100-4):(100+4),(150-4):(150+4)),(50-4):(105+4),1:32)
# trees + 0.4 cm on each side (0.8 m per row)
inbetween_rows <- list(c((50+5):(100-5),(100+5):(150-5)),(50-4):(105+4),1:32)
#w <- 3.7
#h <- 3.7
zones_col <- c(rgb(0, 18, 242, 55, maxColorValue = 255),rgb(0, 231, 100, 65, maxColorValue = 255),rgb(255, 255, 0, 65, maxColorValue = 255))

# FIX LEGENDS MicroLs to PPB s or nL s

# Functions ----
z.test = function(a, mu, sd){
  zeta = (mean(a) - mu) / (sd / sqrt(length(a)))
  return(abs(zeta))
}

z.test_s = function(a, mu, sd){
  zeta = (mean(a,na.rm = T) - mu) / (sd / sqrt(length(a)))
  return(zeta)
}

z.test_n = function(x,n, mu, sd){
  zeta = (x - mu) / (sd / sqrt(length(n)))
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

# Setting up ----

X <- 1:199
Y <- 1:164
Z <- 1:59
ii_prec <- cut(emission_prec*1000, breaks = seq(min(emission_prec*1000), max(emission_prec*1000), len = 5), include.lowest = TRUE)
ii_entc <- cut(emission_entc*1000, breaks = seq(min(emission_entc*1000), max(emission_entc*1000), len = 5), include.lowest = TRUE)
ii_c <- cut(emission_c*1000, breaks = seq(min(emission_c*1000), max(emission_c*1000), len = 5), include.lowest = TRUE)
ii <- list(ii_prec,ii_entc,ii_c)

sims <- list(sim_emission_c_0ms_s, sim_emission_c_2msX_s,sim_emission_c_2msY_s,sim_emission_c_5msX_s,sim_emission_c_5msY_s,sim_emission_entc_0ms_s,sim_emission_entc_2msX_s,sim_emission_entc_2msY_s,sim_emission_entc_5msX_s,sim_emission_entc_5msY_s,sim_emission_prec_0ms_s,sim_emission_prec_2msX_s,sim_emission_prec_2msY_s,sim_emission_prec_5msX_s,sim_emission_prec_5msY_s)

names(sims) <- c('sim_emission_c_0ms_s',"sim_emission_c_2msX_s","sim_emission_c_2msY_s","sim_emission_c_5msX_s","sim_emission_c_5msY_s",'sim_emission_entc_0ms_s',"sim_emission_entc_2msX_s","sim_emission_entc_2msY_s","sim_emission_entc_5msX_s","sim_emission_entc_5msY_s",'sim_emission_prec_0ms_s',"sim_emission_prec_2msX_s","sim_emission_prec_2msY_s","sim_emission_prec_5msX_s","sim_emission_prec_5msY_s")

sims_drone <- list(sim_emission_c_drone1_s,sim_emission_entc_drone1_s,sim_emission_prec_drone1_s,sim_emission_c_drone2_s,sim_emission_entc_drone2_s,sim_emission_prec_drone2_s)
names(sims_drone) <- c('sim_emission_c_drone1_s','sim_emission_entc_drone1_s','sim_emission_prec_drone1_s','sim_emission_c_drone2_s','sim_emission_entc_drone2_s','sim_emission_prec_drone2_s')

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

for(sim in sims){
  is.na(sim) <- !sim
  
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

number_of_samples <- c(1,4,16)

# Plotting 
# Maximum Concentration XY maps ----
breaks_stages <- list(seq(0,12,1.2),seq(0,35,3.5),seq(0,50,5))
# 
for(n in 1:15){
xy_plane <- apply(sims[[n]],c(1,2),max)
xz_plane <- apply(sims[[n]],c(1,3),max)
yz_plane <- apply(sims[[n]],c(2,3),max)

print(paste('range XY',range(xy_plane)[1], '-', range(xy_plane)[2]))
if(n %in% 1:5){ stage <- 3 }
if(n %in% 6:10){ stage <- 2 }
if(n %in% 11:15){ stage <- 1 }

pdf(paste0(path_to_sims,names(sims)[[n]],'_XY.pdf'))
#plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
image(X,Y,xy_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,174),ylim=c(25,141), ann=FALSE)
image(X,Y,trees_xy, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
points(coords$e_x*10,coords$e_y*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
"red"))(4)[ii[[stage]]])
dev.off()

pdf(paste0(path_to_sims,names(sims)[[n]],'_XZ.pdf'))
#plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
image(X,Z,xz_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,174),ylim=c(1,60), ann=FALSE)
image(X,Z,trees_xz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
points(coords$e_x*10,coords$z*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
                                                                                  "red"))(4)[ii[[stage]]])
dev.off()

pdf(paste0(path_to_sims,names(sims)[[n]],'_YZ.pdf'))
#plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
image.plot(Y,Z,yz_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,141),ylim=c(1,60), ann=FALSE, legend.lab = 'Ethylene concentration (ppb)', legend.line = 2.5)
image(Y,Z,trees_yz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
points(coords$e_y*10,coords$z*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
                                                                                  "red"))(4)[ii[[stage]]])
dev.off()
}

rm(s,n) 

# Legend Maximum maps ----
breaks = breaks_stages[[stage]]

tikz(file = paste0(path_to_sims, 'legend_c_XY.tex'),height = 0.7) 
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) 
plot(0,0,type = "n", bty = "n", xaxt = "n", yaxt = "n") 
legend('top',levels(ii_c),pch = 23,pt.bg = colorRampPalette(c("white", "red"))(4),xpd = TRUE,horiz =TRUE,bty = "n",title = '$E_3$ ($nLs^{-1}$)') 
dev.off()

tikz(file = paste0(path_to_sims, 'legend_prec_XY.tex'),
     height = 0.7) 
par(
       fig =
         c(0, 1, 0, 1),
       oma = c(0, 0, 0, 0),
       mar = c(0, 0, 0, 0)
     ) 
plot(
       0,
       0,
       type =
         "n",
       bty = "n",
       xaxt = "n",
       yaxt = "n"
     ) 
legend(
       'top',
       levels(ii_prec),
       pch = 23,
       pt.bg = colorRampPalette(c("white", "red"))(4),
       xpd = TRUE,
       horiz =
         TRUE,
       bty = "n",
       title = '$E_1$ ($nLs^{-1}$)'
     )
dev.off()

tikz(file = paste0(path_to_sims, 'legend_entc_XY.tex'),
     height = 0.7) 
par(
       fig =
         c(0, 1, 0, 1),
       oma = c(0, 0, 0, 0),
       mar = c(0, 0, 0, 0)
     ) 
plot(
       0,
       0,
       type =
         "n",
       bty = "n",
       xaxt = "n",
       yaxt = "n"
     ) 
legend(
       'top',
       levels(ii_entc),
       pch = 23,
       pt.bg = colorRampPalette(c("white", "red"))(4),
       xpd = TRUE,
       horiz =
         TRUE,
       bty = "n",
       title = '$E_2$ ($nLs^{-1}$)'
     ) 
dev.off()


# Makes 0 <- NA ----
for(i in 1:length(sims)){
  is.na(sims[[i]]) <- !sims[[i]]
}
rm(i)
sims_avg <- lapply(sims,function(x) apply(x,c(1,2,3),mean, na.rm=T ))
sim_avg_all <- lapply(sim_avg,mean, na.rm=T)
sim_std_all <- lapply(sim_std,mean, na.rm=T)

# Average Ethylene Concentration ----
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
for(number in number_of_samples){
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

#hist(sims[[1]][,,,1])
#boxcox(sims[[15]][,,,1]~1)
#hist(log(sims[[1]][main_volume[[1]],main_volume[[2]],main_volume[[3]],1]))
#hist(log(sims[[1]][in_rows[[1]],in_rows[[2]],in_rows[[3]],1]))
#hist(log(sims[[1]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],1]))

sim_num <- 1
lambda_values <- list()
lambda_v <- c()
for(s in sims){
  n <- 0
  for(time in 1:dim(s)[4]){
    bc <- boxcox(s[,,,time]~1, plotit=F)
    lambda <- bc$x[which.max(bc$y)]
    print(lambda)
    lambda_v <- c(lambda_v, lambda) 

    #pop <- log(na.omit(as.vector(s[,,,time])))
    #pop_main <- log(na.omit(as.vector(s[main_volume[[1]],main_volume[[2]],main_volume[[3]],time])))
    #pop_in <- log(na.omit(as.vector(s[in_rows[[1]],in_rows[[2]],in_rows[[3]],time])))
    #pop_inbet <- log(na.omit(as.vector(s[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]],time])))
    
    pop <- powerTransform(na.omit(as.vector(s[,,,time])), lambda)
    pop_main <- powerTransform(na.omit(as.vector(s[main_volume[[1]],main_volume[[2]],main_volume[[3]],time])), lambda)
    #m_pop <- mean(pop_main)
    #sd_pop <- sd(pop_main)
    m_pop <- mean(pop)
    sd_pop <- sd(pop)
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
      s_all <- sample(pop,number)
      s_main <- sample(pop_main,number)
      s_in <- sample(pop_in,number)
      s_inbet <- sample(pop_inbet,number)
      
      s_all_na <- sample(pop_na,number)
      s_main_na <- sample(pop_main_na,number)
      s_in_na <- sample(pop_in_na,number)
      s_inbet_na <- sample(pop_inbet_na,number)
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
  lambda_values <- c(lambda_values,list(lambda_v))
  lambda_v <- c()
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
  tikz(file = paste0(path_to_sims,'RandomSampling_',zones_files[z],'_',number,'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Confidence level of a random sample', main = zones[z], lwd=3)
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
  tikz(file = paste0(path_to_sims,'RandomSamplingNA_',zones_files[z],'_',number,'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Probability of ethylene existing in a random sample', main = zones[z], lwd=3)
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
  tikz(file = paste0(path_to_sims,'RandomSamplingComp_',zones_files[z],'_',number,'.tex'))
  plot(timesteps,s[1,], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], type = 'b', xlab = 'Time ($s$)',ylab = 'Composite confidence level of a random sample', main = zones[z], lwd=3)
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

avg_sampling_na.all <- avg_sampling
avg_sampling_na.main <- avg_sampling
avg_sampling_na.in <- avg_sampling
avg_sampling_na.inbet <- avg_sampling
rm(avg_sampling)

for(master in c(1:15)){
  for(slave in c(1:15)){
    t_all <- c()
    t_main <- c()
    t_in <- c()
    t_inbet <- c()
    
    t_na <- c()
    t_main_na <- c()
    t_in_na <- c()
    t_inbet_na <- c()
    
    bc <- boxcox(sims_avg[[slave]]~1, plotit=F)
    lambda <- bc$x[which.max(bc$y)]
    transformed_all <- powerTransform(na.omit(as.vector(sims_avg[[slave]])), lambda)
    transformed_main <- powerTransform(na.omit(as.vector(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]])), lambda)
    transformed_in <- powerTransform(na.omit(as.vector(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]])), lambda)
    transformed_inbet <- powerTransform(na.omit(as.vector(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]])), lambda)
    
    pop_na <- !is.na(sims_avg[[slave]])
    pop_main_na <- !is.na(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]])
    pop_in_na <- !is.na(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]])
    pop_inbet_na <- !is.na(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]])
    
    m_master <- mean(powerTransform(sims_avg[[master]],lambda), na.rm = T)
    sd_master <- sd(powerTransform(sims_avg[[master]],lambda), na.rm = T)
    for(i in 1:100){
      
      #s_all <- sample(transformed,4)
      #t_all <- c(t_all,!(my.t.test.p.value(s_all,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_all <- log(sample(sims_avg[[slave]],4))
      #t_all <- c(t_all,!(z.test(s_all,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_all <- sample(transformed_all,number)
      #t_all <- c(t_all,!(my.t.test.p.value(s_all,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_main <- log1p(sample(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]],number))
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_main <- log(sample(sims_avg[[slave]][main_volume[[1]],main_volume[[2]],main_volume[[3]]],number))
      #t_main <- c(t_main,!(z.test(s_main,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_main <- sample(transformed_main,number)
      #t_main <- c(t_main,!(my.t.test.p.value(s_main,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_in <- log1p(sample(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]],number))
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=log1p(sim_avg_all[[master]])) < 0.05))
      #s_in <- log(sample(sims_avg[[slave]][in_rows[[1]],in_rows[[2]],in_rows[[3]]],number))
      #t_in <- c(t_in,!(z.test(s_in,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_in <- sample(transformed_in,number)
      #t_in <- c(t_in,!(my.t.test.p.value(s_in,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      #s_inbet <- log1p(sample(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]],number))
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=log1p(sim_avg_all [[master]]))$p.value < 0.05))
      #s_inbet <- log(sample(sims_avg[[slave]][inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]],number))
      #t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=log(sim_avg_all[[master]]),var=log(sim_std_all[[master]]) > qnorm(1-.05/2))))
      s_inbet <- sample(transformed_inbet,number)
      #t_inbet <- c(t_inbet,!(my.t.test.p.value(s_inbet,mu=powerTransform(sim_avg_all[[master]],lambda)) < 0.05))
      
      s_all_na <- sample(pop_na,number)
      s_main_na <- sample(pop_main_na,number)
      s_in_na <- sample(pop_in_na,number)
      s_inbet_na <- sample(pop_inbet_na,number)
      
      t_all <- c(t_all,!(z.test(s_all,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_main <- c(t_main,!(z.test(s_main,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_in <- c(t_in,!(z.test(s_in,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      t_inbet <- c(t_inbet,!(z.test(s_inbet,mu=m_master,sd=sd_master) > qnorm(1-.05/2)))
      
      t_na <- c(t_na,any(s_all_na))
      t_main_na <- c(t_main_na,any(s_main_na))
      t_in_na <- c(t_in_na,any(s_in_na))
      t_inbet_na <- c(t_inbet_na,any(s_inbet_na))
      
    }
    avg_sampling.all[master,slave] <- as.numeric(sum(t_all))
    avg_sampling.main[master,slave] <- as.numeric(sum(t_main))
    avg_sampling.inbet[master,slave] <- as.numeric(sum(t_inbet))
    avg_sampling.in[master,slave] <- as.numeric(sum(t_in))
    
    avg_sampling_na.all[master,slave] <- as.numeric(sum(t_na))
    avg_sampling_na.main[master,slave] <- as.numeric(sum(t_main_na))
    avg_sampling_na.inbet[master,slave] <- as.numeric(sum(t_inbet_na))
    avg_sampling_na.in[master,slave] <- as.numeric(sum(t_in_na))
  }
}
rm(master,slave,t_all,t_main,t_in,t_inbet,s_in,s_inbet,s_main,s_all, transformed_all,transformed_in, transformed_inbet, m_master, sd_master)

#random_sampling_na.all_m <- c(as.vector(apply(as.matrix(random_sampling_na.all),1, mean, na.rm=T)),0)
#random_sampling_na.main_m <- c(as.vector(apply(as.matrix(random_sampling_na.main),1, mean, na.rm=T)),0)
#random_sampling_na.in_m <- c(as.vector(apply(as.matrix(random_sampling_na.in),1, mean, na.rm=T)),0)
#random_sampling_na.inbet_m <- c(as.vector(apply(as.matrix(random_sampling_na.inbet),1, mean, na.rm=T)),0)

avg_sampling_comp.all <- round(avg_sampling.all*avg_sampling_na.all*0.01)
avg_sampling_comp.main <- round(avg_sampling.main*avg_sampling_na.main*0.01)
avg_sampling_comp.in <- round(avg_sampling.in*avg_sampling_na.in*0.01)
avg_sampling_comp.inbet <- round(avg_sampling.inbet*avg_sampling_na.inbet*0.01)

avg_sampling_comp.all[,16]<-round(as.vector(apply(as.matrix(avg_sampling_comp.all),1,mean,na.rm=T)))
avg_sampling_comp.all[16,16] <- NA
avg_sampling_comp.all[16,]<-round(as.vector(apply(as.matrix(avg_sampling_comp.all),2,mean,na.rm=T)))
avg_sampling_comp.all[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling_comp.all))),na.rm = T))
avg_sampling_comp.main[,16]<-round(as.vector(apply(as.matrix(avg_sampling_comp.main),1,mean,na.rm=T)))
avg_sampling_comp.main[16,16] <- NA
avg_sampling_comp.main[16,]<-round(as.vector(apply(as.matrix(avg_sampling_comp.main),2,mean,na.rm=T)))
avg_sampling_comp.main[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling_comp.main))),na.rm = T))
avg_sampling_comp.in[,16]<-round(as.vector(apply(as.matrix(avg_sampling_comp.in),1,mean,na.rm=T)))
avg_sampling_comp.in[16,16] <- NA
avg_sampling_comp.in[16,]<-round(as.vector(apply(as.matrix(avg_sampling_comp.in),2,mean,na.rm=T)))
avg_sampling_comp.in[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling_comp.in))),na.rm = T))
avg_sampling_comp.inbet[,16]<-round(as.vector(apply(as.matrix(avg_sampling_comp.inbet),1,mean,na.rm=T)))
avg_sampling_comp.inbet[16,16] <- NA
avg_sampling_comp.inbet[16,]<-round(as.vector(apply(as.matrix(avg_sampling_comp.inbet),2,mean,na.rm=T)))
avg_sampling_comp.inbet[16,16] <- round(mean(as.vector(diag(as.matrix(avg_sampling_comp.inbet))),na.rm = T))

# Plot the result
z <- 1
for(s in list(avg_sampling_comp.all,avg_sampling_comp.main,avg_sampling_comp.in,avg_sampling_comp.inbet)){
tikz(file=paste0(path_to_sims,'AvgComp_',zones_files[z],'_',number,'.tex'))
image(1:16, 1:16, t(s),
      col = brewer.pal(10,'RdYlGn'),
      main=zones[z],
      breaks = seq(0,100,10),
      xaxt = 'n', 
      yaxt = 'n', 
      xlab = 'Population (Simulation number)', 
      ylab = 'Sample (Simulation number)',
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

}


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
table_avg <- data.frame(as.array(lapply(sim_avg,mean)),as.array(lapply(sim_std,mean)), c(1:15),as.array(sim_avg_all),as.array(sim_std_all), c(1:15), as.array(lapply(sim_avg_in,mean)), as.array(lapply(sim_std_in,mean)), c(1:15),as.array(lapply(sim_avg_inbet,mean)), as.array(lapply(sim_std_inbet,mean)),c(1:15), row.names = 1:15)
table_avg[1,3] <- mean(unlist(table_avg[1:5,1]))
table_avg[2,3] <- mean(unlist(table_avg[1:5,2]))^2
table_avg[6,3] <- mean(unlist(table_avg[6:10,1]))
table_avg[7,3] <- mean(unlist(table_avg[6:10,2]))^2
table_avg[11,3] <- mean(unlist(table_avg[11:15,1]))
table_avg[12,3] <- mean(unlist(table_avg[11:15,2]))^2

table_avg[1,6] <- mean(unlist(table_avg[1:5,1+3]))
table_avg[2,6] <- mean(unlist(table_avg[1:5,2+3]))^2
table_avg[6,6] <- mean(unlist(table_avg[6:10,1+3]))
table_avg[7,6] <- mean(unlist(table_avg[6:10,2+3]))^2
table_avg[11,6] <- mean(unlist(table_avg[11:15,1+3]))
table_avg[12,6] <- mean(unlist(table_avg[11:15,2+3]))^2

table_avg[1,9] <- mean(unlist(table_avg[1:5,1+6]))
table_avg[2,9] <- mean(unlist(table_avg[1:5,2+6]))^2
table_avg[6,9] <- mean(unlist(table_avg[6:10,1+6]))
table_avg[7,9] <- mean(unlist(table_avg[6:10,2+6]))^2
table_avg[11,9] <- mean(unlist(table_avg[11:15,1+6]))
table_avg[12,9] <- mean(unlist(table_avg[11:15,2+6]))^2

table_avg[1,12] <- mean(unlist(table_avg[1:5,1+9]))
table_avg[2,12] <- mean(unlist(table_avg[1:5,2+9]))^2
table_avg[6,12] <- mean(unlist(table_avg[6:10,1+9]))
table_avg[7,12] <- mean(unlist(table_avg[6:10,2+9]))^2
table_avg[11,12] <- mean(unlist(table_avg[11:15,1+9]))
table_avg[12,12] <- mean(unlist(table_avg[11:15,2+9]))^2

colnames(table_avg) <- c(zones[1],'','',zones[2],'','',zones[3],'','',zones[4],'','')

table_avg[c(3:5,8:10,13:15),c(3,6,9,12)] <- ''

print(xtable(table_avg, type = "latex", caption = 'Average concentration of ethylene ($ppb$) in the different simulations and zones.', digits = 2, label='tbl:avg_concentration'), file = paste0(path_to_sims,"MeanConcentration.tex"))

# Make average plot ----

plot(c(1,2,3),table_avg[c(11,6,1),3],xlab='$E_i$',ylab = 'Ethylene concentration ($ppb$)', ylim=c(0,10), type = 'b', pch=1)

polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),3])-sqrt(as.numeric(table_avg[c(12,7,2),3])), rev(as.numeric(table_avg[c(11,6,1),3])+sqrt(as.numeric(table_avg[c(12,7,2),3])))))
polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),6])-sqrt(as.numeric(table_avg[c(12,7,2),6])), rev(as.numeric(table_avg[c(11,6,1),6])+sqrt(as.numeric(table_avg[c(12,7,2),6])))), col=zones_col[1], border = NA)
polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),9])-sqrt(as.numeric(table_avg[c(12,7,2),9])), rev(as.numeric(table_avg[c(11,6,1),9])+sqrt(as.numeric(table_avg[c(12,7,2),9])))), col=zones_col[2], border = NA)
polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),12])-sqrt(as.numeric(table_avg[c(12,7,2),12])), rev(as.numeric(table_avg[c(11,6,1),12])+sqrt(as.numeric(table_avg[c(12,7,2),12])))), col=zones_col[3], border = NA)

points(c(1,2,3),table_avg[c(11,6,1),6], type='b',pch=2, col = zones_col[1])
points(c(1,2,3),table_avg[c(11,6,1),9], type='b',pch=3, col = zones_col[2])
points(c(1,2,3),table_avg[c(11,6,1),12], type='b', pch=4, col = zones_col[3])


# Make histograms ----
tikz(file = paste0(path_to_sims,'hist_avg.tex'))
hist(sims_avg[[1]]+sims_avg[[2]]+sims_avg[[3]]+sims_avg[[4]]+sims_avg[[5]], col=adjustcolor(emission_col[3], alpha.f = 0.7), xlab="Ethylene concentration ($ppb$)", main='')
hist(sims_avg[[6]]+sims_avg[[7]]+sims_avg[[8]]+sims_avg[[9]]+sims_avg[[10]], col=adjustcolor(emission_col[2], alpha.f = 0.7), add=T)
hist(sims_avg[[11]]+sims_avg[[12]]+sims_avg[[13]]+sims_avg[[14]]+sims_avg[[15]], col=adjustcolor(emission_col[1], alpha.f = 0.7), add=T)
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
plot(s_1p[1],s_1p[2], xlim= range(main_volume[[1]]),ylim=range(main_volume[[2]]),pch=1,xlab = '$x$ ($cm$)',ylab = '$y$ ($cm$)')
points(s_4p[[1]][1],s_4p[[1]][2],pch=2)
points(s_4p[[2]][1],s_4p[[2]][2],pch=2)
points(s_4p[[3]][1],s_4p[[3]][2],pch=2)
points(s_4p[[4]][1],s_4p[[4]][2],pch=2)
points(s_4p[[4]][1],s_4p[[4]][2],pch=2)
for(i in 1:16){
  points(s_16p[[i]][1],s_16p[[i]][2],pch=4)
}
points(coords$x*10,coords$y*10,pch=16, cex=2)
# Main volume
polygon(c(25,174,174,25),c(130,130,25,25), border = NA, col = rgb(0, 18, 242, 55, maxColorValue = 255))
# In-rows
polygon(c((50-4),(50+4),(50+4),(50-4)),c((50-4),(50-4),(105+4),(105+4)), border = NA, col = rgb(0, 231, 100, 65, maxColorValue = 255))
polygon(c((100-4),(100+4),(100+4),(100-4)),c((50-4),(50-4),(105+4),(105+4)), border = NA, col = rgb(0, 231, 100, 65, maxColorValue = 255))
polygon(c((150-4),(150+4),(150+4),(150-4)),c((50-4),(50-4),(105+4),(105+4)), border = NA, col = rgb(0, 231, 100, 65, maxColorValue = 255))

# In between rows
polygon(c((50+5),(100-5),(100-5),(50+5)),c((50-4),(50-4),(105+4),(105+4)), border = NA, col = rgb(255, 255, 0, 65, maxColorValue = 255))
polygon(c((100+5),(150-5),(150-5),(100+5)),c((50-4),(50-4),(105+4),(105+4)), border = NA, col = rgb(255, 255, 0, 65, maxColorValue = 255))
legend('topleft',zones[2:4],fill=c(rgb(0, 18, 242, 55, maxColorValue = 255),rgb(0, 231, 100, 65, maxColorValue = 255),rgb(255, 255, 0, 65, maxColorValue = 255)), bty='n')
dev.off()

# Get the measurement values
measures_s_1p <- list()
measures_s_4p <- list(list(),list(),list(),list())
measures_s_16p <- list(list(),list(),list(),list(),list(),list(),list(),list(),list(),list(),list(),list(), list(),list(),list(),list())

for(s in sims){
  # 1 point
  measures_s_1p <- c(measures_s_1p,list(s[s_1p[1], s_1p[2], s_1p[3],]))
  
  # 4 points
  for(i in 1:4){
    measures_s_4p[[i]] <- c(measures_s_4p[[i]],list(s[s_4p[[i]][1], s_4p[[i]][2], s_4p[[i]][3],]))
  }
  
  # 16 points
  for(i in 1:16){
    measures_s_16p[[i]] <- c(measures_s_16p[[i]],list(s[s_16p[[i]][1], s_16p[[i]][2], s_16p[[i]][3],]))
  }
}

rm(s,i)

measures_s_4p_na <- measures_s_1p
measures_s_4p_na <- rapply(measures_s_4p_na,function(x) ifelse(x!=0,0,x), how = "replace")
measures_s_4p_na <- rapply(measures_s_4p_na,function(x) ifelse(is.na(x),0,x), how = "replace")

measures_s_1p_na <- measures_s_4p_na

measures_s_16p_na <- measures_s_4p_na

measures_s_4p_z <- measures_s_1p
measures_s_4p_z <- rapply(measures_s_4p_z,function(x) ifelse(x!=0,NA,x), how = "replace")

measures_s_1p_z <- measures_s_4p_z

measures_s_16p_z <- measures_s_4p_z

# Calculate NA
for (s in 1:15){
  for(t in 1:length(measures_s_1p[[s]])){
      measures_s_1p_na[[s]][[t]] <- measures_s_1p_na[[s]][[t]] + as.numeric(!is.na(measures_s_1p[[s]][[t]]))
    }
  }
measures_s_1p_na <- lapply(measures_s_1p_na, function(x) (x)*100)

for(p in 1:4){
  for (s in 1:15){
    for(t in 1:length(measures_s_1p[[s]])){
      measures_s_4p_na[[s]][[t]] <- measures_s_4p_na[[s]][[t]] + as.numeric(!is.na(measures_s_4p[[p]][[s]][[t]]))
    }
  }
}
measures_s_4p_na <- lapply(measures_s_4p_na, function(x) (x/4)*100)

for(p in 1:16){
  for (s in 1:15){
    for(t in 1:length(measures_s_1p[[s]])){
      measures_s_16p_na[[s]][[t]] <- measures_s_16p_na[[s]][[t]] + as.numeric(!is.na(measures_s_16p[[p]][[s]][[t]]))
    }
  }
}
measures_s_16p_na <- lapply(measures_s_16p_na, function(x) (x/16)*100)

# Calculate Z-score 

for (s in 1:15){
  for(t in 1:length(measures_s_1p[[s]])){
    measures_s_1p_z[[s]][[t]] <- z.test_s(powerTransform(measures_s_1p[[s]][[t]],lambda_values[[s]][[t]]),mu=powerTransform(sim_avg_main[[s]][[t]],lambda_values[[s]][[t]]),sd=powerTransform(sim_std_main[[s]][[t]],lambda_values[[s]][[t]]))
  }
}

for (s in 1:15){
  for(t in 1:length(measures_s_1p[[s]])){
    measures_s_4p_z[[s]][[t]] <- z.test_s(powerTransform(c(measures_s_4p[[1]][[s]][[t]],measures_s_4p[[2]][[s]][[t]],measures_s_4p[[3]][[s]][[t]],measures_s_4p[[4]][[s]][[t]]),lambda_values[[s]][[t]]),mu=powerTransform(sim_avg_main[[s]][[t]],lambda_values[[s]][[t]]),sd=powerTransform(sim_std_main[[s]][[t]],lambda_values[[s]][[t]]))
  }
}

for (s in 1:15){
  for(t in 1:length(measures_s_1p[[s]])){
    measures_s_16p_z[[s]][[t]] <- z.test_s(powerTransform(c(
      measures_s_16p[[1]][[s]][[t]],measures_s_16p[[2]][[s]][[t]],measures_s_16p[[3]][[s]][[t]],measures_s_16p[[4]][[s]][[t]],
      measures_s_16p[[5]][[s]][[t]],measures_s_16p[[6]][[s]][[t]],measures_s_16p[[7]][[s]][[t]],measures_s_16p[[8]][[s]][[t]],
      measures_s_16p[[9]][[s]][[t]],measures_s_16p[[10]][[s]][[t]],measures_s_16p[[11]][[s]][[t]],measures_s_16p[[12]][[s]][[t]],
      measures_s_16p[[13]][[s]][[t]],measures_s_16p[[14]][[s]][[t]],measures_s_16p[[15]][[s]][[t]],measures_s_16p[[16]][[s]][[t]]),lambda_values[[s]][[t]]),mu=powerTransform(sim_avg_main[[s]][[t]],lambda_values[[s]][[t]]),sd=powerTransform(sim_std_main[[s]][[t]],lambda_values[[s]][[t]]))
  }
}

# NA 1 p ----
plot(timesteps[1:14],measures_s_1p_na[[1]], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = 'Probability of ethylene existing in the sample', main = '$n=1$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_1p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_1p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}
# NA 4 p ----
plot(timesteps[1:14],measures_s_4p_na[[1]], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = 'Probability of ethylene existing in the sample', main = '$n=4$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_4p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_4p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}

# NA 16 p ----
plot(timesteps[1:14],measures_s_16p_na[[1]], ylim = c(0,100), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = 'Probability of ethylene existing in the sample', main = '$n=16$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_16p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_16p_na[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}



# Z 1 p ----
tikz(file=paste0(path_to_sims,'RegularGrid1p_Z.tex'))
plot(timesteps[1:14],measures_s_1p_z[[1]], ylim = range(measures_s_1p_z,na.rm = T), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = '$z$-score', main = '$n=1$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_1p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_1p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}
abline(h=1.96,lty=3)
abline(h=-1.96, lty=3)
dev.off()

# Z 4 p ----
tikz(file=paste0(path_to_sims,'RegularGrid4p_Z.tex'))
plot(timesteps[1:14],measures_s_4p_z[[1]], ylim = range(measures_s_4p_z,na.rm = T), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = '$z$-score', main = '$n=4$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_4p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_4p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}
abline(h=1.96,lty=3)
abline(h=-1.96, lty=3)
dev.off()

# Z 16 p ----
tikz(file=paste0(path_to_sims,'RegularGrid16p_Z.tex'))
plot(timesteps[1:14],measures_s_16p_z[[1]], ylim = range(measures_s_16p_z,na.rm = T), xlim=range(timesteps), col=emission_col[3],pch=wind_pch[1], lty=3, type = 'b', xlab = 'Time ($s$)',ylab = '$z$-score', main = '$n=16$', lwd=3)
for(i in 2:15){
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
  if(i %in% c(11,6)){
    points(timesteps[1:14],measures_s_16p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
  else{
    points(timesteps,measures_s_16p_z[[i]], col=p_t,pch=p_ch, type = 'b', lwd=3)
  }
}
abline(h=1.96,lty=3)
abline(h=-1.96, lty=3)
dev.off()


# Comparision between samples in different sims ----
reg_sampling <- array(NA,dim = c(16,16))
row.names(reg_sampling) <- c(1:15,'Mean')
colnames(reg_sampling) <- c(1:15,'Mean')
reg_sampling <- as.data.frame(reg_sampling)

reg_sampling.1 <- reg_sampling
reg_sampling.4 <- reg_sampling
reg_sampling.16 <- reg_sampling
rm(reg_sampling)

measures_s_1p.mean <- lapply(measures_s_1p, mean,na.rm=T)

measures_s_4p.mean <- lapply(measures_s_4p, function(x) unlist(lapply(x,mean,na.rm=T)))
measures_s_4p.mean <- apply(as.data.frame(measures_s_4p.mean),1, mean,na.rm=T)

measures_s_16p.mean <- lapply(measures_s_16p, function(x) unlist(lapply(x,mean,na.rm=T)))
measures_s_16p.mean <- apply(as.data.frame(measures_s_16p.mean),1, mean,na.rm=T)

lambda_values.mean <- lapply(lambda_values, mean,na.rm=T)

for(master in c(1:15)){
  for(slave in c(1:15)){
    mu_t <- powerTransform(sim_avg_all[[master]], lambda_values.mean[[slave]])
    sd_t <- powerTransform(sim_std_all[[master]], lambda_values.mean[[slave]])
    
    #reg_sampling.1[master,slave] <- as.numeric(z.test_n(powerTransform(measures_s_1p.mean[[slave]],lambda_values.mean[[slave]]) ,1,mu_t,sd_t) <= 1.96)
    #reg_sampling.4[master,slave] <- as.numeric(z.test_n(powerTransform(measures_s_4p.mean[[slave]],lambda_values.mean[[slave]]),4,mu_t,sd_t) <= 1.96)
    #reg_sampling.16[master,slave] <- as.numeric(z.test_n(powerTransform(measures_s_16p.mean[[slave]],lambda_values.mean[[slave]]),16,mu_t,sd_t) <= 1.96)
    
    reg_sampling.1[master,slave] <- as.integer(!(z.test_n(powerTransform(measures_s_1p.mean[[slave]],lambda_values.mean[[slave]]) ,1,mu_t,sd_t) > qnorm(1-.05/2)))
    reg_sampling.4[master,slave] <- as.integer(!(z.test_n(powerTransform(measures_s_4p.mean[[slave]],lambda_values.mean[[slave]]) ,4,mu_t,sd_t) > qnorm(1-.05/2)))
    reg_sampling.16[master,slave] <- as.integer(!(z.test_n(powerTransform(measures_s_16p.mean[[slave]],lambda_values.mean[[slave]]) ,16,mu_t,sd_t) > qnorm(1-.05/2)))
  }
}

# Plotting

reg_sampling.1[,16] <- as.vector(apply(as.matrix(reg_sampling.1),1,mean,na.rm=T))
reg_sampling.1[16,16] <- NA
reg_sampling.1[16,]<-as.vector(apply(as.matrix(reg_sampling.1),2,mean,na.rm=T))
reg_sampling.1[16,16] <-mean(as.vector(diag(as.matrix(reg_sampling.1))),na.rm = T)

reg_sampling.4[,16]<-as.vector(apply(as.matrix(reg_sampling.4),1,mean,na.rm=T))
reg_sampling.4[16,16] <- NA
reg_sampling.4[16,]<- as.vector(apply(as.matrix(reg_sampling.4),2,mean,na.rm=T))
reg_sampling.4[16,16] <- mean(as.vector(diag(as.matrix(reg_sampling.4))),na.rm = T)

reg_sampling.16[,16]<-as.vector(apply(as.matrix(reg_sampling.16),1,mean,na.rm=T))
reg_sampling.16[16,16] <- NA
reg_sampling.16[16,]<-as.vector(apply(as.matrix(reg_sampling.16),2,mean,na.rm=T))
reg_sampling.16[16,16] <- mean(as.vector(diag(as.matrix(reg_sampling.16))),na.rm = T)

# Plot the result
number_samples <- c('1','4','16')
z <- 1
for(s in list(round(reg_sampling.1,1),round(reg_sampling.4,1),round(reg_sampling.16,1))){
  tikz(file=paste0(path_to_sims,'RegularGridComp',number_samples[z],'p.tex'))
  image(1:16, 1:16, t(s),
        col = brewer.pal(10,'RdYlGn'),
        main=paste0('$n=',number_samples[z],'$'),
        breaks = seq(0,1,0.1),
        xaxt = 'n', 
        yaxt = 'n', 
        xlab = 'Population (Simulation number)', 
        ylab = 'Sample (Simulation number)',
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


# Scatter plot of average emission/average concentration ----
tikz(file=paste0(path_to_sims,'Model',zones_files[1],'.tex'), height = 3.5)

y <- unlist(table_avg$Environment)
x <- c(rep(mean(emission_c*1000),5),rep(mean(emission_entc*1000),5),rep(mean(emission_prec*1000),5))
exp_model <- lm(log(y)~x)
pred_exp <- exp(predict(exp_model,list(x=10:140)))
lin_model <- lm(y~x)

plot(x,y, pch=rep(c(1,2,2,3,3),3), col=c(rep(emission_col[3],5),rep(emission_col[2],5),rep(emission_col[1],5)), ylab = 'Average ethylene concentration ($ppb$)', xlab= 'Average emission rate ($nLs^{-1}$)', main = zones[1])
abline(lin_model, lty=2)
lines(10:140, pred_exp, lty=3)
legend("topleft", bty="n", lty = c(2,NA,3,NA), legend=c('Linear model',paste("$R^2=$", format(summary(lin_model)$adj.r.squared, digits=4)), 'Exponential model', paste("$R^2=$", format(summary(exp_model)$adj.r.squared, digits=4))))
dev.off()

tikz(file=paste0(path_to_sims,'Model',zones_files[2],'.tex'), height = 3.5)

y <- unlist(table_avg$`Main volume`)
x <- c(rep(mean(emission_c*1000),5),rep(mean(emission_entc*1000),5),rep(mean(emission_prec*1000),5))
exp_model <- lm(log(y)~x)
pred_exp <- exp(predict(exp_model,list(x=10:140)))
lin_model <- lm(y~x)

plot(x,y, pch=rep(c(1,2,2,3,3),3), col=c(rep(emission_col[3],5),rep(emission_col[2],5),rep(emission_col[1],5)), ylab = 'Average ethylene concentration ($ppb$)', xlab= 'Average emission rate ($nLs^{-1}$)', main = zones[2])
abline(lin_model, lty=2)
lines(10:140, pred_exp, lty=3)
legend("topleft", bty="n", lty = c(2,NA,3,NA), legend=c('Linear model',paste("$R^2=$", format(summary(lin_model)$adj.r.squared, digits=4)), 'Exponential model', paste("$R^2=$", format(summary(exp_model)$adj.r.squared, digits=4))))
dev.off()

tikz(file=paste0(path_to_sims,'Model',zones_files[3],'.tex'), height = 3.5)

y <- unlist(table_avg$`In rows`)
x <- c(rep(mean(emission_c*1000),5),rep(mean(emission_entc*1000),5),rep(mean(emission_prec*1000),5))
exp_model <- lm(log(y)~x)
pred_exp <- exp(predict(exp_model,list(x=10:140)))
lin_model <- lm(y~x)

plot(x,y, pch=rep(c(1,2,2,3,3),3), col=c(rep(emission_col[3],5),rep(emission_col[2],5),rep(emission_col[1],5)), ylab = 'Average ethylene concentration ($ppb$)', xlab= 'Average emission rate ($nLs^{-1}$)', main = zones[3])
abline(lin_model, lty=2)
lines(10:140, pred_exp, lty=3)
legend("topleft", bty="n", lty = c(2,NA,3,NA), legend=c('Linear model',paste("$R^2=$", format(summary(lin_model)$adj.r.squared, digits=4)), 'Exponential model', paste("$R^2=$", format(summary(exp_model)$adj.r.squared, digits=4))))
dev.off()

tikz(file=paste0(path_to_sims,'Model',zones_files[4],'.tex'), height = 3.5)

y <- unlist(table_avg$`In-between rows`)
x <- c(rep(mean(emission_c*1000),5),rep(mean(emission_entc*1000),5),rep(mean(emission_prec*1000),5))
exp_model <- lm(log(y)~x)
pred_exp <- exp(predict(exp_model,list(x=10:140)))
lin_model <- lm(y~x)

plot(x,y, pch=rep(c(1,2,2,3,3),3), col=c(rep(emission_col[3],5),rep(emission_col[2],5),rep(emission_col[1],5)), ylab = 'Average ethylene concentration ($ppb$)', xlab= 'Average emission rate ($nLs^{-1}$)', main = zones[4])
abline(lin_model, lty=2)
lines(10:140, pred_exp, lty=3)
legend("topleft", bty="n", lty = c(2,NA,3,NA), legend=c('Linear model',paste("$R^2=$", format(summary(lin_model)$adj.r.squared, digits=4)), 'Exponential model', paste("$R^2=$", format(summary(exp_model)$adj.r.squared, digits=4))))
dev.off()

rm(x,y,exp_model,pred_exp,lin_model)

# Plot evolution of emission across stages ----

emission_all <- data.frame(emission_prec,emission_entc,emission_c)*1000
tikz(file=paste0(path_to_sims,'EmissionEvolution.tex'), height = 4)
plot(c(1,2,3),emission_all[1,], xlab='$E_i$', ylab = 'Emission rate ($nLs^{-1}$)', xaxt="n",ylim = range(emission_all), type='b', pch=3, lty=2)
axis(1, at = c(1,2,3))
for(i in 2:18){points(c(1,2,3),emission_all[i,],type = 'b', pch=3, lty=2)}

x <- c(rep(1,18),rep(2,18),rep(3,18))
y <- log(c(emission_prec,emission_entc,emission_c)*1000)
exp_model <- lm(y~x)
pred_exp <- exp(predict(exp_model,list(x=seq(0,4,0.1))))
lines(seq(0,4,0.1), pred_exp, col='orange',lwd=3)
legend("topleft", bty="n", lty = c(1,NA), legend='Exponential trend line',col='orange',lwd=3)
dev.off()
rm(x,y,exp_model,pred_exp)

# Plot drone sims maps ----

for(n in 1:6){
  xy_plane <- apply(sims_drone[[n]],c(1,2),max)
  xz_plane <- apply(sims_drone[[n]],c(1,3),max)
  yz_plane <- apply(sims_drone[[n]],c(2,3),max)
  
  print(paste('range XY',range(xy_plane)[1], '-', range(xy_plane)[2]))
  if(n %in% c(1,4)){ stage <- 3 }
  if(n %in% c(2,5)){ stage <- 2 }
  if(n %in% c(3,6)){ stage <- 1 }
  
  if(n %in% c(1:3)){point <- c(s_1p[1:2],40)}
  if(n %in% c(4:6)){point <- c(125,s_1p[2],20)}
  
  pdf(paste0(path_to_sims,names(sims_drone)[[n]],'_XY.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image(X,Y,xy_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image(X,Y,trees_xy, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
  points(coords$e_x*10,coords$e_y*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
                                                                                    "red"))(4)[ii[[stage]]])
  points(point[1],point[2], pch=17, col='magenta',cex=2)
  dev.off()
  
  pdf(paste0(path_to_sims,names(sims_drone)[[n]],'_XZ.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image(X,Z,xz_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,174),ylim=c(1,60), ann=FALSE)
  image(X,Z,trees_xz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
  points(coords$e_x*10,coords$z*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
                                                                                  "red"))(4)[ii[[stage]]])
  points(point[1],point[3], pch=17, col='magenta',cex=2)
  dev.off()
  
  pdf(paste0(path_to_sims,names(sims_drone)[[n]],'_YZ.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image.plot(Y,Z,yz_plane, col = brewer.pal(10,'BrBG'), xlim=c(25,141),ylim=c(1,60), ann=FALSE, legend.lab = 'Ethylene concentration (ppb)', legend.line = 2.5)
  image(Y,Z,trees_yz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.7)), add = T)
  points(coords$e_y*10,coords$z*10,cex=2,pch=23,col='black',bg=colorRampPalette(c("white",
                                                                                  "red"))(4)[ii[[stage]]])
  points(point[2],point[3], pch=17, col='magenta',cex=2)
  dev.off()
}

rm(s,n, point) 

tikz(file = paste0(path_to_sims, 'legend_drone_c_XY.tex'),height = 0.7) 
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) 
plot(0,0,type = "n", bty = "n", xaxt = "n", yaxt = "n") 
legend('top',c(levels(ii_c), 'Drone'),pch = c(rep(23,4),17),pt.bg = c(colorRampPalette(c("white", "red"))(4),'magenta'),xpd = TRUE,horiz =TRUE,bty = "n",title = '$E_3$ ($nLs^{-1}$)', col=c(rep('black',4),'magenta'))
dev.off()

tikz(file = paste0(path_to_sims, 'legend_drone_entc_XY.tex'),height = 0.7) 
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) 
plot(0,0,type = "n", bty = "n", xaxt = "n", yaxt = "n") 
legend('top',c(levels(ii_entc), 'Drone'),pch = c(rep(23,4),17),pt.bg = c(colorRampPalette(c("white", "red"))(4),'magenta'),xpd = TRUE,horiz =TRUE,bty = "n",title = '$E_2$ ($nLs^{-1}$)', col=c(rep('black',4),'magenta'))
dev.off()

tikz(file = paste0(path_to_sims, 'legend_drone_prec_XY.tex'),height = 0.7) 
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) 
plot(0,0,type = "n", bty = "n", xaxt = "n", yaxt = "n") 
legend('top',c(levels(ii_prec), 'Drone'),pch = c(rep(23,4),17),pt.bg = c(colorRampPalette(c("white", "red"))(4),'magenta'),xpd = TRUE,horiz =TRUE,bty = "n",title = '$E_1$ ($nLs^{-1}$)', col=c(rep('black',4),'magenta'))
dev.off()

# Plot basic stats drone ----
# For position around the drone, average concentration

for(i in 1:length(sims_drone)){
  is.na(sims_drone[[i]]) <- !sims_drone[[i]]
}
rm(i)

point1 <- c(s_1p[1:2],40)
point1_zone <- list(((point1[1]-2):(point1[1]+2)),((point1[2]-2):(point1[2]+2)),((point1[3]-2):(point1[3]+2)))
point2 <- c(125,s_1p[2],20)
point2_zone <- list(((point2[1]-2):(point2[1]+2)),((point2[2]-2):(point2[2]+2)),((point2[3]-2):(point2[3]+2)))

sims_drone_avg_pos1 <- lapply(sims_drone[1:3], function(x) apply(x[point1_zone[[1]],point1_zone[[2]],point1_zone[[3]],], MARGIN = c(4),mean, na.rm=T))

sims_drone_avg_pos2 <- lapply(sims_drone[4:6], function(x) apply(x[point2_zone[[1]],point2_zone[[2]],point2_zone[[3]],], MARGIN = c(4),mean, na.rm=T))

z <- 1
for(s in list(sims_drone_avg_pos1,sims_drone_avg_pos2)){
  tikz(file = paste0(path_to_sims,'DroneAvg_Pos',z,'.tex'),height = 3.5)
  plot(timesteps[1:length(s[[1]])],s[[1]], ylim = range(s,na.rm=T), xlim=range(timesteps), col=emission_col[3], type = 'b',pch=2, xlab = 'Time ($s$)',ylab = 'Average ethylene concentration ($ppb$)', main = paste('Position',z), lwd=3)
  z =z+ 1
  for(i in 2:length(s)){
    if(i %in% 1){
      p_t <- emission_col[3]
    }
    if(i %in% 2){
      p_t <- emission_col[2]
    }
    if(i %in% 3){
      p_t <- emission_col[1]
    }
    points(timesteps[1:length(s[[i]])],s[[i]], col=p_t,pch=2, type = 'b',lwd=3)
  }
  dev.off()
}
rm(z)

# Adaptive sampling ----
translation_all <- list()
point_all <- list()
for(i in 1:15){
test <- sims_avg[[i]]

test_z <- aperm(test)
test_y <- aperm(test, perm=c(2,1,3))

id <- array(1:length(test),dim = dim(test))
id_z <- aperm(id)
id_y <- aperm(id, perm=c(2,1,3))

#test_bin <- diff(sign(diff(test)))==-2
#test_z_bin <- diff(sign(diff(test_z)))==-2
#test_y_bin <- diff(sign(diff(test_y)))==-2

diff_test <- which(diff(sign(diff(test)))==-2)
diff_test_z <- which(diff(sign(diff(test_z)))==-2)
diff_test_y <- which(diff(sign(diff(test_y)))==-2)

inbetween_rows_id <- id[inbetween_rows[[1]],inbetween_rows[[2]],inbetween_rows[[3]]]
inbetween_rows_id_z <- id_z[inbetween_rows[[3]],inbetween_rows[[2]],inbetween_rows[[1]]]
inbetween_rows_id_y <- id_y[inbetween_rows[[2]],inbetween_rows[[1]],inbetween_rows[[3]]]

diff_test_inbetween_rows<- diff_test[diff_test %in% inbetween_rows_id]
diff_test_z_inbetween_rows<- diff_test_z[diff_test_z %in% inbetween_rows_id_z]
diff_test_y_inbetween_rows<- diff_test_y[diff_test_y %in% inbetween_rows_id_y]

test_coords <- as.data.frame(t(sapply(diff_test_inbetween_rows, function(x) which(id==x,arr.ind = T))))
names(test_coords) <- c('x','y','z')
test_coords$combine <- as.character(interaction(test_coords$x,test_coords$y,test_coords$z))
test_coords_z <- as.data.frame(t(sapply(diff_test_z_inbetween_rows, function(x) which(id_z==x,arr.ind = T))))
names(test_coords_z) <- c('z','y','x')
test_coords_z$combine <- as.character(interaction(test_coords_z$x,test_coords_z$y,test_coords_z$z))
test_coords_y <- as.data.frame(t(sapply(diff_test_y_inbetween_rows, function(x) which(id_y==x,arr.ind = T))))
names(test_coords_y) <- c('y','x','z')
test_coords_y$combine <- as.character(interaction(test_coords_y$x,test_coords_y$y,test_coords_y$z))

test_coords_match <- test_coords[complete.cases(match(test_coords$combine, test_coords_z$combine)),] 

#test_coords_match <- test_coords_match[complete.cases(match(test_coords_match$combine, test_coords_y$combine)),] 

test_coords <- test_coords_match

trans_init <- array(NA,dim=c(100,3))
point_init<- array(NA,dim=c(100,3))
for(z in 1:100){
init <- which(id==sample(inbetween_rows_id,1), arr.ind = T)
test_coords$dist_init <- ((test_coords$x-init[1])^2)+((test_coords$y-init[2])^2)+((test_coords$z-init[3])^2)
translation <- test_coords[which(test_coords$dist==min(test_coords$dist)),1:3] - init
trans_init[z,1] <- translation[1,]$x
trans_init[z,2] <- translation[1,]$y
trans_init[z,3] <- translation[1,]$z
point_init[z,1] <- init[1]
point_init[z,2] <- init[2]
point_init[z,3] <- init[3]
}
translation_all[[i]] <- trans_init
point_all[[i]] <- point_init
}
rm(i,z)

plot(density(translation_all[[1]][,1]), main = 'X', ylim=c(0,0.3), col=emission_col[3], lwd=2, lty=1)
lines(density(translation_all[[2]][,1]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[3]][,1]), col=emission_col[3], lwd=2, lty=3)
lines(density(translation_all[[4]][,1]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[5]][,1]), col=emission_col[3],lwd=2, lty=3)

lines(density(translation_all[[6]][,1]), col=emission_col[2], lwd=2, lty=1)
lines(density(translation_all[[7]][,1]), col=emission_col[2], lwd=2, lty=2)
lines(density(translation_all[[8]][,1]), col=emission_col[2], lwd=2, lty=3)
lines(density(translation_all[[9]][,1]), col=emission_col[2],lwd=2, lty=2)
lines(density(translation_all[[10]][,1]), col=emission_col[2],lwd=2, lty=3)

lines(density(translation_all[[11]][,1]), col=emission_col[1], lwd=2, lty=1)
lines(density(translation_all[[12]][,1]), col=emission_col[1], lwd=2, lty=2)
lines(density(translation_all[[13]][,1]), col=emission_col[1], lwd=2, lty=3)
lines(density(translation_all[[14]][,1]), col=emission_col[1],lwd=2, lty=2)
lines(density(translation_all[[15]][,1]), col=emission_col[1],lwd=2, lty=3)
abline(v=mean(sapply(translation_all[c(1,6,11)], function(x) mean(x[,1]))), lty=1)
abline(v=mean(sapply(translation_all[c(2,7,12,4,9,14)], function(x) mean(x[,1]))), lty=2)
abline(v=mean(sapply(translation_all[c(3,8,13,5,10,15)], function(x) mean(x[,1]))), lty=3)

plot(density(translation_all[[1]][,2]), main = 'Y', ylim=c(0,0.6), col=emission_col[3], lwd=2, lty=1)
lines(density(translation_all[[2]][,2]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[3]][,2]), col=emission_col[3], lwd=2, lty=3)
lines(density(translation_all[[4]][,2]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[5]][,2]), col=emission_col[3],lwd=2, lty=3)

lines(density(translation_all[[6]][,2]), col=emission_col[2], lwd=2, lty=1)
lines(density(translation_all[[7]][,2]), col=emission_col[2], lwd=2, lty=2)
lines(density(translation_all[[8]][,2]), col=emission_col[2], lwd=2, lty=3)
lines(density(translation_all[[9]][,2]), col=emission_col[2],lwd=2, lty=2)
lines(density(translation_all[[10]][,2]), col=emission_col[2],lwd=2, lty=3)

lines(density(translation_all[[11]][,2]), col=emission_col[1], lwd=2, lty=1)
lines(density(translation_all[[12]][,2]), col=emission_col[1], lwd=2, lty=2)
lines(density(translation_all[[13]][,2]), col=emission_col[1], lwd=2, lty=3)
lines(density(translation_all[[14]][,2]), col=emission_col[1],lwd=2, lty=2)
lines(density(translation_all[[15]][,2]), col=emission_col[1],lwd=2, lty=3)
abline(v=mean(sapply(translation_all[c(1,6,11)], function(x) mean(x[,2]))), lty=1)
abline(v=mean(sapply(translation_all[c(2,7,12,4,9,14)], function(x) mean(x[,2]))), lty=2)
abline(v=mean(sapply(translation_all[c(3,8,13,5,10,15)], function(x) mean(x[,2]))), lty=3)

plot(density(translation_all[[1]][,3]), main = 'Z', ylim=c(0,0.7), col=emission_col[3], lwd=2, lty=1)
lines(density(translation_all[[2]][,3]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[3]][,3]), col=emission_col[3], lwd=2, lty=3)
lines(density(translation_all[[4]][,3]), col=emission_col[3], lwd=2, lty=2)
lines(density(translation_all[[5]][,3]), col=emission_col[3],lwd=2, lty=3)

lines(density(translation_all[[6]][,3]), col=emission_col[2], lwd=2, lty=1)
lines(density(translation_all[[7]][,3]), col=emission_col[2], lwd=2, lty=2)
lines(density(translation_all[[8]][,3]), col=emission_col[2], lwd=2, lty=3)
lines(density(translation_all[[9]][,3]), col=emission_col[2],lwd=2, lty=2)
lines(density(translation_all[[10]][,3]), col=emission_col[2],lwd=2, lty=3)

lines(density(translation_all[[11]][,3]), col=emission_col[1], lwd=2, lty=1)
lines(density(translation_all[[12]][,3]), col=emission_col[1], lwd=2, lty=2)
lines(density(translation_all[[13]][,3]), col=emission_col[1], lwd=2, lty=3)
lines(density(translation_all[[14]][,3]), col=emission_col[1],lwd=2, lty=2)
lines(density(translation_all[[15]][,3]), col=emission_col[1],lwd=2, lty=3)
abline(v=mean(sapply(translation_all[c(1,6,11)], function(x) mean(x[,3]))), lty=1)
abline(v=mean(sapply(translation_all[c(2,7,12,4,9,14)], function(x) mean(x[,3]))), lty=2)
abline(v=mean(sapply(translation_all[c(3,8,13,5,10,15)], function(x) mean(x[,3]))), lty=3)

# x,y,z,trans_x,trans_y,trans_z,dir,vel,stage
wind0ms <- cbind(rbind(point_all[[1]],point_all[[6]],point_all[[11]]),rbind(translation_all[[1]],translation_all[[6]],translation_all[[11]]))
wind0ms <- cbind(wind0ms,rep(360,300),rep(0,300), c(rep(1,100),rep(2,100),rep(3,100)))

wind2msX <- cbind(rbind(point_all[[2]],point_all[[7]],point_all[[12]]),rbind(translation_all[[2]],translation_all[[7]],translation_all[[12]]))
wind2msX <- cbind(wind2msX,rep(0,300),rep(2,300), c(rep(1,100),rep(2,100),rep(3,100)))

wind2msY <- cbind(rbind(point_all[[3]],point_all[[8]],point_all[[13]]),rbind(translation_all[[3]],translation_all[[8]],translation_all[[13]]))
wind2msY <- cbind(wind2msY,rep(90,300),rep(2,300), c(rep(1,100),rep(2,100),rep(3,100)))

wind5msX <- cbind(rbind(point_all[[4]],point_all[[9]],point_all[[14]]),rbind(translation_all[[4]],translation_all[[9]],translation_all[[14]]))
wind5msX <- cbind(wind5msX,rep(0,300),rep(5,300), c(rep(1,100),rep(2,100),rep(3,100)))

wind5msY <- cbind(rbind(point_all[[5]],point_all[[10]],point_all[[15]]),rbind(translation_all[[5]],translation_all[[10]],translation_all[[15]]))
wind5msY <- cbind(wind5msY,rep(90,300),rep(5,300), c(rep(1,100),rep(2,100),rep(3,100)))

wind_all <- rbind(wind0ms,wind2msX,wind2msY,wind5msX,wind5msY)
wind_all <- cbind(wind_all,rep(1:15,1,each=100))
wind_all <- as.data.frame(wind_all)
names(wind_all) <- c('x','y','z','trans_x','trans_y','trans_z','dir','vel', 'stage','sim_num')
wind_all$e <- apply(wind_all, 1, function(x) sims_avg[[x[10]]][x[1],x[2],x[3]])
wind_all$e[is.nan(wind_all$e)] <- 0

model_x <- randomForest(trans_x~x+y+z+dir+vel+e, data = wind_all, importance=T)
model_y <- randomForest(trans_y~x+y+z+dir+vel+e+trans_x+trans_z, data = wind_all,importance=T)
model_z <- randomForest(trans_z~x+y+z+dir+vel+e+trans_x, data = wind_all, importance=T)

# Testing adaptive sampling
sim_sample_of_4 <- list()
for(f in 1:15){
one_sim <- sims_avg[[f]]
sample_of_4 <- list()
if(f %in% c(1,6,11)){dir<-360}
if(f %in% c(2,4,7,9,12,14)){dir<-0}
if(f %in% c(3,5,8,10,13,15)){dir<-90}

if(f %in% c(1,6,11)){vel<-0}
if(f %in% c(2,3,7,8,12,13)){dir<-0}
if(f %in% c(4,5,9,10,14,15)){dir<-90}

for(n in 1:100){
# first get X, then Z, then Y
x <- sample(inbetween_rows[[1]],1)
y <- sample(inbetween_rows[[2]],1)
z <- sample(inbetween_rows[[3]],1)
e <- one_sim[x,y,z]

if(is.nan(e)){e<-0}

sample_adp <- c(e)

for(i in 2:4){
# Get X
input <- as.data.frame(t(c(x,y,z,dir,vel,e)))
names(input) <- c('x','y','z','dir','vel','e')
test_trans_x <- round(predict(model_x,input))

# Get Z
input <- as.data.frame(t(c(x,y,z,dir,vel,e,test_trans_x)))
names(input) <- c('x','y','z','dir','vel','e','trans_x')
test_trans_z <- round(predict(model_z,input))

# Get Y
input <- as.data.frame(t(c(x,y,z,dir,vel,e,test_trans_x, test_trans_z)))
names(input) <- c('x','y','z','dir','vel','e','trans_x','trans_z')
test_trans_y <- round(predict(model_y,input))

x <- x+test_trans_x
y <- y+test_trans_y
z <- z+test_trans_z
sample_adp <- c(sample_adp,e)
}
sample_of_4[[n]] <- sample_adp
}
sim_sample_of_4[[f]] <- sample_of_4
}