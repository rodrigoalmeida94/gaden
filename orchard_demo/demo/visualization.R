library(readr)
#library(xml2)
library(methods)
library(plot3D)
require(tikzDevice)
library(fields)
library(abind)
library(RColorBrewer)
setwd("~/catkin_ws/src/gaden/orchard_demo/demo")
load("~/catkin_ws/src/gaden/orchard_demo/demo/parameters.RData")

# tikz('e_boxplot.tex')
# boxplot(e_prec, e_entc, e_c, names = c('$e_1$','$e_2$','$e_3$'), ylab = 'Ethylene emission ($\\mu Lh^{-1}kg^{-1}$)')
# dev.off()
# 
# tikz('l_boxplot.tex')
# boxplot(fruit_load, names= '$l$', ylab= "Fruit load (kg per tree)")
# dev.off()
# 
# tikz('ee_boxplot.tex')
# boxplot(emission_prec,emission_entc,emission_c, names = c('$E_1$','$E_2$','$E_3$'), ylab = 'Emission per tree ($\\mu Ls^{-1}\ per\ tree$)')
# dev.off()
# 
# tikz('h_boxplot.tex')
#boxplot(fruit_position, names = '$h$', ylab='Height ($m$)')
# dev.off()

X <- 1:199
Y <- 1:164
Z <- 1:59

# Tree ocupation in several planes
trees_xy <- read.table('OccupancyGrid3D.csv',header=F,skip=4, sep=' ',nrows = 200)
trees_xy <- as.matrix(trees_xy)
trees_xy <- trees_xy[2:199,2:164]

trees_xz <- array(0,dim=c(max(X),max(Z)))
trees_yz <- array(0,dim=c(max(Y),max(Z)))

for(i in 1:nrow(trees_xy)){
  # x dimension
  if(1 %in% trees_xy[i,]){
    trees_xz[i,1:30] <- 1
  }
}

for(i in 1:ncol(trees_xy)){
  # y dimension
  if(1 %in% trees_xy[,i]){
    trees_yz[i,1:30] <- 1
  }
}

# Make colors for emission points
ii_prec <- cut(emission_prec*1000, breaks = seq(min(emission_prec*1000), max(emission_prec*1000), len = 5), include.lowest = TRUE)
ii_entc <- cut(emission_entc*1000, breaks = seq(min(emission_entc*1000), max(emission_entc*1000), len = 5), include.lowest = TRUE)
ii_c <- cut(emission_c*1000, breaks = seq(min(emission_c*1000), max(emission_c*1000), len = 5), include.lowest = TRUE)
ii <- list(ii_prec,ii_entc,ii_c)

timesteps <- seq(15,300,15)
#timesteps <- seq(15,300,15)x
#steps <- 0:19
#steps <- 0:19
#steps <- 0:8
# Remove zero sims if needed
#simulation_runs <- simulation_runs[-c(1,6,11),]
save_files <- F
#c(6,7,13,14,20,21)
for(index in 1:nrow(simulation_runs)){
sim_name <- simulation_runs$name[index]
print(sim_name)
if(simulation_runs$Var2[index] == 'emission_prec'){
  stage <- 1
} else if(simulation_runs$Var2[index] == 'emission_entc'){
  stage <- 2
} else if(simulation_runs$Var2[index] == 'emission_c'){
  stage <- 3
}
print(paste('ethylene emission',stage))
steps <- 0:length(list.files(path=simulation_runs$name[index],pattern = 'sum_[0-9]{1,2}$'))
print(paste('max timesteps',max(steps)))
for(e in steps){
  # Read result file
  print(paste('timestep',e))
  f <- read.table(paste0(sim_name,'/sum_',e), header= T, sep = '', col.names=c("Cell_x","Cell_y","Cell_z","Gas_conc","Wind_u","Wind_v","Wind_w"), skip = 6)
  
  f <- f[f$Gas_conc>0,]
  
  m <- array(0,dim = c(max(X),max(Y),max(Z)))
  
  for(i in 1:nrow(f)){
    m[f$Cell_x[i],f$Cell_y[i],f$Cell_z[i]] <- f$Gas_conc[i]
  }
  
  m <- m*1000 # In ppb
  if(e==0){
    assign(sim_name, m)
  }
  else{
    assign(sim_name, abind(m,get(sim_name),along = 4))
    #test <- abind(m,test, along = 4)
  }
  
  # TODO for every file, change scale to fit all.
  # XY plane, 380x380
  if(save_files){
    # Make means
    xy_plane <- apply(m,c(1,2),mean)
    print(paste('range XY',range(xy_plane)[1], '-', range(xy_plane)[2]))
    xz_plane <- apply(m,c(1,3),mean)
    print(paste('range XZ',range(xz_plane)[1], '-', range(xz_plane)[2]))
    yz_plane <- apply(m,c(2,3),mean)
    print(paste('range YZ',range(yz_plane)[1], '-', range(yz_plane)[2]))
    
    #Plots
  png(filename=paste0(sim_name,'/sum_',e,'_XY.png'),width = 380,height = 380)
  plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), xaxt='n', yaxt='n', ann=FALSE)
  title(main=paste0('XY, ',timesteps[e+1],' s'))
  image.plot(X,Y,xy_plane,col=colorRampPalette(c(adjustcolor( "white", alpha.f = 0), "green"))(100), add=T, horizontal = T)
  image(X,Y,trees_xy, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.5)), add = T)
  points(coords$e_x*10,coords$e_y*10,cex=0.5,pch=23,col='black',bg=colorRampPalette(c("white", "red"))(5)[ii[[stage]]])
  dev.off()
  
  # XZ plane
  png(filename=paste0(sim_name,'/sum_',e,'_XZ.png'),width = 380,height = 380)
  plot(coords$x*10,coords$z*10,xlim=c(25,174),ylim=range(Z), xaxt='n', yaxt='n', ann=FALSE)
  title(main=paste0('XZ, ',timesteps[e+1],' s'))
  image.plot(X,Z,xz_plane,col=colorRampPalette(c(adjustcolor( "white", alpha.f = 0), "green"))(100), add=T, horizontal = T)
  image(X,Z,trees_xz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.5)), add = T)
  points(coords$e_x*10,coords$z*10,cex=0.5,pch=23,col='black',bg=colorRampPalette(c("white", "red"))(5)[ii[[stage]]])
  dev.off()
  
  # YZ plane
  png(filename=paste0(sim_name,'/sum_',e,'_YZ.png'),width = 380,height = 380)
  plot(coords$y*10,coords$z*10,xlim=c(25,141),ylim=range(Z), xaxt='n', yaxt='n', ann=FALSE)
  title(main=paste0('YZ, ',timesteps[e+1],' s'))
  image.plot(Y,Z,yz_plane,col=colorRampPalette(c(adjustcolor( "white", alpha.f = 0), "green"))(100), add=T, horizontal = T)
  image(Y,Z,trees_yz, col=c(adjustcolor( "white", alpha.f = 0),adjustcolor( "black", alpha.f = 0.5)), add = T)
  points(coords$e_y*10,coords$z*10,cex=0.5,pch=23,col='black',bg=colorRampPalette(c("white", "red"))(5)[ii[[stage]]])
  dev.off()
  }
  
}
if(save_files){system(paste0('source gif_maker.sh ',sim_name))}

graphics.off() 
}

rm(list=setdiff(ls(), c('sim_emission_c_0ms_s','sim_emission_prec_0ms_s', 'sim_emission_entc_0ms_s','sim_emission_c_01ms_s','sim_emission_prec_01ms_s', 'sim_emission_entc_01ms_s', 'sim_emission_c_2msX_drone','sim_emission_prec_2ms_drone', 'sim_emission_entc_2msX_drone', simulation_runs$name,'coords','emission_data','timesteps','steps','simulation_runs', 'trees_xy', 'trees_xz', 'trees_yz', 'emission_c', 'emission_entc', 'emission_prec')))

#save.image(file='sim_results_01.RData')
#save.image(file='sim_results_drone.RData')
save.image(file='sim_results_all_201.RData')


