library(XML)
library(readr)
#library(xml2)
library(methods)
setwd("~/catkin_ws/src/gaden/orchard_demo/demo")

# For Fruit load
figs <- FALSE
# number of trees
n <- 18

# Yield in ton in a 2000 tree per ha field, over several years
yield <- c(5,16,29,42,53,63,65)
yield <- (yield*1000)/(10000/5.5)
mean_yield <- mean(yield)
sd_yield <- sd(yield)

lower <- 0
upper <- max(yield)

set.seed(12345678)
fruit_load <- qnorm(runif(n, pnorm(lower, mean=mean_yield, sd=sd_yield), pnorm(upper, mean=mean_yield, sd=sd_yield)), mean=mean_yield, sd=sd_yield)

# fruit_load in kg/tree
if(figs){boxplot(fruit_load)}

# Fruit "center of gravity position" in the tree
lower <- 0.4
upper <- 2.5
mean_position <- 1.4
sd_position <- 0.5

fruit_position <- qnorm(runif(n, pnorm(lower, mean=mean_position, sd=sd_position), pnorm(upper, mean=mean_position, sd=sd_position)), mean=mean_position, sd=sd_position)

# Fruit central position in m
if(figs){boxplot(fruit_position)}

lower <- 0
upper <- 55
# Ethylene emission
# Pre-climacteric
mean_e_prec <- 2.7
sd_e_prec <- 1.6

e_prec <- qnorm(runif(n, pnorm(lower, mean=mean_e_prec, sd=sd_e_prec), pnorm(upper, mean=mean_e_prec, sd=sd_e_prec)), mean=mean_e_prec, sd=sd_e_prec)

# Entering climacteric
mean_e_entc <- 7.3
sd_e_entc <- 4.5

e_entc <- qnorm(runif(n, pnorm(lower, mean=mean_e_entc, sd=sd_e_entc), pnorm(upper, mean=mean_e_entc, sd=sd_e_entc)), mean=mean_e_entc, sd=sd_e_entc)

# Climacteric
mean_e_c <- 20
sd_e_c <- 15

e_c <- qnorm(runif(n, pnorm(lower, mean=mean_e_c, sd=sd_e_c), pnorm(upper, mean=mean_e_c, sd=sd_e_c)), mean=mean_e_c, sd=sd_e_c)

if(figs){boxplot(e_prec,e_entc,e_c)}

# Emission rate per tree (ppm s)
emission_prec <- (fruit_load*e_prec)/3600
emission_entc <- (fruit_load*e_entc)/3600
emission_c <- (fruit_load*e_c)/3600

emission_prec*180

# # Stochastic Simulations
# # Varying all inputs (emission rate, fruit load)
# result_all <- c()
# for(i in 1:1000){
#   fruit_load <- qnorm(runif(n, pnorm(lower, mean=mean_yield, sd=sd_yield), pnorm(upper, mean=mean_yield, sd=sd_yield)), mean=mean_yield, sd=sd_yield)
#   e_c <- qnorm(runif(n, pnorm(lower, mean=mean_e_c, sd=sd_e_c), pnorm(upper, mean=mean_e_c, sd=sd_e_c)), mean=mean_e_c, sd=sd_e_c)
# 
#   emission_c <- (fruit_load*e_c)/3600
#   
#   result_all <- c(result_all,emission_c)
# }
# 
# # Varying emission rate
# result_emission <- c()
# for(i in 1:1000){
#   fruit_load <- mean_yield
#   e_c <- qnorm(runif(n, pnorm(lower, mean=mean_e_c, sd=sd_e_c), pnorm(upper, mean=mean_e_c, sd=sd_e_c)), mean=mean_e_c, sd=sd_e_c)
#   
#   emission_c <- (fruit_load*e_c)/3600
#   
#   result_emission <- c(result_emission,emission_c)
# }
# 
# # Varying load
# result_load <- c()
# for(i in 1:1000){
#   fruit_load <- qnorm(runif(n, pnorm(lower, mean=mean_yield, sd=sd_yield), pnorm(upper, mean=mean_yield, sd=sd_yield)), mean=mean_yield, sd=sd_yield)
#   e_c <- mean_e_c
#   
#   emission_c <- (fruit_load*e_c)/3600
#   
#   result_load <- c(result_load,emission_c)
# }
# 
# var(result_all)
# var(result_emission)
# var(result_load)
# var(result_emission) + var(result_load)
# if(figs){boxplot(result_all, result_emission,result_load)
# }

# Make coordinates for trees

x_coord <- seq(5,15,5)
y_coord <- seq(5,10.5,1.1)

coords <- expand.grid(x_coord,y_coord)
colnames(coords) <- c('x','y')
coords$z <- round(fruit_position,1)

# Distribute the positions evenly for each coordinate
coords$side <- sample(rep(1:6,(n/6)))
coords$e_x <- coords$x
coords$e_y <- coords$y

# For position 1
coords$e_y[coords$side == 1] <- coords$e_y[coords$side == 1] - 0.3

# For position 2 
coords$e_x[coords$side == 2]  <- coords$e_x[coords$side == 2] + 0.3

# For position 3 
coords$e_y[coords$side == 3] <- coords$e_y[coords$side == 3] + 0.3

# For position 4 
coords$e_x[coords$side == 4]  <- coords$e_x[coords$side == 4] - 0.3

# For position 5 
coords$e_x[coords$side == 5]  <- coords$e_x[coords$side == 5] - 0.3
coords$e_y[coords$side == 5]  <- coords$e_y[coords$side == 5] - 0.3

# For position 6 
coords$e_x[coords$side == 6]  <- coords$e_x[coords$side == 6] + 0.3
coords$e_y[coords$side == 6]  <- coords$e_y[coords$side == 6] + 0.3

wind_data <- c('$(find orchard_demo)/demo/wind_at_cell_center_points_0ms',"$(find orchard_demo)/demo/wind_at_cell_center_points_2msX","$(find orchard_demo)/demo/wind_at_cell_center_points_2msY","$(find orchard_demo)/demo/wind_at_cell_center_points_5msX","$(find orchard_demo)/demo/wind_at_cell_center_points_5msY","$(find orchard_demo)/demo/wind_at_cell_center_points_2msX_drone")
emission_data <- c('emission_c','emission_entc','emission_prec')

simulation_runs <- expand.grid(wind_data,emission_data, stringsAsFactors = FALSE)
simulation_runs$wind_speed <- sapply(strsplit(simulation_runs$Var1,'_'),tail,1)
simulation_runs$name <- paste0('sim_',simulation_runs$Var2,'_',simulation_runs$wind_speed,'_s')

simulation_runs$time_step <- rep(c('0.001', '0.05', '0.05', '0.02','0.02'),3)

save.image(file='parameters.RData')

# Load the template launch file
template <- xmlParse(file='../launch/gaden_simulator_demo_template_s.launch')

player <- xmlParse(file='../launch/gaden_player_demo_template_s.launch')

# General parameters
sim_time<- "301" ### [sec] Total time of the gas dispersion simulation
#SET FOR EVERY SIM 
time_step <- "0.1"  ### [sec] Time increment between snapshots. Set aprox = cell_size/max_wind_speed.
num_filaments_sec <- "10"   ### Num of filaments released each second
# GAS CONCENTRATION
filament_initial_std <- "10"  ### [cm] Sigma of the filament at t=0-> 3DGaussian shape
filament_growth_gamma <- "10" ### [cm²/s] Growth ratio of the filament_std
filament_noise_std <- '0.02'  ### [m] Range of the white noise added on each iteration
gas_type <- "10"  ### 0=Ethanol, 1=Methane, 2=Hydrogen, 6=Acetone, 10-ethylene
temperature <- "298"      ### [Kelvins]
pressure <- "1"   ### [Atm]
concentration_unit_choice <- "1" ### 0=molecules/cm3, 1=ppm (when ppm is used, set temp and pressure accordingly)
# In the occupancyGrid.csv file we set: cell_size, num_cells, etc. which come from the CFD wind simulation
occupancy3D_data <- "$(find orchard_demo)/demo/OccupancyGrid3D.csv"
# Path to wind simulations (from CFD)- In this demo we use just a static wind (just time-step = 0)
# WIND DATA
wind_time_step <- "1" ### (sec) time increment between Wind snapshot
# POSITIONS - general
save_results <- "1" #1=true, 0=false
restuls_time_step <- "15"  #(sec) Time increment between saving state to file

for(i in 0:(n-1)){
  
  # For plotting the source positions
  xp_x <- paste0('//param[@name="/source_',i,'_position_x"]')
  xp_y <- paste0('//param[@name="/source_',i,'_position_y"]')
  xp_z <- paste0('//param[@name="/source_',i,'_position_z"]')
  
  nodeSet <- xpathApply(template,xp_x)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$e_x
  
  nodeSet <- xpathApply(template,xp_y)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$e_y
  
  nodeSet <- xpathApply(template,xp_z)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$z
  
  nodeSet <- xpathApply(player,xp_x)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$e_x
  
  nodeSet <- xpathApply(player,xp_y)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$e_y
  
  nodeSet <- xpathApply(player,xp_z)
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$z
  
  nodeSet <- xpathApply(player,'//param[@name="/occupancy3D_data"]')
  xmlAttrs(nodeSet[[1]])[2] <- occupancy3D_data
  
  # Setting filament simulator parameters
  xp_filament <- paste0('//node[@name="filament_simulator',i+1,'"]')
  # Position
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/source_position_x"]'))
  xmlAttrs(nodeSet[[1]])[2] = coords[i+1,]$e_x
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/source_position_y"]'))
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$e_y
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/source_position_z"]'))
  xmlAttrs(nodeSet[[1]])[2] <- coords[i+1,]$z
    
  # General parameters
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/sim_time"]'))
  xmlAttrs(nodeSet[[1]])[2] <- sim_time
  
  #SET FOR EVERY SIM
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/time_step"]'))
  xmlAttrs(nodeSet[[1]])[2] <- time_step
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/num_filaments_sec"]'))
  xmlAttrs(nodeSet[[1]])[2] <- num_filaments_sec
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/filament_initial_std"]'))
  xmlAttrs(nodeSet[[1]])[2] <- filament_initial_std
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/filament_growth_gamma"]'))
  xmlAttrs(nodeSet[[1]])[2] <- filament_growth_gamma
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/filament_noise_std"]'))
  xmlAttrs(nodeSet[[1]])[2] <- filament_noise_std
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/gas_type"]'))
  xmlAttrs(nodeSet[[1]])[2] <- gas_type
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/temperature"]'))
  xmlAttrs(nodeSet[[1]])[2] <- temperature
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/pressure"]'))
  xmlAttrs(nodeSet[[1]])[2] <- pressure
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/concentration_unit_choice"]'))
  xmlAttrs(nodeSet[[1]])[2] <- concentration_unit_choice
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/occupancy3D_data"]'))
  xmlAttrs(nodeSet[[1]])[2] <- occupancy3D_data
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/wind_time_step"]'))
  xmlAttrs(nodeSet[[1]])[2] <- wind_time_step
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/save_results"]'))
  xmlAttrs(nodeSet[[1]])[2] <- save_results
  
  nodeSet <- xpathApply(template,paste0(xp_filament,'/param[@name="/restuls_time_step"]'))
  xmlAttrs(nodeSet[[1]])[2] <- restuls_time_step

}

  
for(s in 1:15){
  simulation_launch <- template
  player_launch <- player
  wind <- simulation_runs$Var1[s]
  emission <- simulation_runs$Var2[s]
  time_step <- simulation_runs$time_step[s]
  sim_data <- paste0('//param[@name="/simulation_data_0"]')
  nodeSet <- xpathApply(player_launch,sim_data)
  xmlAttrs(nodeSet[[1]])[2] <- paste0("$(find orchard_demo)/demo/",simulation_runs$name[s],"/sum_")
  for(i in 1:n){
    xp_filament <- paste0('//node[@name="filament_simulator',i,'"]')
    # Assign the emission
    nodeSet <- xpathApply(simulation_launch,paste0(xp_filament,'/param[@name="/ppm_filament_center"]'))
    xmlAttrs(nodeSet[[1]])[2] <- (eval(parse(text=emission)))[i]/10 # per each filament (10)
    # Assign the wind file
    nodeSet <- xpathApply(simulation_launch,paste0(xp_filament,'/param[@name="/wind_data"]'))
    xmlAttrs(nodeSet[[1]])[2] <- wind
    # Assign the output folder
    nodeSet <- xpathApply(simulation_launch,paste0(xp_filament,'/param[@name="/results_location"]'))
    xmlAttrs(nodeSet[[1]])[2] <- paste0("$(find orchard_demo)/demo/",simulation_runs$name[s])
    
    #nodeSet <- xpathApply(simulation_launch,paste0(xp_filament,'/param[@name="/time_step"]'))
    #xmlAttrs(nodeSet[[1]])[2] <- time_step
    
    #Player
    #X <- format(coords[i,]$e_x,nsmall=2)
    #Y <- format(coords[i,]$e_y,nsmall=2)
    #Z <- format(coords[i,]$z,nsmall=2)
    #sim_data <- paste0('//param[@name="/simulation_data_',i-1,'"]')
    #nodeSet <- xpathApply(player_launch,sim_data)
    #xmlAttrs(nodeSet[[1]])[2] <- paste0("$(find orchard_demo)/demo/",simulation_runs$name[s],"/FilamentSimulation_gasType_10_sourcePosition_",X,"_",Y,"_",Z,"_iteration_")
    
  }
  # save the file with a nice name
  saveXML(simulation_launch, file = paste0('../launch/',simulation_runs$name[s],'.launch'))
  saveXML(player_launch, file = paste0('../launch/',simulation_runs$name[s],'_player.launch'))
  if(!dir.exists(simulation_runs$name[s])){dir.create(simulation_runs$name[s])}
}


