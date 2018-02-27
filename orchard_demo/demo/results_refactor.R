setwd("~/catkin_ws/src/gaden/orchard_demo/demo")
set.seed(12345678)
library(RColorBrewer)
require(tikzDevice)
library(fields)
library(MASS)
library(xtable)
library(fmsb)
library(extrafont)
font_import()
loadfonts()

# Start ----
# Load simulation data
load("~/catkin_ws/src/gaden/orchard_demo/demo/sim_results_new.RData")

# Path to save figures
path_to_sims <-
  '/Users/rodrigoalmeida/Dropbox/Rodrigo/Thesis/Simulations/new_figures/'
# Making zones
main_volume <- list(25:174, 25:130, 1:32)
# trees + 2.5 m buffer around it
in_rows <-
  list(c((50 - 4):(50 + 4), (100 - 4):(100 + 4), (150 - 4):(150 + 4)), (50 -
                                                                          4):(105 + 4), 1:32)
# trees + 0.4 cm on each side (0.8 m per row)
inbetween_rows <-
  list(c((50 + 5):(100 - 5), (100 + 5):(150 - 5)), (50 - 4):(105 + 4), 1:32)
#w <- 3.7
#h <- 3.7
zones_col <-
  c(
    rgb(0, 18, 242, 55, maxColorValue = 255),
    rgb(0, 231, 100, 65, maxColorValue = 255),
    rgb(255, 255, 0, 65, maxColorValue = 255)
  )

# Functions ----
z.test = function(a, mu, sd) {
  zeta = (mean(a) - mu) / (sd / sqrt(length(a)))
  return(abs(zeta))
}

z.test_s = function(a, mu, sd) {
  zeta = (mean(a, na.rm = T) - mu) / (sd / sqrt(length(a)))
  return(zeta)
}

z.test_n = function(x, n, mu, sd) {
  zeta = (x - mu) / (sd / sqrt(length(n)))
  return(abs(zeta))
}

my.t.test.p.value <- function(...) {
  obj <- try(t.test(...), silent = TRUE)
  if (is(obj, "try-error"))
    return(1)
  else
    return(obj$p.value)
}

ihs <- function(x) {
    transformed <- log(x + sqrt((x ^ 2) + 1))
    return(transformed)
  }

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
    boxcoxTrans <- function(x, lam1, lam2 = NULL) {
      # if we set lambda2 to zero, it becomes the one parameter transformation
      lam2 <- ifelse(is.null(lam2), 0, lam2)
      
      if (lam1 == 0L) {
        log(y + lam2)
      } else {
        (((y + lam2) ^ lam1) - 1) / lam1
      }
    }
    
    switch(method
           ,
           boxcox = boxcoxTrans(y, lambda1, lambda2)
           ,
           tukey = y ^ lambda1)
  }

# Setting up ----


X <- 1:199
Y <- 1:164
Z <- 1:59
ii_prec <-
  cut(
    emission_prec * 1000,
    breaks = seq(min(emission_prec * 1000), max(emission_prec * 1000), len = 5),
    include.lowest = TRUE
  )
ii_entc <-
  cut(
    emission_entc * 1000,
    breaks = seq(min(emission_entc * 1000), max(emission_entc * 1000), len = 5),
    include.lowest = TRUE
  )
ii_c <-
  cut(emission_c * 1000,
      breaks = seq(min(emission_c * 1000), max(emission_c * 1000), len = 5),
      include.lowest = TRUE)
ii <- list(ii_prec, ii_entc, ii_c)

sims <-
  list(
    sim_emission_c_0ms_s,
    sim_emission_c_2msX_s,
    sim_emission_c_2msY_s,
    sim_emission_c_5msX_s,
    sim_emission_c_5msY_s,
    sim_emission_entc_0ms_s,
    sim_emission_entc_2msX_s,
    sim_emission_entc_2msY_s,
    sim_emission_entc_5msX_s,
    sim_emission_entc_5msY_s,
    sim_emission_prec_0ms_s,
    sim_emission_prec_2msX_s,
    sim_emission_prec_2msY_s,
    sim_emission_prec_5msX_s,
    sim_emission_prec_5msY_s
  )

names(sims) <-
  c(
    'sim_emission_c_0ms_s',
    "sim_emission_c_2msX_s",
    "sim_emission_c_2msY_s",
    "sim_emission_c_5msX_s",
    "sim_emission_c_5msY_s",
    'sim_emission_entc_0ms_s',
    "sim_emission_entc_2msX_s",
    "sim_emission_entc_2msY_s",
    "sim_emission_entc_5msX_s",
    "sim_emission_entc_5msY_s",
    'sim_emission_prec_0ms_s',
    "sim_emission_prec_2msX_s",
    "sim_emission_prec_2msY_s",
    "sim_emission_prec_5msX_s",
    "sim_emission_prec_5msY_s"
  )

sims_drone <-
  list(
    sim_emission_c_drone1_s,
    sim_emission_entc_drone1_s,
    sim_emission_prec_drone1_s,
    sim_emission_c_drone2_s,
    sim_emission_entc_drone2_s,
    sim_emission_prec_drone2_s
  )
names(sims_drone) <-
  c(
    'sim_emission_c_drone1_s',
    'sim_emission_entc_drone1_s',
    'sim_emission_prec_drone1_s',
    'sim_emission_c_drone2_s',
    'sim_emission_entc_drone2_s',
    'sim_emission_prec_drone2_s'
  )

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

for (sim in sims) {
  # Don't take out NA, keep 0
  #is.na(sim) <- !sim
  
  sim_avg <- c(sim_avg, list(apply(sim, 4, mean, na.rm = T)))
  sim_avg_main <-
    c(sim_avg_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, mean, na.rm =
                                 T)))
  sim_avg_in <-
    c(sim_avg_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, mean, na.rm =
                               T)))
  sim_avg_inbet <-
    c(sim_avg_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, mean, na.rm =
                                  T)))
  
  sim_std <- c(sim_std, list(apply(sim, 4, sd, na.rm = T)))
  sim_std_main <-
    c(sim_std_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, sd, na.rm =
                                 T)))
  sim_std_in <-
    c(sim_std_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, sd, na.rm =
                               T)))
  sim_std_inbet <-
    c(sim_std_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, sd, na.rm =
                                  T)))
  
  sim_var <- c(sim_var, list(apply(sim, 4, var, na.rm = T)))
  sim_var_main <-
    c(sim_var_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, var, na.rm =
                                 T)))
  sim_var_in <-
    c(sim_var_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, var, na.rm =
                               T)))
  sim_var_inbet <-
    c(sim_var_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, var, na.rm =
                                  T)))
  
  sim_max <- c(sim_max, list(apply(sim, 4, max, na.rm = T)))
  sim_max_main <-
    c(sim_max_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, max, na.rm =
                                 T)))
  sim_max_in <-
    c(sim_max_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, max, na.rm =
                               T)))
  sim_max_inbet <-
    c(sim_max_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, max, na.rm =
                                  T)))
  
  is.na(sim) <- !sim
  sim_n <-
    c(sim_n, list(apply(sim, 4, function(x)
      length(which(
        !is.na(x)
      )))))
  sim_n_main <-
    c(sim_n_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
  sim_n_in <-
    c(sim_n_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
  sim_n_inbet <-
    c(sim_n_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
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

# DRONE SIMS
drone_sim_avg <- list()
drone_sim_std <- list()
drone_sim_var <- list()
drone_sim_max <- list()
drone_sim_n <- list()

drone_sim_avg_main <- list()
drone_sim_std_main <- list()
drone_sim_var_main <- list()
drone_sim_max_main <- list()
drone_sim_n_main <- list()

drone_sim_avg_in <- list()
drone_sim_std_in <- list()
drone_sim_var_in <- list()
drone_sim_max_in <- list()
drone_sim_n_in <- list()

drone_sim_avg_inbet <- list()
drone_sim_std_inbet <- list()
drone_sim_var_inbet <- list()
drone_sim_max_inbet <- list()
drone_sim_n_inbet <- list()

for (sim in sims_drone) {
  #is.na(sim) <- !sim
  
  drone_sim_avg <- c(drone_sim_avg, list(apply(sim, 4, mean, na.rm = T)))
  drone_sim_avg_main <-
    c(drone_sim_avg_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, mean, na.rm =
                                 T)))
  drone_sim_avg_in <-
    c(drone_sim_avg_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, mean, na.rm =
                               T)))
  drone_sim_avg_inbet <-
    c(drone_sim_avg_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, mean, na.rm =
                                  T)))
  
  drone_sim_std <- c(drone_sim_std, list(apply(sim, 4, sd, na.rm = T)))
  drone_sim_std_main <-
    c(drone_sim_std_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, sd, na.rm =
                                 T)))
  drone_sim_std_in <-
    c(drone_sim_std_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, sd, na.rm =
                               T)))
  drone_sim_std_inbet <-
    c(drone_sim_std_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, sd, na.rm =
                                  T)))
  
  drone_sim_var <- c(drone_sim_var, list(apply(sim, 4, var, na.rm = T)))
  drone_sim_var_main <-
    c(drone_sim_var_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, var, na.rm =
                                 T)))
  drone_sim_var_in <-
    c(drone_sim_var_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, var, na.rm =
                               T)))
  drone_sim_var_inbet <-
    c(drone_sim_var_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, var, na.rm =
                                  T)))
  
  drone_sim_max <- c(drone_sim_max, list(apply(sim, 4, max, na.rm = T)))
  drone_sim_max_main <-
    c(drone_sim_max_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, max, na.rm =
                                 T)))
  drone_sim_max_in <-
    c(drone_sim_max_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, max, na.rm =
                               T)))
  drone_sim_max_inbet <-
    c(drone_sim_max_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, max, na.rm =
                                  T)))
  
  is.na(sim) <- !sim
  drone_sim_n <-
    c(drone_sim_n, list(apply(sim, 4, function(x)
      length(which(
        !is.na(x)
      )))))
  drone_sim_n_main <-
    c(drone_sim_n_main, list(apply(sim[main_volume[[1]], main_volume[[2]], main_volume[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
  drone_sim_n_in <-
    c(drone_sim_n_in, list(apply(sim[in_rows[[1]], in_rows[[2]], in_rows[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
  drone_sim_n_inbet <-
    c(drone_sim_n_inbet, list(apply(sim[inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], 4, function(x)
      length(which(
        !is.na(x)
      )))))
}

rm(sim)

names(drone_sim_avg) <- names(sims_drone)
names(drone_sim_std) <- names(sims_drone)
names(drone_sim_var) <- names(sims_drone)
names(drone_sim_max) <- names(sims_drone)
names(drone_sim_n) <- names(sims_drone)

names(drone_sim_avg_in) <- names(sims_drone)
names(drone_sim_std_in) <- names(sims_drone)
names(drone_sim_var_in) <- names(sims_drone)
names(drone_sim_max_in) <- names(sims_drone)
names(drone_sim_n_in) <- names(sims_drone)

names(drone_sim_avg_inbet) <- names(sims_drone)
names(drone_sim_std_inbet) <- names(sims_drone)
names(drone_sim_var_inbet) <- names(sims_drone)
names(drone_sim_max_inbet) <- names(sims_drone)
names(drone_sim_n_inbet) <- names(sims_drone)

names(drone_sim_avg_main) <- names(sims_drone)
names(drone_sim_std_main) <- names(sims_drone)
names(drone_sim_var_main) <- names(sims_drone)
names(drone_sim_max_main) <- names(sims_drone)
names(drone_sim_n_main) <- names(sims_drone)

# Variables for sampling ----
wind_pch <- c(1, 2, 3)
emission_col <- brewer.pal(3, 'Reds')
wind_col <- brewer.pal(3, 'Blues')
wind_dir_col <- brewer.pal(3, 'PRGn')
zones <- c('Environment', 'Main volume', 'In rows', 'In-between rows')
zones_files <-
  c('Environment', 'Main_volume', 'In_rows', 'Inbetween_rows')

#number_of_samples <- c(1, 4, 16)
number_of_samples <- c(4, 8, 16, 32)

# Calculate averages overall ----
sims_avg <-
  lapply(sims, function(x)
    apply(x, c(1, 2, 3), mean, na.rm = T))
sim_avg_all <- lapply(sim_avg, mean, na.rm = T)
sim_std_all <- lapply(sim_std, mean, na.rm = T)

# Average Ethylene Concentration ----
z <- 1
for (s in list(sim_avg, sim_avg_main, sim_avg_in, sim_avg_inbet)) {
  tikz(file = paste0(path_to_sims, 'Avg_', zones_files[z], '.tex'))
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s),
    xlim = range(timesteps),
    col = emission_col[3],
    pch = wind_pch[1],
    type = 'b',
    xlab = 'Time ($s$)',
    ylab = 'Average ethylene concentration ($ppb$)',
    main = zones[z],
    lwd = 3
  )
  z = z + 1
  for (i in 2:length(sims)) {
    if (i %in% 2:5) {
      p_t <- emission_col[3]
    }
    if (i %in% 6:10) {
      p_t <- emission_col[2]
    }
    if (i %in% 11:15) {
      p_t <- emission_col[1]
    }
    
    if (i %in% c(6, 11)) {
      p_ch <- wind_pch[1]
    }
    if (i %in% c(2, 3, 7, 8, 12, 13)) {
      p_ch <- wind_pch[2]
    }
    if (i %in% c(4, 5, 9, 10, 14, 15)) {
      p_ch <- wind_pch[3]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      pch = p_ch,
      type = 'b',
      lwd = 3
    )
  }
  dev.off()
}

# Make Boxplots per ethylene stage ----
all_c <-
  rbind(
    apply(sims[[2]], MARGIN = 4, FUN = c),
    apply(sims[[3]], MARGIN = 4, FUN = c),
    apply(sims[[4]], MARGIN = 4, FUN = c),
    apply(sims[[5]], MARGIN = 4, FUN = c),
    cbind(apply(sims[[1]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA)
  )

all_c_main <-
  rbind(
    apply(sims[[2]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[3]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[4]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[5]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[1]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_c_inbet <-
  rbind(
    apply(sims[[2]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[3]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[4]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[5]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[1]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_c_inrow <-
  rbind(
    apply(sims[[2]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[3]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[4]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[5]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[1]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )


all_entc <-
  rbind(
    apply(sims[[7]], MARGIN = 4, FUN = c),
    apply(sims[[8]], MARGIN = 4, FUN = c),
    apply(sims[[9]], MARGIN = 4, FUN = c),
    apply(sims[[10]], MARGIN = 4, FUN = c),
    cbind(apply(sims[[6]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA)
  )

all_entc_main <-
  rbind(
    apply(sims[[7]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[8]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[9]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[10]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[6]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_entc_inrow <-
  rbind(
    apply(sims[[7]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[8]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[9]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[10]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[6]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_entc_inbet <-
  rbind(
    apply(sims[[7]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[8]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[9]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[10]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[6]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )


all_prec <-
  rbind(
    apply(sims[[12]], MARGIN = 4, FUN = c),
    apply(sims[[13]], MARGIN = 4, FUN = c),
    apply(sims[[14]], MARGIN = 4, FUN = c),
    apply(sims[[15]], MARGIN = 4, FUN = c),
    cbind(apply(sims[[11]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA)
  )

all_prec_main <-
  rbind(
    apply(sims[[12]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[13]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[14]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[15]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[11]][main_volume[[1]], main_volume[[2]], main_volume[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_prec_inrow <-
  rbind(
    apply(sims[[12]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[13]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[14]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[15]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[11]][in_rows[[1]], in_rows[[2]], in_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

all_prec_inbet <-
  rbind(
    apply(sims[[12]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[13]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[14]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    apply(sims[[15]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c),
    cbind(apply(
      sims[[11]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]], ], MARGIN = 4, FUN = c
    ), NA, NA, NA, NA, NA)
  )

tikz(file = paste0(path_to_sims, 'Boxplots_', zones_files[1], '.tex'))
par(mar = c(5, 5, 2, 5))
boxplot(
  all_c+10,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = zones[1],
  col = emission_col[3],
  outline = FALSE,
  range = 0
)
boxplot(
  all_entc+10,
  log = 'y',
  boxwex = 0.25,
  at = 1:20,
  add = T,
  col = emission_col[2],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
boxplot(
  all_prec+10,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  col = emission_col[1],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
dev.off()

tikz(file = paste0(path_to_sims,'Legend_boxplots_sims.tex'), height = 0.3)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", c("Pre-climacteric", "Entering climacteric", "Climacteric"), xpd = TRUE, horiz = TRUE, bty = "n", fill = emission_col)
dev.off()

tikz(file = paste0(path_to_sims,'Legend_Conf.tex'), height = 0.3)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", c("$n=1$", "$n=4$", "$n=16$"), xpd = TRUE, horiz = TRUE, bty = "n", pch=c(1,2,3))
dev.off()

tikz(file = paste0(path_to_sims, 'Boxplots_', zones_files[2], '.tex'))
par(mar = c(5, 5, 2, 5))
boxplot(
  all_c_main+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = zones[2],
  col = emission_col[3],
  outline = FALSE,
  range = 0
)
boxplot(
  all_entc_main+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20,
  add = T,
  col = emission_col[2],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
boxplot(
  all_prec_main+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  col = emission_col[1],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
dev.off()

tikz(file = paste0(path_to_sims, 'Boxplots_', zones_files[3], '.tex'))
par(mar = c(5, 5, 2, 5))
boxplot(
  all_c_inrow+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = zones[3],
  col = emission_col[3],
  outline = FALSE,
  range = 0
)
boxplot(
  all_entc_inrow+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20,
  add = T,
  col = emission_col[2],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
boxplot(
  all_prec_inrow+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  col = emission_col[1],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
dev.off()

tikz(file = paste0(path_to_sims, 'Boxplots_', zones_files[4], '.tex'))
par(mar = c(5, 5, 2, 5))
boxplot(
  all_c_inbet+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = zones[4],
  col = emission_col[3],
  outline = FALSE,
  range = 0
)
boxplot(
  all_entc_inbet+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20,
  add = T,
  col = emission_col[2],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
boxplot(
  all_prec_inbet+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  col = emission_col[1],
  axes = FALSE,
  outline = FALSE,
  range = 0
)
dev.off()

all_0ms <-
  rbind(
    cbind(apply(sims[[1]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA),
    cbind(apply(sims[[6]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA),
    cbind(apply(sims[[11]], MARGIN = 4, FUN = c), NA, NA, NA, NA, NA)
  )

all_2ms <-
  rbind(
    apply(sims[[2]], MARGIN = 4, FUN = c),
    apply(sims[[3]], MARGIN = 4, FUN = c),
    apply(sims[[7]], MARGIN = 4, FUN = c),
    apply(sims[[8]], MARGIN = 4, FUN = c),
    apply(sims[[12]], MARGIN = 4, FUN = c),
    apply(sims[[13]], MARGIN = 4, FUN = c)
  )

all_5ms <-
  rbind(
    apply(sims[[4]], MARGIN = 4, FUN = c),
    apply(sims[[5]], MARGIN = 4, FUN = c),
    apply(sims[[9]], MARGIN = 4, FUN = c),
    apply(sims[[10]], MARGIN = 4, FUN = c),
    apply(sims[[14]], MARGIN = 4, FUN = c),
    apply(sims[[15]], MARGIN = 4, FUN = c)
  )

all_X <-
  rbind(
    apply(sims[[2]], MARGIN = 4, FUN = c),
    apply(sims[[4]], MARGIN = 4, FUN = c),
    apply(sims[[7]], MARGIN = 4, FUN = c),
    apply(sims[[9]], MARGIN = 4, FUN = c),
    apply(sims[[12]], MARGIN = 4, FUN = c),
    apply(sims[[14]], MARGIN = 4, FUN = c)
  )

all_Y <-
  rbind(
    apply(sims[[3]], MARGIN = 4, FUN = c),
    apply(sims[[5]], MARGIN = 4, FUN = c),
    apply(sims[[8]], MARGIN = 4, FUN = c),
    apply(sims[[10]], MARGIN = 4, FUN = c),
    apply(sims[[13]], MARGIN = 4, FUN = c),
    apply(sims[[15]], MARGIN = 4, FUN = c)
  )

tikz(file = paste0(path_to_sims, 'Boxplots_WindSpeed.tex'),
     height = 5)
boxplot(
  all_0ms+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = 'Wind speed',
  col = wind_col[1],
  outline = FALSE,
  range = 0
)
boxplot(
  all_2ms+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20,
  add = T,
  axes = FALSE,
  outline = FALSE,
  range = 0,
  col = wind_col[2]
)
boxplot(
  all_5ms+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  axes = FALSE,
  outline = FALSE,
  range = 0,
  col = wind_col[3]
)
dev.off()

tikz(file = paste0(path_to_sims, 'Boxplots_WindDirection.tex'),
     height = 5)
boxplot(
  all_X+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 - 0.25,
  xlim = c(0.5, 20.5),
  names = timesteps,
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = 'Wind direction',
  col = wind_dir_col[1],
  outline = FALSE,
  range = 0
)
boxplot(
  all_Y+1,
  log = 'y',
  boxwex = 0.25,
  at = 1:20 + 0.25,
  add = T,
  axes = FALSE,
  outline = FALSE,
  range = 0,
  col = wind_dir_col[3]
)
dev.off()

tikz(file = paste0(path_to_sims, 'Legend_Wind.tex'),
     height = 0.7)
par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0)
)
plot(
  0,
  0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n"
)
legend(
  "top",
  c("0 $ms^{-1}$", "2 $ms^{-1}$", "5 $ms^{-1}$"),
  bty = "n",
  xpd = TRUE,
  horiz = TRUE,
  fill = wind_col
)
legend(
  'top',
  inset = c(0, 0.6),
  c("$\\vec{x}$", "$\\vec{y}$"),
  xpd = TRUE,
  horiz = TRUE,
  bty = "n",
  fill = c(wind_dir_col[c(1, 3)])
)
dev.off()

tikz(file = paste0(path_to_sims, 'Legend_EthEvo.tex'),
     height = 0.7)
par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0)
)
plot(
  0,
  0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n"
)
legend(
  "top",
  c("0 $ms^{-1}$", "2 $ms^{-1}$", "5 $ms^{-1}$"),
  xpd = TRUE, horiz = TRUE, bty = "n", pch=wind_pch
)
legend(
  'top',
  inset = c(0, 0.6),
  c("$\\vec{x}$", "$\\vec{y}$"),
  xpd = TRUE,
  horiz = TRUE,
  bty = "n",
  fill = c(wind_dir_col[c(1, 3)])
)
dev.off()


# Variance ----
z <- 1
for (s in list(sim_var, sim_var_main, sim_var_in, sim_var_inbet)) {
  tikz(file = paste0(path_to_sims, 'Var_', zones_files[z], '.tex'))
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s),
    xlim = range(timesteps),
    col = emission_col[3],
    type = 'b',
    lwd = 3,
    pch = wind_pch[1],
    xlab = 'Time ($s$)',
    ylab = 'Variance of ethylene concentration',
    main = zones[z]
  )
  z = z + 1
  for (i in 2:length(sims)) {
    if (i %in% 2:5) {
      p_t <- emission_col[3]
    }
    if (i %in% 6:10) {
      p_t <- emission_col[2]
    }
    if (i %in% 11:15) {
      p_t <- emission_col[1]
    }
    if (i %in% c(6, 11)) {
      p_ch <- wind_pch[1]
    }
    if (i %in% c(2, 3, 7, 8, 12, 13)) {
      p_ch <- wind_pch[2]
    }
    if (i %in% c(4, 5, 9, 10, 14, 15)) {
      p_ch <- wind_pch[3]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      pch = p_ch,
      type = 'b',
      lwd = 3
    )
  }
  dev.off()
}

# Maximum ----
z <- 1
for (s in list(sim_max, sim_max_main, sim_max_in, sim_max_inbet)) {
  tikz(file = paste0(path_to_sims, 'Max_', zones_files[z], '.tex'))
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s),
    xlim = range(timesteps),
    col = emission_col[3],
    type = 'b',
    pch = wind_pch[1],
    lwd = 3,
    xlab = 'Time ($s$)',
    ylab = 'Maximum ethylene concentration ($ppb$)',
    main = zones[z]
  )
  z = z + 1
  for (i in 2:length(sims)) {
    if (i %in% 2:5) {
      p_t <- emission_col[3]
    }
    if (i %in% 6:10) {
      p_t <- emission_col[2]
    }
    if (i %in% 11:15) {
      p_t <- emission_col[1]
    }
    if (i %in% c(6, 11)) {
      p_ch <- wind_pch[1]
    }
    if (i %in% c(2, 3, 7, 8, 12, 13)) {
      p_ch <- wind_pch[2]
    }
    if (i %in% c(4, 5, 9, 10, 14, 15)) {
      p_ch <- wind_pch[3]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      type = 'b',
      pch = p_ch,
      lwd = 3
    )
  }
  dev.off()
}

# N ----
z <- 1
for (s in list(sim_n, sim_n_main, sim_n_in, sim_n_inbet)) {
  tikz(file = paste0(path_to_sims, 'N_', zones_files[z], '.tex'))
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s),
    col = emission_col[3],
    xlim = range(timesteps),
    type = 'b',
    pch = wind_pch[1],
    xlab = 'Time ($s$)',
    ylab = '$n$',
    main = zones[z],
    lwd = 3
  )
  z = z + 1
  for (i in 2:length(sims)) {
    if (i %in% 2:5) {
      p_t <- emission_col[3]
    }
    if (i %in% 6:10) {
      p_t <- emission_col[2]
    }
    if (i %in% 10:15) {
      p_t <- emission_col[1]
    }
    if (i %in% c(6, 11)) {
      p_ch <- wind_pch[1]
    }
    if (i %in% c(2, 3, 7, 8, 12, 13)) {
      p_ch <- wind_pch[2]
    }
    if (i %in% c(4, 5, 9, 10, 14, 15)) {
      p_ch <- wind_pch[3]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      type = 'b',
      pch = p_ch,
      lwd = 3
    )
  }
  dev.off()
}

# Standard deviation ----
z <- 1
for (s in list(sim_std, sim_std_main, sim_std_in, sim_std_inbet)) {
  tikz(file = paste0(path_to_sims, 'Sd_', zones_files[z], '.tex'))
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s),
    col = emission_col[3],
    xlim = range(timesteps),
    type = 'b',
    pch = wind_pch[1],
    xlab = 'Time ($s$)',
    ylab = 'Standard deviation of ethylene concentration ($ppb$)',
    main = zones[z],
    lwd = 3
  )
  z = z + 1
  for (i in 2:length(sims)) {
    if (i %in% 2:5) {
      p_t <- emission_col[3]
    }
    if (i %in% 6:10) {
      p_t <- emission_col[2]
    }
    if (i %in% 10:15) {
      p_t <- emission_col[1]
    }
    if (i %in% c(6, 11)) {
      p_ch <- wind_pch[1]
    }
    if (i %in% c(2, 3, 7, 8, 12, 13)) {
      p_ch <- wind_pch[2]
    }
    if (i %in% c(4, 5, 9, 10, 14, 15)) {
      p_ch <- wind_pch[3]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      type = 'b',
      pch = p_ch,
      lwd = 3
    )
  }
  dev.off()
}

rm(z, s, i, p_ch, p_t)


# Make average table ----
table_avg <-
  data.frame(
    as.array(sapply(sim_avg, mean)),
    as.array(sapply(sim_std, mean)),
    as.array(sapply(sim_std, mean)/sapply(sim_avg, mean))*100,
    as.array(sapply(sim_avg_main, mean)),
    as.array(sapply(sim_std_main, mean)),
    as.array(sapply(sim_std_main, mean)/sapply(sim_avg_main, mean))*100,
    as.array(sapply(sim_avg_in, mean)),
    as.array(sapply(sim_std_in, mean)),
    as.array(sapply(sim_std_in, mean)/sapply(sim_avg_in, mean))*100,
    as.array(sapply(sim_avg_inbet, mean)),
    as.array(sapply(sim_std_inbet, mean)),
    as.array(sapply(sim_std_inbet, mean)/sapply(sim_avg_inbet, mean))*100,
    row.names = 1:15
  )

colnames(table_avg) <-
  c(zones[1], '', '', zones[2], '', '', zones[3], '', '', zones[4], '', '')

print(
  xtable(
    table_avg,
    type = "latex",
    caption = 'Average concentration of ethylene ($ppb$) in the different simulations and zones.',
    digits = 2,
    label = 'tbl:avg_concentration'
  ),
  file = paste0(path_to_sims, "MeanConcentration.tex")
)


# Make average table DRONE ----
drone_table_avg <-
  data.frame(
    as.array(lapply(drone_sim_avg, mean)),
    as.array(lapply(drone_sim_std, mean)),
    as.array(sapply(drone_sim_std, mean)/sapply(drone_sim_avg, mean))*100,
    as.array(lapply(drone_sim_avg_main, mean)),
    as.array(lapply(drone_sim_std_main, mean)),
    as.array(sapply(drone_sim_std_main, mean)/sapply(drone_sim_avg_main, mean))*100,
    as.array(lapply(drone_sim_avg_in, mean)),
    as.array(lapply(drone_sim_std_in, mean)),
    as.array(sapply(drone_sim_std_in, mean)/sapply(drone_sim_avg_in, mean))*100,
    as.array(lapply(drone_sim_avg_inbet, mean)),
    as.array(lapply(drone_sim_std_inbet, mean)),
    as.array(sapply(drone_sim_std_inbet, mean)/sapply(drone_sim_avg_inbet, mean))*100,
    row.names = 1:6
  )


print(
  xtable(
    drone_table_avg,
    type = "latex",
    caption = 'Average concentration of ethylene ($ppb$) in the different drone simulations and zones.',
    digits = 2,
    label = 'tbl:drone_avg_concentration'
  ),
  file = paste0(path_to_sims, "DroneMeanConcentration.tex")
)

# Plot average across the same wind conditions ----
tikz(file = paste0(path_to_sims, 'EthyleneEvolution', zones_files[1], '.tex'),
     height = 3.5)
plot(c(3,2,1),table_avg[c(1,6,11),1], type='b',ylim=range(table_avg[,1]), pch=wind_pch[1], lwd=3, xlab = '$E_i$', ylab = 'Ethylene concentration ($ppb$)',main=zones[1])
lines(c(3,2,1),table_avg[c(2,7,12),1], type='b',pch=wind_pch[2], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(3,8,13),1], type='b',pch=wind_pch[2], col = wind_dir_col[3], lwd=3)
lines(c(3,2,1),table_avg[c(4,9,14),1], type='b',pch=wind_pch[3], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(5,10,15),1], type='b', pch=wind_pch[3], col = wind_dir_col[3], lwd=3)
y <- c(mean(unlist(table_avg[11:15,1])), mean(unlist(table_avg[6:10,1])),mean(unlist(table_avg[1:5,1])))
x <- 1:3
fit= lm(y~x)
abline(fit, lty=2)
legend('topleft', paste('$a=$',round(fit$coefficients[1],2),'$b=$',round(fit$coefficients[2],2)), bty='n')
dev.off()


tikz(file = paste0(path_to_sims, 'EthyleneEvolution', zones_files[2], '.tex'),
     height = 3.5)
plot(c(3,2,1),table_avg[c(1,6,11),1+3], type='b',ylim=range(table_avg[,1+3]), pch=wind_pch[1], lwd=3, xlab = '$E_i$', ylab = 'Ethylene concentration ($ppb$)',main=zones[2])
lines(c(3,2,1),table_avg[c(2,7,12),1+3], type='b',pch=wind_pch[2], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(3,8,13),1+3], type='b',pch=wind_pch[2], col = wind_dir_col[3], lwd=3)
lines(c(3,2,1),table_avg[c(4,9,14),1+3], type='b',pch=wind_pch[3], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(5,10,15),1+3], type='b', pch=wind_pch[3], col = wind_dir_col[3], lwd=3)
y <- c(mean(unlist(table_avg[11:15,1+3])), mean(unlist(table_avg[6:10,1+3])),mean(unlist(table_avg[1:5,1+3])))
x <- 1:3
fit= lm(y~x)
abline(fit, lty=2)
legend('topleft', paste('$a=$',round(fit$coefficients[1],2),'$b=$',round(fit$coefficients[2],2)), bty='n')
dev.off()

tikz(file = paste0(path_to_sims, 'EthyleneEvolution', zones_files[3], '.tex'),
     height = 3.5)
plot(c(3,2,1),table_avg[c(1,6,11),1+6], type='b',ylim=range(table_avg[,1+6]), pch=wind_pch[1], lwd=3, xlab = '$E_i$', ylab = 'Ethylene concentration ($ppb$)',main=zones[3])
lines(c(3,2,1),table_avg[c(2,7,12),1+6], type='b',pch=wind_pch[2], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(3,8,13),1+6], type='b',pch=wind_pch[2], col = wind_dir_col[3], lwd=3)
lines(c(3,2,1),table_avg[c(4,9,14),1+6], type='b',pch=wind_pch[3], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(5,10,15),1+6], type='b', pch=wind_pch[3], col = wind_dir_col[3], lwd=3)
y <- c(mean(unlist(table_avg[11:15,1+6])), mean(unlist(table_avg[6:10,1+6])),mean(unlist(table_avg[1:5,1+6])))
x <- 1:3
fit= lm(y~x)
abline(fit, lty=2)
legend('topleft', paste('$a=$',round(fit$coefficients[1],2),'$b=$',round(fit$coefficients[2],2)), bty='n')
dev.off()

tikz(file = paste0(path_to_sims, 'EthyleneEvolution', zones_files[4], '.tex'),
     height = 3.5)
plot(c(3,2,1),table_avg[c(1,6,11),1+9], type='b',ylim=range(table_avg[,1+9]), pch=wind_pch[1], lwd=3, xlab = '$E_i$', ylab = 'Ethylene concentration ($ppb$)',main=zones[4])
lines(c(3,2,1),table_avg[c(2,7,12),1+9], type='b',pch=wind_pch[2], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(3,8,13),1+9], type='b',pch=wind_pch[2], col = wind_dir_col[3], lwd=3)
lines(c(3,2,1),table_avg[c(4,9,14),1+9], type='b',pch=wind_pch[3], col = wind_dir_col[1], lwd=3)
lines(c(3,2,1),table_avg[c(5,10,15),1+9], type='b', pch=wind_pch[3], col = wind_dir_col[3], lwd=3)
y <- c(mean(unlist(table_avg[11:15,1+9])), mean(unlist(table_avg[6:10,1+9])),mean(unlist(table_avg[1:5,1+9])))
x <- 1:3
fit= lm(y~x)
abline(fit, lty=2)
legend('topleft', paste('$a=$',round(fit$coefficients[1],2),'$b=$',round(fit$coefficients[2],2)), bty='n')
dev.off()



# Make average plot ----

# plot(c(1,2,3),table_avg[c(11,6,1),3],xlab='$E_i$',ylab = 'Ethylene concentration ($ppb$)', ylim=c(0,10), type = 'b', pch=1)
#
# polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),3])-sqrt(as.numeric(table_avg[c(12,7,2),3])), rev(as.numeric(table_avg[c(11,6,1),3])+sqrt(as.numeric(table_avg[c(12,7,2),3])))))
# polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),6])-sqrt(as.numeric(table_avg[c(12,7,2),6])), rev(as.numeric(table_avg[c(11,6,1),6])+sqrt(as.numeric(table_avg[c(12,7,2),6])))), col=zones_col[1], border = NA)
# polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),9])-sqrt(as.numeric(table_avg[c(12,7,2),9])), rev(as.numeric(table_avg[c(11,6,1),9])+sqrt(as.numeric(table_avg[c(12,7,2),9])))), col=zones_col[2], border = NA)
# polygon(c(1:3,rev(1:3)),c(as.numeric(table_avg[c(11,6,1),12])-sqrt(as.numeric(table_avg[c(12,7,2),12])), rev(as.numeric(table_avg[c(11,6,1),12])+sqrt(as.numeric(table_avg[c(12,7,2),12])))), col=zones_col[3], border = NA)
#
# points(c(1,2,3),table_avg[c(11,6,1),6], type='b',pch=2, col = zones_col[1])
# points(c(1,2,3),table_avg[c(11,6,1),9], type='b',pch=3, col = zones_col[2])
# points(c(1,2,3),table_avg[c(11,6,1),12], type='b', pch=4, col = zones_col[3])


# Make histograms ----
tikz(file = paste0(path_to_sims, 'hist_avg.tex'))
hist(
  sims_avg[[1]] + sims_avg[[2]] + sims_avg[[3]] + sims_avg[[4]] + sims_avg[[5]],
  col = adjustcolor(emission_col[3], alpha.f = 0.7),
  xlab = "Ethylene concentration ($ppb$)",
  main = ''
)
hist(
  sims_avg[[6]] + sims_avg[[7]] + sims_avg[[8]] + sims_avg[[9]] + sims_avg[[10]],
  col = adjustcolor(emission_col[2], alpha.f = 0.7),
  add = T
)
hist(
  sims_avg[[11]] + sims_avg[[12]] + sims_avg[[13]] + sims_avg[[14]] + sims_avg[[15]],
  col = adjustcolor(emission_col[1], alpha.f = 0.7),
  add = T
)
legend(
  'topright',
  c("Pre-climacteric", "Entering climacteric", "Climacteric"),
  bty = "n",
  fill = emission_col
)
dev.off()

tikz(file = paste0(path_to_sims, 'hist_logavg.tex'))
hist(
  powerTransform(sims_avg[[1]] + sims_avg[[2]] + sims_avg[[3]] + sims_avg[[4]] +
                   sims_avg[[5]], 0),
  col = adjustcolor(emission_col[3], alpha.f = 0.7),
  xlab = "Ethylene concentration ($log(ppb)$)",
  main = ''
)
hist(
  powerTransform(sims_avg[[6]] + sims_avg[[7]] + sims_avg[[8]] + sims_avg[[9]] +
                   sims_avg[[10]], 0),
  col = adjustcolor(emission_col[2], alpha.f = 0.7),
  add = T
)
hist(
  powerTransform(sims_avg[[11]] + sims_avg[[12]] + sims_avg[[13]] + sims_avg[[14]] +
                   sims_avg[[15]], 0),
  col = adjustcolor(emission_col[1], alpha.f = 0.7),
  add = T
)
dev.off()

# Make regular grid points (1,4,16) ----
s_1p <-
  c(mean(main_volume[[1]]), mean(main_volume[[2]]), mean(main_volume[[3]]))
s_1p <- round(s_1p)

s_4p <- list(
  c((s_1p[1] - min(main_volume[[1]])) / 2 + s_1p[1],
    (s_1p[2] - min(main_volume[[2]])) / 2 + s_1p[2],
    s_1p[3]),
  c((s_1p[1] - min(main_volume[[1]])) / 2 + s_1p[1],-(s_1p[2] -
                                                        min(main_volume[[2]])) / 2 + s_1p[2],
    s_1p[3]),
  c(-(s_1p[1] - min(main_volume[[1]])) / 2 + s_1p[1],-(s_1p[2] -
                                                         min(main_volume[[2]])) / 2 + s_1p[2],
    s_1p[3]),
  c(-(s_1p[1] - min(main_volume[[1]])) / 2 + s_1p[1],
    (s_1p[2] - min(main_volume[[2]])) / 2 + s_1p[2],
    s_1p[3])
)
s_4p <- lapply(s_4p, round)

s_16p <- list(
  c((s_4p[[1]][1] - s_1p[1]) / 2 + s_4p[[1]][1],
    (s_4p[[1]][2] - s_1p[2]) / 2 + s_4p[[1]][2],
    s_1p[3]),
  c((s_4p[[1]][1] - s_1p[1]) / 2 + s_4p[[1]][1],-(s_4p[[1]][2] -
                                                    s_1p[2]) / 2 + s_4p[[1]][2],
    s_1p[3]),
  c(-(s_4p[[1]][1] - s_1p[1]) / 2 + s_4p[[1]][1],-(s_4p[[1]][2] -
                                                     s_1p[2]) / 2 + s_4p[[1]][2],
    s_1p[3]),
  c(-(s_4p[[1]][1] - s_1p[1]) / 2 + s_4p[[1]][1],
    (s_4p[[1]][2] - s_1p[2]) / 2 + s_4p[[1]][2],
    s_1p[3]),
  ##
  c((s_4p[[2]][1] - s_1p[1]) / 2 + s_4p[[2]][1],
    (s_4p[[2]][2] - s_1p[2]) / 2 + s_4p[[2]][2],
    s_1p[3]),
  c((s_4p[[2]][1] - s_1p[1]) / 2 + s_4p[[2]][1],-(s_4p[[2]][2] -
                                                    s_1p[2]) / 2 + s_4p[[2]][2],
    s_1p[3]),
  c(-(s_4p[[2]][1] - s_1p[1]) / 2 + s_4p[[2]][1],-(s_4p[[2]][2] -
                                                     s_1p[2]) / 2 + s_4p[[2]][2],
    s_1p[3]),
  c(-(s_4p[[2]][1] - s_1p[1]) / 2 + s_4p[[2]][1],
    (s_4p[[2]][2] - s_1p[2]) / 2 + s_4p[[2]][2],
    s_1p[3]),
  ##
  c((s_4p[[3]][1] - s_1p[1]) / 2 + s_4p[[3]][1],
    (s_4p[[3]][2] - s_1p[2]) / 2 + s_4p[[3]][2],
    s_1p[3]),
  c((s_4p[[3]][1] - s_1p[1]) / 2 + s_4p[[3]][1],-(s_4p[[3]][2] -
                                                    s_1p[2]) / 2 + s_4p[[3]][2],
    s_1p[3]),
  c(-(s_4p[[3]][1] - s_1p[1]) / 2 + s_4p[[3]][1],-(s_4p[[3]][2] -
                                                     s_1p[2]) / 2 + s_4p[[3]][2],
    s_1p[3]),
  c(-(s_4p[[3]][1] - s_1p[1]) / 2 + s_4p[[3]][1],
    (s_4p[[3]][2] - s_1p[2]) / 2 + s_4p[[3]][2],
    s_1p[3]),
  ##
  c((s_4p[[4]][1] - s_1p[1]) / 2 + s_4p[[4]][1],
    (s_4p[[4]][2] - s_1p[2]) / 2 + s_4p[[4]][2],
    s_1p[3]),
  c((s_4p[[4]][1] - s_1p[1]) / 2 + s_4p[[4]][1],-(s_4p[[4]][2] -
                                                    s_1p[2]) / 2 + s_4p[[4]][2],
    s_1p[3]),
  c(-(s_4p[[4]][1] - s_1p[1]) / 2 + s_4p[[4]][1],-(s_4p[[4]][2] -
                                                     s_1p[2]) / 2 + s_4p[[4]][2],
    s_1p[3]),
  c(-(s_4p[[4]][1] - s_1p[1]) / 2 + s_4p[[4]][1],
    (s_4p[[4]][2] - s_1p[2]) / 2 + s_4p[[4]][2],
    s_1p[3])
)
s_16p <- lapply(s_16p, round)

# plot the points in the orchard
tikz(file = paste0(path_to_sims, 'points_to_sample.tex'))
plot(
  s_1p[1],
  s_1p[2],
  xlim = range(main_volume[[1]]),
  ylim = range(main_volume[[2]]),
  pch = 1,
  xlab = '$x$ ($cm$)',
  ylab = '$y$ ($cm$)'
)
points(s_4p[[1]][1], s_4p[[1]][2], pch = 2)
points(s_4p[[2]][1], s_4p[[2]][2], pch = 2)
points(s_4p[[3]][1], s_4p[[3]][2], pch = 2)
points(s_4p[[4]][1], s_4p[[4]][2], pch = 2)
points(s_4p[[4]][1], s_4p[[4]][2], pch = 2)
for (i in 1:16) {
  points(s_16p[[i]][1], s_16p[[i]][2], pch = 4)
}
points(coords$x * 10, coords$y * 10, pch = 16, cex = 2)
# Main volume
polygon(
  c(25, 174, 174, 25),
  c(130, 130, 25, 25),
  border = NA,
  col = rgb(0, 18, 242, 55, maxColorValue = 255)
)
# In-rows
polygon(
  c((50 - 4), (50 + 4), (50 + 4), (50 - 4)),
  c((50 - 4), (50 - 4), (105 + 4), (105 + 4)),
  border = NA,
  col = rgb(0, 231, 100, 65, maxColorValue = 255)
)
polygon(
  c((100 - 4), (100 + 4), (100 + 4), (100 - 4)),
  c((50 - 4), (50 - 4), (105 + 4), (105 + 4)),
  border = NA,
  col = rgb(0, 231, 100, 65, maxColorValue = 255)
)
polygon(
  c((150 - 4), (150 + 4), (150 + 4), (150 - 4)),
  c((50 - 4), (50 - 4), (105 + 4), (105 + 4)),
  border = NA,
  col = rgb(0, 231, 100, 65, maxColorValue = 255)
)

# In between rows
polygon(
  c((50 + 5), (100 - 5), (100 - 5), (50 + 5)),
  c((50 - 4), (50 - 4), (105 + 4), (105 + 4)),
  border = NA,
  col = rgb(255, 255, 0, 65, maxColorValue = 255)
)
polygon(
  c((100 + 5), (150 - 5), (150 - 5), (100 + 5)),
  c((50 - 4), (50 - 4), (105 + 4), (105 + 4)),
  border = NA,
  col = rgb(255, 255, 0, 65, maxColorValue = 255)
)
legend('topleft',
       zones[2:4],
       fill = c(
         rgb(0, 18, 242, 55, maxColorValue = 255),
         rgb(0, 231, 100, 65, maxColorValue = 255),
         rgb(255, 255, 0, 65, maxColorValue = 255)
       ),
       bty = 'n')
dev.off()

# Get the measurement values
measures_s_1p <- list()
measures_s_4p <- list(list(), list(), list(), list())
measures_s_16p <-
  list(
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list()
  )

for (s in sims) {
  # 1 point
  measures_s_1p <-
    c(measures_s_1p, list(s[s_1p[1], s_1p[2], s_1p[3], ]))
  
  # 4 points
  for (i in 1:4) {
    measures_s_4p[[i]] <-
      c(measures_s_4p[[i]], list(s[s_4p[[i]][1], s_4p[[i]][2], s_4p[[i]][3], ]))
  }
  
  # 16 points
  for (i in 1:16) {
    measures_s_16p[[i]] <-
      c(measures_s_16p[[i]], list(s[s_16p[[i]][1], s_16p[[i]][2], s_16p[[i]][3], ]))
  }
}

rm(s, i)

measures_s_4p_na <- measures_s_1p
measures_s_4p_na <-
  rapply(measures_s_4p_na, function(x)
    ifelse(x != 0, 0, x), how = "replace")

measures_s_16p_na <- measures_s_1p
measures_s_16p_na <-
  rapply(measures_s_16p_na, function(x)
    ifelse(x != 0, 0, x), how = "replace")


for (p in 1:4) {
  for (s in 1:15) {
    for (t in 1:length(measures_s_1p[[s]])) {
      measures_s_4p_na[[s]][[t]] <-
        measures_s_4p_na[[s]][[t]] + as.numeric(measures_s_4p[[p]][[s]][[t]])
    }
  }
}
measures_s_4p_na <- lapply(measures_s_4p_na, function(x)
  (x / 4))

for (p in 1:16) {
  for (s in 1:15) {
    for (t in 1:length(measures_s_1p[[s]])) {
      measures_s_16p_na[[s]][[t]] <-
        measures_s_16p_na[[s]][[t]] + as.numeric(measures_s_16p[[p]][[s]][[t]])
    }
  }
}
measures_s_16p_na <-
  lapply(measures_s_16p_na, function(x)
    (x / 16))

# Calculate Z-score

for (s in 1:15) {
  for (t in 1:length(measures_s_1p[[s]])) {
    measures_s_1p_z[[s]][[t]] <-
      z.test_s(
        measures_s_1p[[s]][[t]],
        mu = sim_avg_main[[s]][[t]],
        sd = sim_std_main[[s]][[t]]
      )
  }
}

for (s in 1:15) {
  for (t in 1:length(measures_s_4p[[s]])) {
    measures_s_4p_z[[s]][[t]] <-
      z.test_s(
        measures_s_1p[[s]][[t]],
        mu = sim_avg_main[[s]][[t]],
        sd = sim_std_main[[s]][[t]]
      )
  }
}


# Measures 1 p ----
tikz(file = paste0(path_to_sims, 'RegularGrid1p.tex'))
plot(
  timesteps[1:length(measures_s_1p[[1]])],
  measures_s_1p[[1]],
  ylim = range(measures_s_1p, na.rm = T),
  xlim = range(timesteps),
  col = emission_col[3],
  pch = wind_pch[1],
  type = 'b',
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = '$n=1$',
  lwd = 3
)
for (i in 2:15) {
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
  points(
      timesteps[1:length(measures_s_1p[[i]])],
      measures_s_1p[[i]],
      col = p_t,
      pch = p_ch,
      type = 'b',
      lwd = 3
    )
}
dev.off()

# Measure 4 p ----
tikz(file = paste0(path_to_sims, 'RegularGrid4p.tex'))
plot(
  timesteps[1:length(measures_s_4p_na[[1]])],
  measures_s_4p_na[[1]],
  ylim = range(measures_s_4p_na, na.rm = T),
  xlim = range(timesteps),
  col = emission_col[3],
  pch = wind_pch[1],
  type = 'b',
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = '$n=4$',
  lwd = 3
)
for (i in 2:15) {
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
  points(
    timesteps[1:length(measures_s_4p_na[[i]])],
    measures_s_4p_na[[i]],
    col = p_t,
    pch = p_ch,
    type = 'b',
    lwd = 3
  )
}
dev.off()

# Measures 16 p ----
tikz(file = paste0(path_to_sims, 'RegularGrid16p.tex'))
plot(
  timesteps[1:length(measures_s_16p_na[[1]])],
  measures_s_16p_na[[1]],
  ylim = range(measures_s_16p_na, na.rm = T),
  xlim = range(timesteps),
  col = emission_col[3],
  pch = wind_pch[1],
  type = 'b',
  xlab = 'Time ($s$)',
  ylab = 'Ethylene concentration ($ppb$)',
  main = '$n=16$',
  lwd = 3
)
for (i in 2:15) {
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
    points(
      timesteps[1:length(measures_s_16p_na[[i]])],
      measures_s_16p_na[[i]],
      col = p_t,
      pch = p_ch,
      type = 'b',
      lwd = 3
    )
}
dev.off()



regular_grid_error <- data.frame(table_avg[4], table_avg[5], sapply(measures_s_1p, mean), (sapply(measures_s_1p, mean)-table_avg[4])/table_avg[4]*100, sapply(measures_s_4p_na, mean),(sapply(measures_s_4p_na, mean)-table_avg[4])/table_avg[4]*100, sapply(measures_s_16p_na, mean), (sapply(measures_s_16p_na, mean)-table_avg[4])/table_avg[4]*100)

regular_grid_error <- rbind(regular_grid_error,apply(abs(regular_grid_error),MARGIN = 2, mean))

print(
  xtable(
    regular_grid_error,
    type = "latex",
    digits = 3,
    label = 'tbl:regular_grid'
  ),
  file = paste0(path_to_sims, "RegularGrid.tex")
)                                  


# Scatter plot of average emission/average concentration ----
tikz(file = paste0(path_to_sims, 'Model', zones_files[1], '.tex'),
     height = 3.5)

y <- unlist(table_avg$Environment)
x <-
  c(rep(mean(emission_c * 1000), 5), rep(mean(emission_entc * 1000), 5), rep(mean(emission_prec *
                                                                                    1000), 5))
exp_model <- lm(log(y) ~ x)
pred_exp <- exp(predict(exp_model, list(x = 10:140)))
lin_model <- lm(y ~ x)

plot(
  x,
  y,
  pch = rep(c(1, 2, 2, 3, 3), 3),
  col = c(
    rep(emission_col[3], 5),
    rep(emission_col[2], 5),
    rep(emission_col[1], 5)
  ),
  ylab = 'Average ethylene concentration ($ppb$)',
  xlab = 'Average emission rate ($nLs^{-1}$)',
  main = zones[1]
)
abline(lin_model, lty = 2)
lines(10:140, pred_exp, lty = 3)
legend(
  "topleft",
  bty = "n",
  lty = c(2, NA, 3, NA),
  legend = c(
    'Linear model',
    paste("$R^2=$", format(
      summary(lin_model)$adj.r.squared, digits = 4
    )),
    'Exponential model',
    paste("$R^2=$", format(
      summary(exp_model)$adj.r.squared, digits = 4
    ))
  )
)
dev.off()

tikz(file = paste0(path_to_sims, 'Model', zones_files[2], '.tex'),
     height = 3.5)

y <- unlist(table_avg$`Main volume`)
x <-
  c(rep(mean(emission_c * 1000), 5), rep(mean(emission_entc * 1000), 5), rep(mean(emission_prec *
                                                                                    1000), 5))
exp_model <- lm(log(y) ~ x)
pred_exp <- exp(predict(exp_model, list(x = 10:140)))
lin_model <- lm(y ~ x)

plot(
  x,
  y,
  pch = rep(c(1, 2, 2, 3, 3), 3),
  col = c(
    rep(emission_col[3], 5),
    rep(emission_col[2], 5),
    rep(emission_col[1], 5)
  ),
  ylab = 'Average ethylene concentration ($ppb$)',
  xlab = 'Average emission rate ($nLs^{-1}$)',
  main = zones[2]
)
abline(lin_model, lty = 2)
lines(10:140, pred_exp, lty = 3)
legend(
  "topleft",
  bty = "n",
  lty = c(2, NA, 3, NA),
  legend = c(
    'Linear model',
    paste("$R^2=$", format(
      summary(lin_model)$adj.r.squared, digits = 4
    )),
    'Exponential model',
    paste("$R^2=$", format(
      summary(exp_model)$adj.r.squared, digits = 4
    ))
  )
)
dev.off()

tikz(file = paste0(path_to_sims, 'Model', zones_files[3], '.tex'),
     height = 3.5)

y <- unlist(table_avg$`In rows`)
x <-
  c(rep(mean(emission_c * 1000), 5), rep(mean(emission_entc * 1000), 5), rep(mean(emission_prec *
                                                                                    1000), 5))
exp_model <- lm(log(y) ~ x)
pred_exp <- exp(predict(exp_model, list(x = 10:140)))
lin_model <- lm(y ~ x)

plot(
  x,
  y,
  pch = rep(c(1, 2, 2, 3, 3), 3),
  col = c(
    rep(emission_col[3], 5),
    rep(emission_col[2], 5),
    rep(emission_col[1], 5)
  ),
  ylab = 'Average ethylene concentration ($ppb$)',
  xlab = 'Average emission rate ($nLs^{-1}$)',
  main = zones[3]
)
abline(lin_model, lty = 2)
lines(10:140, pred_exp, lty = 3)
legend(
  "topleft",
  bty = "n",
  lty = c(2, NA, 3, NA),
  legend = c(
    'Linear model',
    paste("$R^2=$", format(
      summary(lin_model)$adj.r.squared, digits = 4
    )),
    'Exponential model',
    paste("$R^2=$", format(
      summary(exp_model)$adj.r.squared, digits = 4
    ))
  )
)
dev.off()

tikz(file = paste0(path_to_sims, 'Model', zones_files[4], '.tex'),
     height = 3.5)

y <- unlist(table_avg$`In-between rows`)
x <-
  c(rep(mean(emission_c * 1000), 5), rep(mean(emission_entc * 1000), 5), rep(mean(emission_prec *
                                                                                    1000), 5))
exp_model <- lm(log(y) ~ x)
pred_exp <- exp(predict(exp_model, list(x = 10:140)))
lin_model <- lm(y ~ x)

plot(
  x,
  y,
  pch = rep(c(1, 2, 2, 3, 3), 3),
  col = c(
    rep(emission_col[3], 5),
    rep(emission_col[2], 5),
    rep(emission_col[1], 5)
  ),
  ylab = 'Average ethylene concentration ($ppb$)',
  xlab = 'Average emission rate ($nLs^{-1}$)',
  main = zones[4]
)
abline(lin_model, lty = 2)
lines(10:140, pred_exp, lty = 3)
legend(
  "topleft",
  bty = "n",
  lty = c(2, NA, 3, NA),
  legend = c(
    'Linear model',
    paste("$R^2=$", format(
      summary(lin_model)$adj.r.squared, digits = 4
    )),
    'Exponential model',
    paste("$R^2=$", format(
      summary(exp_model)$adj.r.squared, digits = 4
    ))
  )
)
dev.off()

rm(x, y, exp_model, pred_exp, lin_model)

# Plot evolution of emission across stages ----

emission_all <-
  data.frame(emission_prec, emission_entc, emission_c) * 1000
tikz(file = paste0(path_to_sims, 'EmissionEvolution.tex'),
     height = 4)
plot(
  c(1, 2, 3),
  emission_all[1, ],
  xlab = '$E_i$',
  ylab = 'Emission rate ($nLs^{-1}$)',
  xaxt = "n",
  ylim = range(emission_all),
  type = 'b',
  pch = 3,
  lty = 2
)
axis(1, at = c(1, 2, 3))
for (i in 2:18) {
  points(
    c(1, 2, 3),
    emission_all[i, ],
    type = 'b',
    pch = 3,
    lty = 2
  )
}

x <- c(rep(1, 18), rep(2, 18), rep(3, 18))
y <- log(c(emission_prec, emission_entc, emission_c) * 1000)
exp_model <- lm(y ~ x)
pred_exp <- exp(predict(exp_model, list(x = seq(0, 4, 0.1))))
lines(seq(0, 4, 0.1), pred_exp, col = 'orange', lwd = 3)
legend(
  "topleft",
  bty = "n",
  lty = c(1, NA),
  legend = 'Exponential trend line',
  col = 'orange',
  lwd = 3
)
dev.off()
rm(x, y, exp_model, pred_exp)

# Plot drone sims maps ----

for (n in 1:6) {
  xy_plane <- apply(sims_drone[[n]], c(1, 2), max)
  xz_plane <- apply(sims_drone[[n]], c(1, 3), max)
  yz_plane <- apply(sims_drone[[n]], c(2, 3), max)
  
  print(paste('range XY', range(xy_plane)[1], '-', range(xy_plane)[2]))
  if (n %in% c(1, 4)) {
    stage <- 3
  }
  if (n %in% c(2, 5)) {
    stage <- 2
  }
  if (n %in% c(3, 6)) {
    stage <- 1
  }
  
  if (n %in% c(1:3)) {
    point <- c(s_1p[1:2], 40)
  }
  if (n %in% c(4:6)) {
    point <- c(125, s_1p[2], 20)
  }
  
  pdf(paste0(path_to_sims, names(sims_drone)[[n]], '_XY.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image(
    X,
    Y,
    xy_plane,
    col = brewer.pal(10, 'BrBG'),
    xlim = c(25, 174),
    ylim = c(25, 141),
    ann = FALSE
  )
  image(X,
        Y,
        trees_xy,
        col = c(
          adjustcolor("white", alpha.f = 0),
          adjustcolor("black", alpha.f = 0.7)
        ),
        add = T)
  points(
    coords$e_x * 10,
    coords$e_y * 10,
    cex = 2,
    pch = 23,
    col = 'black',
    bg = colorRampPalette(c("white",
                            "red"))(4)[ii[[stage]]]
  )
  points(point[1],
         point[2],
         pch = 17,
         col = 'magenta',
         cex = 2)
  dev.off()
  
  pdf(paste0(path_to_sims, names(sims_drone)[[n]], '_XZ.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image(
    X,
    Z,
    xz_plane,
    col = brewer.pal(10, 'BrBG'),
    xlim = c(25, 174),
    ylim = c(1, 60),
    ann = FALSE
  )
  image(X,
        Z,
        trees_xz,
        col = c(
          adjustcolor("white", alpha.f = 0),
          adjustcolor("black", alpha.f = 0.7)
        ),
        add = T)
  points(
    coords$e_x * 10,
    coords$z * 10,
    cex = 2,
    pch = 23,
    col = 'black',
    bg = colorRampPalette(c("white",
                            "red"))(4)[ii[[stage]]]
  )
  points(point[1],
         point[3],
         pch = 17,
         col = 'magenta',
         cex = 2)
  dev.off()
  
  pdf(paste0(path_to_sims, names(sims_drone)[[n]], '_YZ.pdf'))
  #plot(coords$x*10,coords$y*10,xlim=c(25,174),ylim=c(25,141), ann=FALSE)
  image.plot(
    Y,
    Z,
    yz_plane,
    col = brewer.pal(10, 'BrBG'),
    xlim = c(25, 141),
    ylim = c(1, 60),
    ann = FALSE,
    legend.lab = 'Ethylene concentration (ppb)',
    legend.line = 2.5
  )
  image(Y,
        Z,
        trees_yz,
        col = c(
          adjustcolor("white", alpha.f = 0),
          adjustcolor("black", alpha.f = 0.7)
        ),
        add = T)
  points(
    coords$e_y * 10,
    coords$z * 10,
    cex = 2,
    pch = 23,
    col = 'black',
    bg = colorRampPalette(c("white",
                            "red"))(4)[ii[[stage]]]
  )
  points(point[2],
         point[3],
         pch = 17,
         col = 'magenta',
         cex = 2)
  dev.off()
}

rm(s, n, point)

tikz(file = paste0(path_to_sims, 'legend_drone_c_XY.tex'),
     height = 0.7)
par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0)
)
plot(
  0,
  0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n"
)
legend(
  'top',
  c(levels(ii_c), 'Drone'),
  pch = c(rep(23, 4), 17),
  pt.bg = c(colorRampPalette(c("white", "red"))(4), 'magenta'),
  xpd = TRUE,
  horiz = TRUE,
  bty = "n",
  title = '$E_3$ ($nLs^{-1}$)',
  col = c(rep('black', 4), 'magenta')
)
dev.off()

tikz(file = paste0(path_to_sims, 'legend_drone_entc_XY.tex'),
     height = 0.7)
par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0)
)
plot(
  0,
  0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n"
)
legend(
  'top',
  c(levels(ii_entc), 'Drone'),
  pch = c(rep(23, 4), 17),
  pt.bg = c(colorRampPalette(c("white", "red"))(4), 'magenta'),
  xpd = TRUE,
  horiz = TRUE,
  bty = "n",
  title = '$E_2$ ($nLs^{-1}$)',
  col = c(rep('black', 4), 'magenta')
)
dev.off()

tikz(file = paste0(path_to_sims, 'legend_drone_prec_XY.tex'),
     height = 0.7)
par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0)
)
plot(
  0,
  0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n"
)
legend(
  'top',
  c(levels(ii_prec), 'Drone'),
  pch = c(rep(23, 4), 17),
  pt.bg = c(colorRampPalette(c("white", "red"))(4), 'magenta'),
  xpd = TRUE,
  horiz = TRUE,
  bty = "n",
  title = '$E_1$ ($nLs^{-1}$)',
  col = c(rep('black', 4), 'magenta')
)
dev.off()

# Plot basic stats drone ----
# For position around the drone, average concentration

point1 <- c(s_1p[1:2], 40)
point1_zone <-
  list(((point1[1] - 2):(point1[1] + 2)), ((point1[2] - 2):(point1[2] + 2)), ((point1[3] -
                                                                                 2):(point1[3] + 2)))
point2 <- c(125, s_1p[2], 20)
point2_zone <-
  list(((point2[1] - 2):(point2[1] + 2)), ((point2[2] - 2):(point2[2] + 2)), ((point2[3] -
                                                                                 2):(point2[3] + 2)))

sims_drone_avg_pos1 <-
  lapply(sims_drone[1:3], function(x)
    apply(x[point1_zone[[1]], point1_zone[[2]], point1_zone[[3]], ], MARGIN = c(4), mean, na.rm =
            T))

sims_drone_avg_pos2 <-
  lapply(sims_drone[4:6], function(x)
    apply(x[point2_zone[[1]], point2_zone[[2]], point2_zone[[3]], ], MARGIN = c(4), mean, na.rm =
            T))

z <- 1
for (s in list(sims_drone_avg_pos1, sims_drone_avg_pos2)) {
  tikz(file = paste0(path_to_sims, 'DroneAvg_Pos', z, '.tex'),
       height = 3.5)
  plot(
    timesteps[1:length(s[[1]])],
    s[[1]],
    ylim = range(s, na.rm = T),
    xlim = range(timesteps),
    col = emission_col[3],
    type = 'b',
    pch = 2,
    xlab = 'Time ($s$)',
    ylab = 'Average ethylene concentration ($ppb$)',
    main = paste('Position', z),
    lwd = 3
  )
  z = z + 1
  for (i in 2:length(s)) {
    if (i %in% 1) {
      p_t <- emission_col[3]
    }
    if (i %in% 2) {
      p_t <- emission_col[2]
    }
    if (i %in% 3) {
      p_t <- emission_col[1]
    }
    points(
      timesteps[1:length(s[[i]])],
      s[[i]],
      col = p_t,
      pch = 2,
      type = 'b',
      lwd = 3
    )
  }
  dev.off()
}
rm(z)

# Plot wind speed to average ethylene in zones ----

tikz(file = paste0(path_to_sims, 'WindSpeedEthylene.tex'),
     height = 3.5)
r <- range(c(
  log(c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])))),
       log(c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])))),
             log(c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))),
  log(c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)]))))
             ))

plot(c(0,2,5),
     log(c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_all[c(4,5,9,10,14,15)])))), 
     pch=21, ylab = '$log$ Ethylene concentration ($ppb$)',xlab='Wind speed ($ms^{-1}$)', ylim=range(r), bg='black')
     
points(c(0,2,5),
       log(c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_main[c(4,5,9,10,14,15)])))), 
       bg=zones_col[1], pch=21)
points(c(0,2,5),
       log(c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))), 
       bg=zones_col[2], pch=21)
points(c(0,2,5),
       log(c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)])))), 
bg=zones_col[3], pch=21)

trend_all <- line(c(0,2,5),log(c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_all[c(4,5,9,10,14,15)])))))
trend_main <- line(c(0,2,5),log(c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_main[c(4,5,9,10,14,15)])))))
trend_in <- line(c(0,2,5),log(c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))))
trend_inbet <- line(c(0,2,5),log(c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)])))))

abline(trend_all)
abline(trend_main,lwd=3, col=zones_col[1])
abline(trend_in,lwd=3, col=zones_col[2])
abline(trend_inbet,lwd=3, col=zones_col[3])

legend('top',paste(zones,'$a=$',round(c(trend_all$coefficients[1],trend_main$coefficients[1],trend_in$coefficients[1],trend_inbet$coefficients[1]),1),'$b=$', round(c(trend_all$coefficients[2],trend_main$coefficients[2],trend_in$coefficients[2],trend_inbet$coefficients[2]),1)),fill=c('black',zones_col), bty = 'n', xjust=0)
dev.off()

tikz(file = paste0(path_to_sims, 'WindSpeedEthyleneNotLog.tex'),
     height = 3.5)
r <- range(c(
  (c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])))),
  (c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])))),
  (c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))),
  (c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)]))))
))

plot(c(0,2,5),
     (c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_all[c(4,5,9,10,14,15)])))), 
     pch=21, ylab = 'Ethylene concentration ($ppb$)',xlab='Wind speed ($ms^{-1}$)', ylim=range(r), bg='black')

points(c(0,2,5),
       (c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_main[c(4,5,9,10,14,15)])))), 
       bg=zones_col[1], pch=21)
points(c(0,2,5),
       (c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))), 
       bg=zones_col[2], pch=21)
points(c(0,2,5),
       (c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)])))), 
       bg=zones_col[3], pch=21)

trend_all <- line(c(0,2,5),(c(mean(unlist(sim_avg_all[c(1,6,11)])),mean(unlist(sim_avg_all[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_all[c(4,5,9,10,14,15)])))))
trend_main <- line(c(0,2,5),(c(mean(unlist(sim_avg_main[c(1,6,11)])),mean(unlist(sim_avg_main[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_main[c(4,5,9,10,14,15)])))))
trend_in <- line(c(0,2,5),(c(mean(unlist(sim_avg_in[c(1,6,11)])),mean(unlist(sim_avg_in[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_in[c(4,5,9,10,14,15)])))))
trend_inbet <- line(c(0,2,5),(c(mean(unlist(sim_avg_inbet[c(1,6,11)])),mean(unlist(sim_avg_inbet[c(2,3,7,8,12,13)])),mean(unlist(sim_avg_inbet[c(4,5,9,10,14,15)])))))

abline(trend_all)
abline(trend_main,lwd=3, col=zones_col[1])
abline(trend_in,lwd=3, col=zones_col[2])
abline(trend_inbet,lwd=3, col=zones_col[3])

legend('top',paste(zones,'$a=$',round(c(trend_all$coefficients[1],trend_main$coefficients[1],trend_in$coefficients[1],trend_inbet$coefficients[1]),1),'$b=$', round(c(trend_all$coefficients[2],trend_main$coefficients[2],trend_in$coefficients[2],trend_inbet$coefficients[2]),1)),fill=c('black',zones_col), bty = 'n', xjust=0)
dev.off()


# Height histogram -----
height_dist <- lapply(sims, FUN = function(y) apply(y,MARGIN = c(3), FUN = function(x) sum(!is.na(x))/length(x))*100)

x_dist <- lapply(sims, FUN = function(y) apply(y,MARGIN = c(1), FUN = function(x) sum(!is.na(x))/length(x))*100)

y_dist <- lapply(sims, FUN = function(y) apply(y,MARGIN = c(2), FUN = function(x) sum(!is.na(x))/length(x))*100)

tikz(file = paste0(
  path_to_sims,
  'HeightDist.tex'
))
plot(height_dist[[1]], (1:59)*0.1, type='b', xlab='Occupied cells (\\%)', ylab='$z$ ($m$)', col=emission_col[3], pch=wind_pch[1], lwd = 3, xlim = c(0,50), ylim = c(0.1,6))
for(i in 2:15){
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
  points(height_dist[[i]], (1:59)*0.1, type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((height_dist[[1]]+height_dist[[6]]+height_dist[[11]])/3, (1:59)*0.1, lwd=3)
lines((height_dist[[2]]+height_dist[[4]]+height_dist[[7]]+height_dist[[9]]+height_dist[[12]]+height_dist[[14]])/6, (1:59)*0.1, lwd=3, lty=2)
lines((height_dist[[3]]+height_dist[[5]]+height_dist[[8]]+height_dist[[10]]+height_dist[[13]]+height_dist[[15]])/6, (1:59)*0.1, lwd=3, lty=3)
abline(v=5,col='red',lwd=3)
abline(h=3, col='green', lwd=3)
legend('topright',c('No wind','$\\vec{x}$','$\\vec{y}$','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,3,1,1))
dev.off()

tikz(file = paste0(
  path_to_sims,
  'XDist.tex'
))
plot( (1:199)*0.1,x_dist[[1]], type='b', ylab='Occupied cells (\\%)', xlab='$x$ ($m$)', col=emission_col[3], pch=wind_pch[1], lwd = 3, ylim = c(0,60), xlim = c(0.1,20))
for(i in 2:15){
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
  points((1:199)*0.1,x_dist[[i]], type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((1:199)*0.1,(x_dist[[1]]+x_dist[[6]]+x_dist[[11]])/3, lwd=3)
lines((1:199)*0.1, (x_dist[[2]]+x_dist[[4]]+x_dist[[7]]+x_dist[[9]]+x_dist[[12]]+x_dist[[14]])/6, lwd=3, lty=2)
lines((1:199)*0.1,(x_dist[[3]]+x_dist[[5]]+x_dist[[8]]+x_dist[[10]]+x_dist[[13]]+x_dist[[15]])/6, lwd=3, lty=3)
#abline(v=5,col='red',lwd=3)
#abline(h=3, col='green', lwd=3)
#legend('topright',c('No wind','$\\vec{x}$','$\\vec{y}$','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,3,1,1))
dev.off()

tikz(file = paste0(
  path_to_sims,
  'YDist.tex'
))
plot((1:164)*0.1,y_dist[[1]], type='b', ylab='Occupied cells (\\%)', xlab='$y$ ($m$)', col=emission_col[3], pch=wind_pch[1], lwd = 3, ylim = c(0,60), xlim = c(0.1,16.5))
for(i in 2:15){
  if (i %in% 2:5) {
    p_t <- emission_col[3]
  }
  if (i %in% 6:10) {
    p_t <- emission_col[2]
  }
  if (i %in% 11:15) {
    p_t <- emission_col[1]
  }
  
  if (i %in% c(6, 11)) {
    p_ch <- wind_pch[1]
  }
  if (i %in% c(2, 3, 7, 8, 12, 13)) {
    p_ch <- wind_pch[2]
  }
  if (i %in% c(4, 5, 9, 10, 14, 15)) {
    p_ch <- wind_pch[3]
  }
  points((1:164)*0.1,y_dist[[i]], type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((1:164)*0.1,(y_dist[[1]]+y_dist[[6]]+y_dist[[11]])/3, lwd=3)
lines((1:164)*0.1, (y_dist[[2]]+y_dist[[4]]+y_dist[[7]]+y_dist[[9]]+y_dist[[12]]+y_dist[[14]])/6, lwd=3, lty=2)
lines((1:164)*0.1,(y_dist[[3]]+y_dist[[5]]+y_dist[[8]]+y_dist[[10]]+y_dist[[13]]+y_dist[[15]])/6, lwd=3, lty=3)
#abline(v=5,col='red',lwd=3)
#abline(h=3, col='green', lwd=3)
#legend('topright',c('No wind','$\\vec{x}$','$\\vec{y}$','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,3,1,1))
dev.off()

# Height dist For DRONE ----
height_dist <- lapply(sims_drone, FUN = function(y) apply(y,MARGIN = c(3), FUN = function(x) sum(!is.na(x))/length(x))*100)

x_dist <- lapply(sims_drone, FUN = function(y) apply(y,MARGIN = c(1), FUN = function(x) sum(!is.na(x))/length(x))*100)

y_dist <- lapply(sims_drone, FUN = function(y) apply(y,MARGIN = c(2), FUN = function(x) sum(!is.na(x))/length(x))*100)

tikz(file = paste0(
  path_to_sims,
  'DroneHeightDist.tex'
))
plot(height_dist[[1]], (1:59)*0.1, type='b', xlab='Occupied cells (\\%)', ylab='$z$ ($m$)', col=emission_col[1], pch=2, lwd = 3, xlim = c(0,50), ylim = c(0.1,6))
for(i in 2:length(height_dist)){
  if (i %in% c(3,6)) {
    p_t <- emission_col[3]
  }
  if (i %in% c(2,5)) {
    p_t <- emission_col[2]
  }
  if (i %in% 4) {
    p_t <- emission_col[1]
  }
  
  if (i %in% 1:3) {
    p_ch <- wind_pch[1]
  }
  if (i %in% 4:6) {
    p_ch <- wind_pch[2]
  }
  points(height_dist[[i]], (1:59)*0.1, type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((height_dist[[1]]+height_dist[[2]]+height_dist[[3]])/3, (1:59)*0.1, lwd=3)
lines((height_dist[[4]]+height_dist[[5]]+height_dist[[6]])/3, (1:59)*0.1, lwd=3, lty=2)
abline(v=5,col='red',lwd=3)
abline(h=3, col='green', lwd=3)
legend('topright',c('Position 1','Position 2','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,1,1))
dev.off()

tikz(file = paste0(
  path_to_sims,
  'DroneXDist.tex'
))
plot( (1:199)*0.1,x_dist[[1]], type='b', ylab='Occupied cells (\\%)', xlab='$x$ ($m$)', col=emission_col[1], pch=wind_pch[2], lwd = 3, ylim = c(0,60), xlim = c(0.1,20))
for(i in 2:length(height_dist)){
  if (i %in% c(3,6)) {
    p_t <- emission_col[3]
  }
  if (i %in% c(2,5)) {
    p_t <- emission_col[2]
  }
  if (i %in% 4) {
    p_t <- emission_col[1]
  }
  
  if (i %in% 1:3) {
    p_ch <- wind_pch[1]
  }
  if (i %in% 4:6) {
    p_ch <- wind_pch[2]
  }
  points((1:199)*0.1,x_dist[[i]], type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((1:199)*0.1,(x_dist[[1]]+x_dist[[2]]+x_dist[[3]])/3, lwd=3)
lines((1:199)*0.1, (x_dist[[4]]+x_dist[[5]]+x_dist[[6]])/3, lwd=3, lty=2)
#abline(v=5,col='red',lwd=3)
#abline(h=3, col='green', lwd=3)
#legend('topright',c('No wind','$\\vec{x}$','$\\vec{y}$','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,3,1,1))
dev.off()

tikz(file = paste0(
  path_to_sims,
  'DroneYDist.tex'
))
plot((1:164)*0.1,y_dist[[1]], type='b', ylab='Occupied cells (\\%)', xlab='$y$ ($m$)', col=emission_col[1], pch=wind_pch[2], lwd = 3, ylim = c(0,60), xlim = c(0.1,16.5))
for(i in 2:length(height_dist)){
  if (i %in% c(3,6)) {
    p_t <- emission_col[3]
  }
  if (i %in% c(2,5)) {
    p_t <- emission_col[2]
  }
  if (i %in% 4) {
    p_t <- emission_col[1]
  }
  
  if (i %in% 1:3) {
    p_ch <- wind_pch[1]
  }
  if (i %in% 4:6) {
    p_ch <- wind_pch[2]
  }
  points((1:164)*0.1,y_dist[[i]], type='b', col=p_t, pch=p_ch, lwd = 3)
}
# 0 ms
lines((1:164)*0.1,(y_dist[[1]]+y_dist[[2]]+y_dist[[3]])/3, lwd=3)
lines((1:164)*0.1, (y_dist[[4]]+y_dist[[5]]+y_dist[[6]])/3, lwd=3, lty=2)
#abline(v=5,col='red',lwd=3)
#abline(h=3, col='green', lwd=3)
#legend('topright',c('No wind','$\\vec{x}$','$\\vec{y}$','$5\\%$','Tree height'),bty='n', col=c(rep('black',3),'red','green'),lty=c(1,2,3,1,1))
dev.off()

# Supposodely n = 16σ2/W2.

# Sampling in the environment ----
fraction_zones <- c(length(sims[[1]][main_volume[[1]], main_volume[[2]], main_volume[[3]],1]),
length(sims[[1]][in_rows[[1]], in_rows[[2]], in_rows[[3]],1]),
length(sims[[1]][inbetween_rows[[1]], inbetween_rows[[2]], inbetween_rows[[3]],1]))/length(sims[[1]][,,,1])*100


lm_ethe_Env <- list( lm(unlist(table_avg[c(1,6,11),1]) ~ c(3,2,1)),
lm(unlist(table_avg[c(2,7,12),1]) ~ c(3,2,1)),
lm(unlist(table_avg[c(3,8,13),1]) ~ c(3,2,1)),
lm(unlist(table_avg[c(4,9,14),1]) ~ c(3,2,1)),
lm(unlist(table_avg[c(5,10,15),1]) ~ c(3,2,1)))

lm_ethe_Main <- list( lm(unlist(table_avg[c(1,6,11),4]) ~ c(3,2,1)),
                     lm(unlist(table_avg[c(2,7,12),4]) ~ c(3,2,1)),
                     lm(unlist(table_avg[c(3,8,13),4]) ~ c(3,2,1)),
                     lm(unlist(table_avg[c(4,9,14),4]) ~ c(3,2,1)),
                     lm(unlist(table_avg[c(5,10,15),4]) ~ c(3,2,1)))

lm_ethe_In <- list( lm(unlist(table_avg[c(1,6,11),4+3]) ~ c(3,2,1)),
                      lm(unlist(table_avg[c(2,7,12),4+3]) ~ c(3,2,1)),
                      lm(unlist(table_avg[c(3,8,13),4+3]) ~ c(3,2,1)),
                      lm(unlist(table_avg[c(4,9,14),4+3]) ~ c(3,2,1)),
                      lm(unlist(table_avg[c(5,10,15),4+3]) ~ c(3,2,1)))

lm_ethe_Inbet <- list( lm(unlist(table_avg[c(1,6,11),4+6]) ~ c(3,2,1)),
                    lm(unlist(table_avg[c(2,7,12),4+6]) ~ c(3,2,1)),
                    lm(unlist(table_avg[c(3,8,13),4+6]) ~ c(3,2,1)),
                    lm(unlist(table_avg[c(4,9,14),4+6]) ~ c(3,2,1)),
                    lm(unlist(table_avg[c(5,10,15),4+6]) ~ c(3,2,1)))

table_slack <- data.frame(
  rep(sapply(lm_ethe_Env,FUN = function(x) return(x$coefficient[2])),3),
  rep(sapply(lm_ethe_Main,FUN = function(x) return(x$coefficient[2])),3),
  rep(sapply(lm_ethe_In,FUN = function(x) return(x$coefficient[2])),3),
  rep(sapply(lm_ethe_Inbet,FUN = function(x) return(x$coefficient[2])),3),
  row.names=1:15)

colnames(table_slack) <- zones

table_slack[5] <- (table_slack[1]/unlist(table_avg[1]))*100

table_slack[6] <- (table_slack[2]/unlist(table_avg[4]))*100

table_slack[7] <- (table_slack[3]/unlist(table_avg[7]))*100

table_slack[8] <- (table_slack[4]/unlist(table_avg[10]))*100

t_table_avg <- as.data.frame(t(table_avg))

diff_table <- data.frame(
  c((t_table_avg$`6`-t_table_avg$`11`)[c(1,4,7,10)], (t_table_avg$`1`-t_table_avg$`6`)[c(1,4,7,10)]),
  c((t_table_avg$`6`-t_table_avg$`11`)[c(1,4,7,10)]/t_table_avg$`11`[c(1,4,7,10)]*100,((t_table_avg$`1`-t_table_avg$`6`)[c(1,4,7,10)]/t_table_avg$`6`[c(1,4,7,10)])*100),
c((t_table_avg$`7`-t_table_avg$`12`)[c(1,4,7,10)], (t_table_avg$`2`-t_table_avg$`7`)[c(1,4,7,10)]),
c((t_table_avg$`7`-t_table_avg$`12`)[c(1,4,7,10)]/t_table_avg$`12`[c(1,4,7,10)]*100,((t_table_avg$`2`-t_table_avg$`7`)[c(1,4,7,10)]/t_table_avg$`7`[c(1,4,7,10)])*100),
c((t_table_avg$`8`-t_table_avg$`13`)[c(1,4,7,10)], (t_table_avg$`3`-t_table_avg$`8`)[c(1,4,7,10)]),
c((t_table_avg$`8`-t_table_avg$`13`)[c(1,4,7,10)]/t_table_avg$`13`[c(1,4,7,10)]*100,((t_table_avg$`3`-t_table_avg$`8`)[c(1,4,7,10)]/t_table_avg$`8`[c(1,4,7,10)])*100),
c((t_table_avg$`9`-t_table_avg$`14`)[c(1,4,7,10)], (t_table_avg$`4`-t_table_avg$`9`)[c(1,4,7,10)]),
c((t_table_avg$`9`-t_table_avg$`14`)[c(1,4,7,10)]/t_table_avg$`14`[c(1,4,7,10)]*100,((t_table_avg$`4`-t_table_avg$`9`)[c(1,4,7,10)]/t_table_avg$`9`[c(1,4,7,10)])*100),

c((t_table_avg$`10`-t_table_avg$`15`)[c(1,4,7,10)], (t_table_avg$`5`-t_table_avg$`10`)[c(1,4,7,10)]), 
c((t_table_avg$`10`-t_table_avg$`15`)[c(1,4,7,10)]/t_table_avg$`15`[c(1,4,7,10)]*100,((t_table_avg$`5`-t_table_avg$`10`)[c(1,4,7,10)]/t_table_avg$`10`[c(1,4,7,10)])*100),
row.names = c(paste(zones,'E2-E1'), paste(zones,'E3-E2')))
colnames(diff_table) <- rep(c('0ms','2msX','2msY','5msX','5msY'), each=2)

print(
  xtable(
    diff_table,
    type = "latex",
    digits = 2,
    label = 'tbl:diff_table'
  ),
  file = paste0(path_to_sims, "DiffTable.tex")
)

diff_table_rev <- data.frame(
  c((t_table_avg$`11`-t_table_avg$`6`)[c(1,4,7,10)], (t_table_avg$`6`-t_table_avg$`1`)[c(1,4,7,10)]),
  c((t_table_avg$`11`-t_table_avg$`6`)[c(1,4,7,10)]/t_table_avg$`6`[c(1,4,7,10)]*100,((t_table_avg$`6`-t_table_avg$`1`)[c(1,4,7,10)]/t_table_avg$`1`[c(1,4,7,10)])*100),
  c((t_table_avg$`12`-t_table_avg$`7`)[c(1,4,7,10)], (t_table_avg$`7`-t_table_avg$`2`)[c(1,4,7,10)]),
  c((t_table_avg$`12`-t_table_avg$`7`)[c(1,4,7,10)]/t_table_avg$`7`[c(1,4,7,10)]*100,((t_table_avg$`7`-t_table_avg$`2`)[c(1,4,7,10)]/t_table_avg$`2`[c(1,4,7,10)])*100),
  c((t_table_avg$`13`-t_table_avg$`8`)[c(1,4,7,10)], (t_table_avg$`8`-t_table_avg$`3`)[c(1,4,7,10)]),
  c((t_table_avg$`13`-t_table_avg$`8`)[c(1,4,7,10)]/t_table_avg$`8`[c(1,4,7,10)]*100,((t_table_avg$`8`-t_table_avg$`3`)[c(1,4,7,10)]/t_table_avg$`3`[c(1,4,7,10)])*100),
  c((t_table_avg$`14`-t_table_avg$`9`)[c(1,4,7,10)], (t_table_avg$`9`-t_table_avg$`4`)[c(1,4,7,10)]),
  c((t_table_avg$`14`-t_table_avg$`9`)[c(1,4,7,10)]/t_table_avg$`9`[c(1,4,7,10)]*100,((t_table_avg$`9`-t_table_avg$`4`)[c(1,4,7,10)]/t_table_avg$`4`[c(1,4,7,10)])*100),
  
  c((t_table_avg$`15`-t_table_avg$`10`)[c(1,4,7,10)], (t_table_avg$`10`-t_table_avg$`5`)[c(1,4,7,10)]),
  c((t_table_avg$`15`-t_table_avg$`10`)[c(1,4,7,10)]/t_table_avg$`10`[c(1,4,7,10)]*100,((t_table_avg$`10`-t_table_avg$`5`)[c(1,4,7,10)]/t_table_avg$`5`[c(1,4,7,10)])*100),
  row.names = c(paste(zones,'E2-E1'), paste(zones,'E3-E2')))
colnames(diff_table) <- rep(c('0ms','2msX','2msY','5msX','5msY'), each=2)



# Percentage error
# PE = (t * std / mean) * 100
qt((1-0.05/2),df=5)

# Make SEM table
table_SME <- table_avg

table_SME$Main4 <- unlist(table_avg[5])/sqrt(4)
table_SME$Main_PE <- 100*((table_SME$Inrow4*qt((1-0.05/2),df=4-1))/unlist(table_avg[4]))

table_SME$Main16 <- unlist(table_avg[5])/sqrt(16)
table_SME$Main16_PE <- 100*((table_SME$Inrow16*qt((1-0.05/2),df=16-1))/unlist(table_avg[4]))

table_SME$Inrow4 <- unlist(table_avg[8])/sqrt(4)
table_SME$Inrow_PE <- 100*((table_SME$Inrow4*qt((1-0.05/2),df=4-1))/unlist(table_avg[7]))

table_SME$Inrow16 <- unlist(table_avg[8])/sqrt(16)
table_SME$Inrow16_PE <- 100*((table_SME$Inrow16*qt((1-0.05/2),df=16-1))/unlist(table_avg[7]))

table_SME$Inbet4 <- unlist(table_avg[11])/sqrt(4)
table_SME$Inbet_PE <- 100*((table_SME$Inbet4*qt((1-0.05/2),df=4-1))/unlist(table_avg[10]))

table_SME$Inbet16 <- unlist(table_avg[11])/sqrt(16)
table_SME$Inbet16_PE <- 100*((table_SME$Inbet16*qt((1-0.05/2),df=16-1))/unlist(table_avg[10]))

table_SME$Allowed <- rep(c(144,174,180,188,188),3)

table_SME$AllowedHalfwidth <- c(44,
                                    49,
                                    44,
                                    44,
                                    42,
                                    35,
                                    34,
                                    46,
                                    52,
                                    44,
                                    59,
                                    58,
                                    68,
                                    72,
                                    67)
table_SME$AllowedAbs <- rep(c(4.69,0.33,0.40,0.40,0.17),3)


table_SME$EnvN <- ((qt((1-0.05/2),df=30)*unlist(table_avg[2]))/rep(unlist(diff_table[1,]),3))^2
table_SME$MainN <- ((qt((1-0.05/2),df=30)*unlist(table_avg[5]))/rep(unlist(diff_table[2,]),3))^2
table_SME$InrowN <- ((1.97*unlist(table_avg[9]))/(table_SME$Allowed*0.5))^2
table_SME$AllowedHalfwidthN <- ((1.97*unlist(table_avg[9]))/(35))^2
table_SME$InrowNAbs <- ((1.97*unlist(table_avg[8]))/(table_SME$AllowedAbs*0.5))^2
table_SME$InbetN <- ((qt((1-0.05/2),df=30)*unlist(table_avg[11]))/rep(unlist(diff_table[4,]),3))^2

table_SME$ErrorNmaxHalf <- unlist(table_avg[8])/sqrt(round(table_SME$AllowedHalfwidthN))
table_SME$ConfNmaxHalf <- (table_SME$ErrorNmaxHalf*1.97)


table_SME$ErrorNmax <- unlist(table_avg[8])/sqrt(144)
table_SME$PENmax <- 100*(table_SME$ErrorNmax*1.97)/unlist(table_avg[7])
table_SME$ConfNmax <- (table_SME$ErrorNmax*1.97)

table_SME$ErrorNmax50 <- unlist(table_avg[8])/sqrt(188)
table_SME$PENmax50 <- 100*(table_SME$ErrorNmax50*1.97)/unlist(table_avg[7])
table_SME$ConfNmax50 <- (table_SME$ErrorNmax50*1.97)

table_SME$ErrorNmaxAbs <- unlist(table_avg[8])/sqrt(486)
table_SME$PENmaxAbs <- 100*(table_SME$ErrorNmaxAbs*1.97)/unlist(table_avg[7])
table_SME$ConfNmaxAbs <- (table_SME$ErrorNmaxAbs*1.97)

mean(table_SME$InrowNAbs)
mean(table_SME$EnvN)
mean(table_SME$MainN)
mean(table_SME$InrowN)
mean(table_SME$InbetN)

mean((table_SME$Inbet16_PE-table_SME$Inrow16_PE)/table_SME$Inrow16_PE)
mean((table_SME$Main16_PE-table_SME$Inrow16_PE)/table_SME$Inrow16_PE)

mean((table_SME$Inbet_PE-table_SME$Inrow_PE)/table_SME$Inrow_PE)

InrowN <- data.frame(table_SME$Allowed, table_SME$Allowed*0.5,table_avg[7]*table_SME$Allowed*0.5/100,table_SME$InrowN,table_SME$ErrorNmax,table_avg[7],table_SME$ConfNmax,table_SME$PENmax, table_SME$ErrorNmaxHalf, table_SME$ConfNmaxHalf,row.names=1:15)


InrowN50 <- data.frame(table_SME$ErrorNmax50,table_avg[7],table_SME$ConfNmax50,table_SME$PENmax50,  row.names=1:15)

reasonable_samples1 <- data.frame( unlist(table_avg[2])/sqrt(4),
                                  (unlist(table_avg[2])/sqrt(4))*1.97/table_avg[1]*100,
                                  unlist(table_avg[2])/sqrt(16),
                                  (unlist(table_avg[2])/sqrt(16))*1.97/table_avg[1]*100,
                                  unlist(table_avg[2+3])/sqrt(4),
                                  (unlist(table_avg[2+3])/sqrt(4))*1.97/table_avg[1+3]*100,
                                  unlist(table_avg[2+3])/sqrt(16),
                                  (unlist(table_avg[2+3])/sqrt(16))*1.97/table_avg[1+3]*100,
                                  row.names = 1:15)
reasonable_samples1 <- rbind(reasonable_samples1, apply(reasonable_samples1, 2,mean))

reasonable_samples2 <- data.frame(unlist(table_avg[2+6])/sqrt(4),
(unlist(table_avg[2+6])/sqrt(4))*1.97/table_avg[1+6]*100,
unlist(table_avg[2+6])/sqrt(16),
(unlist(table_avg[2+6])/sqrt(16))*1.97/table_avg[1+6]*100,
unlist(table_avg[2+9])/sqrt(4),
(unlist(table_avg[2+9])/sqrt(4))*1.97/table_avg[1+9]*100,
unlist(table_avg[2+9])/sqrt(16),
(unlist(table_avg[2+9])/sqrt(16))*1.97/table_avg[1+9]*100,
row.names = 1:15)

reasonable_samples2 <- rbind(reasonable_samples2, apply(reasonable_samples2, 2,mean))
                                  
print(
  xtable(
    reasonable_samples1,
    type = "latex",
    digits = 2,
    label = 'tbl:reasonable_samples'
  ),
  file = paste0(path_to_sims, "ReasonableSamples1.tex")
)           
print(
  xtable(
    reasonable_samples2,
    type = "latex",
    digits = 2,
    label = 'tbl:reasonable_samples'
  ),
  file = paste0(path_to_sims, "ReasonableSamples2.tex")
) 
                                  

print(
  xtable(
    InrowN,
    type = "latex",
    digits = 2,
    label = 'tbl:inrow_n'
  ),
  file = paste0(path_to_sims, "InrowN.tex")
)

print(
  xtable(
    InrowN50,
    type = "latex",
    digits = 2,
    label = 'tbl:inrow_n50'
  ),
  file = paste0(path_to_sims, "InrowN50.tex"))

# Make density plots ----
wind_scenario <- c('$0ms^{-1}$','$\\vec{x}=2ms^{-1}$', '$\\vec{y}=2ms^{-1}$', '$\\vec{x}=5ms^{-1}$','$\\vec{y}=5ms^{-1}$')
wind_scenario_file <- c('0ms','2msX', '2msY', '5msX','5msY')

for(iter in 0:4){
range_values3 <- seq(0,qnorm(0.999,mean=InrowN$In.rows[1+iter], sd=InrowN$table_SME.ErrorNmax[1+iter]),0.01)
range_values2 <- seq(0,qnorm(0.999,mean=InrowN$In.rows.1[6+iter], sd=InrowN$table_SME.ErrorNmax[6+iter]),0.01)
range_values1 <- seq(0,qnorm(0.999,mean=InrowN$In.rows.1[11+iter], sd=InrowN$table_SME.ErrorNmax[11+iter]),0.01)
e3 <- dnorm(range_values3, mean=InrowN$In.rows.1[1+iter], sd=InrowN$table_SME.ErrorNmax[1+iter])
e2 <- dnorm(range_values2, mean=InrowN$In.rows.1[6+iter], sd=InrowN$table_SME.ErrorNmax[6+iter])
e1 <- dnorm(range_values1, mean=InrowN$In.rows.1[11+iter], sd=InrowN$table_SME.ErrorNmax[11+iter])

#range_values350 <- seq(0,qnorm(0.999,mean=InrowN50$In.rows[1+iter], sd=InrowN50$table_SME.ErrorNmax50[1+iter]),0.01)
#range_values250 <- seq(0,qnorm(0.999,mean=InrowN50$In.rows[6+iter], sd=InrowN50$table_SME.ErrorNmax50[6+iter]),0.01)
#range_values150 <- seq(0,qnorm(0.999,mean=InrowN50$In.rows[11+iter], sd=InrowN50$table_SME.ErrorNmax50[11+iter]),0.01)
#e350 <- dnorm(range_values350, mean=InrowN50$In.rows[1+iter], sd=InrowN50$table_SME.ErrorNmax50[1+iter])
#e250 <- dnorm(range_values250, mean=InrowN50$In.rows[6+iter], sd=InrowN50$table_SME.ErrorNmax50[6+iter])
#e150 <- dnorm(range_values150, mean=InrowN50$In.rows[11+iter], sd=InrowN50$table_SME.ErrorNmax50[11+iter])

tikz(file = paste0(
  path_to_sims,
  'DistSample',wind_scenario_file[iter+1],'.tex'
), height = 4)
plot(range(c(range_values1,range_values2,range_values3)),range(c(e1,e2,e3)), type='n', main=wind_scenario[iter+1], xlab = '$\\bar{x}$ ($ppb$)', ylab = '$F(\\bar{x})$')
lines(range_values1,e1, col=emission_col[1], lwd=3)
lines(range_values2,e2, col=emission_col[2], lwd=3)
lines(range_values3,e3, col=emission_col[3], lwd=3)

#lines(range_values150,e150, col=emission_col[1], lwd=3, lty=2)
#lines(range_values250,e250, col=emission_col[2], lwd=3, lty=2)
#lines(range_values350,e350, col=emission_col[3], lwd=3, lty=2)

polygon(rep(c(InrowN$In.rows.1[11+iter]-InrowN$table_SME.ConfNmax[11+iter], InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter]),each=2),c(-20,20,20,-20), col=adjustcolor(emission_col[1],alpha.f=0.2), border = NA)
polygon(rep(c(InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter], InrowN$In.rows.1[6+iter]+InrowN$table_SME.ConfNmax[6+iter]),each=2),c(-20,20,20,-20), col=adjustcolor(emission_col[2],alpha.f=0.2), border = NA)
polygon(rep(c(InrowN$In.rows.1[1+iter]-InrowN$table_SME.ConfNmax[1+iter], InrowN$In.rows.1[1+iter]+InrowN$table_SME.ConfNmax[1+iter]),each=2),c(-20,20,20,-20), col=adjustcolor(emission_col[3],alpha.f=0.2), border = NA)

abline(v= InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter], col=emission_col[1], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[11+iter]-InrowN$table_SME.ConfNmax[11+iter], col=emission_col[1], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[6+iter]+InrowN$table_SME.ConfNmax[6+iter], col=emission_col[2], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter], col=emission_col[2], lwd=3,lty=2)
abline(v= InrowN$In.rows.1[1+iter]+InrowN$table_SME.ConfNmax[1+iter], col=emission_col[3], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[1+iter]-InrowN$table_SME.ConfNmax[1+iter], col=emission_col[3], lwd=3, lty=2)

#abline(v= InrowN50$In.rows[11+iter]+InrowN50$table_SME.ConfNmax50[11+iter], col=emission_col[1], lwd=3, lty=2)
#abline(v= InrowN50$In.rows[6+iter]+InrowN50$table_SME.ConfNmax50[6+iter], col=emission_col[2], lwd=3, lty=2)

e1_e2 <- 1 - round(pnorm(InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter],mean=InrowN$In.rows.1[11+iter], sd=InrowN$table_SME.ErrorNmax[11+iter]),2)
e2_e1 <- round(pnorm(InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter],mean=InrowN$In.rows.1[6+iter], sd=InrowN$table_SME.ErrorNmax[6+iter]),2)
e3_e2 <- 1 - round(pnorm(InrowN$In.rows.1[1+iter]-InrowN$table_SME.ConfNmax[1+iter],mean=InrowN$In.rows.1[6+iter], sd=InrowN$table_SME.ErrorNmax[6+iter]),2)
e2_e3 <- round(pnorm(InrowN$In.rows.1[6+iter]+InrowN$table_SME.ConfNmax[6+iter],mean=InrowN$In.rows.1[1+iter], sd=InrowN$table_SME.ErrorNmax[1+iter]),2)

#e2_e150 <- round(pnorm(InrowN50$In.rows[11+iter]+InrowN50$table_SME.ConfNmax50[11+iter],mean=InrowN50$In.rows[6+iter], sd=InrowN50$table_SME.ErrorNmax50[6+iter]),2)
#e3_e250 <- round(pnorm(InrowN50$In.rows[6+iter]+InrowN50$table_SME.ConfNmax50[6+iter],mean=InrowN50$In.rows[1+iter], sd=InrowN50$table_SME.ErrorNmax50[1+iter]),2)

legend('top', bty='n',c(paste('$P(E_1>95\\%C.I.\ E_2)=$',e1_e2),paste('$P(E_2<95\\%C.I.\ E_1)=$',e2_e1),paste('$P(E_2>95\\%C.I.\ E_3)=$',e3_e2), paste('$P(E_3<95\\%C.I.\ E_2)=$',e2_e3)), cex = 1.5)
dev.off()
}
# Considering we want a maximum of 50% percent error

table_minN <- data.frame(c(diff_table[3,c(1,3,5,7,9)],diff_table[3,c(1,3,5,7,9)],diff_table[6,c(1,3,5,7,9)]))
                        
mean(InrowN$table_SME.PENmax)
mean(InrowN50$table_SME.PENmax)

# Explain tail issue.
iter <- 0
range_values3 <- seq(0,qnorm(0.999,mean=InrowN$In.rows.1[1], sd=table_SME$Inrow16[1]),0.01)
range_values2 <- seq(0,qnorm(0.999,mean=InrowN$In.rows.1[6], sd=table_SME$Inrow16[6]),0.01)
range_values1 <- seq(0,qnorm(0.999,mean=InrowN$In.rows.1[11+iter], sd=table_SME$Inrow16[11]),0.01)
e3 <- dnorm(range_values3, mean=InrowN$In.rows.1[1+iter], sd=table_SME$Inrow16[1])
e2 <- dnorm(range_values2, mean=InrowN$In.rows.1[6+iter], sd=table_SME$Inrow16[6])
e1 <- dnorm(range_values1, mean=InrowN$In.rows.1[11+iter], sd=table_SME$Inrow16[11])

plot(range(range_values1,range_values2,range_values3),range(e1,e2,e3), type='n', main=paste(wind_scenario[iter], '$n=16$'), xlab = '$\\bar{x}$ ($ppb$)', ylab = '$F(\\bar{x})$')

#polygon(rep(c(InrowN$In.rows.1[11+iter]-InrowN$table_SME.ConfNmax[11+iter], InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter]),each=2),c(-1,1,1,-1), col=adjustcolor(emission_col[1],alpha.f=0.1), border = NA)
#polygon(rep(c(InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter], InrowN$In.rows.1[6+iter]+InrowN$table_SME.ConfNmax[6+iter]),each=2),c(-1,1,1,-1), col=adjustcolor(emission_col[2],alpha.f=0.1), border = NA)

lines(range_values1,e1, col=emission_col[1], lwd=3)
lines(range_values2,e2, col=emission_col[2], lwd=3)
lines(range_values3,e3, col=emission_col[3], lwd=3)

abline(v= InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter], col=emission_col[1], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[11+iter]-InrowN$table_SME.ConfNmax[11+iter], col=emission_col[1], lwd=3, lty=2)
abline(v= InrowN$In.rows.1[6+iter]+InrowN$table_SME.ConfNmax[6+iter], col=emission_col[2], lwd=3,lty=2)
abline(v= InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter], col=emission_col[2], lwd=3, lty=2)

e2_e1 <- round(pnorm(InrowN$In.rows.1[11+iter]+InrowN$table_SME.ConfNmax[11+iter],mean=InrowN$In.rows.1[6+iter], sd=InrowN$table_SME.ErrorNmax[6+iter]),2)
e1_e2 <- 1 - round(pnorm(InrowN$In.rows.1[6+iter]-InrowN$table_SME.ConfNmax[6+iter],mean=InrowN$In.rows.1[11+iter], sd=InrowN$table_SME.ErrorNmax[11+iter]),2)




# Calculate p value, for Prec to Entc
PrectoEnt <- as.data.frame(matrix(c(
pt((unlist(table_avg[11,1]) - unlist(table_avg[6,1]))/table_SME$Env4[[11]],4-1),
pt((unlist(table_avg[11+1,1]) - unlist(table_avg[6+1,1]))/table_SME$Env4[[11+1]],4-1),
pt((unlist(table_avg[11+2,1]) - unlist(table_avg[6+2,1]))/table_SME$Env4[[11+2]],4-1),
pt((unlist(table_avg[11+3,1]) - unlist(table_avg[6+3,1]))/table_SME$Env4[[11+3]],4-1),
pt((unlist(table_avg[11+4,1]) - unlist(table_avg[6+4,1]))/table_SME$Env4[[11+4]],4-1),

pt((unlist(table_avg[11,1]) - unlist(table_avg[6,1]))/table_SME$Env8[[11]],8-1),
pt((unlist(table_avg[11+1,1]) - unlist(table_avg[6+1,1]))/table_SME$Env8[[11+1]],8-1),
pt((unlist(table_avg[11+2,1]) - unlist(table_avg[6+2,1]))/table_SME$Env8[[11+2]],8-1),
pt((unlist(table_avg[11+3,1]) - unlist(table_avg[6+3,1]))/table_SME$Env8[[11+3]],8-1),
pt((unlist(table_avg[11+4,1]) - unlist(table_avg[6+4,1]))/table_SME$Env8[[11+4]],8-1),

pt((unlist(table_avg[11,1]) - unlist(table_avg[6,1]))/table_SME$Env16[[11]],16-1),
pt((unlist(table_avg[11+1,1]) - unlist(table_avg[6+1,1]))/table_SME$Env16[[11+1]],16-1),
pt((unlist(table_avg[11+2,1]) - unlist(table_avg[6+2,1]))/table_SME$Env16[[11+2]],16-1),
pt((unlist(table_avg[11+3,1]) - unlist(table_avg[6+3,1]))/table_SME$Env16[[11+3]],16-1),
pt((unlist(table_avg[11+4,1]) - unlist(table_avg[6+4,1]))/table_SME$Env16[[11+4]],16-1),

pt((unlist(table_avg[11,1]) - unlist(table_avg[6,1]))/table_SME$Env32[[11]],32-1),
pt((unlist(table_avg[11+1,1]) - unlist(table_avg[6+1,1]))/table_SME$Env32[[11+1]],32-1),
pt((unlist(table_avg[11+2,1]) - unlist(table_avg[6+2,1]))/table_SME$Env32[[11+2]],32-1),
pt((unlist(table_avg[11+3,1]) - unlist(table_avg[6+3,1]))/table_SME$Env32[[11+3]],32-1),
pt((unlist(table_avg[11+4,1]) - unlist(table_avg[6+4,1]))/table_SME$Env32[[11+4]],32-1)
),nrow=5, ncol=4))


# Calculate p value, for Entc to C
pt((unlist(table_avg[6,1]) - unlist(table_avg[1,1]))/table_SME$Env4[[6]],4-1)

pt((unlist(table_avg[6,1]) - unlist(table_avg[1,1]))/table_SME$Env8[[6]],8-1)

pt((unlist(table_avg[6,1]) - unlist(table_avg[1,1]))/table_SME$Env16[[6]],16-1)

pt((unlist(table_avg[6,1]) - unlist(table_avg[1,1]))/table_SME$Env32[[6]],32-1)

allocate <- function (Ni, si, ci = rep(1, length(Ni)), c0 = 0, ct = NA, vt = NA) 
{
  f <- Ni * si/sqrt(ci)/sum(Ni * si/sqrt(ci))
  N <- sum(Ni)
  if (!is.na(ct) & !is.na(vt)) {
    stop("both survey cost and variance cannot be fixed")
  }
  else if (is.na(ct) & is.na(vt)) {
    return(list(fractions = f, ni = NA, variance = NA, cost = NA))
  }
  else {
    t1 <- sum(Ni * si/sqrt(ci))
    t2 <- sum(Ni * si * sqrt(ci))
    if (!is.na(vt)) {
      n <- t1 * t2/(vt * N^2 + sum(Ni * si^2))
      ni <- n * f
      if (any(ni > Ni)) 
        warning("optimum sample size exceeds available units")
      return(list(fractions = f, ni = n * f, n = n, variance = ifelse(all(ni <= 
                                                                            Ni), sum(Ni^2 * (Ni - ni)/Ni * (si^2/ni))/N^2, 
                                                                      NA), cost = ifelse(all(ni <= Ni), c0 + sum(ni * 
                                                                                                                   ci), NA)))
    }
    if (!is.na(ct)) {
      n <- (ct - c0) * t1/t2
      ni <- n * f
      if (any(ni > Ni)) 
        warning("optimum sample size exceeds available units")
      return(list(fractions = f, ni = n * f, n = n, variance = ifelse(all(ni <= 
                                                                            Ni), sum(Ni^2 * (Ni - ni)/Ni * (si^2/ni))/N^2, 
                                                                      NA), cost = ifelse(all(ni <= Ni), c0 + sum(ni * 
                                                                                                                   ci), NA)))
    }
  }
}
