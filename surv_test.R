##########################################################################
# Simple script to load survival data and show 
#  survival functionality
#
# Authors: Soumya Banerjee, Tom Bishop, Demetris Avraam, Paul Burton
##########################################################################

####################
# Load libraries
####################
library(survival)
library(RANN)

##############
# Load data
##############
file <- read.csv(file = "expand_no_missing_study1.csv", header = TRUE, stringsAsFactors = FALSE)
# file <- read.csv(file = "https://raw.githubusercontent.com/neelsoumya/survival_curve_privacy_prototype/main/expand_no_missing_study1.csv", header = TRUE, stringsAsFactors = FALSE)


###################
# set up variables
###################

SURVTIME  <- as.numeric(file$survtime)
EVENT     <- as.numeric(file$cens)
STARTTIME <- as.numeric(file$starttime)
ENDTIME   <- as.numeric(file$endtime)

AGE <- as.numeric(file$age.60)

# build survival object
s <- survival::Surv(time=SURVTIME,event=EVENT)
# survival::coxph(formula = "survival::Surv(time=SURVTIME,event=EVENT) ~ file$age.60", data = file)


##################
# Plotting
##################
my_surv = Surv(SURVTIME, EVENT)
no_noise <- survfit(Surv(SURVTIME, EVENT) ~ 1)
survfit_model_variable <- survfit(Surv(SURVTIME, EVENT) ~ 1)

# make a copy
survfit_model_variable_deterministic <- survfit_model_variable

# original survival curve

plot(survfit_model_variable, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Original survival curve')

#noise = 0.0003 # 0.03 0.26
noise = 0.25
percentage <- noise


# make a copy
survfit_model_variable_noise_y <- survfit_model_variable


##########################################
# Approach 1: probabilistic anonymization
#     add noise before plotting
#
# add noise to:
# surv (i.e. proportion surviving)
# time (times at which events occur, ie when the proportion changes)
# this is for the y axis
# and for time on x axis
##########################################

sd= var(survfit_model_variable$time)^0.5

for ( i_temp_counter_inner in c(2:length(survfit_model_variable$surv)) )
{
  
  # current value, upper, lower at this index
  # value_temp <- survfit_model_variable$surv[i_temp_counter_inner]
  # upper_temp <- survfit_model_variable$upper[i_temp_counter_inner]
  # lower_temp <- survfit_model_variable$lower[i_temp_counter_inner]
  
  # previous value, upper, lower
  # prev_value_temp <- survfit_model_variable$surv[i_temp_counter_inner - 1]
  # prev_upper_temp <- survfit_model_variable$upper[i_temp_counter_inner - 1]
  # prev_lower_temp <- survfit_model_variable$lower[i_temp_counter_inner - 1]
  
  # add some noise 
  # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
  # delta_noise <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  
  # SUBTRACT this noise from the PREVIOUS VALUE if it does not cause problems with monotonicity
  
  # value_noise = value_temp - delta_noise
  # upper_noise = upper_temp - delta_noise
  # lower_noise = lower_temp - delta_noise
  
  # if (prev_value_temp >= value_noise)
  # {
  #   survfit_model_variable$surv[i_temp_counter_inner] <- value_noise
  #   survfit_model_variable$upper[i_temp_counter_inner] <- upper_noise
  #   survfit_model_variable$lower[i_temp_counter_inner] <- lower_noise
  # }
  # else
  # {
  #   survfit_model_variable$surv[i_temp_counter_inner] = prev_value_temp
  #   survfit_model_variable$upper[i_temp_counter_inner] = prev_upper_temp
  #   survfit_model_variable$lower[i_temp_counter_inner] = prev_lower_temp
  # }
  # 
  # survfit_model_variable$mono[i_temp_counter_inner] = prev_value_temp - survfit_model_variable$surv[i_temp_counter_inner]
  
  # new noise for x axis
  # needs more work, also monotonic
  # take absolute value so that no negative values subtracted (this will increase time instead of decreasing time)
  # delta_noise_time <- abs( stats::rnorm(n = 1, mean = 0, sd = percentage) )
  # do not take absolute value but have it reject values if not monotonically increasing
  
  delta_noise_time <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  # survfit_model_variable$time[i_temp_counter_inner] <- survfit_model_variable$time[i_temp_counter_inner] + delta_noise
  
  # previous value time
  prev_value_temp_time <- survfit_model_variable$time[i_temp_counter_inner - 1]
  # current value temp time
  curr_value_temp_time <- survfit_model_variable$time[i_temp_counter_inner]
  # proposed value of time with noise subtracted
  value_noise_time_proposed = curr_value_temp_time + (curr_value_temp_time*delta_noise_time)
  
  if (prev_value_temp_time <= value_noise_time_proposed)
  {
    # if previous value time is less then proposed value then ok 
    # since time is supposed to be increasing
    
    # set this value to current time
    survfit_model_variable$time[i_temp_counter_inner] = value_noise_time_proposed
    
  }
  else
  {
    # set to previous time value
    survfit_model_variable$time[i_temp_counter_inner] = prev_value_temp_time
  }
  
  
  
}

# modified survival curve

plot(survfit_model_variable, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Survival curve with probabilistic anonymization')



##########################################
# Approach 1b: probabilistic anonymization
#     add noise before plotting
#     on Y axis
#
# add noise to:
# surv (i.e. proportion surviving)
# time (times at which events occur, ie when the proportion changes)
# this is for the y axis
##########################################

sd_y_axis = var(survfit_model_variable_noise_y$surv)^0.5

for ( i_temp_counter_inner in c(2:length(survfit_model_variable_noise_y$surv)) )
{
  
  # current value, upper, lower at this index
  value_temp <- survfit_model_variable_noise_y$surv[i_temp_counter_inner]
  upper_temp <- survfit_model_variable_noise_y$upper[i_temp_counter_inner]
  lower_temp <- survfit_model_variable_noise_y$lower[i_temp_counter_inner]
  
  # previous value, upper, lower
  prev_value_temp <- survfit_model_variable_noise_y$surv[i_temp_counter_inner - 1]
  prev_upper_temp <- survfit_model_variable_noise_y$upper[i_temp_counter_inner - 1]
  prev_lower_temp <- survfit_model_variable_noise_y$lower[i_temp_counter_inner - 1]
  
  # add some noise 
  # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
  
  delta_noise <- stats::rnorm(n = 1, mean = 0, sd = sd_y_axis)
  # delta_noise <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  
  # SUBTRACT this noise from the PREVIOUS VALUE if it does not cause problems with monotonicity
  
  value_noise = value_temp - delta_noise
  upper_noise = upper_temp - delta_noise
  lower_noise = lower_temp - delta_noise
  
  if (prev_value_temp >= value_noise)
  {
    survfit_model_variable_noise_y$surv[i_temp_counter_inner] <- value_noise
    survfit_model_variable_noise_y$upper[i_temp_counter_inner] <- upper_noise
    survfit_model_variable_noise_y$lower[i_temp_counter_inner] <- lower_noise
  }
  else
  {
    survfit_model_variable_noise_y$surv[i_temp_counter_inner] = prev_value_temp
    survfit_model_variable_noise_y$upper[i_temp_counter_inner] = prev_upper_temp
    survfit_model_variable_noise_y$lower[i_temp_counter_inner] = prev_lower_temp
  }
  
  survfit_model_variable_noise_y$mono[i_temp_counter_inner] = prev_value_temp - survfit_model_variable_noise_y$surv[i_temp_counter_inner]
  
  # new noise for x axis
  # needs more work, also monotonic
  # take absolute value so that no negative values subtracted (this will increase time instead of decreasing time)
  # delta_noise_time <- abs( stats::rnorm(n = 1, mean = 0, sd = percentage) )
  # do not take absolute value but have it reject values if not monotonically increasing
  # delta_noise_time <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  # # survfit_model_variable_noise_y$time[i_temp_counter_inner] <- survfit_model_variable_noise_y$time[i_temp_counter_inner] + delta_noise
  # 
  # # previous value time
  # prev_value_temp_time <- survfit_model_variable_noise_y$time[i_temp_counter_inner - 1]
  # # current value temp time
  # curr_value_temp_time <- survfit_model_variable_noise_y$time[i_temp_counter_inner]
  # # proposed value of time with noise subtracted
  # value_noise_time_proposed = curr_value_temp_time + (curr_value_temp_time*delta_noise_time)
  # 
  # if (prev_value_temp_time <= value_noise_time_proposed)
  # {
  #   # if previous value time is less then proposed value then ok 
  #   # since time is supposed to be increasing
  #   
  #   # set this value to current time
  #   survfit_model_variable_noise_y$time[i_temp_counter_inner] = value_noise_time_proposed
  #   
  # }
  # else
  # {
  #   # set to previous time value
  #   survfit_model_variable_noise_y$time[i_temp_counter_inner] = prev_value_temp_time
  # }
  # 
  
  
}

# modified survival curve
plot(survfit_model_variable_noise_y, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Survival curve with probabilistic anonymization')




##########################################
# Approach 2: deterministic anonymization
#
##########################################

##################
# Anonymise survival times using the deterministic anonymisation
##################

knn <- 20

# Step 1: Standardise the variable
time.standardised <- (SURVTIME-mean(SURVTIME))/stats::sd(SURVTIME)

# Step 2: Find the k-1 nearest neighbours of each data point
nearest <- RANN::nn2(time.standardised, k = knn)

# Step 3: Calculate the centroid of each n nearest data points
time.centroid <- matrix()
for (i in 1:length(SURVTIME)){
  time.centroid[i] <- mean(time.standardised[nearest$nn.idx[i,1:knn]])
}

# Step 4: Calculate the scaling factor
time.scalingFactor <- stats::sd(time.standardised)/stats::sd(time.centroid)

# Step 5: Apply the scaling factor to the centroids
time.masked <- time.centroid * time.scalingFactor

# Step 6: Shift the centroids back to the actual position and scale of the original data
SURVTIME.anon <- (time.masked * stats::sd(SURVTIME)) + mean(SURVTIME)


survfit_model_variable.anon <- survfit(Surv(SURVTIME.anon, EVENT) ~ 1)

lines(survfit_model_variable.anon, col='red', add=TRUE)

survfit_model_variable_determ <- survfit_model_variable.anon

plot(survfit_model_variable_determ, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Survival curve with deterministic anonymization')




##########################################
# Approach 3: deterministic anonymization take 2
#
##########################################

##################
# Anonymise survival times using the deterministic anonymisation
##################

knn <- 20

# Step 1: Standardise the variable
time.standardised <- (survfit_model_variable_deterministic$time-mean(survfit_model_variable_deterministic$time))/stats::sd(survfit_model_variable_deterministic$time)

# Step 2: Find the k-1 nearest neighbours of each data point
nearest <- RANN::nn2(time.standardised, k = knn)

# Step 3: Calculate the centroid of each n nearest data points
time.centroid <- matrix()
for (i in 1:length(survfit_model_variable_deterministic$time)){
  time.centroid[i] <- mean(time.standardised[nearest$nn.idx[i,1:knn]])
}

# Step 4: Calculate the scaling factor
time.scalingFactor <- stats::sd(time.standardised)/stats::sd(time.centroid)

# Step 5: Apply the scaling factor to the centroids
time.masked <- time.centroid * time.scalingFactor

# Step 6: Shift the centroids back to the actual position and scale of the original data
SURVTIME_anon <- (time.masked * stats::sd(survfit_model_variable_deterministic$time)) + mean(survfit_model_variable_deterministic$time)

# modofy time in survfit object (instead of original time)
survfit_model_variable_deterministic$time <- SURVTIME_anon

# TODO: commenting out these to have no survfit call
#survfit_model_variable.anon <- survfit(Surv(SURVTIME_anon, EVENT) ~ 1)
#lines(survfit_model_variable.anon, col='red', add=TRUE)
# survfit_model_variable_determ <- survfit_model_variable.anon
#plot(survfit_model_variable_determ)

plot(survfit_model_variable_deterministic, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Survival curve with deterministic anonymization')







#noise = 0.0003 # 0.03 0.26
noise = 0.03
percentage <- noise

library(ExtDist)

##########################################
# Approach 4: probabilistic anonymization
#     add noise before plotting Laplace
#
# add noise to:
# surv (i.e. proportion surviving)
# time (times at which events occur, ie when the proportion changes)
# this is for the y axis
# and for time on x axis
##########################################
for ( i_temp_counter_inner in c(2:length(survfit_model_variable$surv)) )
{
  
  # current value, upper, lower at this index
  # value_temp <- survfit_model_variable$surv[i_temp_counter_inner]
  # upper_temp <- survfit_model_variable$upper[i_temp_counter_inner]
  # lower_temp <- survfit_model_variable$lower[i_temp_counter_inner]
  
  # previous value, upper, lower
  # prev_value_temp <- survfit_model_variable$surv[i_temp_counter_inner - 1]
  # prev_upper_temp <- survfit_model_variable$upper[i_temp_counter_inner - 1]
  # prev_lower_temp <- survfit_model_variable$lower[i_temp_counter_inner - 1]
  
  # add some noise 
  # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
  # delta_noise <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  
  # SUBTRACT this noise from the PREVIOUS VALUE if it does not cause problems with monotonicity
  
  # value_noise = value_temp - delta_noise
  # upper_noise = upper_temp - delta_noise
  # lower_noise = lower_temp - delta_noise
  
  # if (prev_value_temp >= value_noise)
  # {
  #   survfit_model_variable$surv[i_temp_counter_inner] <- value_noise
  #   survfit_model_variable$upper[i_temp_counter_inner] <- upper_noise
  #   survfit_model_variable$lower[i_temp_counter_inner] <- lower_noise
  # }
  # else
  # {
  #   survfit_model_variable$surv[i_temp_counter_inner] = prev_value_temp
  #   survfit_model_variable$upper[i_temp_counter_inner] = prev_upper_temp
  #   survfit_model_variable$lower[i_temp_counter_inner] = prev_lower_temp
  # }
  # 
  # survfit_model_variable$mono[i_temp_counter_inner] = prev_value_temp - survfit_model_variable$surv[i_temp_counter_inner]
  
  # new noise for x axis
  # needs more work, also monotonic
  # take absolute value so that no negative values subtracted (this will increase time instead of decreasing time)
  # delta_noise_time <- abs( stats::rnorm(n = 1, mean = 0, sd = percentage) )
  # do not take absolute value but have it reject values if not monotonically increasing
  
  # TODO: randomly permute failure times in a rank preserving fashion
  # TODO: waiting times from Poisson or negative binomial   
  # TODO: use Paul Burton script generate synthetic dataset
  # TODO: use differential privacy Laplace distribution (see Algorithm below)
  #            http://proceedings.mlr.press/v126/gondara20a/gondara20a.pdf   
  # TODO: compare approach 2 (deterministic)(no noise added to model) and approach 2a (noise added to model)   
  
  # delta_noise_time <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  
  delta_noise_time <- ExtDist::rLaplace(n = 1, mu = 0, b = percentage)
  
  # survfit_model_variable$time[i_temp_counter_inner] <- survfit_model_variable$time[i_temp_counter_inner] + delta_noise
  
  # previous value time
  prev_value_temp_time <- survfit_model_variable$time[i_temp_counter_inner - 1]
  # current value temp time
  curr_value_temp_time <- survfit_model_variable$time[i_temp_counter_inner]
  # proposed value of time with noise subtracted
  value_noise_time_proposed = curr_value_temp_time + (curr_value_temp_time*delta_noise_time)
  
  if (prev_value_temp_time <= value_noise_time_proposed)
  {
    # if previous value time is less then proposed value then ok 
    # since time is supposed to be increasing
    
    # set this value to current time
    survfit_model_variable$time[i_temp_counter_inner] = value_noise_time_proposed
    
  }
  else
  {
    # set to previous time value
    survfit_model_variable$time[i_temp_counter_inner] = prev_value_temp_time
  }
  
  
  
}

# modified survival curve
plot(survfit_model_variable, xlab = 'Time', ylab = 'Proportion surviving', 
     main = 'Survival curve with probabilistic anonymization')



###################################
# Smoothing option
#
###################################

plot(survfit_model_variable$time, survfit_model_variable$n.risk)

# LOESS smoothing
#  https://www.statology.org/lowess-smoothing-r/

plot( lowess(survfit_model_variable$time, survfit_model_variable$n.risk) )

# try other parameters for smoothness
f_loess_smoothness = 0.1
plot( lowess(survfit_model_variable$time, survfit_model_variable$n.risk, f = f_loess_smoothness ) )

f_loess_smoothness = 0.03
plot( lowess(survfit_model_variable$time, survfit_model_variable$n.risk, f = f_loess_smoothness ) )

f_loess_smoothness = 0.01
plot( lowess(survfit_model_variable$time, survfit_model_variable$n.risk, f = f_loess_smoothness ) )


# another implementation
loess10 = stats::loess(no_noise$surv ~ no_noise$time, span=0.10)
smoothed10 <- stats::predict(loess10)

# TODO: assign to returned object Y axis
#   survfit_model_variable$surv
# survfit_model_variable$surv  =smoothed10
# plot(survfit_model_variable$surv, survfit_model_variable$time)

loess25 = stats::loess(no_noise$surv ~ no_noise$time, span=0.25)
smoothed25 <- stats::predict(loess25)
plot(no_noise)
plot(no_noise$surv, x=no_noise$time, type="l", main="Loess Smoothing and Prediction", xlab="time", ylab="surv")
lines(smoothed10, x=no_noise$time, col="red")
lines(smoothed25, x=no_noise$time, col="green")


# TODO: smooth 95% CI also




# TODO: use isotonic regression
#     https://www.r-bloggers.com/2020/05/what-is-isotonic-regression/
#     https://cran.r-project.org/web/packages/isotone/isotone.pdf

# install.packages('isotone')
library(isotone)

y <- rnorm(9)
w1 <- rep(1:9)
Atot <- cbind(1:8, 2:9)
fit_iso <- isotone::activeSet(isomat = Atot, mySolver = "LS", y = y, weights = w1)

summary(fit_iso)
plot(fit_iso$y, fit_iso$x)
