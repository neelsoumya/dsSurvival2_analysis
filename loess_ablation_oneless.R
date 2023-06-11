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
library(fANCOVA)


##############
# Load data
##############
file <- read.csv(file = "expand_no_missing_study1.csv", header = TRUE, stringsAsFactors = FALSE)
# file <- read.csv(file = "https://raw.githubusercontent.com/neelsoumya/survival_curve_privacy_prototype/main/expand_no_missing_study1.csv", header = TRUE, stringsAsFactors = FALSE)
lung
bladder
veteran
cgd
colon
diabetic
gbsg
heart
jasa
mgus
myeloid
nafld1



# '80 dataset - all events before t=80 in veteran
# '61 dataset - as above with event at t=61 removed
# use case 1 - can you identify event at t=61 from the '80 curve? (crude eyeballing attack)
# use case 2 - can you identify event at t=61 through a difference attack (compare '61 and '80)
# use case 3 - assume I know an event has happened, and I know the times of all other events, can I see when that event happened? By comparing curves



##########################################
# Approach 0: LOESS smoothing
#
# useable span 0.3-0.55
##########################################

# all data

survObj <- survival::Surv(time = veteran$time, event = veteran$status)#, data=veteran)

# amount of ablation is 1:50 
# TODO: make it parametric

# ablation: take out patients
survObj <- survObj[1:50]

# build survfit object
survFit <-  survfit(survObj~trt, data=veteran[1:50,])

# take away one data
# TODO: remove from here not like above
veteran_61 = veteran[veteran$time != 61,]
survFit61 <- survfit(Surv(time, status)~trt, data=veteran_61)

my_span = 0.3

loess25_1 = loess(survFit[1]$surv ~ survFit[1]$time, span=my_span)
loess25_2 = loess(survFit[2]$surv ~ survFit[2]$time, span=my_span)

smoothed25_1 <- predict(loess25_1)
smoothed25_2 <- predict(loess25_2)
plot(survFit, lty = c("solid", "dashed"), col = c("black", "grey"), main = "veteran 80", xmax=80, ymin = 0.4)
lines(smoothed25_1, x=survFit[1]$time, col="red")
lines(smoothed25_2, x=survFit[2]$time, col="red")


loess25_61_1 = loess(survFit61[1]$surv ~ survFit61[1]$time, span=my_span)
loess25_61_2 = loess(survFit61[2]$surv ~ survFit61[2]$time, span=my_span)
smoothed25_61_1 <- predict(loess25_61_1)
smoothed25_61_2 <- predict(loess25_61_2)
plot(survFit61, lty = c("solid", "dashed"), col = c("black", "grey"), main = "veteran without 61", xmax=80, ymin = 0.4)
lines(smoothed25_61_1, x=survFit61[1]$time, col="blue")
lines(smoothed25_61_2, x=survFit61[2]$time, col="blue")

# 
# ##################
# # new auto
# ##################
# library(fANCOVA)
# 
# # Fit Local Polynomial Regression with Automatic Smoothing Parameter Selection
# n1 <- 100
# x1 <- runif(n1,min=0, max=3)
# sd1 <- 0.2
# e1 <- rnorm(n1,sd=sd1)
# y1 <- sin(2*x1) + e1
# 
# plot(x1,y1)
# 
# (y1.fit <- fANCOVA::loess.as(x1, y1, plot=TRUE))
# 
# 
# # on data fit model
# loess_as_fit <- fANCOVA::loess.as(survFit$time, survFit$surv, plot = TRUE)
# 
# # predict
# smoothed_loess_as = stats::predict(loess_as_fit)
# 
# # assign to survfit object and modify it
# # commented out for now
# # survFit$surv <- smoothed_loess_as
# 
# # plot
# plot(survFit$surv, x = survFit$time)
# 
# # return survfit object that is modified
# survFit

########################################
# use synthetic dataset privacy risk
########################################

#  original
loess25_1_auto = fANCOVA::loess.as(survFit[1]$time, survFit[1]$surv)
loess25_2_auto = fANCOVA::loess.as(survFit[2]$time, survFit[2]$surv)
smoothed25_1_auto <- predict(loess25_1_auto)
smoothed25_2_auto <- predict(loess25_2_auto)
# plot(survFit, lty = c("solid", "dashed"), col = c("black", "grey"), main = "veteran 80 automatic LOESS", xmax=80, ymin = 0.4)
plot(survFit, lty = c("solid", "dashed"), col = c("black", "grey"), 
     xmax=80, ymin = 0.4, xlab = 'Time', ylab = 'Fraction survived'
)
lines(smoothed25_1_auto, x=survFit[1]$time, col="red")
lines(smoothed25_2_auto, x=survFit[2]$time, col="red")

#  take away one person -61
loess25_61_1_auto = fANCOVA::loess.as(survFit61[1]$time, survFit61[1]$surv)
loess25_61_2_auto = fANCOVA::loess.as(survFit61[2]$time, survFit61[2]$surv)
smoothed25_61_1_auto <- predict(loess25_61_1_auto)
smoothed25_61_2_auto <- predict(loess25_61_2_auto)
#plot(survFit61, lty = c("solid", "dashed"), col = c("black", "grey"), main = "veteran without 61 automatic LOESS", 
#        xmax=80, ymin = 0.4, xlab = 'Time', ylab = 'Fraction survived')
plot(survFit61, lty = c("solid", "dashed"), col = c("black", "grey"), 
     xmax=80, ymin = 0.4, xlab = 'Time', ylab = 'Fraction survived'
)

lines(smoothed25_61_1_auto, x=survFit61[1]$time, col="blue")
lines(smoothed25_61_2_auto, x=survFit61[2]$time, col="blue")


# repeat no title
loess25_61_1_auto = fANCOVA::loess.as(survFit61[1]$time, survFit61[1]$surv)
loess25_61_2_auto = fANCOVA::loess.as(survFit61[2]$time, survFit61[2]$surv)
smoothed25_61_1_auto <- predict(loess25_61_1_auto)
smoothed25_61_2_auto <- predict(loess25_61_2_auto)
plot(survFit61, lty = c("solid", "dashed"), col = c("black", "grey"), xmax=80, ymin = 0.4, xlab = 'Time', ylab = 'Fraction survived')
lines(smoothed25_61_1_auto, x=survFit61[1]$time, col="blue")
lines(smoothed25_61_2_auto, x=survFit61[2]$time, col="blue")

XXXX stop here 


##################
# Plotting
##################

# original survival curve
plot(survFit, lty = c("solid", "dashed"), col = c("black", "grey"), main= "veteran 80 - noise x axis", xmax=80, ymin = 0.4)
#plot(survFit, main= "lung")
# make a copy
with_noise <- survFit
#noise = 0.0003 # 0.03 0.26
noise = 0.0003

##########################################
# Approach 1a: probabilistic anonymization
#     add noise before plotting
#
# add noise to:
# time (times at which events occur, ie when the proportion changes)
# this is for  time on x axis
##########################################

sd= var(with_noise$time)^0.5

for ( i_temp_counter_inner in c(2:length(with_noise$surv)) )
{
  if (i_temp_counter_inner == with_noise$strata[1]+1)
  {
    next
  }
  delta_noise_time <- stats::rnorm(n = 1, mean = 0, sd = noise*sd)
  # with_noise$time[i_temp_counter_inner] <- with_noise$time[i_temp_counter_inner] + delta_noise
  
  # previous value time
  prev_value_temp_time <- with_noise$time[i_temp_counter_inner - 1]
  # current value temp time
  curr_value_temp_time <- with_noise$time[i_temp_counter_inner]
  # proposed value of time with noise subtracted
  value_noise_time_proposed = curr_value_temp_time + (curr_value_temp_time*delta_noise_time)
  
  if (prev_value_temp_time <= value_noise_time_proposed)
  {
    # if previous value time is less then proposed value then ok 
    # since time is supposed to be increasing
    
    # set this value to current time
    with_noise$time[i_temp_counter_inner] = value_noise_time_proposed
    
  }
  else
  {
    # set to previous time value
    with_noise$time[i_temp_counter_inner] = prev_value_temp_time
  }
  
}

# modified survival curve
#plot(with_noise, main = "probabilistic")
lines(with_noise, col='blue', add=TRUE)

##########################################
# Approach 1b: probabilistic anonymization
#     add noise before plotting
#     on Y axis
#
# add noise to:
# surv (i.e. proportion surviving)
# this is for the y axis
##########################################

noise = 0.0003
survfit_model_variable_noise_y <- survFit

plot(survFit, lty = c("solid", "dashed"), col = c("black", "grey"), main= "veteran 80 - noise y axis", xmax=80, ymin = 0.4)

sd_y_axis = var(survfit_model_variable_noise_y$surv)^0.5


for ( i_temp_counter_inner in c(2:length(survfit_model_variable_noise_y$surv)) )
{
  if (i_temp_counter_inner == survfit_model_variable_noise_y$strata[1]+1)
  {
    next
  }
  # current value, upper, lower at this index
  value_temp <- survfit_model_variable_noise_y$surv[i_temp_counter_inner]
  
  # previous value, upper, lower
  prev_value_temp <- survfit_model_variable_noise_y$surv[i_temp_counter_inner - 1]
  
  
  delta_noise <- stats::rnorm(n = 1, mean = 0, sd = sd_y_axis)
  # delta_noise <- stats::rnorm(n = 1, mean = 0, sd = percentage)
  
  # SUBTRACT this noise from the PREVIOUS VALUE if it does not cause problems with monotonicity
  
  value_noise = value_temp - delta_noise
  
  if (prev_value_temp >= value_noise)
  {
    survfit_model_variable_noise_y$surv[i_temp_counter_inner] <- value_noise
  }
  else
  {
    survfit_model_variable_noise_y$surv[i_temp_counter_inner] = prev_value_temp
    
  }
  
  survfit_model_variable_noise_y$mono[i_temp_counter_inner] = prev_value_temp - survfit_model_variable_noise_y$surv[i_temp_counter_inner]
  
}

# modified survival curve
lines(survfit_model_variable_noise_y, col='green', add=TRUE)


##########################################
# Approach 3: deterministic anonymization take 2
#
##########################################

##################
# Anonymise survival times using the deterministic anonymisation
##################

knn <- 20
with_knn <- survFit

# Step 1: Standardise the variable
time.standardised <- (with_knn$time-mean(with_knn$time))/stats::sd(with_knn$time)

# Step 2: Find the k-1 nearest neighbours of each data point
nearest <- RANN::nn2(time.standardised, k = knn)

# Step 3: Calculate the centroid of each n nearest data points
time.centroid <- matrix()
for (i in 1:length(with_knn$time)){
  time.centroid[i] <- mean(time.standardised[nearest$nn.idx[i,1:knn]])
}

# Step 4: Calculate the scaling factor
time.scalingFactor <- stats::sd(time.standardised)/stats::sd(time.centroid)

# Step 5: Apply the scaling factor to the centroids
time.masked <- time.centroid * time.scalingFactor

# Step 6: Shift the centroids back to the actual position and scale of the original data
SURVTIME_anon <- (time.masked * stats::sd(with_knn$time)) + mean(with_knn$time)

# modofy time in survfit object (instead of original time)
with_knn$time <- SURVTIME_anon

# TODO: commenting out these to have no survfit call
#with_noise.anon <- survfit(Surv(SURVTIME_anon, EVENT) ~ 1)
#lines(with_noise.anon, col='red', add=TRUE)
# with_noise_determ <- with_noise.anon
#plot(with_noise_determ)
#plot(with_knn, main = "deterministic v2")
lines(with_knn, col='green', add=TRUE)

##########################################
# Approach 4: deterministic anonymization take 3
#
##########################################

##################
# In this case we don't bother standardising - gets rid of negatives
##################

knn <- 20
with_knn4 <- survFit

# Step 1: Standardise the variable
time.standardised <- (with_knn4$time-mean(with_knn4$time))/stats::sd(with_knn4$time)

# Step 2: Find the k-1 nearest neighbours of each data point
nearest <- RANN::nn2(with_knn4$time, k = knn)

# Step 3: Calculate the centroid of each n nearest data points
time.centroid <- matrix()
for (i in 1:length(with_knn4$time)){
  time.centroid[i] <- mean(with_knn4$time[nearest$nn.idx[i,1:knn]])
}

# Step 4: Calculate the scaling factor
time.scalingFactor <- stats::sd(with_knn4$time)/stats::sd(time.centroid)

# Step 5: Apply the scaling factor to the centroids
time.masked <- time.centroid * time.scalingFactor

# Step 6: Shift the centroids back to the actual position and scale of the original data
SURVTIME_anon <- time.masked

# modofy time in survfit object (instead of original time)
with_knn4$time <- SURVTIME_anon

# TODO: commenting out these to have no survfit call
#with_noise.anon <- survfit(Surv(SURVTIME_anon, EVENT) ~ 1)
#lines(with_noise.anon, col='red', add=TRUE)
# with_noise_determ <- with_noise.anon
#plot(with_noise_determ)
#plot(with_knn4, main = "deterministic v2")
lines(with_knn4, col='purple', add=TRUE)



##########################################
# Approach 5: Bonomi et al
#
# allows DP
# time bucketing and noise added to counts
##########################################

N = survFit$n
T = 1000
epsilon = 2
partition = ceiling(log(min(N,T),2)/epsilon)
partition = 3

acc = 0
label = 1
survFit$label = c(1:length(survFit$time))


# function to define partitioning groups
for (i in 1:length(survFit$time)){
  # need to add partition noise to take half of half the budget
  acc = acc + survFit$n.event[i] + survFit$n.censor[i]
  if (acc > partition){
    label = label + 1
    acc = 0
  }
  survFit$label[i] = label
  
}

my_df = data.frame(groups = survFit$label, time = survFit$time, n.event = survFit$n.event,
                   n.censor = survFit$n.censor, n.risk = survFit$n.risk)

library(dplyr)

# section to sum up events within a partition - no noise yet
my_df2 <- my_df %>%
  group_by(groups) %>%
  summarise(
    across(.cols = time, .fns = max),
    across(.cols = c(n.event,n.censor), .fns = sum)
  ) %>% arrange(time)

my_df2$cum_sum_censor = cumsum(my_df2$n.censor)
my_df2$cum_sum_event = cumsum(my_df2$n.event)
# add half of half the budget of noise to these

# function to generate binary representation of the partition number

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

# recursive function to create a binary tree like structure
binarise2 <- function(N, the_list)
{
  tree_level = c()
  for (i in seq(2,length(N),2)){
    
    part_sum = N[i]+N[i-1]
    # trick for end of odd length N, keep it until the end
    if ((length(N) - i)==1) {
      tree_level = c(tree_level,part_sum,N[i+1])
    }
    else {tree_level = c(tree_level,part_sum)}
    
  }
  the_list = append(the_list, list(tree_level))
  if (length(tree_level) == 1)
    return(the_list)
  else
    binarise2(tree_level, the_list)
}

#then go through and apply remaining budget as noise to the intermediate counts

test = c(1,2,0,3,1,1,0,1,3)
test= my_df2$n.event
#put the first tree level in a list and then pass this to the tree generating function
my_list = list(test)
final_list = binarise2(test, my_list)
final_list = rev(final_list)

#this for loop uses the binary representation of the partition index to locate the required
# nodes in the binary tree structure and add them together
depth = length(final_list)
duration = length(test)
results = c()
for (i in 1:duration){
  num = number2binary(i, depth)
  total = 0
  for (j in depth:1){
    if (num[j] ==1) {
      loc = floor(i/2^(depth-j))
      total = total + final_list[[j]][loc]
    }
  }
  
  results = c(results, total)
}



