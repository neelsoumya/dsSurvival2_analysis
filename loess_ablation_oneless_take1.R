##########################################################################
# Simple script to load survival data and show 
#  survival curves
#    1. ablation (reduce number of patients)
#    2. take away one patient and show curves with and without that patient
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
# diabetic
# gbsg
heart
jasa
mgus
myeloid
# nafld1



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


###################
# Objective: a reduced veteran dataset that guarantees the patient at time 61 is included
# That way, we can do the same test of removing that patient to check if they can be detected in the curve
###################

# subset of data forcing inclusion of patient with time 61
# TODO: make parameter
sample_size = 137
#sample_size = 50
#sample_size = 25
#vector the size of veteran
sample_vect = c(1:length(veteran$time))
#find the row number with our special patient time 61
row_number = which(veteran$time == 61)
# randomly sample row numbers without our special patient, then add this patient back on
rows_to_use = sort(c(sample(sample_vect[-row_number],sample_size-1),row_number))
#create the smaller dataset that has patient with time 61
ablation_set = veteran[rows_to_use,]
# smaller dataset without the patient at time 61
ablation_set_61 = ablation_set[ablation_set$time != 61,]

# now create the 2 survival models using these datasets and curves to compare with and without patient 61.....





# all data

survObj <- survival::Surv(time = veteran$time, event = veteran$status)#, data=veteran)

# survObj <- survObj[1:50]

# build survfit object
# survFit <-  survfit(survObj~trt, data=veteran[1:50,])
survFit <-  survfit(survObj~trt, data=veteran)
# survFit <-  survfit(Surv(time, status)~trt, data=veteran)
# take away one data
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

# TODO: high res figures 2 and 3 And fig 3 with 4 panels direct to eps
# TODO: 137 50 25 ABLATION

########################################
# use synthetic dataset privacy risk
########################################
library(fANCOVA)

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

