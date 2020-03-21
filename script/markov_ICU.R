#####################################################################################
##########             Simple 3-state Markov model in R         #####################
#####################################################################################


# Developed by the Decision Analysis in R for Technologies in Health (DARTH) workgroup
# Fernando Alarid-Escudero, PhD (1) 
# Eva A. Enns, MS, PhD (2)	
# M.G. Myriam Hunink, MD, PhD (3,4)
# Hawre J. Jalal, MD, PhD (5) 
# Eline M. Krijkamp, MSc (3)	
# Petros Pechlivanoglou, PhD (6) 

# In collaboration of: 		
# 1 Drug Policy Program, Center for Research and Teaching in Economics (CIDE) - CONACyT, 
#   Aguascalientes, Mexico
# 2 University of Minnesota School of Public Health, Minneapolis, MN, USA
# 3 Erasmus MC, Rotterdam, The Netherlands
# 4 Harvard T.H. Chan School of Public Health, Boston, USA
# 5 University of Pittsburgh Graduate School of Public Health, Pittsburgh, PA, USA
# 6 The Hospital for Sick Children, Toronto and University of Toronto, Toronto ON, Canada

#####################################################################################
# Please cite our publications when using this code
# - Jalal H, Pechlivanoglou P, Krijkamp E, Alarid-Escudero F, Enns E, Hunink MG. 
# An Overview of R in Health Decision Sciences. Med Decis Making. 2017; 37(3): 735-746. 
# https://journals.sagepub.com/doi/abs/10.1177/0272989X16686559
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400–22. 
# https://journals.sagepub.com/doi/abs/10.1177/0272989X18754513
# - Krijkamp EM, Alarid-Escudero F, Enns E, Pechlivanoglou P, Hunink MM, Jalal H. 
# A Multidimensional Array Representation of State-Transition Model Dynamics. 
# BioRxiv 670612 2019.https://www.biorxiv.org/content/10.1101/670612v1

#####################################################################################
# Copyright 2017, THE HOSPITAL FOR SICK CHILDREN AND THE COLLABORATING INSTITUTIONS. 
# All rights reserved in Canada, the United States and worldwide. Copyright, 
# trademarks, trade names and any and all associated intellectual property are 
# exclusively owned by THE HOSPITAL FOR Sick CHILDREN and the collaborating 
# institutions. These materials may be used, reproduced, modified, distributed 
# and adapted with proper attribution.
#####################################################################################

rm(list = ls())      # clear memory (removes all the variables from the workspace)

#### 01 Load packages ####
# no packages required
# library(dampack)

#### 02 Load Functions ####
# no functions required

#### 03 Input Model Parameters ####
## Strategy names
v_names_str <- c("Base Case")  
## Number of strategies
n_str <- length(v_names_str)
## Markov model parameters
v_n  <- c("Healthy", "Sick", "Dead")    # state names
n_s  <- length(v_n)                     # number of states
n_t  <- 60                              # number of cycles

p_HD <- 0.02                    # probability to die when healthy
p_HS <- 0.05                    # probability to become sick when healthy
p_SD <- 0.1                     # probability to die when sick

# Costs and utilities  
c_H  <- 400                     # cost of remaining one cycle healthy
c_S  <- 1000                    # cost of remaining one cycle sick
c_D  <- 0                       # cost of remaining one cycle dead
u_H  <- 0.8                     # utility when healthy 
u_S  <- 0.5                     # utility when sick
u_D  <- 0                       # utility when dead
d_e <- d_c <- 0.03               # equal discount of costs and QALYs by 3%
v_dwc <- 1 / (1 + d_e) ^ (0:n_t) # calculate discount weights for costs for each cycle based on discount rate d_c
v_dwe <- 1 / (1 + d_c) ^ (0:n_t) # calculate discount weights for effectiveness for each cycle based on discount rate d_e

#### 04 Define and initialize matrices and vectors ####
#### 04.1 Cohort trace ####
# create the cohort trace
m_M <- matrix(NA, 
              nrow = n_t + 1 ,             # create Markov trace (n.t + 1 because R doesn't understand  Cycle 0)
              ncol = n_s, 
              dimnames = list(0:n_t, v_n))

m_M[1, ] <- c(1, 0, 0)                     # initialize first cycle of Markov trace

#### 04.2 Transition probability MATRIX ####
# create the transition probability matrix
m_P  <- matrix(0,
               nrow = n_s,
               ncol = n_s,
               dimnames = list(v_n, v_n)) # name the columns and rows of the transition probability matrix
m_P

# fill in the transition probability matrix
### From Healthy
m_P["Healthy", "Healthy"] <- 1 - p_HD - p_HS
m_P["Healthy", "Sick"]    <- p_HS
m_P["Healthy", "Dead"]    <- p_HD

### From Sick
m_P["Sick", "Sick"] <- 1 - p_SD
m_P["Sick", "Dead"] <- p_SD

### From Dead
m_P["Dead", "Dead"] <- 1

# check rows add up to 1
rowSums(m_P)

#### 05 Run Markov model ####
for (t in 1:n_t){ # t<-1                  # loop through the number of cycles
  m_M[t + 1, ] <- m_M[t, ] %*% m_P        # estimate the state vector for the next cycle (t + 1)
}

# the %*% represents the matrix multiplication 

#### 06 Compute and Plot Epidemiological Outcomes ####
#### 06.1 Cohort trace #####
matplot(m_M, type = 'l', 
        ylab = "Probability of state occupancy",
        xlab = "Cycle",
        main = "Cohort Trace", lwd = 3)          # create a plot of the data
legend("right", v_n, col = c("black", "red", "green"), 
       lty = 1:3, bty = "n")  # add a legend to the graph

abline(v=which.max(m_M[, "Sick"]), col = "gray")

#### 06.2 Overall Survival (OS) #####
v_os <- 1 - m_M[, "Dead"]                  # calculate the overall survival (OS) probability
v_os <- rowSums(m_M[, 1:2])                # alternative way of calculating the OS probability   

plot(v_os, type = 'l', 
     ylim = c(0, 1),
     ylab = "Survival probability",
     xlab = "Cycle",
     main = "Overall Survival")             # create a simple plot showing the OS
grid(nx = n_t, ny = 10, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE) # add grid 

#### 06.2.1 Life Expectancy (LE) #####
v_le <- sum(v_os)                             # summing probablity of OS over time  (i.e. life expectancy)

#### 06.3 Disease prevalence #####
v_prev <- m_M[, "Sick"]/v_os
plot(v_prev,
     ylim = c(0, 1),
     ylab = "Prevalence",
     xlab = "Cycle",
     main = "Disease prevalence")