# ======================================
# Author : Abhishek Mishra 
# Lab 4  : Network growth  
# ======================================

#getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
#dir_path <-“~/YourWorkingDirectoryFilePath”
#setwd(dir_path)
# clear everything out of memory
#rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

## Load package
library(igraph)

#infile<-"Macrae_table5.5_mjbook.csv"

macrae_frame=read.csv("C:/E!!/UIC/IDS564/Lab4/Macrae_table5.5_mjbook.csv", header = TRUE, sep = ",")
macrae_frame$nop <-macrae_frame$Number.of.prisoners
# This is the empirical cumulative distribution function; but it's not useful for this dataset
# It calculates F(d) for unaggregated data. But the current dataset is already aggregated; so you 
# should calculate F(d) using the cumulative sum instead
# F_d<-ecdf(macrae_frame$nop)
# plot(F_d)

# Some useful functions, Suggested help look-ups to learn more:
help(cumsum)
help(lm)
help(coefficients) # Run after lm, to get the value of your Beta slope estimate. Then convert it to the alpha estimate.
help(log)


# Get cumulative SUM 
F_d <- cumsum(macrae_frame$nop)
Total_degree <- sum(macrae_frame$Degree * macrae_frame$nop)
Total_nop <- sum(macrae_frame$nop)

# CDF (Need values from 0 to 1)
P_F_d <- F_d/Total_nop

Avg_degree <- Total_degree/Total_nop
m <- 0.5*Avg_degree

alpha_0 <- 0.11
Y <- log(1-P_F_d[-length(P_F_d)])
Y[9] <- 0
X <- log(macrae_frame$Degree + 2*alpha_0*m/(1-alpha_0))
macrae_frame <- cbind(macrae_frame,Y)
macrae_frame <- cbind(macrae_frame,X)
macrae_frame <- cbind(macrae_frame,F_d)
macrae_frame <- cbind(macrae_frame,P_F_d )

# Q3
alpha_0 <- 0.11
macrae_frame$X <- log(macrae_frame$Degree + 2*alpha_0*m/(1-alpha_0))
l_m_f <- lm(macrae_frame$Y[-nrow(macrae_frame)] ~ macrae_frame$X[-nrow(macrae_frame)])
Beta_1 <- l_m_f$coefficients[2]

# Q4
alpha_0 <- 0.10
macrae_frame$X <- log(macrae_frame$Degree + 2*alpha_0*m/(1-alpha_0))
l_m_f <- lm(macrae_frame$Y[-nrow(macrae_frame)] ~ macrae_frame$X[-nrow(macrae_frame)])
Beta_1 <- l_m_f$coefficients[2]
alpha_1 <- 1 + 2/Beta_1
alpha_1

# Q5
Array_Beta_1 <- array(0,9)
Array_Alpha_1 <- array(0,9)
# Execute  the entire for loop code block together 
for(i in 1:9) {
  #print(i) 
  alpha_0<-i/10
  # print("Alpha 0: ")
  # print (alpha_0)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
  macrae_frame$X <- log(macrae_frame$Degree + 2*alpha_0*m/(1-alpha_0))
  print(macrae_frame$X)
  l_m_f <- lm(macrae_frame$Y[-nrow(macrae_frame)] ~ macrae_frame$X[-nrow(macrae_frame)])
  Array_Beta_1[i] <- l_m_f$coefficients[2]
  print(Array_Beta_1)
  Array_Alpha_1[i] <- 1 + 2/Array_Beta_1[i] 
  print(Array_Alpha_1)
  print("----------------------------------")
}

plot(Array_Alpha_1,seq(0.1,0.9,0.1))
# check where alpha_1 is closest to alpha_0
print(Array_Alpha_1 - seq(0.1,0.9,0.1))

# This it is also useful to calculate alpha_1 values for the following
alpha_0<-0.99
alpha_0<-0.999
alpha_0<-0.9999

Array_Narrow_range_Alpha_0 <- c(0.99,0.999,0.9999)
Array_Narrow_Beta_1 <- array(0,3)
Array_Narrow_Alpha_1 <- array(0.3)
for(i in 1:3) {
  #print(i) 
  alpha_0<-Array_Narrow_range_Alpha_0[i]
  # print("Alpha 0: ")
  # print (alpha_0)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
  macrae_frame$X <- log(macrae_frame$Degree + 2*alpha_0*m/(1-alpha_0))
  print(macrae_frame$X)
  l_m_f <- lm(macrae_frame$Y[-nrow(macrae_frame)] ~ macrae_frame$X[-nrow(macrae_frame)])
  Array_Narrow_Beta_1[i] <- l_m_f$coefficients[2]
  print(Array_Narrow_Beta_1)
  Array_Narrow_Alpha_1[i] <- 1 + 2/Array_Narrow_Beta_1[i] 
  print(Array_Narrow_Alpha_1)
  print("----------------------------------")
}
print(Array_Narrow_Alpha_1 -  c(0.99,0.999,0.9999) )
# alpha_0 & alpha_1 are closest to 0.9999 -> NEARLY EXPONENTIAL

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

Goyal_Et_All_frame=read.csv("C:/E!!/UIC/IDS564/Lab4/Coauthorship_GoyalEtAl.csv", header = TRUE, sep = ",")
Goyal_Et_All_frame$noa <-Goyal_Et_All_frame$Number.of.authors

# Get cumulative SUM 
F_d_2 <- cumsum(Goyal_Et_All_frame$noa)
Total_degree_2 <- sum(Goyal_Et_All_frame$Degree * Goyal_Et_All_frame$noa)
Total_noa_2 <- sum(Goyal_Et_All_frame$noa)

# CDF (Need values from 0 to 1)
P_F_d_2 <- F_d_2/Total_noa_2

Avg_degree_2 <- Total_degree_2/Total_noa_2
m <- 0.5*Avg_degree_2

alpha_0 <- 0.11
Y2 <- log(1-P_F_d_2[-length(P_F_d_2)])
Y2[62] <- 0
X2 <- log(Goyal_Et_All_frame$Degree + 2*alpha_0*m/(1-alpha_0))
Goyal_Et_All_frame <- cbind(Goyal_Et_All_frame,Y2)
Goyal_Et_All_frame <- cbind(Goyal_Et_All_frame,X2)
Goyal_Et_All_frame <- cbind(Goyal_Et_All_frame,F_d_2)
Goyal_Et_All_frame <- cbind(Goyal_Et_All_frame,P_F_d_2 )

Array_Beta_1_G <- array(0,9)
Array_Alpha_1_G <- array(0,9)

for(i in 1:9) {
  #print(i) 
  alpha_0<-i/10
  # print("Alpha 0: ")
  # print (alpha_0)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
  Goyal_Et_All_frame$X2 <- log(Goyal_Et_All_frame$Degree + 2*alpha_0*m/(1-alpha_0))
  print(Goyal_Et_All_frame$X2)
  l_m_f <- lm(Goyal_Et_All_frame$Y2[-nrow(Goyal_Et_All_frame)] ~ Goyal_Et_All_frame$X2[-nrow(Goyal_Et_All_frame)])
  Array_Beta_1_G[i] <- l_m_f$coefficients[2]
  print(Array_Beta_1_G)
  Array_Alpha_1_G[i] <- 1 + 2/Array_Beta_1_G[i] 
  print(Array_Alpha_1_G)
  print("----------------------------------")
}

plot(Array_Alpha_1_G,seq(0.1,0.9,0.1))
# check where alpha_1 is closest to alpha_0
print(Array_Alpha_1_G - seq(0.1,0.9,0.1))

print(which.min(abs(Array_Alpha_1_G - seq(0.1,0.9,0.1))))

#====================================================================================

#HamRadioOperators_Killworth

Ham_Rad_Op_frame=read.csv("C:/E!!/UIC/IDS564/Lab4/HamRadioOperators_Killworth.csv", header = TRUE, sep = ",")
Ham_Rad_Op_frame$noa <-Ham_Rad_Op_frame$Number.of.Operators

# Get cumulative SUM 
F_d_2 <- cumsum(Ham_Rad_Op_frame$noa)
Total_degree_2 <- sum(Ham_Rad_Op_frame$Degree * Ham_Rad_Op_frame$noa)
Total_noa_2 <- sum(Ham_Rad_Op_frame$noa)

# CDF (Need values from 0 to 1)
P_F_d_2 <- F_d_2/Total_noa_2

Avg_degree_2 <- Total_degree_2/Total_noa_2
m <- 0.5*Avg_degree_2

alpha_0 <- 0.11
Y2 <- log(1-P_F_d_2[-length(P_F_d_2)])
Y2[62] <- 0
X2 <- log(Ham_Rad_Op_frame$Degree + 2*alpha_0*m/(1-alpha_0))
Ham_Rad_Op_frame <- cbind(Ham_Rad_Op_frame,Y2)
Ham_Rad_Op_frame <- cbind(Ham_Rad_Op_frame,X2)
Ham_Rad_Op_frame <- cbind(Ham_Rad_Op_frame,F_d_2)
Ham_Rad_Op_frame <- cbind(Ham_Rad_Op_frame,P_F_d_2 )

Array_Beta_1_G <- array(0,9)
Array_Alpha_1_G <- array(0,9)

for(i in 1:9) {
  #print(i) 
  alpha_0<-i/10
  # print("Alpha 0: ")
  # print (alpha_0)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
  Ham_Rad_Op_frame$X2 <- log(Ham_Rad_Op_frame$Degree + 2*alpha_0*m/(1-alpha_0))
  print(Ham_Rad_Op_frame$X2)
  l_m_f <- lm(Ham_Rad_Op_frame$Y2[-nrow(Ham_Rad_Op_frame)] ~ Ham_Rad_Op_frame$X2[-nrow(Ham_Rad_Op_frame)])
  Array_Beta_1_G[i] <- l_m_f$coefficients[2]
  print(Array_Beta_1_G)
  Array_Alpha_1_G[i] <- 1 + 2/Array_Beta_1_G[i] 
  print(Array_Alpha_1_G)
  print("----------------------------------")
}

plot(Array_Alpha_1_G,seq(0.1,0.9,0.1))
# check where alpha_1 is closest to alpha_0
print(Array_Alpha_1_G - seq(0.1,0.9,0.1))

print(which.min(abs(Array_Alpha_1_G - seq(0.1,0.9,0.1))))

