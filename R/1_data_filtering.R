# Verification of the referential used by motor control in reaching movements
#
# Description: this code will process the range motion data, filtering the data
# with a low-pass filter and calculating the trajectories and
# velocities of the markers
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 18/08/2022

# Necessary packages -----------------------------------------------------------

#install.packages("stringi")
library(stringi)
library(signal)

# Setting filter parameters-----------------------------------------------------

low_pass <- 10                                                                  # Low pass filter frequency
order <- 4                                                                      # Low pass filter order

# Configuring-------------------------------------------------------------------

data_path <- list.files(path = "data/raw/csv",                                  # List all .csv data paths
                         pattern = "csv$",
                         recursive = TRUE,
                         full.names = TRUE)

data_names <- gsub(".csv", "", basename(data_path), fixed = TRUE)               # List all data names

data_laterality <- stri_sub(data_names, -3, -3)

number_data <- length(data_names)

R_markers <- c("RUSP", "RHME")
L_markers <- c("LUSP", "LHME")

for(i_data in seq(1, number_data)){

  if(data_laterality[i_data] == "R"){
    markers <- R_markers
  }
  else{
    markers <- L_markers
  }

  data <- read.csv(data_path[i_data])

  freq_sample <- 1/(data$t[2] - data$t[1])

  time <- data[data$marker == markers[1], 2]
  inert <- data[data$marker == markers[1], 3 : 5]
  ref_non_inert <- data[data$marker == markers[2], 3 : 5]

  bf <- butter(order, 1/low_pass)

  for(ax in seq(1, 3)){
    inert[, ax] <- filtfilt(bf, inert[, ax])
    ref_non_inert[, ax] <- filtfilt(bf, ref_non_inert[, ax])
  }

  non_inert <- inert - ref_non_inert

  inert$vx <- c(diff(inert$x)*freq_sample, 0)
  inert$vy <- c(diff(inert$y)*freq_sample, 0)
  inert$vz <- c(diff(inert$z)*freq_sample, 0)
  inert$t <- time
  inert <- inert[-c(length(inert$x)),]

  non_inert$vx <- c(diff(non_inert$x)*freq_sample, 0)
  non_inert$vy <- c(diff(non_inert$y)*freq_sample, 0)
  non_inert$vz <- c(diff(non_inert$z)*freq_sample, 0)
  non_inert$t <- time
  non_inert <- non_inert[-c(length(non_inert$x)),]

  name_file <- stri_sub(data_names[i_data], 1, 10)

  dir_inert <- "./data/processed/filtered_data/inertial/"
  dir_non_inert <- "./data/processed/filtered_data/non_inertial/"

  if (!dir.exists(dir_inert)) {dir.create(dir_inert, recursive = TRUE)}
  if (!dir.exists(dir_non_inert)) {dir.create(dir_non_inert, recursive = TRUE)}

  write.csv(inert, paste(dir_inert, name_file, "_inertial.csv", sep=""), row.names = FALSE)
  write.csv(non_inert, paste(dir_non_inert, name_file, "_non_inertial.csv", sep=""), row.names = FALSE)
}

