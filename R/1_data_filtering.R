# ------------------------------------------------------------------------------
# Verification of the referential used by motor control in reaching movements
#
# Description: This code will process the reaching movement data,
#   filtering the data with a low-pass filter and calculating the
#   trajectories and velocities of the markers
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 18/08/2022
# ------------------------------------------------------------------------------


# Necessary packages -----------------------------------------------------------

#install.packages("stringi")
library(stringi)
library(signal)

# Configuring ------------------------------------------------------------------

low_pass <- 10                                                                  # Low pass filter frequency
order <- 4                                                                      # Low pass filter order
R_markers <- c("RHMC2", "RUSP")                                                 # Markers for right-arm movements
L_markers <- c("LHMC2", "LUSP")                                                 # Markers for left-arm movements

data_path <- list.files(path = "data/raw/csv",                                  # List all .csv data paths
                        pattern = "csv$",
                        recursive = TRUE,
                        full.names = TRUE)

data_names <- gsub(".csv", "", basename(data_path), fixed = TRUE)               # List all data names

data_laterality <- stri_sub(data_names, -3, -3)                                 # Checking the laterality of the movement ("R" = right, "L" = left)

number_data <- length(data_names)


# Loop that goes through all the files -----------------------------------------

for(i_data in seq(1, number_data)){

  # Reading file and creating the vectors --------------------------------------

  data <- read.csv(data_path[i_data])

  if(data_laterality[i_data] == "R"){                                           # Selecting the marker corresponding to the laterality of i_data
    markers <- R_markers
  }
  else{
    markers <- L_markers
  }

  time <- data[data$marker == markers[1], 2]                                    # Time column
  inert <- data[data$marker == markers[1], 3 : 5]                               # Data of movement with inertial referential (laboratory)
  ref_non_inert <- data[data$marker == markers[2], 3 : 5]                       # Data of movement of the non-inertial referential

  freq_sample <- 1/(data$t[2] - data$t[1])                                      # Calculating the data capture frequency

  # Filtration process ---------------------------------------------------------

  bf <- butter(order, 1/low_pass)                                               # Low-pass filter process
  for(ax in seq(1, 3)){
    inert[, ax] <- filtfilt(bf, inert[, ax])
    ref_non_inert[, ax] <- filtfilt(bf, ref_non_inert[, ax])
  }

  non_inert <- inert - ref_non_inert                                            # Data of movement with non-inertial referential (i.e. inertial subtracted by the non-inertial referential)

  # Calculating velocities and filling data frames -----------------------------

  inert$vx <- c(diff(inert$x)*freq_sample, 0)                                   # Calculating the velocity through the variation of the position divided by the variation of time
  inert$vy <- c(diff(inert$y)*freq_sample, 0)
  inert$vz <- c(diff(inert$z)*freq_sample, 0)
  inert$t <- time
  inert <- inert[freq_sample : (length(inert$x) - freq_sample), ]               # Excludes the beginning and end of the data due to artifacts that happen sometimes,
                                                                                # we default to exclude the first and end half seconds, as they correspond to few data, and are sufficient to take of these artifacts
  non_inert$vx <- c(diff(non_inert$x)*freq_sample, 0)
  non_inert$vy <- c(diff(non_inert$y)*freq_sample, 0)
  non_inert$vz <- c(diff(non_inert$z)*freq_sample, 0)
  non_inert$t <- time
  non_inert <- non_inert[freq_sample : (length(non_inert$x) - freq_sample), ]

  # Saving Files ---------------------------------------------------------------

  name_file <- stri_sub(data_names[i_data], 1, 10)                              # Setting the name of the filtered file
  dir_inert <- "./data/processed/filtered_data/inertial/"                       # Directories where the files will be saved
  dir_non_inert <- "./data/processed/filtered_data/non_inertial/"

  if (!dir.exists(dir_inert)) {dir.create(dir_inert, recursive = TRUE)}         # Creating the directories
  if (!dir.exists(dir_non_inert)) {dir.create(dir_non_inert, recursive = TRUE)}

  write.csv(inert, paste(dir_inert, name_file, "_inertial.csv",                 # Writing the files
                         sep = ""), row.names = FALSE)
  write.csv(non_inert, paste(dir_non_inert, name_file, "_non_inertial.csv",
                             sep = ""), row.names = FALSE)
}
