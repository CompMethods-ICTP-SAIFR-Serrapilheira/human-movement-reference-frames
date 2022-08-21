# ------------------------------------------------------------------------------
# Verification of the referential used by motor control in reaching movements
#
# Description: This code will apply the movement element decomposition method
# on the reaching movement data in the two frames of reference (inertial and
# non-inertial) and then calculate the W and R^2 indices by comparing the
# movement elements obtained with the theoretical curve , in order to verify
# which of the two references can best represent the optimization process
# that the motor control performs.
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 18/08/2022
# ------------------------------------------------------------------------------


# Necessary packages -----------------------------------------------------------

#install.packages('pracma')
library(pracma)
library(stringi)

# Loading of necessary functions -----------------------------------------------

source('fct/segment_MED.R')
source('fct/analyse_elements_MED.R')
source('fct/fit_MED.R')

# Configuring ------------------------------------------------------------------

path_inertial <- list.files(path = "data/processed/filtered_data/inertial/",    # List processed data paths
                            pattern = "csv$",
                            recursive = TRUE,
                            full.names = TRUE)

path_n_inertial <- list.files(path = "data/processed/filtered_data/non_inertial",
                              pattern = "csv$",
                              recursive = TRUE,
                              full.names = TRUE)

min_D <- 0.003                                                                  # Minimum displacement threshold
min_T <- 0.1                                                                    # Minimum duration threshold
min_V <- 0.01                                                                   # Minimum velocity threshold


W_elements <- c()                                                               # Vectors to accumulating the indices
R2_elements <- c()
referential_elements <- c()
name_output <- c()

W_mean_inertial <- c()
W_mean_n_inertial <- c()
R2_mean_inertial <- c()
R2_mean_n_inertial <- c()
name_mean_output <- c()
ind_output <- c()

# Performing the movement element decomposition method for each data file ------

for(i_data in seq(1, length(path_inertial))){                                   # Loop that goes through all the files

  data_inertial <- read.csv(path_inertial[i_data])                              # Reading the filtered files
  data_n_inertial <- read.csv(path_n_inertial[i_data])

  for(referential in c(1, 2)){                                                  # Loop performing the process for the referential = 1 (inertial) // = 2 (non-inertial)

    if(referential == 1){                                                       # Turning the file columns into vectors
      x <- data_inertial$x
      y <- data_inertial$y
      z <- data_inertial$z
      vx <- data_inertial$vx
      vy <- data_inertial$vy
      vz <- data_inertial$vz
      t <- data_inertial$t
    }
    else{
      x <- data_n_inertial$x
      y <- data_n_inertial$y
      z <- data_n_inertial$z
      vx <- data_n_inertial$vx
      vy <- data_n_inertial$vy
      vz <- data_n_inertial$vz
      t <- data_n_inertial$t
    }

    x_segments <- segment_MED(t, x, vx, min_D, min_T, min_V)                    # Segmenting the motion elements on each axis
    y_segments <- segment_MED(t, y, vy, min_D, min_T, min_V)
    z_segments <- segment_MED(t, z, vz, min_D, min_T, min_V)

    x_indices <- analyze_elements_MED(t, x, vx, x_segments)                     # Calculating MED indices for each axis
    y_indices <- analyze_elements_MED(t, y, vy, y_segments)
    z_indices <- analyze_elements_MED(t, z, vz, z_segments)

    W <- c(x_indices$W, y_indices$W, z_indices$W)                               # Concatenating the indices of the movement elements of each axis
    R2 <- c(x_indices$R2, y_indices$R2, z_indices$R2)

    W_mean <- mean(W)                                                           # Calculating the average indices
    R2_mean <- mean(R2)

    if(referential == 1){                                                       # Accumulating the indices for later outputs
      W_elements <- append(W_elements, W)
      R2_elements <- append(R2_elements, R2)
      referential_elements <- append(referential_elements,
                                     rep(1, length(W)))

      W_mean_inertial <- append(W_mean_inertial, W_mean)
      R2_mean_inertial <- append(R2_mean_inertial, R2_mean)
    }
    else{
      W_elements <- append(W_elements, W)
      R2_elements <- append(R2_elements, R2)
      referential_elements <- append(referential_elements,
                                     rep(2, length(W)))

      W_mean_n_inertial <- append(W_mean_n_inertial, W_mean)
      R2_mean_n_inertial <- append(R2_mean_n_inertial, R2_mean)
    }
    name_output <- append(name_output,                                          # Concatenating the name of the file to use in the output table
                          rep(stri_sub(basename(path_inertial[i_data]), 1, 10),
                              length(W)))

    ind_output <- append(ind_output,
                         rep(stri_sub(basename(path_inertial[i_data]), 10, 10),
                                        length(W)))
  }
}

W_mean_diff <- W_mean_inertial - W_mean_n_inertial                              # Calculating the paired difference between the referential
R2_mean_diff <- R2_mean_inertial - R2_mean_n_inertial

# Saving outputs ---------------------------------------------------------------

name_mean_output <- stri_sub(basename(path_inertial), 1, 10)                    # Vector with the name of the files to use in the output "mean"

output <- data.frame(name_output, ind_output, referential_elements,             # Data frame with indices output
                     W_elements, R2_elements)

output_mean <- data.frame(name_mean_output, W_mean_inertial, R2_mean_inertial,  # Data frame with mean indices output
                          W_mean_n_inertial, R2_mean_n_inertial,
                          W_mean_diff, R2_mean_diff)

write.csv(output, paste("./output/indices_MED.csv",                             # Writing output
                             sep = ""), row.names = FALSE)

write.csv(output_mean, paste("./output/mean_indices_MED.csv",
                             sep = ""), row.names = FALSE)
