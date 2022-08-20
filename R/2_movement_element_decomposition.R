# ------------------------------------------------------------------------------
# Verification of the referential used by motor control in reaching movements
#
# Description: this code will process the range motion data, filtering the data
#   with a low-pass filter and calculating the trajectories and
#   velocities of the markers
#
# Database: The database used in this study is available at
#   https://doi.org/10.1016/j.dib.2018.05.088
#
# Author: Mateus Souza Silva
# Date: 18/08/2022


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

path_non_inertial <- list.files(path = "data/processed/filtered_data/non_inertial",
                                pattern = "csv$",
                                recursive = TRUE,
                                full.names = TRUE)

min_D <- 0.003                                                                  # Minimum displacement threshold
min_T <- 0.1                                                                    # Minimum duration threshold
min_V <- 0.01                                                                   # Minimum velocity threshold

W_inertial <- c()
W_non_inertial <- c()
R2_inertial <- c()
R2_non_inertial <- c()
name_output <- c()

for(i_data in seq(1,length(path_inertial))){

  data_inertial <- read.csv(path_inertial[i_data])
  data_non_inertial <- read.csv(path_non_inertial[i_data])

  for(referential in c(1,2)){

    if(referential == 1){
      x <- data_inertial$x
      y <- data_inertial$y
      z <- data_inertial$z
      vx <- data_inertial$vx
      vy <- data_inertial$vy
      vz <- data_inertial$vz
      t <- data_inertial$t
    }
    else{
      x <- data_non_inertial$x
      y <- data_non_inertial$y
      z <- data_non_inertial$z
      vx <- data_non_inertial$vx
      vy <- data_non_inertial$vy
      vz <- data_non_inertial$vz
      t <- data_non_inertial$t
    }

    x_segments <- segment_MED(t, x, vx, min_D, min_T, min_V)
    y_segments <- segment_MED(t, y, vy, min_D, min_T, min_V)
    z_segments <- segment_MED(t, z, vz, min_D, min_T, min_V)

    x_indices <- analyze_elements_MED(t, x, vx, x_segments)
    y_indices <- analyze_elements_MED(t, y, vy, y_segments)
    z_indices <- analyze_elements_MED(t, z, vz, z_segments)

    W_mean <- mean(c(x_indices$W, y_indices$W, z_indices$W))
    R2_mean <- mean(c(x_indices$R2, y_indices$R2, z_indices$R2))

    if(referential == 1){
      W_inertial <- append(W_inertial, W_mean)
      R2_inertial <- append(R2_inertial, R2_mean)
    }
    else{
      W_non_inertial <- append(W_non_inertial, W_mean)
      R2_non_inertial <- append(R2_non_inertial, R2_mean)
    }
  }
}

W_diff <- W_inertial - W_non_inertial
R2_diff <- R2_inertial - R2_non_inertial

name_output <- stri_sub(basename(path_inertial), 1, 10)

output <- data.frame(name_output, W_inertial, R2_inertial,
                     W_non_inertial, R2_non_inertial, W_diff, R2_diff)

write.csv(output, paste("./output/indices_MED.csv",
                           sep=""), row.names = FALSE)

