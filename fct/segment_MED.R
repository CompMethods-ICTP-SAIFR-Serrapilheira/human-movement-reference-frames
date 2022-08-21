segment_MED <- function(t, r, v, min_D, min_T, min_V){

  # Description ----------------------------------------------------------------
  #
  # This function calculates the coefficient of determination (R^2) and the
  # similarity index (W) between the motion element and the theoretical curve
  # described by Flash and Hogan, 1985 and Hoff, 1994.
  #
  # Inputs:
  #   t = vector with the time of each frame of the data (double vector)
  #   r = time series of the position on one axis (double vector)
  #   v = time series of the velocity (double vector)
  #   min_D = minimum displacement of a movement element (double)
  #   min_T = minimum duration of a movement element (double)
  #   min_V = instrumental limit error (ILE) of the velocity (double)
  #
  # Output:
  #   segment = list vector with two columns, seg_i and seg_f, that represents
  #   the beginning and the end of each movement element
  #
  # Author: Mateus Souza Silva
  # Date: 20/08/2022
  # ----------------------------------------------------------------------------


  pseudo_0 <- which(abs(v) <= min_V)                                            # Makes the beginning and end of the velocity time series to be at points with velocities lower than the ILE
  v <- v[-seq(1, pseudo_0[1] - 1)]
  v <- v[-seq(pseudo_0[length(pseudo_0)] + 1, length(v))]

  pk_M_index <- findpeaks(v);                                                   # Checking the positive peaks in the velocity time series
  pk_m_index <- findpeaks(-v);                                                  # Same for negative peaks (i.e. valleys)
  pk_M_index <- pk_M_index[, 2];                                                # Selecting only the index of the peak
  pk_m_index <- pk_m_index[, 2];

  pk_index <- append(pk_M_index, pk_m_index)                                    # Joining the indices into a single vector and sorting
  pk_index <- sort(pk_index)

  pk_class <- rep(NA, length(pk_index))                                         # Creating a vector to classify the peaks

  pk_class[v[pk_index] > min_V] = 1                                             # Classifies peaks as 1, -1 and 0 depending on how fast they occur
  pk_class[v[pk_index] < -min_V] = -1
  pk_class[abs(v[pk_index]) < min_V] = 0

  end_loop = length(pk_class)
  i = 1
  while(T){                                                                     # Loop to include "0" class peaks when there's a transition between a +1 peak to -1, or the opposite. (-1 to +1)
    if(i == end_loop){                                                          # The reason is that we'll the zero peaks to cut the elements
      break                                                                     # and if there's a transition without a peak passing through 0, we need to create one in the index of the value closes to 0 between these two peaks
    }

    if(pk_class[i] - pk_class[i + 1] == -2 |                                    # Verify if there's two peaks occurring without a peak passing through the zero zone, i.e. between - min_V and + min_V
       pk_class[i] - pk_class[i + 1] == 2){
      new_0 <- which.min(abs(v[pk_index[i] : pk_index[i + 1]]))                 # Find the index of the velocity with the closest value to 0
      new_0 <- new_0 + pk_index[i] - 1
      pk_index <- append(pk_index, new_0, after = i)                            # Include this index between the two peaks
      pk_class <- append(pk_class, 0, after = i)
      end_loop = end_loop + 1
    }
    i = i + 1
  }

  pk_class <- abs(pk_class)                                                     # Changes the -1 classification to +1 because now we are interested only in the change from a 0 peak to a non-zero
  pk_class <- c(0, pk_class, 0)                                                 # Include the beginning and end of the time series as artificial peaks,
  pk_index <- c(1, pk_index, length(v))                                         # because we made, in the beginning of this function, that V would start and end with absolute values < min_V

  seg_class <- diff(pk_class)                                                   # Creates a vector that says when there's a change from a zero peak to a non-zero peak

  seg_i_class <- which(seg_class == 1)                                          # seg_i_class says which of the peaks corresponds to changes from a zero peak to a non-zero, which means a beginning of a element
  seg_f_class <- which(seg_class == -1) + 1                                     # same but for the end of a element

  seg_i <- pk_index[seg_i_class]                                                # seg_i and seg_f says what are the indexes of v that corresponds to a beginning and end of a element
  seg_f <- pk_index[seg_f_class]

  for(i in seq(1, length(seg_i))){                                              # Loop to verify valid elements, i.e. elements with displacement and duration > min_D and min_T

    displacement = r[seg_f[i]] - r[seg_i[i]]
    duration = t[seg_f[i]] - t[seg_i[i]]

    if(abs(displacement) < min_D | abs(duration) < min_T){                      # If the i-element don't have the minimum displacement or duration, make the seg_i and seg_f = NaN to not consider it in the analysis
      seg_i[i] <- NA
      seg_f[i] <- NA
    }
  }

  seg_i <- seg_i + pseudo_0[1] - 1                                              # Correct the index of seg_i and seg_f in accord with the number of frames that were
  seg_f <- seg_f + pseudo_0[1] - 1                                              # cut from the velocity time series in the beginning of the function

  segments <- data.frame(seg_i,seg_f)
  segments <- na.omit(segments)

  return(segments)
}
