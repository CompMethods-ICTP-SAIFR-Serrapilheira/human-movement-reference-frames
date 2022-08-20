segment_MED <- function(t, r, v, min_D, min_T, min_V){

  pseudo_0 <- which(abs(v) <= min_V)
  v <- v[-seq(1, pseudo_0[1] - 1)]
  v <- v[-seq(pseudo_0[length(pseudo_0)] + 1, length(v))]

  pk_M_index <- findpeaks(v);                                                   # Checking the positive peaks in the velocity time series
  pk_m_index <- findpeaks(-v);                                                  # Same for negative
  pk_M_index <- pk_M_index[, 2];                                                 # Selecting only the index of the peak
  pk_m_index <- pk_m_index[, 2];

  pk_index <- append(pk_M_index, pk_m_index)                                    # Joining the indices into a single vector and sorting
  pk_index <- sort(pk_index)

  pk_class <- rep(NA, length(pk_index))                                         # Creating a vector to classify the peaks

  pk_class[v[pk_index] > min_V] = 1
  pk_class[v[pk_index] < -min_V] = -1
  pk_class[abs(v[pk_index]) < min_V] = 0

  end_loop = length(pk_class)
  i = 1
  while(T){
    if(i == end_loop){
      break
    }

    if(pk_class[i] - pk_class[i + 1] == -2 | pk_class[i] - pk_class[i + 1] == 2){
      new_0 <- which.min(abs(v[pk_index[i] : pk_index[i + 1]]))
      new_0 <- new_0 + pk_index[i] - 1
      pk_index <- append(pk_index, new_0, after = i)
      pk_class <- append(pk_class, 0, after = i)
      end_loop = end_loop + 1
    }
    i = i + 1
  }

  pk_class <- abs(pk_class)
  pk_class <- c(0, pk_class, 0)
  pk_index <- c(1, pk_index, length(v))

  seg_class <- diff(pk_class)

  seg_i_class <- which(seg_class == 1)
  seg_f_class <- which(seg_class == -1) + 1

  seg_i <- pk_index[seg_i_class]
  seg_f <- pk_index[seg_f_class]

  for(i in seq(1, length(seg_i))){

    displacement = r[seg_f[i]] - r[seg_i[i]]
    duration = t[seg_f[i]] - t[seg_i[i]]

    if(abs(displacement) < min_D | abs(duration) < min_T){
      seg_i[i] <- NA
      seg_f[i] <- NA
    }
  }

  seg_i <- seg_i + pseudo_0[1] - 1
  seg_f <- seg_f + pseudo_0[1] - 1

  segments <- data.frame(seg_i,seg_f)
  segments <- na.omit(segments)

  return(segments)
}
