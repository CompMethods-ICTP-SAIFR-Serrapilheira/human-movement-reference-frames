analyze_elements_MED <- function(t, r, v, segments){

  N <- length(segments$seg_i)
  D <- c()
  V <- c()
  Dur <- c()
  W <- c()
  R2 <- c()

  if(N == 0){
    return(data.frame(D, V, Dur, W, R2))
  }

  for(i in seq(1,N)){
    D <- append(D, r[segments$seg_f[i]] - r[segments$seg_i[i]])
    V <- append(V, mean(v[segments$seg_i[i] : segments$seg_f[i]]))
    Dur <- append(Dur, t[segments$seg_f[i]] - t[segments$seg_i[i]])

    fit <- fit_MED(t[segments$seg_i[i] : segments$seg_f[i]],
                           v[segments$seg_i[i] : segments$seg_f[i]])

    W <- append(W, as.numeric(fit[1]))
    R2 <- append(R2, as.numeric(fit[2]))
  }

  return(data.frame(D, V, Dur, W, R2))
}
