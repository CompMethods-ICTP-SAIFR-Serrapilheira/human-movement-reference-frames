fit_MED <- function(t, v){
  v_mean <- mean(v)

  t_func <- seq(0, 1, length.out = length(t))

  analytic <- v_mean*30*((t_func^4) - 2*(t_func^3) + (t_func^2))

  dv <- analytic - v

  W <- std(dv)/abs(v_mean)

  R2 <- cor(v, analytic)^2

  return(list(W, R2))
}
