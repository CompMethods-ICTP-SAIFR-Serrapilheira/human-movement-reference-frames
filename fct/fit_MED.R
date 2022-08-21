fit_MED <- function(t, v){

  # Description ----------------------------------------------------------------
  #
  # This function calculates the coefficient of determination (R^2) and the
  # similarity index (W) between the motion element and the theoretical curve
  # (described by Flash and Hogan, 1985 and Hoff, 1994).
  #
  # Input:
  #   t = time series of a movement element  (double vector)
  #   v = time series of the velocity of a movement element (double vector)
  #
  # Output:
  #   W = Similarity index of the element to the theoretical curve
  #   R2 = Calculated coefficient of determination between the element and
  #        the theoretical curve
  #
  # References:
  #   FLASH, T.; HOGAN, N. The coordination of arm movements: an experimentally
  #   confirmed mathematical model.
  #   Journal of neuroscience, v. 5, n. 7, p. 1688-1703, 1985.
  #   ⟨https://doi.org/10.1523/JNEUROSCI.05-07-01688.1985⟩
  #
  #   HOFF, B. A model of duration in normal and perturbed reaching movement.
  #   Biological Cybernetics, v. 71, n. 6, p. 481-488, 1994.
  #   ⟨https://doi.org/10.1007/BF00198466⟩
  #
  # Author: Mateus Souza Silva
  # Date: 20/08/2022
  # ----------------------------------------------------------------------------


  v_mean <- mean(v)

  t_func <- seq(0, 1, length.out = length(t))                                   # Create a time vector that will be used to make the theoretical curve

  analytic <- v_mean*30*((t_func^4) - 2*(t_func^3) + (t_func^2))                # Create the theoretical curve given the function described in the references, this curve has the same v_mean and displacement that the experimental element

  dv <- analytic - v                                                            # Calculate the difference of velocity between the theoretical and the element

  W <- std(dv)/abs(v_mean)                                                      # Calculate the similarity index (W)

  R2 <- cor(v, analytic)^2                                                      # Calculate the coefficient of determination (R^2)

  return(list(W, R2))
}
