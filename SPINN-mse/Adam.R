# Adam

adam <- function(alpha, beta1, beta2,
                 first_moment_1, first_moment_2, first_moment_3,
                 second_moment_1, second_moment_2, second_moment_3,
                 w1, w2, w3, dLw1, dLw2, dLw3, epoch){
  first_moment_1 <- beta1 * first_moment_1 + (1 - beta1) * dLw1
  first_moment_2 <- beta1 * first_moment_2 + (1 - beta1) * dLw2
  first_moment_3 <- beta1 * first_moment_3 + (1 - beta1) * dLw3
  second_moment_1 <- beta2 * second_moment_1 + (1 - beta2) * dLw1 * dLw1
  second_moment_2 <- beta2 * second_moment_2 + (1 - beta2) * dLw2 * dLw2
  second_moment_3 <- beta2 * second_moment_3 + (1 - beta2) * dLw3 * dLw3
  
  first_unbias_1 <- first_moment_1 / (1 - beta1 ** epoch)
  first_unbias_2 <- first_moment_2 / (1 - beta1 ** epoch)
  first_unbias_3 <- first_moment_3 / (1 - beta1 ** epoch)
  second_unbias_1 <- second_moment_1 / (1 - beta2 ** epoch)
  second_unbias_2 <- second_moment_2 / (1 - beta2 ** epoch)
  second_unbias_3 <- second_moment_3 / (1 - beta2 ** epoch)

  w1 <- w1 - alpha * first_unbias_1 / (sqrt(second_unbias_1) + 1e-7)
  w2 <- w2 - alpha * first_unbias_2 / (sqrt(second_unbias_2) + 1e-7)
  w3 <- w3 - alpha * first_unbias_3 / (sqrt(second_unbias_3) + 1e-7)
  
  return(list(w1 = w1, w2 = w2, w3 = w3,
              first_moment_1 = first_moment_1,
              first_moment_2 = first_moment_2,
              first_moment_3 = first_moment_3,
              second_moment_1 = second_moment_1,
              second_moment_2 = second_moment_2,
              second_moment_3 = second_moment_3))
}