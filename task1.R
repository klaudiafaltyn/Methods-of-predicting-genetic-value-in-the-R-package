# Task 1.

heritability1 <- function(var_a, var_p) {
  if (var_a < 0) stop("Genetic variance can not be less than 0")
  if (var_p < 0) stop("Phenotypic variance can not be less than 0")
  if (var_p < var_a) stop("Genetic variance can not be bigger than phenotypic variance")
    h2 = var_a/var_p
      return(h2)
    }
heritability1(1, 5)
