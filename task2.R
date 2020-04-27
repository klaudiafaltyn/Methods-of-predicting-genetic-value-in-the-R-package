# Task 2.

heritability2 <- function(h2 = NULL, var_a = NULL, var_p = NULL) {
  if (sum(sapply(list(h2, var_a, var_p), is.null)) > 1) stop("Only one from the values can be unknown")
  if (!is.null(var_a)) if (var_a < 0) stop("Genetic variance can not be less than 0")
  if (!is.null(var_p)) if (var_p < 0) stop("Phenotypic variance can not be less than 0")
  if (!is.null(var_a) & !is.null(var_p)) if (var_p < var_a) stop("Genetic variance can not be bigger than phenotypic variance")
  if (!is.null(h2)) if (h2 < 0 | h2 > 1) stop("Haritability value is [0,1]")

  if (is.null(h2)) h2 = var_a / var_p
  if (is.null(var_a)) var_a = h2 * var_p
  if (is.null(var_p)) var_p = var_a / h2

  return(list("heritability" = h2, "genetic_variance" = var_a, "phenotypic_variance" = var_p))
}
heritability2(var_p = 5, var_a = 1)


