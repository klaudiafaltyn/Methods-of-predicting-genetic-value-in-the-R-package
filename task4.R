# Task 4.

gen_progress <- function(P0, Pr, T, h2 = NULL, var_a = NULL, var_p = NULL) {
  r = (Pr - P0)
  if ((r <= 0)) stop("Pr-P0 must be bigger than 0")
  if (sum(sapply(list(h2, var_a, var_p), is.null)) > 1) stop("Only one from the values can be unknown")
  if (!is.null(var_a) & !is.null(var_p)) if (var_p < var_a) stop("Genetic variance can not be bigger than phenotypic variance")
  if (!is.null(var_a)) if (var_a < 0) stop("Genetic variance can not be less than 0")
  if (!is.null(var_p)) if (var_p < 0) stop("Phenotypic variance can not be less than 0")
  if (!is.null(h2)) if ((h2 < 0) | (h2 > 1)) stop("Haritability value is [0,1]")

  x = heritability2(h2, var_a, var_p)

  if (x$genetic_variance <= 0) stop("Genetic variance can not be less than 0")
  if (x$phenotypic_variance <= 0) stop("Phenotypic variance can not be less than 0")

  h = sqrt(x$heritability)
  a = sqrt(x$genetic_variance)
  s = r / sqrt(x$phenotypic_variance)
  DA = s * h * a
  AP = DA / T
  print(structure(list("standardized selection differential" = s, "genetic progress" = DA, "genetic progress for 1 year" = AP, "heritability" = x$heritability, "genetic_variance" = x$genetic_variance, "phenotypic_variance" = x$phenotypic_variance)))
}
gen_progress(P0 = 148.8, Pr = 157.49, T = 3, var_p = 249.64, var_a = 122.3236)
