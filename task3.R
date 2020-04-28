# Task 3.

gen_progress <- function(P0, Pr, T, h2, var_a, var_p) {
  r = (Pr - P0)
  if ((r <= 0)) stop("Pr-P0 must be bigger than 0")
  if (var_a < 0) stop("Genetic variance can not be less than 0")
  if (var_p < 0) stop("Phenotypic variance can not be less than 0")
  if (var_p < var_a) stop("Genetic variance can not be bigger than phenotypic variance")
  h = sqrt(h2)
  a = sqrt(var_a)
  s = r / sqrt(var_p)
  DA = s * h * a
  AP = DA / T
  print(structure(list("standardized selection differential" = s, "genetic progress" = DA, "genetic progress for 1 year" = AP)))
}
gen_progress(P0 = 148.8, Pr = 157.49, T = 3, h2 = 0.49, var_a = 122.3236, var_p = 249.64)


