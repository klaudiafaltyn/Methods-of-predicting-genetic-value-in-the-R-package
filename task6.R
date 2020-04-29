# Task 6.

# a)
diag(3, 10)

# b)
diag(1:13, 13)

# c)
D = matrix(c(2, 0, 0, -1, 0, 0, 2, 0, 0, 0, 0, 1, 4, 1, 0, 0), 4, 4)
print(D)
if_singular <- function(D) {
  if (det(D) == 0) stop("Matrix is singular")
  if (det(D) != 0) det(D)
  }
if_singular(D)