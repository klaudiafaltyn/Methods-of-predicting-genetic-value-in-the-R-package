tab <- read.table("data9.txt", sep=";", header=T)
tab
RelMatrixA <- function(tab) {
  a = tab[, 1]
  s = tab[, 2]
  d = tab[, 3]

  for (el in s) {
    if (!(el %in% a) & !(el == 0)) {
      a <- append(a, el, after = 0)
      s <- append(s, 0, after = 0)
      d <- append(d, 0, after = 0)
    }
  }

  for (el in d) {
    if (!(el %in% a) & !(el == 0)) {
      a <- append(a, el, after = 0)
      s <- append(s, 0, after = 0)
      d <- append(d, 0, after = 0)
    }
  }

  a <- sort(a)
  n = length(a)
  N = n+1
  A = diag(0, N)

   s <- (s == 0) * (N) + s
   d <- (d == 0) * (N) + d

  for (i in 1:n) {
    A[i, i] <- 1 + A[s[i], d[i]] / 2

    for (j in (i + 1):n) {
      if(j>n) break
      A[i, j] = 0.5 * (A[i, s[j]] + A[i, d[j]])

      A[j, i] = A[i, j]
    }
  }
  return(A[1:n, 1:n])
  write.table(RelMatrixA(tab), file = "A.txt")

}
print(RelMatrixA(tab))
