# Zadanie 5.

# a)
A1 = matrix(c(1,2,5,-3,3,1),2,3)
A2 = matrix(c(2,-1,3,-3,4,-1,5,-2,1),3,3)
A1%*%A2

# b)
B = matrix(c(-1,3,-1,5,-2,3,4,0,6),3,3)
det(B)

# c)
C = matrix(c(1,2,1,2,3,-1,0,0,1),3,3)
print(C)
solve(C)


# Zadanie 6.

# a)
diag(3,10)

# b)
diag(1:13, 13)

# c)
D = matrix(c(2,0,0,-1,0,0,2,0,0,0,0,1,4,1,0,0),4,4)
print(D)
czy_nieosobliwa <- function(D){
  if (det(D) == 0) stop("Macierz jest osobliwa")
  if (det(D) !=0) det(D)
}
czy_nieosobliwa(D)

# Zadanie 7.

install.packages("PBImisc")
library(PBImisc)
data(milkgene)
milkgene

# a)
dim(milkgene)
str(milkgene)

# b)
table(milkgene$lactation)

# c), d)
milkgene$fat_perc = (milkgene$fat/milkgene$milk)*100
milkgene$fat_perc
str(milkgene)

# e)
write.table(milkgene, file="milkgene1.txt")

