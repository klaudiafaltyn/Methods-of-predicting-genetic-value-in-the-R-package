# Task 7.

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
milkgene$fat_perc = (milkgene$fat / milkgene$milk) * 100
milkgene$fat_perc
str(milkgene)

# e)
write.table(milkgene, file = "milkgene1.txt")
