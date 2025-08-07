
# Zadania domowe 1 --------------------------------------------------------

# 1
setwd("D:/Программирование/R_projekty")
# 2
a<- c(1, 9, 12, 3)
b<- c(21, 3, 422, 5)
c<- c(10, 2, 14, 16)
d<- c(100.25, 9.5, 125.75, 140)
# 3
sum(a)
sum(b)
sum(c)
sum(d)
mean(a)
mean(b)
mean(c)
mean(d)
median(a)
median(b)
median(c)
median(d)
# 4
macierz1 <- matrix(data=a, nrow=1, ncol=4)
macierz1 <- rbind(macierz1, c)
print(macierz1)

macierz2 <- matrix(data=b, nrow=1, ncol=4)
macierz2 <- rbind(macierz2, d)
print(macierz2)
# 5
sort(b, decreasing = TRUE)
# 6
which(c < 10)
# 7
e <- c("Ala", "ma")
f <- c("super", "kotka")
# 8
class(e)
# 9
g <- c(e, f)
# 10
x1 <- 1:20
x2 <- letters[1:20]
x3 <- rnorm(20)
data_frame1 <- data.frame(x1,x2,x3)

View(data_frame1)
