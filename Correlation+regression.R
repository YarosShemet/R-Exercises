x <- c(1, -2, 3, NA, NA)
NA.position <- function(x){
    return(sum(is.na(x) == T))
}
NA.position(x)
x1 <- c(0, NA, NA, -1, NA, -2, 5, -4, -3, 1, -2, 1, 1, -3, -2, 2, 5, -1, 2, -3)
sum_positive <- function(x){
  return(sum(x[which(x > 0)]))
}
sum_positive(x1)
?which

#correlation analysis
library(psych)
corr.calc <- function(x){
  fit <- cor.test(~ x[,1]+x[,2], x) 
  result <- c(fit$estimate, fit$p.value)
  print(result)
}
corr.calc(mtcars[, c(1,5)])

?sapply
filtered.cor <- function(x){
  x <- x[,sapply(x, is.numeric)]
  fit <- corr.test(x)
  diag(fit$r) <- 0
  return(fit$r[which.max(abs(fit$r))])
}
filtered.cor(iris)

smart_cor <- function(x){
  if (shapiro.test(x[[1]])$p.value < 0.05) {
    return(cor.test(~x[[1]]+x[[2]], x, method = "spearman")$estimate)
} else {
    return(cor.test(~x[[1]]+x[[2]], x)$estimate)
    }
}
test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)
test_data[1]
?cor.test



# regresja ----------------------------------------------------------------

data <- read.csv("D:/Programowanie/R_projekty/Stepik/dataset_11508_12.txt", sep = "", head = F, dec = '.')
fit  <- lm(data[[1]] ~ data[[2]], data)
summary(fit)


library(ggplot2)
data <- subset(diamonds, cut == "Ideal" & carat == 0.46)
fit_diamonds <- lm(data$price ~ data$depth, data)
fit_coef <- fit_diamonds$coefficients

regr.calc <- function(x){
  if ((cor.test(~x[,1]+x[,2], x)$p.value) < 0.05) {
    fit_function <- lm(x[[1]] ~ x[[2]], x)
    x$fit <- predict(fit_function, x)
    return(x)
} else {
    return("There is no sense in prediction")
  }
}
regr.calc(iris[,1:2])

library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
                    geom_smooth(method="lm")+
  geom_point()



# MNK ---------------------------------------------------------------------

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(x){
  fit <- lm(x[,3]~x[,1]+x[,2], data = x)
  x$y_full <- ifelse(is.na(x[,3]), predict(fit, x), x[,3])
  return(x)
}
fill_na(test_data)


data <- subset(mtcars[c("wt", "mpg", "disp", "drat", "hp")])
model <- lm(wt ~ mpg+disp+hp, data=data)                  
summary(model)

data <- attitude 
model <- lm(rating ~ complaints*critical, data=data)
summary(model)

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
model <- lm(mpg ~ wt*am, data=mtcars)
summary(model)

library(ggplot2)
mtcars$am <- factor(mtcars$am)

my_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col=am))+
  geom_smooth(method="lm")
