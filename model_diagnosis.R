my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(log(my_vector))

#standaryzowanie 
beta.coef <- function(x){
  result <- lm(scale(x[1]) ~ scale(x[2]),x)
  return(result$coefficients)
}
beta.coef(mtcars[,c(1,3)])

normality.test  <- function(x){
  result <- c(shapiro.test(x[[1]])$p.value)
  for (i in 1:length(x)) {
    result <- append(result, shapiro.test(x[[i]])$p.value)
  }
  result <- result[2:length(result)]
  names(result) <- c(colnames(x))
  return(result)
}
normality.test(mtcars[,1:6])

for (i in 1:length(mtcars[,1:6])) {
  result <- append(result, shapiro.test((mtcars[,1:6])[[i]]))$p.value
}

#odbiór modeli
model_full <- lm(rating ~ ., data = attitude) 

model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)

ideal_model <- step(model_full, scope)
summary(ideal_model)

ideal_model <- step(model_full, direction="backward")

anova(model_full, ideal_model)

model <- step((lm(sr ~ (.)*(.), LifeCycleSavings)), direction="backward")
summary(model)

rm(list = ls())

data <- read.csv("D:/Programowanie/R_projekty/Stepik/homosc.csv")
library(gvlma)
x <- gvlma(DV ~ IV, data = data)
summary(x)

library(ggplot2)
resid.norm  <- function(fit){
  if (shapiro.test(fit$residuals)$p.value<0.05){
    plot <- ggplot(as.data.frame(fit$model), aes(x=fit$residuals))+
      geom_histogram(fill = "red")
  } else {
    plot<- ggplot(as.data.frame(fit$model), aes(x=fit$residuals))+
      geom_histogram(fill = "green")
  }
  return(plot)
}
resid.norm(lm(mpg ~ disp, mtcars))


high.corr <- function(x){
  fit <- corr.test(x)
  diag(fit$r) <- 0
  rownames(which(fit$r == fit$r[which.max(abs(fit$r))], arr.ind = TRUE))
}
high.corr(test_data)
test_data <- as.data.frame(list(V1 = c(-0.2, 1.2, -1.6, -0.1, 0.5), V2 = c(1.1, 1.8, 0.1, -0.5, 0.5), V3 = c(1.9, 0.2, -0.4, -1.8, 1.1), V4 = c(-0.1, -0.5, -0.2, -0.1, -1.5), V5 = c(-1.6, 1.3, -0.4, -0.6, -0.6), V6 = c(-0.2, -0.7, 1.1, 0.9, 0.1), V7 = c(-2, 0.4, 1.3, -0.9, 0.9), V8 = c(1.5, -0.1, 0.8, -0.1, -0.3), V9 = c(0.1, 0.3, 1.7, 2, -0.4), V10 = c(1.6, -0.7, -1.8, 0.2, -0.1), V11 = c(-2, 0.4, 1.3, -0.9, 0.9)))
