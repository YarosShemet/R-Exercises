fit <- glm(am ~ disp+vs+mpg, mtcars, family = "binomial")
log_coef <- fit$coefficients


library("ggplot2")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
obj <- ggplot(data = ToothGrowth, aes(x=supp, y=len, fill=dose))+
  geom_boxplot()


data <- read.csv("D:/Programowanie/R_projekty/Stepik/logit.csv")
fit <- glm(admit ~ rank*gpa, data = data, family = "binomial")
data$admit_new <- ifelse(predict(fit, data, type="response")>0.4, 1, 0)
sum(data$admit_new == 1 & is.na(data$admit))

