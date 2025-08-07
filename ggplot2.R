setwd("D:/Programowanie/R_projekty")
library(ggplot2)

# Wykres I ----------------------------------------------------------------

data(mtcars)  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)

div_bars <- ggplot(mtcars, aes(mpg_z, `car name`, fill = mpg_type)) +  
  geom_bar(stat = "identity")+
  theme_light()+
  #scale_fill_manual(values = c("darkgreen", "red"))+
  scale_fill_manual(name = "Mileage", labels = c("Above average", "Below average"), values = c("darkgreen", "red"))+
  labs(title= 'Diverging bars', subtitle = 'Normalised mileage from mtcars')
div_bars

# Wykres II ---------------------------------------------------------------

library(lubridate)
data("economics", package = "ggplot2")
economics$returns_perc <- c(0,diff(economics$psavert)/economics$psavert[-length(economics$psavert)])
economics_m <- economics[1:24, ]

time_series <- ggplot(economics_m, aes(date, returns_perc)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") +
  theme_light() + 
  labs(x = "date", y = "Returns %", title = "Monthly Time Series", subtitle = "Returns Percentage from Economics Dataset", caption = "Source: Economics")+
  theme(axis.text.x = element_text(angle = 90))
time_series
  

                            