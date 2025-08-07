library(plotly)

# Wykres I ----------------------------------------------------------------



# Wykres II ---------------------------------------------------------------

library(dplyr)
dane <- as.data.frame(mtcars)
N_dane <- dane %>% 
  mutate(cnt = n()) %>% 
  select(cnt, cyl)
wd <- getwd()
number_of_vehicles <- plot_ly(
  x = N_dane$cyl,
  y = N_dane$cnt,
  name = "mtcars",
  type = "bar"
)
.libPaths()
getwd()
