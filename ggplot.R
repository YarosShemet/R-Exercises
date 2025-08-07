setwd("D:/Programowanie/R_projekty")

library(ggplot2)
library(datasets)
data(Seatbelts)
dane <- as.data.frame(Seatbelts)
View(dane)


# Wykres I ----------------------------------------------------------------

ggplot(dane, aes(x = DriversKilled, y = front)) + 
  geom_point(color = "darkred") +  
  labs(x = "Kierowcy", y = "Pasażerowie", title = "Stosunek zgonów kierowców do obrażeń pasareżów") + 
  theme_light() +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"))))

# Wykres II ---------------------------------------------------------------

library(dplyr)  
sorted <- dane %>% 
  group_by(DriversKilled) %>% 
  mutate(cnt = n()) %>% 
  select(cnt, DriversKilled, law, drivers) %>% 
  distinct() %>% 
  ungroup() %>% 
  arrange(DriversKilled)
ggplot(sorted, aes(x=drivers, y = DriversKilled, alpha = cnt, color = law)) +
  labs(title = "Zgony kierowców a obowiązkowe pasy bezpieczeństwa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() + 
  geom_smooth(color = "blue")

# Wykres III --------------------------------------------------------------

library(dplyr)
P <- dane %>%
  mutate(cnt = n()) %>% 
  select(VanKilled, cnt, law) %>% 
  distinct() %>% 
  ungroup()
P <- P %>% 
  mutate(Pasy_bezpieczeństwa = case_when(law == 0 ~ "nieobowiązkowe",
                                         TRUE ~ "obowiązkowe"))
Pasy <- dane %>% 
  inner_join(P)
ggplot(Pasy, aes(x = VanKilled, fill = Pasy_bezpieczeństwa)) + 
  labs(title = "Zgony kierowców vanów a obowiązkowe pasy bezpieczeństwa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar()+
  coord_flip()


