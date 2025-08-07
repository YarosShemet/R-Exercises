
# Zadanie 1 ---------------------------------------------------------------

setwd("D:/Programowanie/R_projekty/Domowe")

data <- read.csv("D:/Programowanie/R_projekty/Warsztaty/beer_reviews.csv",sep = ",",  dec = ".")

beers <- data %>% 
  select(contains("beer"), brewery_id ) %>% 
  distinct() %>% 
  arrange(beer_beerid)

breweries <- data %>% 
  select(contains("brewery")) %>% distinct() %>%
  mutate(brewery_name = replace(brewery_name, brewery_name == "", NA)) %>%
  arrange(brewery_id)

reviews_raw <- data %>%
  select(beer_beerid, contains("review"))

reviews <- merge(reviews_raw, beers, by="beer_beerid") %>% merge(., breweries, by="brewery_id")

reviews %>%
  select(contains("review")) %>% 
  #replace(?, 3.5) %>% 
  replace(is.na(.), 0)

# Zadanie 2 ---------------------------------------------------------------
data_selected_final <- data %>% 
  group_by(brewery_name) %>% 
  mutate(Median_review_overall = mean(review_overall)) %>% 
  arrange(desc(Median_review_overall)) %>% 
  ungroup()

#?head(data_selected_final, n=10)
head(data_selected_final$brewery_name, n=10)


# Zadanie 3 ---------------------------------------------------------------

wide <- data_selected_final %>% select(-review_overall, -Median_review_overall)

wide_to_long <- gather(wide, key="review_parameter", value=rating, review_aroma:beer_abv) %>% as.data.frame()

long_to_wide <- spread(wide, key="review_parameter", value=rating, review_aroma:beer_abv) %>% as.data.frame()





  


