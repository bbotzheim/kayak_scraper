library(dplyr)

products <- read.csv("products.csv", stringsAsFactors = FALSE) 
colnames(products)

#only rated products
rated_products = products %>% filter(., Number_of_reviews > 0)

#rated products ordered by amount of reviews
rated_products %>% filter(., average_rating >= 4) %>% arrange(., desc(Number_of_reviews))

#group by primary use
products %>% group_by(., best_use) %>% summarise(., total = n())

#grouped by price
products %>% 
  mutate(., 'price_category' = ifelse(price >= 1500, 'Over 1500', ifelse((price<1500 & price > 1000), "1000-1500", ifelse((price > 500 & price <= 1000), '500-1000', ifelse(price <500, 'Less than 500', NA))))) %>% 
  group_by(., price_category) %>% 
  summarize(., total = n())

#group by length
products %>% 
  group_by(., length) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

#group by width
products %>% 
  group_by(., width) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))


#misc
products %>% 
  group_by(., tracking_system) %>% 
  summarize(., total = n())

products %>% 
  group_by(., material) %>% 
  summarize(., total = n())

products %>% 
  group_by(., weight) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

