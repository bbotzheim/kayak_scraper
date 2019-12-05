library(dplyr)
library(ggplot2)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

products <- read.csv("products.csv", stringsAsFactors = FALSE) 
colnames(products)


#only rated products
rated_products = products %>% filter(., Number_of_reviews > 0)

#rated products ordered by amount of reviews
rated_products %>% arrange(., desc(Number_of_reviews))

#group by primary use
  #CHANGE 'Fishing, Flatwater Kayaking' and 'Flatwater Kayaking, Fishing' to be the same
products_by_type = products %>% mutate(., type = ifelse(best_use == 'Fishing, Flatwater Kayaking'| best_use == 'Flatwater Kayaking, Fishing', 'Fishing/Flatwater', 
                                                            ifelse(best_use=='Fishing', 'Fishing', 
                                                                   ifelse(best_use == 'Flatwater Kayaking', 'Flatwater' , 
                                                                          ifelse(best_use == 'Flatwater Kayaking, Whitewater Kayaking', 'Flatwater/Whitewater', 
                                                                                 ifelse(best_use == 'Sea Kayaking', 'Sea', 
                                                                                        ifelse(best_use == 'Whitewater Kayaking', 'Whitewater', NA)))))))

rating_by_use = products_by_type %>% filter(., Number_of_reviews > 0) %>% group_by(., type) %>% summarise(., Average_Rating = sum(average_rating)/n(), total = n())

#grouped by price
products_by_price =products %>% 
  mutate(., 'price_category' = ifelse(price >= 1500, 'Over 1500', ifelse((price<1500 & price > 1000), "1000-1500", ifelse((price > 500 & price <= 1000), '500-1000', ifelse(price <500, 'Less than 500', NA))))) %>% 
  group_by(., price_category) %>% 
  summarize(., total = n())

price_by_type = products_by_type %>% group_by(., type) %>% summarize(., average_price = sum(price)/n())

#group by dimensions
products_fixed_dims = products

for (i in 1:length(products_fixed_dims$length)) {
  products_fixed_dims$length[i] = ifelse(length(strsplit(products_fixed_dims$length[i], split = " ")[[1]]) <= 2, 
                                         as.numeric(strsplit(products_fixed_dims$length[i], split = ' ')[[1]][1]),
                                         as.numeric(paste(strsplit(products_fixed_dims$length[i], split = ' ')[[1]][1],strsplit(products_fixed_dims$length[i], split = ' ')[[1]][3], sep = '.')))
}
for (i in 1:length(products_fixed_dims$width)) {
  products_fixed_dims$width[i] = strsplit(products_fixed_dims$width[i], split = " ")[[1]][1]
}

products_fixed_dims %>% 
  group_by(., length) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

products_fixed_dims %>% 
  group_by(., width) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

#group by depth

products_with_depth = products_fixed_dims %>% filter(., depth != "" & depth != "Unavailable")
products_with_depth$depth
for (i in 1:length(products_with_depth$depth)) {
  products_with_depth$depth[i] = strsplit(products_with_depth$depth[i], split = " ")[[1]][1]
}

products_with_depth %>% 
  group_by(., depth) %>% 
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


###charts

a = ggplot(products_by_type, aes(x=type)) +
  geom_bar()
a


b = ggplot(products_fixed_dims, aes(x=best_use)) +
  geom_bar(aes(fill = length), position = "fill")
b

c = ggplot(products_by_price, aes(x = price_category, y = total)) +
  geom_bar(stat = 'identity')
c

d = ggplot(price_by_type, aes(x = type, y = average_price)) +
  geom_bar(stat = 'identity')
d
