library(dplyr)
library(ggplot2)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

products <- read.csv("products.csv", stringsAsFactors = FALSE) 
colnames(products)

for (i in 1:length(products$length)) {
  products$length[i] = ifelse(length(strsplit(products$length[i], split = " ")[[1]]) <= 2, 
                                         as.numeric(strsplit(products$length[i], split = ' ')[[1]][1]),
                                         as.numeric(paste(strsplit(products$length[i], split = ' ')[[1]][1],strsplit(products$length[i], split = ' ')[[1]][3], sep = '.')))
}
for (i in 1:length(products$width)) {
  products$width[i] = strsplit(products$width[i], split = " ")[[1]][1]
}
products$length = as.numeric(products$length)
products$width = as.numeric(products$width)

#only rated products
rated_products = products %>% filter(., Number_of_reviews > 0)

#rated products ordered by amount of reviews
rated_products %>% arrange(., desc(Number_of_reviews))

#group by primary use
  #CHANGE 'Fishing, Flatwater Kayaking' and 'Flatwater Kayaking, Fishing' to be the same
products = products %>% mutate(., type = ifelse(best_use == 'Fishing, Flatwater Kayaking'| best_use == 'Flatwater Kayaking, Fishing', 'Fishing/Flatwater', 
                                                            ifelse(best_use=='Fishing', 'Fishing', 
                                                                   ifelse(best_use == 'Flatwater Kayaking', 'Flatwater' , 
                                                                          ifelse(best_use == 'Flatwater Kayaking, Whitewater Kayaking', 'Flatwater/Whitewater', 
                                                                                 ifelse(best_use == 'Sea Kayaking', 'Sea', 
                                                                                        ifelse(best_use == 'Whitewater Kayaking', 'Whitewater', NA)))))))

rating_by_use = products %>% filter(., Number_of_reviews > 0) %>% group_by(., type) %>% summarise(., Average_Rating = sum(average_rating)/n(), total = n())

#grouped by price
products_by_price =products %>% 
  mutate(., 'price_category' = ifelse(price >= 1500, 'Over 1500', ifelse((price<1500 & price > 1000), "1000-1500", ifelse((price > 500 & price <= 1000), '500-1000', ifelse(price <500, 'Less than 500', NA)))))

price_by_type = products_by_type %>% group_by(., type) %>% summarize(., average_price = sum(price)/n())

#group by dimensions

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
  geom_bar() + ggtitle("Number of Products by Type") 
a


b = ggplot(products_by_type, aes(x=length, y = width)) +
  geom_point(aes(color = type))
b

c = ggplot(products_by_price, aes(x = price_category)) +
  geom_bar(aes(fill=type), position = 'fill')
c

d = ggplot(price_by_type, aes(x = type, y = average_price)) +
  geom_bar(stat = 'identity')
d

e = ggplot(rating_by_use, aes(x = type, y = Average_Rating)) +
  geom_bar(stat = 'identity')
e
#above graph is just the mean between types of kayaks. I'd be interested in seeing the box plot....
e2 = ggplot(rated_products) +
  geom_boxplot(aes(x = type, y = average_rating))

e2
