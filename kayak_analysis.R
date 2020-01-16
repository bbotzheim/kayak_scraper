library(dplyr)
library(ggplot2)
library(RColorBrewer) 
library(ggthemes)
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


#CHANGE 'Fishing, Flatwater Kayaking' and 'Flatwater Kayaking, Fishing' to be the same
products = products %>% mutate(., type = ifelse(best_use == 'Fishing, Flatwater Kayaking'| best_use == 'Flatwater Kayaking, Fishing', 'Fishing/Flatwater', 
                                                ifelse(best_use=='Fishing', 'Fishing', 
                                                       ifelse(best_use == 'Flatwater Kayaking', 'Flatwater' , 
                                                              ifelse(best_use == 'Flatwater Kayaking, Whitewater Kayaking', 'Flatwater/Whitewater', 
                                                                     ifelse(best_use == 'Sea Kayaking', 'Sea', 
                                                                            ifelse(best_use == 'Whitewater Kayaking', 'Whitewater', NA)))))))

#grouped by price
products = products %>% 
  mutate(., 'price_category' = ifelse(price >= 1500, 'Over 1500', 
                                      ifelse((price<1500 & price > 1000), "1000-1500", 
                                             ifelse((price > 500 & price <= 1000), '500-1000', 
                                                    ifelse(price <500, 'Less than 500', NA)))))

#only rated products
rated_products = products %>% filter(., Number_of_reviews > 0)


#group by primary use
rating_by_use = rated_products %>% group_by(., type) %>% summarise(., Average_Rating = sum(average_rating)/n(), total = n())

price_by_type = products %>% group_by(., type) %>% summarize(., average_price = sum(price)/n())

#group by dimensions

products %>% 
  group_by(., length) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

products %>% 
  group_by(., width) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

#group by depth

products_with_depth = products %>% filter(., depth != "" & depth != "Unavailable")
products_with_depth$depth
for (i in 1:length(products_with_depth$depth)) {
  products_with_depth$depth[i] = strsplit(products_with_depth$depth[i], split = " ")[[1]][1]
}

products_with_depth %>% 
  group_by(., depth) %>% 
  summarize(., total = n()) %>% 
  arrange(., desc(total))

products_with_depth$depth = as.numeric(products_with_depth$width)

#misc

rated_products %>% arrange(., desc(Number_of_reviews))

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

# Total Products by Type
a = ggplot(products, aes(x=type)) +
  geom_bar(aes(fill = type), show.legend = FALSE) + 
  ggtitle("Total Kayaks by Type") +
  theme_stata() +
  scale_fill_stata()
a
  ## Recreational kayaks are by far the largest category. This finding corresponds with my research, as they also make up the highest percentage of 
  # kayak sales. As with most leisure hobbies, the casual shopper is most common. After flatwater kayaks, sea kayals are the next biggest category, 
  # followed by fishing, and very few kayaks marketed as whitewater.

# Width vs Lengh, colored by type
b = ggplot(products, aes(x=length, y = width)) +
  geom_point(aes(color = type, shape = type)) + 
  scale_color_brewer( palette = "Set1") +
  ggtitle("Type of Kayak by Width/Length") + 
  scale_shape_manual(values = c(0, 15, 1, 16, 3, 17)) +
  theme_bw()
b
  ## Here we see that kayak specifications pretty closely match the differences between kayak types I laid out in my introduction. Sea kayaks are the longest
  # and have the shortest width across the board. Fishing kayaks are much wider for stability, and have a midling length. Whitewater kayaks, though few, are
  # shorter and split the difference in width. Recreational kayaks are all over the spectrum of widths, but do tend toward the middle of both length and width
  # as recreational kayak producers try to compromise between payoffs.

# Type of Products by Price Category
c = ggplot(products_by_price) +
  geom_bar(aes(x = type, fill=price_category), position = 'fill') + 
  ggtitle("Type of Products by Price Category") +
  scale_fill_brewer( palette = "Set2") + theme_bw()
c
  ## Here we see that fishing and sea kayaks tend to be more expensive, while flatwater has a greater range of variation due to the different markets each
  # kayak type is trying to sell to.

#Average Price by Kayak Type
positions <- c("Sea", "Fishing", "Fishing/Flatwater", 'Flatwater', 'Flatwater/Whitewater', 'Whitewater')
d = ggplot(price_by_type, aes(x = type, y = average_price, fill = type)) +
  geom_bar(stat = 'identity', show.legend = FALSE) + 
  ggtitle("Average Price by Kayak Type") +
  theme_stata() +
  scale_fill_stata() +
  scale_x_discrete(limits = positions)
d
  ## Similar to the above graph, we see that sea kayaks are the most expensive, followed by fishing, flatwater, and finally whitewater. In my opinion,
  # it is odd that whitewater has such a low average price, but we must also consider that a) there are not many whitewater kayaks sold on this retailer
  # website and b) there can be some bias because perhaps avid whitewater kayakers buy their kayaks from a retailer that specifically sells kayak supplies, not
  # just general outdoor supplies. 

d2 =  ggplot(products) +
  geom_boxplot(aes(x = type, y = price, fill = type), show.legend = FALSE) + 
  ggtitle("Price Distributions by Kayak Type") +
  theme_stata() +
  scale_fill_stata() +
  scale_x_discrete(limits = positions)
d2
  ## We can also see variation here, with sea kayaks having the greatest variation, and recreational kayaks having some significant outliers.

# Average Rating by Kayak Type
e = ggplot(rating_by_use, aes(x = type, y = Average_Rating, fill = type)) +
  geom_bar(stat = 'identity', show.legend = FALSE) + 
  ggtitle("Average Rating by Kayak Type") +
  theme_stata() +
  scale_fill_stata()
e
#above graph is just the mean between types of kayaks. I'd be interested in seeing the box plot....
e2 = ggplot(rated_products) +
  geom_boxplot(aes(x = type, y = average_rating, fill = type), show.legend = FALSE) + 
  ggtitle("Rating Distributions by Kayak Type") +
  theme_stata() +
  scale_fill_stata()
e2
  ## Many of the kayaks are highly rated, recreational kayaks again have some significant outliers here/ Fishing/Flatwater is the least favorably rated. Notice
  # that whitewater is missing. The few whitewater kayaks on the market did not have reviews, lending support to my theory that whitewater kayaks may buy
  # their kayaks elsewhere.

#Flatwater Kayaks by Price Bracket
rec_kayaks = products %>% filter(., type == 'Flatwater')
f = ggplot(rec_kayaks) +
  geom_bar(aes(x = type, fill=price_category), position = 'fill') + 
  ggtitle("Flatwater Kayaks by Price Bracket") + 
  coord_polar(theta = "y") +
  theme_few() + scale_fill_few() +
  theme(panel.border = element_rect(colour = "white", fill = NA))

f
  ## Pie chart of a portion of graph C

# Length/Width/Depth of Kayaks with Depth Data
g = ggplot(products_with_depth) +
  geom_point(aes(x = length, y = width, color = depth)) +
  ggtitle("Length/Width/Depth of Kayaks with Depth Data") +
  scale_color_gradient(low = "chartreuse", high = "dark green") +
  theme_bw()
g

  ## Disclaimer: not all products had a depth rating, so this specifically deals with a subset of the data. Depth, however, is the best measurement we have for
  # a kayaks rocker. We can see a relationship here: as length increases, width decreases and depth also decreases. 



#NUMERICAL EDA

summary(products)

#mean, median, mode, and variation for cont. variables

# frequency tables for categorical variables

ftable(table(products$type))
