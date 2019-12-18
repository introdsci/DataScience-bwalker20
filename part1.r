## ------------------------------------------------------------------------
suppressMessages(suppressWarnings(library(tidyverse)))


## ------------------------------------------------------------------------
setwd("./")
airbnb <- read.csv("./AB_NYC_2019.csv")


## ------------------------------------------------------------------------
colnames(airbnb)
str(airbnb)


## ------------------------------------------------------------------------
airbnb$name <- as.character(airbnb$name)
airbnb$host_name <- as.character(airbnb$host_name)
airbnb$neighbourhood <- as.character(airbnb$neighbourhood)
airbnb$neighbourhood_group <- as.character(airbnb$neighbourhood_group)
airbnb$room_type <- as.character(airbnb$room_type)
airbnb$last_review <- as.Date(airbnb$last_review)



## ------------------------------------------------------------------------
airbnb$neighbourhood_group <- as.factor(airbnb$neighbourhood_group)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
airbnb$room_type <- as.factor(airbnb$room_type)



## ------------------------------------------------------------------------
for(i in 1:16) {
  print(typeof(airbnb[1,i]))
}
head(levels(airbnb$neighbourhood))
levels(airbnb$neighbourhood_group)
levels(airbnb$room_type)


## ------------------------------------------------------------------------
Hosts <- tibble(host_id = airbnb$host_id, host_name = airbnb$host_name, number_of_listings = airbnb$calculated_host_listings_count)

Listings <- tibble(listing_id = airbnb$id, host_id = airbnb$host_id, name = airbnb$name, neighborhood = airbnb$neighbourhood, neighborhood_group = airbnb$neighbourhood_group, latitude = airbnb$latitude, longitude = airbnb$longitude, room_type = airbnb$room_type, price = airbnb$price, minimum_nights = airbnb$minimum_nights, number_of_reviews = airbnb$number_of_reviews, last_review = airbnb$last_review, reviews_per_month = airbnb$reviews_per_month, availability = airbnb$availability_365)

Hosts <- distinct(Hosts)
Listings <- Listings[order(Listings$host_id),]
Hosts <- Hosts[order(Hosts$host_id),]


## ------------------------------------------------------------------------
ggplot(Hosts, aes(number_of_listings)) + geom_bar() + xlim(0, 25)# + scale_x_continuous(breaks = seq(0, max(Hosts$number_of_listings), 1))


## ------------------------------------------------------------------------
ggplot(Listings, aes(neighborhood_group)) + geom_bar()


## ------------------------------------------------------------------------
ggplot(Listings, aes(room_type)) + geom_bar()


## ------------------------------------------------------------------------
ggplot(Listings, aes(neighborhood_group, price)) + stat_summary(fun.y = "mean", geom = "bar")

