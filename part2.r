## ------------------------------------------------------------------------
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(knitr)))
suppressMessages(suppressWarnings(library(caret)))
purl("Discovery_DataPrep.Rmd", output = "part1.r")


## ------------------------------------------------------------------------
read1 <- read.csv("https://data.cityofnewyork.us/resource/k397-673e.csv?fiscal_year=2017&$where=base_salary>200000&$order=base_salary")
read2 <- read.csv("https://data.cityofnewyork.us/resource/k397-673e.csv?fiscal_year=2017&$where=base_salary>150000&$order=base_salary")
read3 <- read.csv("https://data.cityofnewyork.us/resource/k397-673e.csv?fiscal_year=2017&$where=base_salary>100000&$order=base_salary")
read4 <- read.csv("https://data.cityofnewyork.us/resource/k397-673e.csv?fiscal_year=2017&$where=base_salary>50000&$order=base_salary")


NYC_Salaries <- rbind(read1, read2, read3, read4)


## ------------------------------------------------------------------------
summary(NYC_Salaries)
colnames(NYC_Salaries)


## ------------------------------------------------------------------------
NYC_Salaries <- NYC_Salaries[, !(names(NYC_Salaries) %in% c("payroll_number", "mid_init"))]


## ------------------------------------------------------------------------
NYC_Salaries$agency_start_date <- parse_date(as.character(NYC_Salaries$agency_start_date), format = "%Y-%m-%dT00:00:00.000")


## ------------------------------------------------------------------------
colnames(NYC_Salaries)[colnames(NYC_Salaries) == "work_location_borough"] <- "work_location"
colnames(NYC_Salaries)[colnames(NYC_Salaries) == "leave_status_as_of_july_31"] <- "leave_status"
colnames(NYC_Salaries)[colnames(NYC_Salaries) == "agency_start_date"] <- "start_date"


## ------------------------------------------------------------------------
ggplot(NYC_Salaries, aes(work_location)) + geom_bar()


## ------------------------------------------------------------------------
ggplot(NYC_Salaries, aes(work_location, regular_gross_paid)) + stat_summary(fun.y = "mean", geom = "bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ------------------------------------------------------------------------
airbnb <- na.omit(airbnb)
#airbnb$neighbourhood <- as.integer(airbnb$neighbourhood)
#airbnb$neighbourhood_group <- as.integer(airbnb$neighbourhood_group)
#airbnb$room_type <- as.integer(airbnb$room_type)


## ------------------------------------------------------------------------
index <- createDataPartition(airbnb$price, p=.80, list = F)
train = airbnb[index,]
test = airbnb[-index,]

train_airbnb <- lm(data = train, price ~ as.integer(neighbourhood_group) + as.integer(neighbourhood) + latitude + longitude + as.integer(room_type) + minimum_nights + number_of_reviews + reviews_per_month + availability_365)

summary(train_airbnb)
plot(train_airbnb)


## ------------------------------------------------------------------------
prediction <- train_airbnb %>% predict(test)
R2(prediction, test$price)
MAE(prediction, test$price)
ggplot(test, aes(prediction, price)) + geom_point()

## ------------------------------------------------------------------------
mean(test$price)


## ------------------------------------------------------------------------
#NYC_Salaries$agency_name <- as.integer(NYC_Salaries$agency_name)
#NYC_Salaries$work_location <- as.integer(NYC_Salaries$work_location)
#NYC_Salaries$title_description <- as.integer(NYC_Salaries$title_description)


## ------------------------------------------------------------------------
index <- createDataPartition(NYC_Salaries$regular_gross_paid, p=.80, list = F)
train = NYC_Salaries[index,]
test = NYC_Salaries[-index,]

train_nyc <- lm(data = train, regular_gross_paid ~ as.integer(agency_name) + start_date + as.integer(work_location) + as.integer(title_description) + base_salary + regular_hours)

summary(train_nyc)
plot(train_nyc)


## ------------------------------------------------------------------------
prediction <- train_nyc %>% predict(test)
R2(prediction, test$regular_gross_paid)
MAE(prediction, test$regular_gross_paid)
ggplot(test, aes(prediction, regular_gross_paid)) + geom_point()

