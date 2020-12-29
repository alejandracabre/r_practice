# 1- Download packages
library("ggplot2")
library("dplyr")
library("readr")

## 2- Download data set

#Create data folder
if(!file.exists('data'))
{dir.create('data')}

#read data into memory
v_dem <- readRDS("data/V-Dem-CY-Full+Others-v10.rds")
human_surveys <- read.csv("data/HUMAN_Surveys_Country_Year_Data.csv",
                            stringsAsFactors= FALSE)

## 3- Prepare data set

## Select required data
#data set 1
v_dem_clean_selected <- c("country_name", 
                          "historical_date", 
                          "country_text_id", 
                          "codingstart", 
                          "year", 
                          "v2x_polyarchy", 
                          "v2x_libdem")
v_dem_selected <- v_dem[v_dem_clean_selected]
v_dem_selected %>% group_by(country_text_id) %>% summarise(count = n())
names(v_dem_selected)
str(v_dem_selected)

#data set 2

human_surveys_renamed <- human_surveys %>%
  mutate(country_name_new = id_101 ,
         country_text_id_new = id_103 ,
         year_new = id_200) %>% 
  select(-id_101,-id_103, -id_200)
human_surveys_renamed %>% group_by(country_text_id_new) %>% summarise(count = n())
head(human_surveys_renamed)
names(human_surveys_renamed)
str(human_surveys_renamed)

# Filter out data (based on years for example)

v_dem_clean <- filter(v_dem_selected, year >= 2000 & country_text_id != "" )
str(v_dem_clean)
human_surveys_clean <- filter(human_surveys_renamed, year_new >= 2000 & country_text_id_new != "" )
human_surveys_clean_null <- filter(human_surveys_renamed,country_text_id_new == "" )
str(human_surveys_clean_null)
## Enrich data

# Find common key # key data set 1  (year, country_text_id) # key data set 2  (id_200, id_103)
names(v_dem_clean)
str(v_dem_clean)
str(v_dem_selected)
head(v_dem_clean$year)
head(v_dem_clean$country_text_id)

  
names(human_surveys_clean)
str(human_surveys_clean)
head(human_surveys$id_101)
head(human_surveys$id_103)
head(human_surveys$id_200)

# Find unique values por variables
unique(human_surveys$id_102)
unique(v_dem$country_text_id)

## Join

joined_data <- left_join(v_dem_clean, human_surveys_clean, 
                           by = c("country_text_id" = "country_text_id_new", "year" = "year_new"))
head(v_dem_clean)
head(human_surveys_clean)
str(v_dem_clean)
str(human_surveys_clean)
joined_data %>% group_by(joined_data$country_text_id) %>% summarise(count = n()) 
names(joined_data)
str(joined_data)

# QA test to validate if there is available data
human_surveys_renamed %>%
  filter(year_new == 1789) %>% # where condition
  head()


# QA need to validate the join, should return 0 values left after the join
joined_data <- left_join(human_surveys_renamed, v_dem_clean, 
                         by = c("country_text_id_new" = "country_text_id", "year_new" = "year"))
names(joined_data_test)
str(joined_data_test)
joined_data_test %>%
  filter(is.na(country_name_new)) %>% # where condition
  head()

## modify columns types (if required)
joined_data$year_numeric <- as.numeric(practice_r$year)
summary(joined_data$year_numeric)


## 4- Explore dataset


# Basic statistics
names(joined_data)
str(joined_data)
head(joined_data$v2x_libdem)
tail(joined_data$v2x_libdem)
summary(joined_data$v2x_libdem)
head(joined_data$v2x_polyarchy)
tail(joined_data$v2x_polyarchy)
summary(joined_data$v2x_polyarchy)

# frequency 

joined_data_test %>% group_by(country_text_id) %>% summarise(count = n())
joined_data_test %>% filter(is.na(country_text_id)) %>% summarise(count = n()) %>%  head()## there is not null values as NA or NULL
joined_data_test %>% filter(country_text_id == "") %>% summarise(count = n())  %>%  head() ## this is to check empty values ""
joined_data_test %>% filter(country_text_id == "") %>%  head(100) ## see the firts values when I have empty values
joined_data_test %>% filter(country_text_id == "") %>%  tail() ## see the last values when I have empty values
joined_data_test %>% filter(country_text_id == ".") %>% summarise(count = n())
joined_data_test %>% filter(country_text_id == ".") %>% head() # this is to check rows with .
  
 

# Recode and create new variables (if required)


# create a previous year data variable
enriched_data <- joined_data %>%
  group_by(country_text_id) %>%
  mutate(next_v2x_polyarchy = lead(v2x_polyarchy, order_by=year),
         before_v2x_polyarchy = lag(v2x_polyarchy, order_by=year))

# validate the data with an use case
enriched_data %>%
  filter(country_text_id == "MEX") %>% # where condition
  select(country_name,country_text_id, year, v2x_polyarchy, v2x_libdem, next_v2x_polyarchy, before_v2x_polyarchy)
  head(20)

# plot
ggplot(enriched_data)  # if only the dataset is known.
ggplot(data = enriched_data, mapping = aes(x=joined_data$v2x_polyarchy, y=joined_data$v2x_libdem ,  color="v2x_libdem")) + geom_point()+scale_x_log10() + geom_smooth(method="lm")

ggplot(v_dem_clean)  # if only the dataset is known.
ggplot(data = v_dem_clean, mapping = aes(x=v_dem_clean$v2x_polyarchy, y=v_dem_clean$v2x_libdem ,  color="v2x_libdem")) + geom_point()+scale_x_log10() + geom_smooth(method="lm")

ggplot(data = enriched_data, mapping = aes(x = v2x_polyarchy, y = v2x_libdem)) +
  geom_boxplot()

## 5- Regression



