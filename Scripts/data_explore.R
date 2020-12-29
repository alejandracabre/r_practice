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
indice_satisfaccion <- read.csv("data/HUMAN_Surveys_Country_Year_Data.csv",
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
v_dem_clean <- v_dem[v_dem_clean_selected]
names(v_dem_clean)
str(v_dem_clean)

#data set 2

indice_satisfaccion_renamed <- indice_satisfaccion %>%
  mutate(country_name_new = id_101 ,
         country_text_id_new = id_103 ,
         year_new = id_200) %>% 
  select(-id_101,-id_103, -id_200)
head(indice_satisfaccion_renamed)
names(indice_satisfaccion_renamed)
str(indice_satisfaccion_renamed)


# Find common key 
names(v_dem_clean)
str(v_dem_clean)
head(v_dem_clean$year)
head(v_dem_clean$country_text_id)
v_dem_clean %>% group_by(v_dem_clean$country_text_id) %>% summarise(count = n()) ??

# key data set 1  (year, country_text_id)
names(indice_satisfaccion_clean)
str(indice_satisfaccion_clean)
head(indice_satisfaccion_clean$id_101)
head(indice_satisfaccion_clean$id_103)
head(indice_satisfaccion_clean$id_200)


# Find unique values por variables
unique(indice_satisfaccion$id_102)
unique(v_dem$country_text_id)

# Enrich data
joined_data <- left_join(v_dem_clean, indice_satisfaccion_renamed, 
                           by = c("country_text_id" = "country_text_id_new", "year" = "year_new"))
head(v_dem_clean)
head(indice_satisfaccion_renamed)
names(joined_data)
str(joined_data)

# QA test to validate if there is available data
indice_satisfaccion_renamed %>%
  filter(year_new == 1789) %>% # where condition
  head()


# QA need to validate the join
joined_data <- left_join(indice_satisfaccion_renamed, v_dem_clean, 
                         by = c("country_text_id_new" = "country_text_id", "year_new" = "year"))
names(joined_data_test)
str(joined_data_test)
joined_data_test %>%
  filter(is.na(country_name_new)) %>% # where condition
  head()
joined_data_test %>% group_by(country_text_id, id_103) %>% summarise(count = n())


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

# modify columns types
joined_data$year_numeric <- as.numeric(practice_r$year)
summary(joined_data$year_numeric)

# Recode and create new variables





# plot
ggplot(joined_data)  # if only the dataset is known.
ggplot(data = joined_data, mapping = aes(x=joined_data$v2x_polyarchy, y=joined_data$v2x_libdem ,  color="v2x_libdem")) + geom_point()+scale_x_log10() + geom_smooth(method="lm")




## 5- Regression



