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

# Find common key 
names(v_dem)
str(v_dem)
head(v_dem$year)
head(v_dem$country_text_id)
# key data set 1  (year, country_text_id)
names(indice_satisfaccion)
str(indice_satisfaccion)
head(indice_satisfaccion$id_101)
head(indice_satisfaccion$id_103)
head(indice_satisfaccion$id_200)


# Find unique values por variables
unique(indice_satisfaccion$id_103)
unique(v_dem$country_text_id)

# Enrich data
joined_data <- left_join(practice_r, indice_satisfaccion, 
                           by = c("country_text_id" = "id_103", "year" = "id_200"))

names(joined_data)
str(joined_data)
# need to validate the join


# Select required data

myvars <- c("country_name", "historical_date", "codingstart", "year", "v2x_polyarchy", "v2x_libdem")
cleaned_data <- practice_r[myvars]

## 4- Explore dataset

# Basic statistics
names(cleaned_data)
str(cleaned_data)
head(cleaned_data$v2x_libdem)
tail(cleaned_data$v2x_libdem)
summary(cleaned_data$v2x_libdem)
head(cleaned_data$v2x_polyarchy)
tail(cleaned_data$v2x_polyarchy)
summary(cleaned_data$v2x_polyarchy)

# frequency 

# modify columns types
practice_r$year_numeric <- as.numeric(practice_r$year)
summary(practice_r$year_numeric)

# Recode and create new variables





# plot
ggplot(cleaned_data)  # if only the dataset is known.
ggplot(data = cleaned_data, mapping = aes(x=cleaned_data$v2x_polyarchy, y=cleaned_data$v2x_libdem ,  color="v2x_libdem")) + geom_point()+scale_x_log10() + geom_smooth(method="lm")




## 5- Regression



