library("ggplot2")

##dowload data set

#Create data folder
if(!file.exists('data'))
{dir.create('data')}

#read data into memory
practice_r <- readRDS("data/V-Dem-CY-Full+Others-v10.rds")

#Explore dataset
names(practice_r)
str(practice_r)
head(practice_r$year)
tail(practice_r$year)
summary(practice_r$year)
summary(practice_r$v2x_polyarchy)
summary(practice_r$v2x_libdem)
View(practice_r)

## Remove not required data

myvars <- c("country_name", "historical_date", "codingstart", "year", "v2x_polyarchy", "v2x_libdem")
cleaned_data <- practice_r[myvars]

##modify columns types
practice_r$year_numeric <- as.numeric(practice_r$year)
summary(practice_r$year_numeric)

#Explore dataset
names(cleaned_data)
str(cleaned_data)
head(cleaned_data$v2x_libdem)
tail(cleaned_data$v2x_libdem)
summary(cleaned_data$v2x_libdem)
head(cleaned_data$v2x_polyarchy)
tail(cleaned_data$v2x_polyarchy)
summary(cleaned_data$v2x_polyarchy)


## frequency 


## plot
ggplot(cleaned_data)  # if only the dataset is known.
ggplot(data = cleaned_data, mapping = aes(x=cleaned_data$v2x_polyarchy, y=cleaned_data$v2x_libdem ,  color="v2x_libdem")) + geom_point()+scale_x_log10() + geom_smooth(method="lm")
