##dowload data set

#Create data folder
if(!file.exists('data'))
{dir.create('data')}

#read data into memory
practice_r <- readRDS("V-Dem-CY-Full+Others-v10.rds")

#Explore dataset
names(practice_r)
str(practice_r)
head(practice_r$year)
tail(practice_r$year)

##modify columns types
practice_r$year_numeric <- as.numeric(practice_r$year)
summary(practice_r$year_numeric)