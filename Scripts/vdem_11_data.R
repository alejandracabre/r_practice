# Loading packages

library(tidyverse) # Data wranling 
library(lubridate) # Working with dates
require("devtools")

setwd("~/DsProjects/r_practice")

#Create a Output folder

if(!file.exists("outputs")){
  (dir.create("outputs"))
}

## Downloading V-Dem R Package
require("vdemdata")
# if not installed, use this code
# devtools::install_github("vdeminstitute/vdemdata") 

## Loading data set
vdem_data<-vdem #Latest V-Dem data V11.1
saveRDS(vdem_data, "data/vdem_data_v11_1.RDS")

# Prepare Data set

## Selected the main indexes
selected_vars<- c("country_name",
                  "year",
                  "country_text_id",
                  "e_regionpol_6C",
                  "v2x_polyarchy",
                  "v2x_libdem",
                  "v2x_partipdem",
                  "v2x_delibdem",
                  "v2x_egaldem"
                  )

# Filtering by Region (LACR) and by Year >=2000
vdem_data_selected <- vdem_data%>%
  select(selected_vars)%>%
  filter(year >=2000, 
         e_regionpol_6C == 2) # Latin America (including Cuba and the Dominican Republic)


## Checking Data Quality
counting_nas <- function(x){ # Function to count NAs in Variables
  sum(is.na(x))
}
sapply(vdem_data_selected, counting_nas) #No NAs detected

#  Exploring Polyarchy Index

Polyarchy_summary <- vdem_data_selected%>%
  group_by(country_text_id)%>%
  summarise(N_years = n(),
            Polyarchy_mean = mean(v2x_polyarchy),
            Polyarchy_sd = sd(v2x_polyarchy),
            Polyarchy_median = median(v2x_polyarchy)
            )
head(Polyarchy_summary)
tail(Polyarchy_summary)

Polyarchy_mean_by_year <- vdem_data_selected%>%
  group_by(year)%>%
  summarise(Year_mean = mean(v2x_polyarchy))
head(Polyarchy_mean_by_year)
tail(Polyarchy_mean_by_year)

## Adding Polyarchy year mean to each case
vdem_data_selected_2<-right_join(vdem_data_selected,Polyarchy_mean_by_year)

# Plot Polyarchy Index

my_theme <- theme_classic()+ #Personalized theme
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text( angle = 45,
                                    size =7),
        axis.text.y = element_text(size =6),
        strip.text = element_text(size = 7),
        plot.caption = element_text(size = 5),
        plot.title = element_text(size = 8)
  )


Caption_1 <-"Coppedge, Michael, et al. V-Dem [Country-Year/Country-Date] 
Dataset v11.1 Varieties of Democracy Project. 
https://doi.org/10.23696/vdemds21" #Source Caption


G_Polyarchy_LACR <- # Longitudinal analysis
  ggplot(vdem_data_selected_2)+ 
  geom_line(aes(x= year, v2x_polyarchy))+
  geom_line(aes(x= year, Year_mean, 
                linetype ="LACR Yearly Average"), 
            color = "gray",
            show.legend = TRUE)+
  my_theme +facet_wrap(vars(country_text_id),
             strip.position = "bottom")+
  xlab("Year") + ylab("Polyarchy Index")+
  labs(title = "Polyarchy Index in Latin American Countries 2000-2020",
       caption = Caption_1)
  

ggsave(G_Polyarchy_LACR, path = "outputs",
       filename = "G_Polyarchy_LACR.png", 
        dpi = 320)

G_Polyarchy_LACR_2020 <- # Last year result
  vdem_data_selected_2%>% 
  filter(year == 2020)%>%
  arrange(desc(v2x_polyarchy))%>%
  ggplot()+
  geom_col(aes(reorder(country_text_id, -v2x_polyarchy), v2x_polyarchy),
           fill = "blue")+
  geom_hline(aes(yintercept =  Year_mean, 
                 linetype ="LACR Average 2020"), 
             color = "gray",
             show.legend = TRUE)+
  my_theme +xlab("Year") + ylab("Polyarchy Index")+
  labs(title = "Polyarchy Index in Latin American Countries 2020",
       caption = Caption_1)

ggsave(G_Polyarchy_LACR_2020, path = "outputs",
       filename = "G_Polyarchy_LACR_2020.png", 
       dpi = 320)

G_Partpdem_Polyarchy <- #Participatory Demo vs Polyarchy
  ggplot(vdem_data_selected_2,
       aes(v2x_partipdem, v2x_polyarchy))+
  geom_point()+
  my_theme+
  labs(title = "Participatory Democracy Index vs. Polyarchy Index in Latin American Countries 2000-2020",
       caption = Caption_1)+
  xlab("Participatory Democracy Index") + 
  ylab("Polyarchy Index")


ggsave(G_Partpdem_Polyarchy, path = "outputs",
         filename = "G_Partpdem_Polyarchy.png", 
         dpi = 320)
  

