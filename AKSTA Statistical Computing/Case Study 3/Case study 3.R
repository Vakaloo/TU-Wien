setwd("C:/Users/vaka1/Desktop/Case Study 3")
library(ggplot2)
library(dplyr)
data <- read.csv("file_out.csv", sep = ";")
data[data == "."] <- NA
data <- data %>%
  filter(!is.na(Developed...Developing.Countries))
str(data)

data$median_age <- chartr(",", ".", data$median_age)
data$youth_unempl_rate <- chartr(",", ".", data$youth_unempl_rate)
str(data)

data$median_age <- as.numeric(data$median_age)
data$youth_unempl_rate <- as.numeric(data$youth_unempl_rate)
str(data)

################################Question 1################################

ggplot(data, aes(x=median_age, fill = Developed...Developing.Countries)) + 
  geom_density(color = "black", alpha = 0.5)+
  theme(legend.position = "top", 
        legend.title = element_blank())+
  xlab("Median age of population")

################################Question 2################################

ggplot(data, aes(x=youth_unempl_rate, fill = Developed...Developing.Countries)) + 
  geom_density(color = "black", alpha = 0.5)+
  theme(legend.position = "top", legend.title = element_blank())+
  xlab("Median age of population")

################################Question 3################################

ggplot(data, aes(x = Region.Name, fill = Developed...Developing.Countries))+
  geom_bar()+
  theme(legend.position = "top", 
        legend.title = element_blank())+
  labs(x = "Region Name", y = "Absolute frequency")

ggplot(data, aes(x = Region.Name, group=interaction(Developed...Developing.Countries, Region.Name)
                 ,fill = Developed...Developing.Countries))+
  geom_bar(position = "fill")+
  theme(legend.position = "top", 
        legend.title = element_blank())+
  labs(x = "Region Name", y = "Relative frequency")

################################Question 4################################

ggplot(data,aes(x = median_age, y = youth_unempl_rate, color = Developed...Developing.Countries))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme(legend.position = "top", 
        legend.title = element_blank()) + 
  labs(x = "Median Age", y = "Youth Unemployment Rate")


################################Question 5################################

ggplot(data, aes(x = Region.Name, y = youth_unempl_rate))+
  geom_boxplot()+
  labs(x = "Region Name", y = "Youth Unemployment Rate")

################################Question 6################################

ggplot(data, aes(x = Region.Name, y = median_age))+
  geom_boxplot()+
  labs(x = "Region Name", y = "Median Age")

################################Question 7################################
#Introduce the library forcats
library(forcats)

#From data we create the median_unempl_rate_region dataset
#We group by Sub.region.Name and Region.Name
#Finally we reorder the Sub.region.Name based on the median
median_unempl_rate_region <- data %>% 
  group_by(Sub.region.Name, Region.Name) %>%
  summarise(median = median(youth_unempl_rate), .groups = "drop") %>%
  mutate(Sub.region.Name = fct_reorder(Sub.region.Name, median))

#Create the palette for the colorblind based on the exercise
my_palette <- c("#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(median_unempl_rate_region, aes(median, Sub.region.Name, color = Region.Name))+
  geom_point(size = 5)+
  scale_colour_manual(values=my_palette)+
  labs(x = "Median Age", y = "Sub region name")+
  theme(legend.position = "top", 
        legend.title = element_blank())
  
################################Question 8################################
#We ignore the first 4 rows of the population csv from the internet
pop_online <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_5436324.csv", skip = 4)
#Get an idea of the data
str(pop_online)

#Keep the country code (aka ISO code) column and the X2020 column for the population of 2020
#Change the name of the column X2020 to Year.2020 using the Rename command
pop_online_2020 <- pop_online %>% select(Country.Code, X2020) %>% rename("Population.2020" = "X2020")

#Merge the 2 data sets (data and pop_online_2020) with left join based on the ISO code
merged_dataset <- data %>% left_join(pop_online_2020, by=c('ISO.3166.3'='Country.Code'))


################################Question 9################################
#Install ploty package
#install.packages("ploty")

#Introduce the library plotly
library(plotly)

task.4.plot <- ggplot(merged_dataset,aes(x = median_age, y = youth_unempl_rate, 
                                         color = Developed...Developing.Countries))+
  geom_point(aes(text = country, size = Population.2020)) +
  theme(legend.position = "top", 
        legend.title = element_blank()) + 
  labs(x = "Median Age", y = "Youth Unemployment Rate")

#create the interactive plot using ggploty
interactive.plot <- ggplotly(task.4.plot, tooltip = c("text", "x", "y", "size"))

#Move the legend to the top of the plot and also remove the title
interactive.plot <- layout(interactive.plot, legend = list(orientation = "h", 
                                                           x = 0.5, 
                                                           y = 1.1, 
                                                           xanchor = "center",
                                                           yanchor = "bottom",
                                                           title = ""))
interactive.plot

