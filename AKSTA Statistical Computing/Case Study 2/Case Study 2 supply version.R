library(dplyr)
library(readr)
setwd("C:/Users/vaka1/Desktop/Case Study 2")
##############################Question 1######################################

country_code <- read.csv("country-codes_csv.csv")

data_code <-country_code %>% select(official_name_en,ISO3166.1.Alpha.2,ISO3166.1.Alpha.3,
                     Developed...Developing.Countries, Region.Name, Sub.region.Name)

##############################################################################

##############################Question 2######################################

rawdata_343 <- read_fwf("rawdata_343.txt", fwf_empty("rawdata_343.txt"),
                                skip = 2)

rawdata_343 <- as.data.frame(rawdata_343)
rawdata_343 <- rawdata_343 %>% select(X2,X5)

names(rawdata_343) <- c("Country_Names","Median_Age")
rawdata_343$Median_Age <- substr(rawdata_343$Median_Age, 1, 4) #Observation 198 has a problem
rawdata_343$Median_Age <- as.numeric(rawdata_343$Median_Age)


rawdata_373 <- read.csv("rawdata_373.csv")
rawdata_373$country_name <- trimws(rawdata_373$country_name) #Remove spaces

##############################################################################

##############################Question 3######################################

merge_data <- rawdata_343 %>% full_join(rawdata_373, by=c("Country_Names"="country_name")) #The key is country names
merge_data <- merge_data %>% arrange(Country_Names) #sort based on country names

##############################################################################

##############################Question 4######################################

df_vars <- merge_data %>% inner_join(data_code, by=c("Country_Names"="official_name_en"))

#data_code$official_name_en[1:20]
#merge_data$Country_Names[1:20] Example the Bahamas

library("readxl")
iso <- read_excel("CIA_factbook_matching_table_iso.xlsx")
iso <- as.data.frame(iso)

temp_data <- merge_data %>% inner_join(iso, by=c("Country_Names"="Country"))
final_df_vars <- temp_data %>% 
  inner_join(data_code, by=c("ISO 3166 2"="ISO3166.1.Alpha.2","ISO 3166 3"="ISO3166.1.Alpha.3")) %>%
  select(-official_name_en)
##############################################################################

##############################Question 5######################################

##############################################################################

##############################Question 6######################################

final_df_vars %>% count(Developed...Developing.Countries)

##############################################################################

##############################Question 7######################################

final_df_vars %>% count(Region.Name)

##############################################################################

##############################Question 8######################################

final_df_vars %>%
  group_by(Region.Name) %>%
  count(Developed...Developing.Countries)

##############################################################################

##############################Question 9######################################

final_df_vars_removed_NA <- final_df_vars %>% filter(!is.na(Median_Age))
final_df_vars_removed_NA <- final_df_vars_removed_NA %>% filter(!is.na(youth_unempl_rate))

final_df_vars_removed_NA %>% 
  group_by(Developed...Developing.Countries) %>%
  summarize(average_median_age = mean(Median_Age), average_unemployment_rate=mean(youth_unempl_rate),
            sd_median_age = sd(Median_Age), sd_unemployment_rate=sd(youth_unempl_rate))

##############################################################################

##############################Question 10#####################################

final_df_vars_removed_NA %>% 
  group_by(Developed...Developing.Countries,Region.Name) %>%
  summarize(average_median_age = mean(Median_Age), average_unemployment_rate=mean(youth_unempl_rate),
            sd_median_age = sd(Median_Age), sd_unemployment_rate=sd(youth_unempl_rate))

##############################################################################

##############################Question 11#####################################

above <- function(x,mean_value){
  if (x>mean_value){
    result <- "yes"
  }else{
    result <- "no"
  }
  return(result)
}

average_region <- final_df_vars_removed_NA %>% 
  group_by(Region.Name) %>%
  summarize(average_median_age = mean(Median_Age), 
            average_unemployment_rate=mean(youth_unempl_rate))

final_dataset_sapply <- data.frame()

for (region in average_region$Region.Name){
  
  filtered_data_per_region <- final_df_vars_removed_NA %>% filter(Region.Name==region)
  filtered_average <- average_region %>% filter(Region.Name==region)
  
  
  temporary <- filtered_data_per_region %>% 
    mutate(
      above_average_median_age = sapply(filtered_data_per_region$Median_Age,function(x) {
        above(x, filtered_average$average_median_age)}),
      
      above_average_yu = sapply(filtered_data_per_region$youth_unempl_rate, function(x) {
        above(x, filtered_average$average_unemployment_rate)}))
  
  final_dataset_sapply <- rbind(final_dataset_sapply,temporary)
  
}


##############################################################################

##############################Question 12#####################################

#write.table(final_dataset, file = "Final_data.csv", sep = ";", na = ".", row.names = FALSE,col.names = TRUE)

##############################################################################

