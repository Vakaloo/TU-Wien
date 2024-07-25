library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

process.pipeline.exercise.2 <- function(df){

  colnames(df) <- make.names(colnames(df))
  colnames(df) <- make.unique(colnames(df))
  
  # Remove the last row
  df <- df %>% 
    slice(1:(n() - 1))
  
  # Remove the second the seventh column
  df <- df %>%
    select(-c(2,7))
  
  # Col names to the first row and remove the first row
  colnames(df) <- df[1,]
  df <- df[-1,]
  
  # Remove the columns: Weather, `Feels Like`, Chance and Amount
  df <- df %>%
    select(-Weather, -`Feels Like`, -Chance, -Amount)
  
  # Change the column Temperature, Wind, Humidity to numeric
  df <- df %>%
    mutate(Temp = as.numeric(gsub("\\D", "", Temp)), 
           Wind = as.numeric(gsub("\\D", "", Wind)),
           Humidity = as.numeric(gsub("\\D", "", Humidity)))
  
}

# Exercise 2
link.24hours <- "https://www.timeanddate.com/weather/jamaica/kingston/hourly"
page.24hours <- read_html(link.24hours)

table.24hours <- page.24hours %>%
  html_nodes("table#wt-hbh") %>%
  .[[1]] %>%
  html_table()

final.table.24hours <- process.pipeline.exercise.2(table.24hours)


# Prepare the data for the visualizations

today <- format(Sys.Date(), format = "%d-%m-%Y")
tomorrow <- format(Sys.Date() + 1, format = "%d-%m-%Y")

time.column <- substr(final.table.24hours$Time, 1, 5)

# Function to add today's and tomorrow's date
temp <<- FALSE
add_date <- function(time_str, position) {
  if ((substr(time_str, 1, 2) == "00" | temp) & (position != 1)){
    date <- tomorrow
    temp <<- TRUE
  }
  else{
    date <- today
  }
  return(paste(date, time_str, sep = " "))
}

final.table.24hours$Time <- sapply(seq_along(time.column), 
                                   function(i) add_date(time.column[i], i), 
                                   USE.NAMES = FALSE)

#  Change the Time variable into factor
final.table.24hours$Time <- factor(final.table.24hours$Time, 
                                    levels =final.table.24hours$Time) 


# Plot for temperature

ggplot(final.table.24hours, aes(x = Time, y = Temp, group = 1)) +
  geom_line() +
  labs(title = "Temperature Forecast - Next 24 hours",
       x = "Time",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

# Plot for wind

ggplot(final.table.24hours, aes(x = Time, y = Wind, group = 1)) +
  geom_line() +
  labs(title = "Wind Forecast - Next 24 hours",
       x = "Time",
       y = "Wind (km/h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 


# Plot for humidity 

ggplot(final.table.24hours, aes(x = Time, y = Humidity, group = 1)) +
  geom_line() +
  labs(title = "Humidity Forecast - Next 24 hours",
       x = "Time",
       y = "Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



