library(rvest)
library(dplyr)
library(stringr)
# Exercise 1 
link.48hours <- "https://www.timeanddate.com/weather/jamaica/kingston"
page.48hours <- read_html(link.48hours)

table.48hours <- page.48hours %>%
  html_nodes("table") %>% 
  .[[3]] %>%
  html_table()

# Start the preprocessing 

colnames(table.48hours) <- make.names(colnames(table.48hours))
colnames(table.48hours) <- make.unique(colnames(table.48hours))

# Remove the last observation

table.48hours <- table.48hours %>% 
   slice(1:(n() - 1))

# Remove the second observation about forecast because the symbol is not added to the table

table.48hours <- table.48hours %>% 
  filter(!row_number() %in% c(2))

# Add names of the 1 and 3 rows of the first column

table.48hours$X[1] <- "Daytime"
table.48hours$X[3] <- "Description of the weather"

# Remove the arrow

table.48hours <- table.48hours %>%
  mutate(across(everything(), ~ ifelse(row_number() == 6, gsub("↑", "", .), .)))



#####################
# # Exercise 2
# link.24hours <- "https://www.timeanddate.com/weather/jamaica/kingston/hourly"
# page.24hours <- read_html(link.24hours)
# 
# table.24hours <- page.24hours %>%
#   html_nodes("table") %>%
#   .[[1]] %>%
#   html_table()
# 
# 
# 
# colnames(table.24hours) <- make.names(colnames(table.24hours))
# colnames(table.24hours) <- make.unique(colnames(table.24hours))
# 
# 
# # Remove the last row
# 
# table.24hours <- table.24hours %>% 
#   slice(1:(n() - 1))
# 
# # Remove the second the seventh column
# 
# table.24hours <- table.24hours %>%
#   select(-c(2,7))
# 
# # Col names to the first row and remove the first row
# 
# colnames(table.24hours) <- table.24hours[1,]
# 
# table.24hours <- table.24hours[-1,]
# 
# 
# # Remove the columns: Weather, `Feels Like`, Chance and Amount
# 
# table.24hours <- table.24hours %>%
#   select(-Weather, -`Feels Like`, -Chance, -Amount)
# 
# # Change the column Temperature to numeric
# 
# table.24hours <- table.24hours %>%
#   mutate(Temp = gsub("\\D", "", Temp))
# 
# table.24hours$Temp <- as.numeric(table.24hours$Temp)
# 
# # Change the column Wind to numeric
# 
# table.24hours <- table.24hours %>%
#   mutate(Wind = gsub("\\D", "", Wind))
# 
# table.24hours$Wind <- as.numeric(table.24hours$Wind)
# 
# # Change the column Humidity to numeric
# 
# table.24hours <- table.24hours %>%
#   mutate(Humidity = gsub("\\D", "", Humidity))
# 
# table.24hours$Humidity <- as.numeric(table.24hours$Humidity)
# 
# # Change the format of the column Time
# library(lubridate)
# today <- Sys.Date()
# tomorrow <- today + 1 
# 
# time.column <- substr(table.24hours$Time, 1, 5)
# 
# # Function to add today's and tomorrow's date
# temp <<- FALSE
# add_date <- function(time_str, position) {
#   if ((substr(time_str, 1, 2) == "00" | temp) & (position != 1)){
#     date <- tomorrow
#     temp <<- TRUE
#   }
#   else{
#     date <- today
#   }
#   return(paste(time_str, date, sep = " "))
# }
# 
# table.24hours$Time <- sapply(seq_along(time.column), function(i) add_date(time.column[i], i), USE.NAMES = FALSE)
# 
# 
# table.24hours$Time <- factor(table.24hours$Time, levels =table.24hours$Time) # Change the Time variable into factor
# 
# 
# # Plot for temperature
# 
# ggplot(table.24hours, aes(x = Time, y = Temp, group = 1)) +
#   geom_line() +
#   labs(title = "Temperature Forecast - Next 24 hours",
#        x = "Time",
#        y = "Temperature") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
# 
# # Plot for wind
# 
# ggplot(table.24hours, aes(x = Time, y = Wind, group = 1)) +
#   geom_line() +
#   labs(title = "Wind Forecast - Next 24 hours",
#        x = "Time",
#        y = "Wind (km/h)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
# 
# 
# # Plot for humidity 
# 
# ggplot(table.24hours, aes(x = Time, y = Humidity, group = 1)) +
#   geom_line() +
#   labs(title = "Humidity Forecast - Next 24 hours",
#        x = "Time",
#        y = "Humidity (%)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))









