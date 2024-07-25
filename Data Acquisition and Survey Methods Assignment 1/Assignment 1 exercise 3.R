library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

# Exercise 3
link.annual<- "https://www.timeanddate.com/weather/jamaica/kingston/climate"
page.annual <- read_html(link.annual)


Time <- c("allyear", "january", 
          "february", "march", 
          "april", "may", 
          "june", "july", 
          "august", "september", "october", 
          "november", "december")

path <- "//div[@class='climate-month climate-month--"

all.path <- paste(path, Time, "']" ,sep = "")

annual.table <- data.frame()

for (p in all.path){
  info.annual <- page.annual %>% 
    html_node(xpath = p) %>%
    html_nodes("p") %>%
    html_text()
  
  annual.table <- rbind(annual.table,info.annual[1:4])
  
}
annual.table

colnames(annual.table) <- c("High.Temperature","Low.Temperature","Mean.Temperature","Precipitation")

Time <- str_to_title(Time)
annual.table <- cbind(Time, annual.table)


annual.table <- annual.table %>%
  mutate(High.Temperature = as.numeric(gsub("[^0-9.]", "", High.Temperature)), 
         Low.Temperature = as.numeric(gsub("[^0-9.]", "", Low.Temperature)),
         Mean.Temperature = as.numeric(gsub("[^0-9.]", "", Mean.Temperature)),
         Precipitation = as.numeric(gsub("[^0-9.]", "", Precipitation)))

# This is the final table
print(annual.table)

# Start the plotting part

month.table <- annual.table[-1,] # Remove the annual observation
rownames(month.table) <- NULL # Reset index
month.table$Time <- factor(month.table$Time, levels =month.table$Time) # Change the Time variable into factor

# Plot for the temperature variable

dff.temperature <- melt(month.table[,1:4], id.var = "Time")


ggplot(dff.temperature, aes(x = Time, y = value, color = variable, group=variable)) +
  geom_line() +
  labs(title = "Annual Temperature Variation",
       x = "Months",
       y = "Temperature (°C)") +
  scale_color_manual(values = c("red", "blue", "green"),
                     labels = c("High Temperature", "Low Temperature", "Mean Temperature"),
                     name = "Temperature Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Plot for the precipitation

dff.precipitation <- month.table %>%
  select(Time, Precipitation)

ggplot(dff.precipitation, aes(x = Time, y = Precipitation, group = 1)) +
  geom_line() +
  labs(title = "Annual Precipitation Variation",
       x = "Months",
       y = "Precipitation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


