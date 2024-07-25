

temp_data_1 <- merge_data %>%
  full_join(iso, by = c("Country_Names" = "Country")) %>%
  arrange(Country_Names)


temp_data_2 <- merge_data %>%
  full_join(data_code, by = c("Country_Names" = "official_name_en"))


#by =c("ISO 3166 2"="ISO3166.1.Alpha.2", "ISO 3166 3"="ISO3166.1.Alpha.3")

################################################################################################

temp_data <- data_code %>%
  full_join(iso, by = c("ISO3166.1.Alpha.2"="ISO 3166 2", 
                             "ISO3166.1.Alpha.3"="ISO 3166 3"))


final_1 <- temp_data %>%
  left_join(merge_data, by = c("official_name_en" = "Country_Names"))

final_2 <- temp_data %>%
  left_join(merge_data, by = c("Country" = "Country_Names"))

final <- rbind(final_1, final_2) %>%
  arrange(official_name_en)


final <- final %>%
  filter(!is.na(official_name_en))

final <- final[!(is.na(final$Median_Age) & is.na(final$youth_unempl_rate)), ]
final <- final %>% distinct()
