
firms_df <- read_csv("firms_merged.csv")

library(dplyr)

empl_df <- firms_df %>%
  group_by(CLOSDATE_year, CATEGORY_OF_COMPANY, ao_kreis) %>%
  summarise(total_empl = sum(EMPL, na.rm = TRUE))


sme_employment <- firms_df %>%
  filter(CATEGORY_OF_COMPANY %in% c("MEDIUM SIZED COMPANY", "SMALL COMPANY")) %>%
  group_by(CLOSDATE_year, ao_kreis) %>%
  summarise(total_empl = sum(EMPL, na.rm = TRUE))


sme_growth <- sme_employment %>%
  arrange(ao_kreis, CLOSDATE_year) %>%
  group_by(ao_kreis) %>%
  mutate(
    lag_empl = lag(total_empl),
    growth_rate = ifelse(lag_empl == 0 | is.na(lag_empl), NA, (total_empl / lag_empl - 1) * 100)
  ) %>%
  select(-lag_empl)

sme_growth_clean <- sme_growth %>%
  filter(!is.na(growth_rate), growth_rate <= 90, growth_rate >= -50)


class(sme_growth_clean$growth_rate)
class(main_dataframe$ID)
sme_growth_clean$ao_kreis <- as.character(sme_growth_clean$ao_kreis)

#merge with the main dataframe
empl_merged_df <- main_dataframe %>%
  left_join(
    sme_growth_clean %>% select(ao_kreis, CLOSDATE_year, growth_rate),
    by = c("ID" = "ao_kreis", "Year" = "CLOSDATE_year")
  )

max(empl_merged_df$growth_rate, na.rm = TRUE)
sum(is.na(empl_merged_df$growth_rate))

empl_merged_df$ID <- as.numeric(empl_merged_df$ID)
#export data
write.dta(empl_merged_df, "empl_merged.dta")
