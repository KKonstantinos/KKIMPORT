library(dplyr)

# 1. Flag patients with the disease before, at, or within 1 year after index
disease_flag <- disease_dataset %>%
  filter(!is.na(DISEASE_DATE)) %>%
  inner_join(adsl %>% select(USUBJID, INDEX_DATE), by = "USUBJID") %>%
  mutate(days_from_index = as.numeric(DISEASE_DATE - INDEX_DATE)) %>%
  filter(days_from_index <= 365) %>%                   # prior, same day, or within 1 year
  group_by(USUBJID) %>%
  summarise(first_days = min(days_from_index), .groups = "drop") %>%
  mutate(timing = case_when(
    first_days <  0 ~ "Prior to index",
    first_days == 0 ~ "Same time as index",
    TRUE            ~ "Within 1 year after index"
  ))

adsl2 <- adsl %>%
  left_join(disease_flag, by = "USUBJID") %>%
  mutate(has_disease = !is.na(first_days))

# 2. n and % for patients diagnosed at or before age 45 / 55 / 65
cutoffs <- c(45, 55, 65)

result <- bind_rows(lapply(cutoffs, function(a) {
  adsl2 %>%
    filter(AGE <= a) %>%
    summarise(
      age_group = paste0("<= ", a, " years"),
      N   = n(),
      n   = sum(has_disease),
      pct = round(100 * n / N, 1)
    )
}))

result
