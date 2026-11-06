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

###
library(dplyr)

cutoffs <- c(45, 55, 65)

tab <- bind_rows(lapply(cutoffs, function(a) {
  d <- df %>% filter(AGE <= a)

  bind_rows(d, d %>% mutate(SEX = "Overall")) %>%   # add an overall copy
    group_by(SEX) %>%
    summarise(
      N    = n(),
      d1_n = sum(disease1 == 1, na.rm = TRUE),
      d2_n = sum(disease2 == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(cutoff = a)
})) %>%
  mutate(SEX = factor(SEX, levels = c("M", "F", "Overall"),
                      labels = c("Male", "Female", "Overall"))) %>%
  arrange(cutoff, SEX) %>%
  mutate(
    `Age at onset` = paste0("<= ", cutoff, " years"),
    Disease1 = sprintf("%d (%.1f%%)", d1_n, 100 * d1_n / N),
    Disease2 = sprintf("%d (%.1f%%)", d2_n, 100 * d2_n / N)
  ) %>%
  select(`Age at onset`, Sex = SEX, N, Disease1, Disease2)

tab


library(dplyr)
library(gt)

cutoffs       <- c(45, 55, 65)
timing_levels <- c("Prior to index",
                   "Same time as index",
                   "Within 1 year after index")
diseases      <- c("disease1", "disease2")

# counts for one group: Total + one count per timing category, per disease
summ <- function(d) {
  out <- tibble(N = nrow(d))
  for (dis in diseases) {
    flag <- d[[dis]] == 1
    tim  <- d[[paste0(dis, "_timing")]]
    out[[paste0(dis, "_Total")]] <- sum(flag, na.rm = TRUE)
    for (t in timing_levels) {
      out[[paste0(dis, "_", t)]] <- sum(flag & tim == t, na.rm = TRUE)
    }
  }
  out
}

tab <- bind_rows(lapply(cutoffs, function(a) {
  d <- df %>% filter(AGE <= a)
  bind_rows(d, d %>% mutate(SEX = "Overall")) %>%
    group_by(SEX) %>%
    group_modify(~ summ(.x)) %>%
    ungroup() %>%
    mutate(cutoff = a)
})) %>%
  mutate(SEX = factor(SEX, levels = c("M", "F", "Overall"),
                      labels = c("Male", "Female", "Overall"))) %>%
  arrange(cutoff, SEX) %>%
  mutate(
    `Age at onset` = paste0("<= ", cutoff, " years"),
    across(starts_with("disease"), ~ sprintf("%d (%.1f%%)", .x, 100 * .x / N))
  ) %>%
  select(`Age at onset`, Sex = SEX, N, starts_with("disease"))

tab


