library(dplyr)
library(tidyr)

# Unique id per admission (row)
df_LVAD <- df_LVAD %>% mutate(adm_id = row_number())

# ---- Long format of procedures, ordered within admission ----
proc_seq <- df_LVAD %>%
  select(adm_id, op1:op30, opd1:opd30) %>%
  mutate(across(opd1:opd30, as.Date)) %>%          # adjust if dates are stored differently
  pivot_longer(
    cols          = -adm_id,
    names_to      = c(".value", "pos"),
    names_pattern = "^(op|opd)(\\d+)$"
  ) %>%
  mutate(
    op   = toupper(trimws(op)),
    pos  = as.integer(pos),
    type = case_when(
      grepl("^FXL(30|60|00)", op) ~ "implant",
      grepl("^FXM", op)           ~ "explant"
    )
  ) %>%
  filter(!is.na(type)) %>%
  arrange(adm_id, opd, pos) %>%                    # later operations = later date (position breaks ties)
  group_by(adm_id) %>%
  mutate(ord = row_number()) %>%
  summarise(
    last_fxm_ord  = if (any(type == "explant")) max(ord[type == "explant"]) else NA_integer_,
    last_impl_ord = if (any(type == "implant")) max(ord[type == "implant"]) else NA_integer_,
    last_impl_date = if (any(type == "implant")) opd[ord == max(ord[type == "implant"])] else as.Date(NA),
    .groups = "drop"
  ) %>%
  mutate(
    # FXM NOT followed by FXL30/FXL60/FXL00 -> exclude
    excl_explant = if_else(!is.na(last_fxm_ord) &
                           (is.na(last_impl_ord) | last_impl_ord < last_fxm_ord), 1, 0),
    # FXM followed by FXL30/FXL60/FXL00 -> new baseline
    flag_baseline_reimplant = if_else(!is.na(last_fxm_ord) & !is.na(last_impl_ord) &
                                      last_impl_ord > last_fxm_ord, 1, 0)
  )

# ---- Exclusion criteria ----
df_LVAD2 <- df_LVAD %>%
  left_join(proc_seq %>% select(adm_id, excl_explant, flag_baseline_reimplant, last_impl_date),
            by = "adm_id") %>%
  mutate(
    excl_explant            = coalesce(excl_explant, 0),
    flag_baseline_reimplant = coalesce(flag_baseline_reimplant, 0),
    BASELINE_DATE = if_else(BASELINE_DATE == Inf, INDATUM, BASELINE_DATE),
    BASELINE_DATE = if_else(flag_baseline_reimplant == 1 & !is.na(last_impl_date),
                            last_impl_date, BASELINE_DATE),           # re-implantation date
    excl_age  = if_else(ALDER < 18, 1, 0),
    excl_date = if_else(BASELINE_DATE <  as.Date("2016-02-18") |
                        BASELINE_DATE >= as.Date("2024-01-01"), 1, 0)
  ) %>%
  arrange(LopNr, BASELINE_DATE) %>%
  group_by(LopNr) %>%
  mutate(excl_firstLVAD = if_else(row_number() == 1, 0, 1)) %>%
  ungroup() %>%
  mutate(FAS = if_else(pmax(excl_age, excl_date, excl_firstLVAD, excl_explant) == 0, 1, 0)) %>%
  select(-last_impl_date)

# Quick check
count(df_LVAD2, excl_explant, flag_baseline_reimplant)
