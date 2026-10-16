library(dplyr)

make_disease_outcome <- function(df, disease_data, disease,
                                 new_onset = FALSE,
                                 id_col = "id", date_col = "date") {

  # ---- read disease data (data frame or file path) ----
  if (is.character(disease_data)) {
    ext <- tolower(tools::file_ext(disease_data))
    disease_data <- switch(ext,
      csv      = readr::read_csv(disease_data, show_col_types = FALSE),
      tsv      = ,
      txt      = readr::read_tsv(disease_data, show_col_types = FALSE),
      rds      = readRDS(disease_data),
      sas7bdat = haven::read_sas(disease_data),
      parquet  = arrow::read_parquet(disease_data),
      xlsx     = readxl::read_excel(disease_data),
      stop("Unsupported file type: ", ext)
    )
  }

  events <- disease_data %>%
    transmute(id = .data[[id_col]],
              event_date = as.Date(.data[[date_col]])) %>%
    filter(!is.na(event_date))

  # match id type to main data
  events$id <- if (is.numeric(df$id)) as.numeric(events$id) else as.character(events$id)

  # ---- base: censoring date ----
  base <- df %>%
    mutate(
      across(c(baseline_date, emigration_date, eos_date, htx_date,
               explantation_date, death_date), as.Date),
      censor_date = pmin(htx_date, explantation_date, emigration_date,
                         eos_date, death_date, na.rm = TRUE)
    )

  # ---- first occurrence after baseline ----
  first_ev <- events %>%
    inner_join(select(base, id, baseline_date), by = "id") %>%
    filter(event_date > baseline_date) %>%
    group_by(id) %>%
    summarise(first_date = min(event_date), .groups = "drop")

  # ---- patients with disease on/before baseline ----
  prior_ids <- events %>%
    inner_join(select(base, id, baseline_date), by = "id") %>%
    filter(event_date <= baseline_date) %>%
    pull(id) %>% unique()

  # ---- outcome ----
  out <- base %>%
    left_join(first_ev, by = "id") %>%
    mutate(
      ev   = as.integer(!is.na(first_date) &
                          (is.na(censor_date) | first_date <= censor_date)),
      end_date = if_else(ev == 1L, first_date, censor_date),
      days  = as.numeric(end_date - baseline_date),
      years = days / 365.25
    )

  if (new_onset) {
    excl <- out$id %in% prior_ids
    out$ev[excl]    <- NA_integer_
    out$days[excl]  <- NA_real_
    out$years[excl] <- NA_real_
    message(disease, ": ", sum(excl), " patients excluded (disease before baseline)")
  }

  nm <- paste0("EV_", toupper(disease))

  out %>%
    transmute(id,
              !!nm                    := ev,
              !!paste0(nm, "_DAYS")   := days,
              !!paste0(nm, "_YEARS")  := years)
}
