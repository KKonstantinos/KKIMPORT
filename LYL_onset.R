
calculate_LYL_onset <- function(
    index_age = 18,
    Disease = "MI",
    DO_OVERALL = TRUE,
    DO_MALE = TRUE,
    DO_FEMALE = TRUE,
    Niter = 20,
    cutoffs = c(45, 55, 65, 75, 85),
    tau = 95,
    output_dir = "/safe/data/APNC/SCARP-lifecourse/Data/Derived/ltr/45/Diagnoses/plot_data"
) {

  library(dplyr)
  library(arrow)
  library(readr)
  library(tidyr)
  library(purrr)

  message("Loading data for ", Disease)

  # --------------------------------------------------
  # 1. LOAD DISEASE DATA
  # --------------------------------------------------

  data_name <- paste0(Disease, "_Data")

  if (!exists(data_name, envir = .GlobalEnv)) {

    file_path <- paste0(
      "/safe/data/APNC/DATA/ANALYSIS DATA/ICD_",
      paste(Disease, "all_unique", "2", "1987", "2024",
            sep = "_"),
      ".parquet"
    )

    disease_data <- arrow::read_parquet(file_path) %>%
      group_by(AR) %>%
      filter(AR >= 1990) %>%
      ungroup() %>%
      distinct()

    assign(data_name, disease_data, envir = .GlobalEnv)

  } else {
    disease_data <- get(data_name, envir = .GlobalEnv)
  }

  # --------------------------------------------------
  # 2. LOAD LIFE TABLES
  # --------------------------------------------------

  Lifetables <- read.csv(
    "/safe/data/APNC/SCARP-lifecourse/Data/Derived/Lifetables_per_year.csv"
  )[, -1]

  Columns <- Lifetables %>%
    colnames() %>%
    .[c(2:34)]

  # Preserve your original life-table method
  # but retain the age-specific reference values.

  Total <- disease_data %>%
    distinct(lopnr) %>%
    nrow()

  Weights_for_lifetables <- disease_data %>%
    count(AR) %>%
    mutate(
      Total = Total,
      Weights = n / Total
    )

  Life_exp <- Lifetables %>%
    select(-c(Gender, age)) %>%
    rowwise() %>%
    mutate(
      weighted_Life_exp = weighted.mean(
        c_across(X1990:X2022),
        Weights_for_lifetables$Weights,
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(weighted_Life_exp)

  Control_life_exp <- Lifetables %>%
    select(Gender, age) %>%
    bind_cols(Life_exp) %>%
    mutate(
      LYL = 95 - age - weighted_Life_exp
    )

  # --------------------------------------------------
  # 3. LOAD DEATH DATA
  # --------------------------------------------------

  if (!exists("TPR", envir = .GlobalEnv)) {

    TPR <- arrow::open_dataset(
      "/safe/data/SCARP/DATA_2024/PARQUET/ar_lev_grunduppgifter.parquet"
    ) %>%
      collect()

    assign("TPR", TPR, envir = .GlobalEnv)

  } else {
    TPR <- get("TPR", envir = .GlobalEnv)
  }

  if (!exists("CDR", envir = .GlobalEnv)) {

    CDR <- arrow::open_dataset(
      "/safe/data/SCARP/DATA_2024/PARQUET/ut_r_dors_19328_2023.parquet"
    ) %>%
      collect() %>%
      mutate(
        Death_Year = as.numeric(substr(DODSDAT, 1, 4)),
        Death_dt = as.yearmon(DODSDAT, "%Y%m")
      ) %>%
      select(LopNr, Death_Year, Death_dt) %>%
      rename(lopnr = LopNr)

    assign("CDR", CDR, envir = .GlobalEnv)

  } else {
    CDR <- get("CDR", envir = .GlobalEnv)
  }

  if (!exists("CDR_avi", envir = .GlobalEnv)) {

    CDR_avi <- arrow::open_dataset(
      "/safe/data/SCARP/DATA_2024/PARQUET/ut_r_dors_avi_19328_2023.parquet"
    ) %>%
      collect() %>%
      mutate(
        Death_Year = as.numeric(substr(DODSDAT, 1, 4)),
        Death_dt = as.yearmon(DODSDAT, "%Y%m")
      ) %>%
      select(LopNr, Death_Year, Death_dt) %>%
      rename(lopnr = LopNr) %>%
      anti_join(CDR, by = "lopnr")

    assign("CDR_avi", CDR_avi, envir = .GlobalEnv)

  } else {
    CDR_avi <- get("CDR_avi", envir = .GlobalEnv)
  }

  CDR <- bind_rows(CDR, CDR_avi) %>%
    distinct(lopnr, .keep_all = TRUE)

  gc()

  # --------------------------------------------------
  # 4. LOAD IMMIGRATION DATA
  # --------------------------------------------------

  if (!exists("imig", envir = .GlobalEnv)) {

    imig <- arrow::open_dataset(
      "/safe/data/SCARP/DATA_2024/PARQUET/ar_lev_migrationer.parquet"
    ) %>%
      collect() %>%
      filter(posttyp == "Utv") %>%
      mutate(
        imig_Dates = as.Date(datum, "%Y%m%d"),
        imig_Yr = year(imig_Dates)
      ) %>%
      select(lopnr, imig_Dates, imig_Yr) %>%
      group_by(lopnr) %>%
      slice_min(imig_Dates, with_ties = FALSE) %>%
      ungroup() %>%
      left_join(TPR, by = "lopnr") %>%
      mutate(
        Age_1990 = 1990 - as.numeric(Fodelsear),
        imig_age = imig_Yr - as.numeric(Fodelsear),
        imig_entry_age = imig_age,
        imigated = TRUE,
        older_than_index_age = Age_1990 > index_age
      )

    assign("imig", imig, envir = .GlobalEnv)

  } else {
    imig <- get("imig", envir = .GlobalEnv)
  }

  # --------------------------------------------------
  # 5. PREPARE CASES
  # --------------------------------------------------

  cases <- disease_data %>%
    left_join(CDR, by = "lopnr") %>%
    filter(alder >= index_age) %>%
    mutate(
      ev_Death = if_else(
        Doddattum == "" | alder >= 95,
        0L, 1L
      ),
      group = Disease,
      Death_Age = if_else(
        Doddattum == "",
        pmin(2024 - B_Year, 95),
        pmin(Death_Year - B_Year, 95)
      ),
      Death_dt = as.Date(
        pmin(Death_dt, as.yearmon("2024-01")),
        frac = 0
      ),
      EOS_date = as.Date("2024-01-01")
    ) %>%
    select(
      lopnr, alder, group, INDATUM, UTDATUM,
      EOS_date, Death_dt, B_Year, Kon
    ) %>%
    distinct()

  duplicated_lopnrs <- cases %>%
    count(lopnr) %>%
    filter(n > 1) %>%
    pull(lopnr)

  cases <- cases %>%
    filter(!lopnr %in% duplicated_lopnrs)

  # --------------------------------------------------
  # 6. PREPARE FOLLOW-UP
  # --------------------------------------------------

  temp <- cases %>%
    select(
      lopnr, alder, B_Year, INDATUM, UTDATUM,
      EOS_date, Death_dt, Kon
    ) %>%
    left_join(
      imig %>%
        select(lopnr, imig_Dates, imig_Yr),
      by = "lopnr"
    ) %>%
    mutate(
      imig_Dates = if_else(
        is.na(imig_Dates),
        EOS_date,
        imig_Dates
      ),
      imig_Dates = if_else(
        imig_Dates != INDATUM,
        imig_Dates,
        EOS_date
      ),
      Time_Death = as.numeric(
        pmin(Death_dt, imig_Dates, na.rm = TRUE) -
          INDATUM + 1
      ),
      ev_Death = if_else(
        is.na(Death_dt), "NO", "YES"
      )
    ) %>%
    filter(Time_Death > 0)

  analysis_set <- temp %>%
    mutate(
      Died = if_else(ev_Death == "YES", 1L, 0L),

      Death_age = if_else(
        ev_Death == "YES",
        year(Death_dt) - B_Year +
          year(imig_Dates > B_Year),
        alder + Time_Death / 365.25
      ),

      Death_age = pmin(Death_age, tau),

      onset_age = as.numeric(alder)
    ) %>%
    as.data.frame()

  # --------------------------------------------------
  # 7. DEFINE EARLY/LATE ONSET GROUPS
  # --------------------------------------------------

  onset_data <- purrr::map_dfr(
    cutoffs,
    function(cutoff) {

      analysis_set %>%
        mutate(
          Cutoff = cutoff,
          Onset_group = if_else(
            onset_age < cutoff,
            "Early",
            "Late"
          )
        )
    }
  )

  # --------------------------------------------------
  # 8. HELPER FUNCTION FOR ONE GROUP
  # --------------------------------------------------

  calculate_one_group <- function(
      df,
      cutoff,
      onset_group,
      sex
  ) {

    sub <- df %>%
      filter(
        Cutoff == cutoff,
        Onset_group == onset_group,
        onset_age >= index_age,
        onset_age < tau,
        Death_age > onset_age
      )

    if (sex == "Male") {
      sub <- sub %>% filter(Kon == 1)
    }

    if (sex == "Female") {
      sub <- sub %>% filter(Kon == 2)
    }

    N <- nrow(sub)

    if (N < 10) {
      return(data.frame(
        Disease = Disease,
        Cutoff = cutoff,
        Onset_group = onset_group,
        Sex = sex,
        N = N,
        LYL = NA_real_,
        CI_lower = NA_real_,
        CI_upper = NA_real_,
        Reference_LYL = NA_real_,
        Excess_LYL = NA_real_
      ))
    }

    # Calculate remaining life-years using the
    # same lill method as the original analysis.

    fit <- lill::lyl_ci(
      lyl_range(
        data = sub,
        t0 = alder,
        t = Death_age,
        status = Died,
        age_begin = index_age,
        age_end = 94,
        tau = tau
      ),
      niter = Niter
    )

    s <- summary(fit)

    # ------------------------------------------------
    # REFERENCE LIFE EXPECTANCY FOR THIS ONSET GROUP
    # ------------------------------------------------

    age_weights <- sub %>%
      count(Kon, alder, name = "n") %>%
      mutate(weight = n / sum(n))

    # Adjust gender coding if life-table Gender is
    # stored as character labels instead of 1/2.

    reference <- age_weights %>%
      left_join(
        Control_life_exp,
        by = c(
          "Kon" = "Gender",
          "alder" = "age"
        )
      )

    reference_lyl <- weighted.mean(
      reference$LYL,
      reference$weight,
      na.rm = TRUE
    )

    # The original code calculates:
    # reference life-years - estimated life-years

    estimate <- s$lyl_estimate.TotalLYL
    lower <- s$lyl_ci_left.TotalLYL
    upper <- s$lyl_ci_right.TotalLYL

    data.frame(
      Disease = Disease,
      Cutoff = cutoff,
      Onset_group = onset_group,
      Sex = sex,
      N = N,
      LYL = estimate,
      CI_lower = lower,
      CI_upper = upper,
      Reference_LYL = reference_lyl,
      Excess_LYL = reference_lyl - estimate
    )
  }

  # --------------------------------------------------
  # 9. RUN ALL ANALYSES
  # --------------------------------------------------

  sexes <- c()

  if (DO_OVERALL) sexes <- c(sexes, "Overall")
  if (DO_MALE) sexes <- c(sexes, "Male")
  if (DO_FEMALE) sexes <- c(sexes, "Female")

  results <- tidyr::expand_grid(
    Cutoff = cutoffs,
    Onset_group = c("Early", "Late"),
    Sex = sexes
  ) %>%
    mutate(
      result = purrr::pmap(
        list(Cutoff, Onset_group, Sex),
        function(Cutoff, Onset_group, Sex) {

          message(
            Disease, " | Cutoff: ", Cutoff,
            " | ", Onset_group,
            " | ", Sex
          )

          calculate_one_group(
            df = onset_data,
            cutoff = Cutoff,
            onset_group = Onset_group,
            sex = Sex
          )
        }
      )
    ) %>%
    select(result) %>%
    unnest(result)

  # --------------------------------------------------
  # 10. SAVE RESULTS
  # --------------------------------------------------

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    output_dir,
    paste0(
      Disease,
      "_early_late_onset_LYL.csv"
    )
  )

  readr::write_csv(results, output_file)

  message("Results saved to: ", output_file)

  return(results)
}
HF_results <- calculate_LYL_onset(
  Disease = "HF",
  Niter = 1000
)

IHD_results <- calculate_LYL_onset(
  Disease = "IHD",
  Niter = 1000
)

VTE_results <- calculate_LYL_onset(
  Disease = "VTE",
  Niter = 1000
)

all_results <- bind_rows(
  HF_results,
  IHD_results,
  VTE_results
)
