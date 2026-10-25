calculate_LYL_onset <- function(
    Disease = "HF",
    index_age = 18,
    cutoffs = c(45, 55, 65, 75, 85),
    tau = 95,
    Niter = 1000,
    DO_OVERALL = TRUE,
    DO_MALE = TRUE,
    DO_FEMALE = TRUE,

    # Must be TRUE only if Life_exp is restricted
    # remaining life expectancy up to age 95.
    life_exp_restricted_to_tau = FALSE,

    disease_dir =
      "/safe/data/APNC/DATA/ANALYSIS DATA",

    lifetable_file =
      "/safe/data/APNC/SCARP-lifecourse/Data/Derived/Lifetables_per_year.csv",

    death_file =
      "/safe/data/SCARP/DATA_2024/PARQUET/ut_r_dors_19328_2023.parquet",

    death_avi_file =
      "/safe/data/SCARP/DATA_2024/PARQUET/ut_r_dors_avi_19328_2023.parquet",

    migration_file =
      "/safe/data/SCARP/DATA_2024/PARQUET/ar_lev_migrationer.parquet",

    output_dir =
      "/safe/data/APNC/SCARP-lifecourse/Data/Derived/ltr/45/Diagnoses/plot_data",

    admin_end = as.Date("2024-01-01")
) {

  # ==================================================
  # 1. PACKAGES
  # ==================================================

  library(dplyr)
  library(tidyr)
  library(purrr)
  library(arrow)
  library(readr)
  library(lill)

  message("Starting analysis: ", Disease)

  if (!life_exp_restricted_to_tau) {
    stop(
      "The life-table variable Life_exp must represent ",
      "remaining life expectancy restricted to age ", tau,
      ". Ordinary remaining life expectancy cannot be ",
      "used in tau - age - Life_exp. ",
      "Verify or reconstruct the reference measure first."
    )
  }

  if (tau != 95) {
    stop(
      "This function currently assumes life-table ",
      "expectancy is restricted to age 95."
    )
  }

  # ==================================================
  # 2. LOAD DISEASE DATA
  # ==================================================

  disease_file <- file.path(
    disease_dir,
    paste0(
      "ICD_",
      Disease,
      "_all_unique_2_1987_2024.parquet"
    )
  )

  disease_data <- arrow::read_parquet(
    disease_file
  ) %>%
    filter(
      AR >= 1990,
      !is.na(lopnr),
      !is.na(alder)
    ) %>%
    mutate(
      alder = as.numeric(alder),
      B_Year = as.numeric(B_Year),
      Kon = as.integer(Kon),
      Diagnosis_year = as.integer(AR)
    )

  # Keep first recorded disease diagnosis
  disease_data <- disease_data %>%
    arrange(lopnr, Diagnosis_year, alder) %>%
    distinct(lopnr, .keep_all = TRUE)

  message(
    "Unique disease cases: ",
    nrow(disease_data)
  )

  # ==================================================
  # 3. LOAD AND PREPARE LIFE TABLES
  # ==================================================

  Lifetables <- read.csv(
    lifetable_file,
    check.names = TRUE
  )

  # Remove an optional index column only when present
  Lifetables <- Lifetables %>%
    select(
      -any_of(c("X", "X.1", "...1"))
    )

  Lifetables_long <- Lifetables %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "Year",
      values_to = "Life_exp"
    ) %>%
    mutate(
      Year = as.integer(
        sub("^X", "", Year)
      ),
      age = as.numeric(age),
      Gender = as.integer(Gender),
      Life_exp = as.numeric(Life_exp)
    ) %>%
    select(
      Gender,
      age,
      Year,
      Life_exp
    )

  if (anyDuplicated(
    Lifetables_long[c("Gender", "age", "Year")]
  ) > 0) {
    stop(
      "Life tables contain duplicate ",
      "sex-age-year combinations."
    )
  }

  message("Life tables loaded.")

  # ==================================================
  # 4. LOAD DEATH DATA
  # ==================================================

  read_deaths <- function(path) {

    arrow::read_parquet(path) %>%
      transmute(
        lopnr = LopNr,
        Death_date = as.Date(
          as.character(DODSDAT),
          format = "%Y%m%d"
        )
      ) %>%
      filter(!is.na(Death_date))
  }

  CDR <- bind_rows(
    read_deaths(death_file),
    read_deaths(death_avi_file)
  ) %>%
    group_by(lopnr) %>%
    summarise(
      Death_date = min(Death_date),
      .groups = "drop"
    )

  message("Death data loaded.")

  # ==================================================
  # 5. LOAD EMIGRATION DATA
  # ==================================================

  migration_data <- arrow::read_parquet(
    migration_file
  ) %>%
    filter(posttyp == "Utv") %>%
    transmute(
      lopnr,
      Emigration_date = as.Date(
        as.character(datum),
        format = "%Y%m%d"
      )
    ) %>%
    filter(!is.na(Emigration_date))

  # ==================================================
  # 6. PREPARE CASES AND FOLLOW-UP
  # ==================================================

  # Assumes INDATUM is the date of first diagnosis.
  # The original date must be checked against alder.

  cases <- disease_data %>%
    mutate(
      Diagnosis_date = as.Date(
        as.character(INDATUM),
        format = "%Y%m%d"
      )
    ) %>%
    filter(
      !is.na(Diagnosis_date),
      !is.na(B_Year),
      alder >= index_age,
      alder < tau,
      Diagnosis_date < admin_end
    ) %>%
    select(
      lopnr,
      alder,
      B_Year,
      Kon,
      Diagnosis_year,
      Diagnosis_date
    ) %>%
    left_join(
      CDR,
      by = "lopnr"
    )

  # Select first emigration occurring after diagnosis
  migration_after_diagnosis <- migration_data %>%
    inner_join(
      cases %>%
        select(lopnr, Diagnosis_date),
      by = "lopnr"
    ) %>%
    filter(
      Emigration_date >= Diagnosis_date
    ) %>%
    group_by(lopnr) %>%
    summarise(
      Emigration_date = min(Emigration_date),
      .groups = "drop"
    )

  cases <- cases %>%
    left_join(
      migration_after_diagnosis,
      by = "lopnr"
    )

  # Exclude individuals who died before diagnosis
  cases <- cases %>%
    filter(
      is.na(Death_date) |
        Death_date >= Diagnosis_date
    )

  # Administrative end and emigration censoring
  cases <- cases %>%
    mutate(
      Censor_date = pmin(
        coalesce(Emigration_date, admin_end),
        admin_end
      ),

      # Event occurs only if death precedes censoring
      Died = as.integer(
        !is.na(Death_date) &
          Death_date <= Censor_date
      ),

      End_date = if_else(
        Died == 1L,
        Death_date,
        Censor_date
      ),

      Followup_years = as.numeric(
        End_date - Diagnosis_date
      ) / 365.25,

      # Approximate attained age using recorded
      # age at diagnosis plus elapsed time.
      Death_age = pmin(
        alder + Followup_years,
        tau
      )
    ) %>%
    filter(
      !is.na(Followup_years),
      Followup_years > 0,
      Death_age > alder
    ) %>%
    mutate(
      # Death at or after tau is censored at tau
      Died = if_else(
        alder + Followup_years >= tau,
        0L,
        Died
      )
    )

  message(
    "Cases included after follow-up preparation: ",
    nrow(cases)
  )

  # ==================================================
  # 7. CREATE EARLY- AND LATE-ONSET GROUPS
  # ==================================================

  onset_data <- purrr::map_dfr(
    cutoffs,
    function(cutoff) {

      cases %>%
        mutate(
          Cutoff = cutoff,

          Onset_group = if_else(
            alder < cutoff,
            "Early",
            "Late"
          )
        )
    }
  )

  # ==================================================
  # 8. HELPER: CALCULATE ONE GROUP
  # ==================================================

  calculate_one_group <- function(
      cutoff,
      onset_group,
      sex
  ) {

    message(
      Disease,
      " | Cutoff: ", cutoff,
      " | ", onset_group,
      " | ", sex
    )

    sub <- onset_data %>%
      filter(
        Cutoff == cutoff,
        Onset_group == onset_group
      )

    if (sex == "Male") {
      sub <- sub %>%
        filter(Kon == 1)
    }

    if (sex == "Female") {
      sub <- sub %>%
        filter(Kon == 2)
    }

    N <- nrow(sub)

    empty_result <- function() {
      data.frame(
        Disease = Disease,
        Cutoff = cutoff,
        Onset_group = onset_group,
        Sex = sex,
        N = N,
        Deaths = sum(sub$Died, na.rm = TRUE),
        Patient_LYL = NA_real_,
        Patient_CI_lower = NA_real_,
        Patient_CI_upper = NA_real_,
        Reference_LYL = NA_real_,
        Excess_LYL = NA_real_,
        Excess_CI_lower = NA_real_,
        Excess_CI_upper = NA_real_
      )
    }

    if (N < 10) {
      warning(
        "Too few cases: ",
        Disease, " ", cutoff, " ",
        onset_group, " ", sex
      )
      return(empty_result())
    }

    # ================================================
    # 8A. MATCH LIFE TABLES TO CASES
    # ================================================

    reference <- sub %>%
      transmute(
        lopnr,
        age = as.numeric(alder),
        Gender = as.integer(Kon),
        Year = as.integer(Diagnosis_year)
      ) %>%
      left_join(
        Lifetables_long,
        by = c(
          "age",
          "Gender",
          "Year"
        ),
        relationship = "many-to-one"
      )

    if (nrow(reference) != N) {
      stop(
        "Reference matching changed the number ",
        "of cases for ", Disease, " ", cutoff,
        " ", onset_group, " ", sex
      )
    }

    missing_reference <- sum(
      is.na(reference$Life_exp)
    )

    if (missing_reference > 0) {
      warning(
        missing_reference,
        " missing reference values for ",
        Disease, " | ", cutoff,
        " | ", onset_group,
        " | ", sex
      )

      return(empty_result())
    }

    # ================================================
    # 8B. REFERENCE LIFE-YEARS LOST
    # ================================================

    reference <- reference %>%
      mutate(
        Reference_LYL =
          tau - age - Life_exp
      )

    reference_lyl <- mean(
      reference$Reference_LYL
    )

    # ================================================
    # 8C. PATIENT LIFE-YEARS LOST
    # ================================================

    fit <- tryCatch(

      lill::lyl_ci(
        lill::lyl_range(
          data = sub,
          t0 = alder,
          t = Death_age,
          status = Died,
          age_begin = index_age,
          age_end = tau - 1,
          tau = tau
        ),
        niter = Niter
      ),

      error = function(e) {
        warning(
          "LYL estimation failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (is.null(fit)) {
      return(empty_result())
    }

    s <- tryCatch(
      summary(fit),
      error = function(e) {
        warning(
          "LYL summary failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (is.null(s)) {
      return(empty_result())
    }

    required_fields <- c(
      "lyl_estimate.TotalLYL",
      "lyl_ci_left.TotalLYL",
      "lyl_ci_right.TotalLYL"
    )

    if (!all(required_fields %in% names(s))) {
      warning(
        "Unexpected lill summary structure. ",
        "Inspect names(summary(fit))."
      )
      return(empty_result())
    }

    estimate <- as.numeric(
      s$lyl_estimate.TotalLYL
    )

    lower <- as.numeric(
      s$lyl_ci_left.TotalLYL
    )

    upper <- as.numeric(
      s$lyl_ci_right.TotalLYL
    )

    if (
      length(estimate) != 1 ||
      length(lower) != 1 ||
      length(upper) != 1 ||
      any(!is.finite(c(estimate, lower, upper)))
    ) {
      warning(
        "Invalid LYL estimate or CI for ",
        Disease, " ", cutoff, " ",
        onset_group, " ", sex
      )
      return(empty_result())
    }

    # ================================================
    # 8D. EXCESS LIFE-YEARS LOST
    # ================================================

    excess_lyl <- estimate - reference_lyl

    # Conditional CI: treats the reference as fixed.
    # This is not a full bootstrap CI for excess LYL.

    excess_lower <- lower - reference_lyl
    excess_upper <- upper - reference_lyl

    # ================================================
    # 8E. RETURN RESULTS
    # ================================================

    data.frame(
      Disease = Disease,
      Cutoff = cutoff,
      Onset_group = onset_group,
      Sex = sex,
      N = N,
      Deaths = sum(sub$Died, na.rm = TRUE),

      Patient_LYL = estimate,
      Patient_CI_lower = lower,
      Patient_CI_upper = upper,

      Reference_LYL = reference_lyl,

      Excess_LYL = excess_lyl,
      Excess_CI_lower = excess_lower,
      Excess_CI_upper = excess_upper
    )
  }

  # ==================================================
  # 9. RUN ALL ANALYSES
  # ==================================================

  sexes <- character()

  if (DO_OVERALL) {
    sexes <- c(sexes, "Overall")
  }

  if (DO_MALE) {
    sexes <- c(sexes, "Male")
  }

  if (DO_FEMALE) {
    sexes <- c(sexes, "Female")
  }

  results <- tidyr::expand_grid(
    Cutoff = cutoffs,
    Onset_group = c("Early", "Late"),
    Sex = sexes
  ) %>%
    mutate(
      result = purrr::pmap(
        list(
          Cutoff,
          Onset_group,
          Sex
        ),
        calculate_one_group
      )
    ) %>%
    select(result) %>%
    unnest(result)

  # ==================================================
  # 10. SAVE RESULTS
  # ==================================================

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    output_dir,
    paste0(
      Disease,
      "_early_late_onset_matched_reference.csv"
    )
  )

  readr::write_csv(
    results,
    output_file
  )

  message(
    "Analysis completed: ",
    Disease
  )

  message(
    "Results saved to: ",
    output_file
  )

  return(results)
}
