CREATE_DATA_FOR_PLOT_FIG <- function(dataset = SwedeHF_COHORT_PER3_dcm,
                                     NAME = "DCM",
                                     conf_level = 0.95) {

  z <- qnorm(1 - (1 - conf_level) / 2)

  # helper: turn one cuminc curve into a data frame with log(-log) CI
  curve_to_df <- function(curve, event_name) {
    est <- curve$est
    se  <- sqrt(curve$var)
    inside <- est > 0 & est < 1
    lower <- upper <- est
    lower[inside] <- est[inside]^exp( z * se[inside] / (est[inside] * log(est[inside])))
    upper[inside] <- est[inside]^exp(-z * se[inside] / (est[inside] * log(est[inside])))
    data.frame(time = curve$time, est = est, lower = lower, upper = upper,
               event = event_name)
  }

  # ---- cause-specific deaths ----
  ci <- with(dataset, cuminc(ev_alldeathsyr, ev_alldeaths))

  ci_df <- bind_rows(
    lapply(setdiff(names(ci), "Tests"), function(nm) curve_to_df(ci[[nm]], nm))
  )

  # drop censoring
  ci_df <- ci_df %>% filter(event != "0")

  ci_df$event <- as.character(factor(ci_df$event,
                        levels = sort(unique(ci_df$event)),
                        labels = c("Cancer death",
                                   "Sudden cardiac death",
                                   "Cardiac pump failure death",
                                   "Other cardiovascular death",
                                   "Other death")))

  # ---- all-cause death (any death = event, no competing risks) ----
  ci_tot <- with(dataset, cuminc(ev_alldeathsyr, as.integer(ev_alldeaths != 0)))
  nm_tot <- setdiff(names(ci_tot), "Tests")
  nm_tot <- nm_tot[sub(".* ", "", nm_tot) == "1"]          # the death curve, not censoring

  total_df <- curve_to_df(ci_tot[[nm_tot]], "All-cause death")

  # ---- combine ----
  ci_df <- bind_rows(ci_df, total_df)
  ci_df$event <- factor(ci_df$event,
                        levels = c("Cancer death",
                                   "Sudden cardiac death",
                                   "Cardiac pump failure death",
                                   "Other cardiovascular death",
                                   "Other death",
                                   "All-cause death"))

  ci_df$Grouping <- NAME
  return(ci_df)
}





CREATE_DATA_FOR_PLOT_FIG <- function(dataset = SwedeHF_COHORT_ICD_dcm,
                                     NAME = "DCM",
                                     conf_level = 0.95) {

  z <- qnorm(1 - (1 - conf_level) / 2)

  # event code -> label (same coding as ev_alldeaths)
  cause_labels <- c("1" = "Cancer death",
                    "2" = "Sudden cardiac death",
                    "3" = "Cardiac pump failure death",
                    "4" = "Other cardiovascular death",
                    "5" = "Other death")

  # helper: one cuminc curve -> data frame with log(-log) CI
  curve_to_df <- function(curve, event_name) {
    est <- curve$est
    se  <- sqrt(curve$var)
    inside <- est > 0 & est < 1
    lower <- upper <- est
    lower[inside] <- est[inside]^exp( z * se[inside] / (est[inside] * log(est[inside])))
    upper[inside] <- est[inside]^exp(-z * se[inside] / (est[inside] * log(est[inside])))
    data.frame(time = curve$time, est = est, lower = lower, upper = upper,
               event = event_name)
  }

  # ---- cause-specific deaths ----
  ci <- with(dataset, cuminc(ev_alldeathsyr, ev_alldeaths))
  nms <- setdiff(names(ci), "Tests")

  ci_df <- bind_rows(
    lapply(nms, function(nm) {
      code <- sub(".* ", "", nm)               # "1 3" -> "3"
      if (code == "0") return(NULL)            # drop censoring
      curve_to_df(ci[[nm]], cause_labels[[code]])
    })
  )

  # ---- all-cause death ----
  ci_tot <- with(dataset, cuminc(ev_alldeathsyr, as.integer(ev_alldeaths != 0)))
  nm_tot <- setdiff(names(ci_tot), "Tests")
  nm_tot <- nm_tot[sub(".* ", "", nm_tot) == "1"]

  total_df <- curve_to_df(ci_tot[[nm_tot]], "All-cause death")

  # ---- combine ----
  ci_df <- bind_rows(ci_df, total_df)
  ci_df$event <- factor(ci_df$event,
                        levels = c(unname(cause_labels), "All-cause death"))
  ci_df <- droplevels(ci_df)                   # remove causes that don't occur

  ci_df$Grouping <- NAME
  return(ci_df)
}
