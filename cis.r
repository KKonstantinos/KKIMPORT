CREATE_DATA_FOR_PLOT_FIG <- function(dataset = SwedeHF_COHORT_PER3_dcm,
                                     NAME = "DCM",
                                     conf_level = 0.95) {

  z  <- qnorm(1 - (1 - conf_level) / 2)
  ci <- with(dataset, cuminc(ev_alldeathsyr, ev_alldeaths))

  ci_df <- bind_rows(
    lapply(setdiff(names(ci), "Tests"), function(nm) {
      est <- ci[[nm]]$est
      se  <- sqrt(ci[[nm]]$var)

      # log(-log) transformed CI (bounded between 0 and 1)
      inside <- est > 0 & est < 1
      lower <- upper <- est
      lower[inside] <- est[inside]^exp( z * se[inside] / (est[inside] * log(est[inside])))
      upper[inside] <- est[inside]^exp(-z * se[inside] / (est[inside] * log(est[inside])))

      data.frame(time  = ci[[nm]]$time,
                 est   = est,
                 lower = lower,
                 upper = upper,
                 event = nm)
    })
  )

  # drop censoring
  ci_df <- ci_df %>% filter(event != "0")

  ci_df$event <- factor(ci_df$event,
                        levels = sort(unique(ci_df$event)),
                        labels = c("Cancer death",
                                   "Sudden cardiac death",
                                   "Cardiac pump failure death",
                                   "Other cardiovascular death",
                                   "Other death"))

  ci_df$Grouping <- NAME
  return(ci_df)
}
