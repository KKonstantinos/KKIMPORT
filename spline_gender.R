library(haven)
library(dplyr)
library(splines)
library(nlme)
library(ggplot2)

path <- "/safe/data/APNC/SCARP ACRO-DM/Data/Derived/"
adsl <- read_sas(paste0(path, "adsl.sas7bdat")) %>% select(LopNr, bl_hba1c, randgrp, N_disp, Sex)  

advl <- read_sas(paste0(path, "advl.sas7bdat")) %>%
  transmute(LopNr, randgrp, hba1c, Months_from_baseline = Days_from_baseline / 30.4) %>%
  left_join(adsl, by = c("LopNr", "randgrp")) %>%
  mutate(hba1c_ch = hba1c - bl_hba1c,
         SEX = factor(Sex, levels = 1:2, labels = c("Male", "Female"))) %>%  
  filter(N_disp > 1, randgrp == 1) %>%
  na.omit()

# Common spline basis so curves are comparable across sexes
bknots <- range(advl$Months_from_baseline)
spl <- function(x) setNames(as.data.frame(unclass(ns(x, knots = c(6, 12), Boundary.knots = bknots))[, 1:3]),
                            c("spl1", "spl2", "spl3"))
advl <- bind_cols(advl, spl(advl$Months_from_baseline))
t0   <- bknots[1]

fit_sex <- function(d) {
  fit <- lme(hba1c_ch ~ bl_hba1c + spl1 + spl2 + spl3, random = ~ 1 | LopNr,
             correlation = corExp(form = ~ Months_from_baseline | LopNr),
             method = "REML", data = d)
  grid <- seq(t0, max(d$Months_from_baseline), by = 1)
  pred <- bind_cols(data.frame(Months_from_baseline = grid, bl_hba1c = mean(advl$bl_hba1c)), spl(grid))
  mm   <- model.matrix(~ bl_hba1c + spl1 + spl2 + spl3, pred)
  pred %>% mutate(fit = predict(fit, pred, level = 0),
                  se  = sqrt(rowSums((mm %*% vcov(fit)) * mm)),
                  lwr = fit - 1.96 * se, upr = fit + 1.96 * se,
                  SEX = d$SEX[1], Nobs_SGLT2i = nobs(fit))
}

pred_all <- advl %>% group_split(SEX) %>% lapply(fit_sex) %>% bind_rows()

# Table: 6 months, 1 year, 2 years
res1 <- pred_all %>%
  mutate(offset = round(Months_from_baseline - t0)) %>%
  filter(offset %in% c(6, 12, 24)) %>%
  transmute(SEX, Description = "HbA1c",
            label = paste("Change from baseline at", c(`6` = "6 months", `12` = "1 year", `24` = "2 years")[as.character(offset)]),
            est_SGLT2i = sprintf("%.2f (%.2f, %.2f)", fit, lwr, upr),
            Nobs_SGLT2i, pvalue_SGLT2i = 2 * (1 - pnorm(abs(fit / se))))
print(res1)

# Plot
ggplot(pred_all, aes(Months_from_baseline, fit, colour = SEX, fill = SEX)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Months from baseline", y = "Change in HbA1c from baseline", colour = "Sex", fill = "Sex") +
  theme_minimal()
