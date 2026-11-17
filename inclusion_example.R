# Flowchart counts of unique patients (LopNr), with exclusions applied sequentially
#
# Returns one row per step:
#   All      - all unique ids in the data
#   FAS      - unique ids with the FAS flag
#   excl...  - every column whose name starts with "excl", in column order,
#              applied one after another to what remains of the FAS population
#
# Columns in the result:
#   step         - step number (0 = all, 1 = FAS, 2... = exclusions in order)
#   variable     - the column used at this step
#   description  - what the variable means: taken from the `descriptions`
#                  argument if given, else the column's "label" attribute
#                  (kept by haven::read_sas / read_dta), else the column name
#   n_flagged    - unique ids with this flag anywhere in the data (unconditional)
#   n_excluded   - ids removed at this step (flagged AND still remaining)
#   n_remaining  - ids left after this step
#
# A flag counts when the value is 1 (or TRUE); 0, other values and NA do not.
# An id counts as flagged if ANY of its rows is flagged.

flow_counts <- function(data, id = "LopNr", fas = "FAS", prefix = "^excl",
                        descriptions = NULL) {
  flag <- function(x) x %in% 1                      # NA -> FALSE, TRUE == 1
  ids_where <- function(rows) unique(stats::na.omit(data[[id]][rows]))

  describe <- function(v) {
    if (!is.null(descriptions) && !is.na(descriptions[v])) return(unname(descriptions[v]))
    lab <- attr(data[[v]], "label", exact = TRUE)
    if (!is.null(lab) && nzchar(lab)) return(as.character(lab))
    v
  }

  all_ids   <- unique(stats::na.omit(data[[id]]))
  fas_ids   <- ids_where(flag(data[[fas]]))
  excl_vars <- grep(prefix, names(data), value = TRUE)  # keeps column order

  out <- data.frame(
    step        = c(0L, 1L),
    variable    = c(id, fas),
    description = c("All unique patients in data", describe(fas)),
    n_flagged   = c(length(all_ids), length(fas_ids)),
    n_excluded  = c(0L, length(all_ids) - length(fas_ids)),
    n_remaining = c(length(all_ids), length(fas_ids))
  )

  remaining <- fas_ids
  for (i in seq_along(excl_vars)) {
    v         <- excl_vars[i]
    flagged   <- ids_where(flag(data[[v]]))
    excluded  <- intersect(remaining, flagged)   # only those still in the cohort
    remaining <- setdiff(remaining, excluded)
    out <- rbind(out, data.frame(
      step        = i + 1L,
      variable    = v,
      description = describe(v),
      n_flagged   = length(flagged),
      n_excluded  = length(excluded),
      n_remaining = length(remaining)
    ))
  }
  out
}

# Example
# flow_counts(mydata)
#
# With your own descriptions (any variable left out falls back to its label/name):
# flow_counts(mydata, descriptions = c(
#   FAS   = "Full analysis set",
#   excl1 = "Age < 18 at index",
#   excl2 = "Prior heart transplant"
# ))
