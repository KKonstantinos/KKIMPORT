# ---------------------------------------------------------------------
# pull_repo.R
# Clones the GitHub repo the first time, and pulls updates after that.
# ---------------------------------------------------------------------

# ---- Settings: change these -----------------------------------------
repo_url <- "https://github.com/KKonstantinos/KKIMPORT.git"
repo_dir <- "C:/projects/KKIMPORT"   # where the code should live locally
branch   <- "main"                   # branch to pull ("main" or "master")

# ---- Helper: run a git command and stop if it fails ------------------
run_git <- function(args) {
  out <- system2("git", args, stdout = TRUE, stderr = TRUE)
  cat(out, sep = "\n")
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("Git command failed: git ", paste(args, collapse = " "), call. = FALSE)
  }
  invisible(out)
}

# ---- Check that git is installed ------------------------------------
if (Sys.which("git") == "") {
  stop("Git was not found. Install it from https://git-scm.com ",
       "and restart RStudio.", call. = FALSE)
}

# ---- Clone or pull --------------------------------------------------
if (dir.exists(file.path(repo_dir, ".git"))) {
  message("Repo found in ", repo_dir, " - pulling latest changes...")
  run_git(c("-C", shQuote(repo_dir), "pull", "origin", branch))
} else {
  message("Repo not found locally - cloning into ", repo_dir, "...")
  dir.create(dirname(repo_dir), recursive = TRUE, showWarnings = FALSE)
  run_git(c("clone", "--branch", branch, repo_url, shQuote(repo_dir)))
}

# ---- Show the latest commit -----------------------------------------
message("Latest commit:")
run_git(c("-C", shQuote(repo_dir), "log", "-1", "--oneline"))

message("Done.")
