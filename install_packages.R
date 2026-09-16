packages <- c(
  "tidyr",
  "dplyr",
  "broom.mixed",
  "lmerTest",
  "emmeans",
  "ggplot2",
  "lme4"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing)
}
