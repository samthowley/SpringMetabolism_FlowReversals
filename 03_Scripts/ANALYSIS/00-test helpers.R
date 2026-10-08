# Shared setup for the hypothesis tests (scripts 18, 19, 20).
# Reads the two csvs built by 10-build flood metrics.R and 11-breakpoints NEP.R,
# so the tests can be run without running anything else first.

library(tidyverse)
library(lme4)
library(lmerTest)   # p-values for lmer (Satterthwaite df)
library(emmeans)
library(cowplot)
select <- dplyr::select


#site info########
# vulnerability score, one value per site (increasing = more disturbed)
vulnerability <- c(IU = 1, ID = 2, GB = 3, LF = 4, AM = 5, OS = 6)
site.order    <- names(vulnerability)

site_colors <- c(AM = "#E41A1C", GB = "#377EB8", ID = "#4DAF4A",
                 LF = "#984EA3", OS = "#FF7F00", IU = "#A65628")
class_colors <- c(FR = "#1B9E77", HI = "#7570B3", BO = "#D95F02")
var.order <- c("GPP", "ER", "DO", "CO2")   # regime first, then raw signal

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

#data########
flood.metrics <- read_csv("04_Outputs/flood impacts/flood_metrics.csv", show_col_types = FALSE) %>%
  mutate(
    ID       = factor(ID, levels = site.order),
    variable = factor(variable, levels = var.order),
    class    = factor(class, levels = c("FR", "HI", "BO")),
    flood.id = interaction(ID, flood, drop = TRUE)   # one level per flood event (shared by the 4 variables)
  )

breakpoints <- read_csv("04_Outputs/breakpoints.csv", show_col_types = FALSE) %>%
  mutate(
    ID       = factor(ID, levels = site.order),
    variable = factor(variable, levels = var.order),
    vulnerable.score = vulnerability[as.character(ID)]
  )

#model helpers########
# fit an lmer; returns NULL (with a message) if there is too little data or the fit fails
safe_lmer <- function(formula, data, min.n = 8, min.sites = 3) {
  data <- data %>% drop_na(all_of(all.vars(formula)))
  if (nrow(data) < min.n || n_distinct(data$ID) < min.sites) {
    message("  skipped (n = ", nrow(data), ", sites = ", n_distinct(data$ID), "): ", deparse(formula))
    return(NULL)
  }
  tryCatch(lmer(formula, data = data),
           error = function(e) { message("  fit failed: ", conditionMessage(e)); NULL })
}

# fixed effects table: estimate, se, df, 95% CI, p, n, sites, singular flag
tidy_lmer <- function(m) {
  if (is.null(m)) return(tibble())
  co <- as.data.frame(summary(m)$coefficients)
  names(co) <- c("estimate", "se", "df", "t", "p")
  co %>%
    rownames_to_column("term") %>%
    mutate(
      ci.low  = estimate - qt(0.975, df) * se,
      ci.high = estimate + qt(0.975, df) * se,
      n       = nobs(m),
      n.sites = as.integer(ngrps(m)["ID"]),
      singular = isSingular(m)
    ) %>%
    as_tibble()
}
