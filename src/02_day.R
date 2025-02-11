# ------------------------------------------------------------------------------
# EcoEvo Workshop in R -- Day 2: data analysis & advanced data visualization
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Load data
dat <- read_tsv("data/raw/lockwood_et_al_2018_survival.txt") |> 
  filter(region %in% c("temperate", "tropical"))

# Survival data
dat |> 
  ggplot(aes(y = survival,
             x = temperature, 
             fill = genotype,
             color = genotype)) +
  geom_point(shape = 21, color = "grey80") +
  geom_smooth(aes(linetype = region),
              method = drc::drm,
              method.args = list(fct = drc::LL.3()),
              se = FALSE) +
  theme_minimal() +
  theme(legend.position = "none")


dat |> 
  ggplot(aes(y = survival,
             x = temperature, 
             group = genotype,
             color = region)) +
  geom_smooth(aes(linetype = region),
              method = drc::drm,
              method.args = list(fct = drc::LL.3()),
              se = FALSE) +
  scale_color_manual(values = c("lightblue", "pink3")) +
  theme_minimal() +
  theme(legend.position = "none")


# Estimate the LT50s for each line
dat |> 
  group_by(genotype) |> 
  nest() |> 
  mutate(fit = map(data, ~ drc::drm(hatched/eggs ~ temperature,
                                    data = .x,
                                    weight = eggs,
                                    fct = drc::LL.3(names = c("slope", 
                                                              "upper limit", 
                                                              "LT50")),
                                    type = "binomial"))) |> 
  mutate(fit_tidy = map(fit, ~broom::tidy(.x))) |> 
  unnest(fit_tidy) |> 
  filter(term == "LT50") |> 
  arrange(desc(estimate))




# Import data & begin again – remake a few visualizations from last week
# Run some statistical tests on the data that goes with each visualization
# T-test – b/w adult and embryo LT50
# ANOVA – one-way within life stage by location – or two-way ANOVA with region & life stage
# Tukey HSD post hoc – maybe, but probably not worth the time or confusion
# linear regression – LT50 & latitude
# Non linear regression – raw survival data – advanced, but fun
# Facet-wrap & multi-panel figures
# End with a couple challenges – alternative data sets – built in data