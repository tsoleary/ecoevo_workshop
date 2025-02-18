# ------------------------------------------------------------------------------
# EcoEvo Workshop in R -- Day 2: data analysis & advanced data visualization
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Import data 
dat <- read_tsv("data/raw/lockwood_et_al_2018_LT50.txt") |> 
  mutate(Pop = factor(Pop, levels = c("VT", "BEA", "RFM", "Tropical")))

# Change the theme to not have the grey
dat |> 
  ggplot() +
  geom_boxplot(aes(x = Pop, 
                   y = Embryo_LT50,
                   fill = Region)) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Add points
dat |> 
  ggplot(aes(x = Pop, 
             y = Embryo_LT50,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21, size = 2) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  scale_y_continuous(name = expression("Embryo LT"[50]*" (°C)")) +
  theme_minimal()

# Change axis labels
dat |> 
  ggplot(aes(x = Pop, 
             y = Embryo_LT50,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21, size = 2) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  scale_y_continuous(name = expression("Embryo LT"[50]*" (°C)")) +
  theme_minimal()

ggsave("figs/embryo_lt50.pdf",
       width = 5,
       height = 4,
       units = "in")

# One-way ANOVA
aov(Embryo_LT50 ~ Pop, data = dat) |> 
  summary()

# You can also use the broom package to save the results in a data frame
aov(Embryo_LT50 ~ Pop, data = dat) |> 
  broom::tidy()

# You can also use the TukeyHSD test to see which specific levels of the factor
# in this case what we are calling population groups are different from each other
aov(Embryo_LT50 ~ Pop, data = dat) |> 
  TukeyHSD() |> 
  broom::tidy() |> 
  arrange(adj.p.value)

# What if we want to run a two-way ANOVA with Life Stage and Population on LT50
dat_long <- dat |> 
  pivot_longer(cols = c("Embryo_LT50", "Adult_LT50"),
               names_to = "LifeStage",
               values_to = "LT50") |> 
  mutate(LifeStage = str_remove_all(LifeStage, "_LT50")) |> 
  mutate(LifeStage = factor(LifeStage, levels = c("Embryo", "Adult")))

# 
dat_long |> 
  ggplot(aes(x = LifeStage,
             y = LT50,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21,
             position = position_dodge(width = 0.75)) +
  theme_minimal()

# A solid plot with all points
dat_long |> 
  ggplot(aes(x = LifeStage,
             y = LT50,
             fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  ggbeeswarm::geom_beeswarm(shape = 21,
                            size = 2,
                            cex = 2,
                            dodge.width = 0.75) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Save it with all LT50s
ggsave("figs/lt50.pdf",
       width = 5,
       height = 4,
       units = "in")

# Two-way ANOVA on Region and Life Stage effect on LT50
lm(LT50 ~ Region*LifeStage, dat = dat_long) |>
  car::Anova() |> 
  broom::tidy()

# Pair-wise comparison of all groups
lm(LT50 ~ Region*LifeStage, dat = dat_long) |>
  emmeans::emmeans(~ Region * LifeStage) |> 
  pairs(adjust = "tukey") |> 
  broom::tidy()
  



# Survival data we are unlikely to have time to cover --------------------------

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