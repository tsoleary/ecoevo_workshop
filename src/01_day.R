# ------------------------------------------------------------------------------
# EcoEvo Workshop in R -- Day 1: setting up a project, importing data, 
# tidyverse, & data visualization
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Import data ------------------------------------------------------------------
dat <- read_tsv("data/raw/lockwood_et_al_2018_LT50.txt")

# Inspect data -----------------------------------------------------------------

# Print it out
dat

# glimpse 
glimpse(dat)

# filter
filter(dat, Region == "Temperate")

# pipes |> and %>% 
dat |> 
  filter(Region == "Temperate")

# summarize
dat |> 
  filter(Region == "Temperate")

# select
dat |> 
  select(Stock, Locale, Lat, Long, Embryo_LT50)

# Visualizations ---------------------------------------------------------------

# Histograms
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50))

# Changing the number of bins
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15)

# Changing the outline color and fill color to separate bars
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80")

# Changing the theme
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80") +
  theme_minimal()

# Color by region
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50, fill = Region), 
                 bins = 15,
                 color = "grey20") +
  theme_minimal()


# Facet wrap -- we will do this more later
dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80") +
  theme_minimal() +
  facet_wrap(~Region)

# Box and whiskers -----
dat |> 
  ggplot() +
  geom_boxplot(aes(y = Embryo_LT50,
                   x = Region)) +
  theme_minimal()


# Add color
dat |> 
  ggplot() +
  geom_boxplot(aes(y = Embryo_LT50,
                   x = Region,
                   fill = Region)) +
  theme_minimal()

# Violin plots
dat |> 
  ggplot() +
  geom_violin(aes(y = Embryo_LT50,
                  x = Region,
                  fill = Region)) +
  theme_minimal()


# Adding violin and box plot on top of each other
dat |> 
  ggplot() +
  geom_violin(aes(y = Embryo_LT50,
                  x = Region,
                  fill = Region)) +
  geom_boxplot(aes(y = Embryo_LT50,
                   x = Region,
                   fill = Region),
               width = 0.3) +
  theme_minimal()

# Moving the aes() up to the ggplot() so it is carried over to lower functions
dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Region,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  theme_minimal()

# Changing the color
dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Region,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Looking at the paper --- 

# Separating out the population
dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Pop,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Ordering to have tropical on the right
# We can actually do this at the top and save it in the original dat object
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Ditch the violin plot and add in points
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Jitter the points
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  geom_jitter(shape = 21, size = 3, width = 0.15) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Remove the outlier dots
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21, size = 3, width = 0.15) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Set the seed so it will be the same each time
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21, size = 3, width = 0.15, set.seed(1)) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Actually there is kinda a better way to avoid overlap, but in an orderly way

# We need to first install the package ggbeeswarm

# Using the ggbeeswarm::geom_beeswarm function
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(shape = 21, size = 3, cex = 1.5) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Some final touches
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(shape = 21, size = 3, cex = 1.5) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  scale_y_continuous(name = "Embryo LT50 (°C)") +
  theme_minimal()

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(shape = 21, size = 3, cex = 1.5) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  scale_y_continuous(name = expression("Embryo LT"[50]*" (°C)")) +
  theme_minimal()



# Save these figures -----
ggsave(
  "output/figs/lt50.pdf",
  width = 6,
  height = 4,
  units = "in"
)


# Statistics -------------------------------------------------------------------

# T-test between adult and embryonic LT50
t.test(dat$Adult_LT50, dat$Embryo_LT50) |> 
  broom::tidy()

# Now we want to compare Adult LT50s
# We could do something complicated like this....
t.test(dat |> filter(Region == "Temperate") |> pull(Embryo_LT50),
       dat |> filter(Region == "Tropical") |> pull(Embryo_LT50)) |> 
  broom::tidy()


# Or we could use a formula do test the different levels of a factor
t.test(Adult_LT50 ~ Region, data = dat) |> 
  broom::tidy()

t.test(Embryo_LT50 ~ Region, data = dat) |> 
  broom::tidy()

# One-way ANOVA of all populations
lm(Adult_LT50 ~ Pop, data = dat) |> 
  car::Anova()

# Two-way ANOVA with life-stage and region -------------------------------------
dat_long <- dat |> 
  pivot_longer(cols = contains("LT50"),
               names_to = "LifeStage",
               values_to = "LT50") |> 
  mutate(LifeStage = str_remove_all(LifeStage, "_LT50")) |> 
  mutate(LifeStage = factor(LifeStage, levels = c("Embryo", "Adult")))

# Quick plot and then we will talk about stats again
dat_long |> 
  ggplot(aes(x = LifeStage,
             y = LT50,
             fill = Region)) +
  geom_boxplot()

# Two-way ANOVA
lm(LT50 ~ LifeStage*Region, data = dat_long) |> 
  car::Anova()


# Challenges -------------------------------------------------------------------
data()

# Just so we can have it in the Global Environment Panel so we can poke it with
# the GUI of RStudio
storms <- storms

storms |> 
  filter(year == "2005") |> 
  mutate(date = make_datetime(year, month, day, hour)) |> 
  ggplot() +
  geom_line(aes(x = date,
                y = wind,
                color = name))

# Here is one with all the years included
storms |> 
  mutate(date = make_datetime(year, month, day, hour)) |> 
  ggplot() +
  geom_line(aes(x = date,
                y = wind,
                color = name)) +
  theme(legend.position = "none") +
  facet_wrap(~year,
             scales = "free_x") 


