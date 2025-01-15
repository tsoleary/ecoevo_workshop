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

# Pipes
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

dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15)

dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80")

dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80") +
  theme_minimal()



dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50), 
                 bins = 15, 
                 color = "grey20",
                 fill = "grey80") +
  theme_minimal() +
  facet_wrap(~Region)


dat |> 
  ggplot() +
  geom_histogram(aes(x = Embryo_LT50, fill = Region), 
                 bins = 15,
                 color = "grey20") +
  theme_minimal()

# Box and whiskers

dat |> 
  ggplot() +
  geom_boxplot(aes(y = Embryo_LT50,
                   x = Region)) +
  theme_minimal()


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


dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Region,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  theme_minimal()


dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Region,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Looking at the paper --- 

dat |> 
  ggplot(aes(y = Adult_LT50,
             x = Pop,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()


dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# Add points
dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()


dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  geom_jitter(shape = 21, size = 3, width = 0.15) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21, size = 3, width = 0.15) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21, size = 3, width = 0.15, set.seed(1)) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()

# install.packages("ggbeeswarm")

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(shape = 21, size = 3, cex = 1.5) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal()


# Statistics -------------------------------------------------------------------

dat |> 
  mutate(Pop = factor(Pop, levels = c("BEA", "RFM", "VT", "Tropical"))) |> 
  ggplot(aes(y = Embryo_LT50,
             x = Pop,
             fill = Region)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(shape = 21, size = 3, cex = 1.5) +
  scale_fill_manual(values = c("lightblue", "pink3")) +
  theme_minimal() 




# Challenges -------------------------------------------------------------------

