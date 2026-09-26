# ============================================================
# Intro to ggplot2 for MCH Epidemiologists
# Data visualization in R -- beginner workshop
# Fake county-level birth-outcomes data (Idaho, Kansas, Montana)
# ============================================================
#
# Packages used (install once if you don't have them):
#   install.packages(c("tidyverse", "gt"))
#
# tidyverse gives us ggplot2 (charts) + dplyr (data wrangling).
# gt makes publication-quality tables.

library(tidyverse)
library(gt)


# ------------------------------------------------------------
# 1. MAKE A FAKE MCH DATASET
# ------------------------------------------------------------
# You don't need to understand this simulation code -- it just
# builds a realistic-looking table so we have something to plot.
# In real work you'd read your own file, e.g.:
#   births <- read_csv("my_data.csv")

set.seed(2024)

counties <- tribble(
  ~state,     ~county,
  "Idaho",    "Ada",         "Idaho",    "Canyon",      "Idaho",   "Kootenai",
  "Idaho",    "Bonneville",  "Idaho",    "Bannock",     "Idaho",   "Twin Falls",
  "Idaho",    "Nez Perce",
  "Kansas",   "Sedgwick",    "Kansas",   "Johnson",     "Kansas",  "Shawnee",
  "Kansas",   "Wyandotte",   "Kansas",   "Douglas",     "Kansas",  "Riley",
  "Kansas",   "Saline",      "Kansas",   "Finney",
  "Montana",  "Yellowstone", "Montana",  "Missoula",    "Montana", "Gallatin",
  "Montana",  "Flathead",    "Montana",  "Cascade",     "Montana", "Lewis & Clark",
  "Montana",  "Silver Bow"
)

# baseline rate per state
base <- tibble(
  state   = c("Idaho", "Kansas", "Montana"),
  b_lbw   = c(7.0, 7.6, 7.2),
  b_pt    = c(8.6, 9.4, 9.0),
  b_pn    = c(80,  77,  75)
)

births <- counties |>
  # give each county a small fixed "personality"
  mutate(o_lbw = rnorm(n(), 0, 0.9),
         o_pt  = rnorm(n(), 0, 1.0),
         o_pn  = rnorm(n(), 0, 4)) |>
  # one row per county per year 2015-2023
  crossing(year = 2015:2023) |>
  left_join(base, by = "state") |>
  mutate(
    t              = year - 2015,
    pct_lbw        = round(b_lbw + o_lbw + 0.05 * t + rnorm(n(), 0, 0.35), 2),
    pct_preterm    = round(b_pt  + o_pt  + 0.06 * t + rnorm(n(), 0, 0.40), 2),
    pct_prenatal   = round(b_pn  + o_pn  - 0.30 * t + rnorm(n(), 0, 2.0), 1)
  ) |>
  select(state, county, year, pct_lbw, pct_preterm, pct_prenatal)

# a one-year slice we'll reuse for several charts
births_2023 <- filter(births, year == 2023)

# Peek at the data -- always do this first!
glimpse(births)
head(births)


# ------------------------------------------------------------
# 2. THE GGPLOT RECIPE:  data  +  aes()  +  geom_
# ------------------------------------------------------------
# Every ggplot has three parts:
#   ggplot(DATA, aes(x = ..., y = ...)) +   # what maps to what
#     geom_something()                      # how to draw it


# ---- Chart 1: BAR -- mean low birth weight by state ----
births_2023 |>
  group_by(state) |>
  summarise(lbw = mean(pct_lbw)) |>
  ggplot(aes(x = state, y = lbw)) +
  geom_col()


# ---- Chart 2: LINE -- trend over time, one line per state ----
births |>
  group_by(state, year) |>
  summarise(lbw = mean(pct_lbw), .groups = "drop") |>
  ggplot(aes(x = year, y = lbw, color = state)) +
  geom_line(linewidth = 1)


# ---- Chart 3: DISTRIBUTION -- boxplot of county rates ----
ggplot(births_2023, aes(x = state, y = pct_lbw, fill = state)) +
  geom_boxplot()


# ---- Chart 4: SCATTER -- prenatal care vs low birth weight ----
ggplot(births_2023, aes(x = pct_prenatal, y = pct_lbw, color = state)) +
  geom_point(size = 3)


# ---- Chart 5: LOLLIPOP -- rank counties by preterm rate ----
births_2023 |>
  slice_max(pct_preterm, n = 12) |>
  ggplot(aes(x = pct_preterm, y = reorder(county, pct_preterm))) +
  geom_segment(aes(xend = 0, yend = county), color = "#8497B0") +
  geom_point(size = 3, color = "#2F5597")


# ---- Chart 6: SMALL MULTIPLES -- one panel per state ----
births |>
  group_by(state, year) |>
  summarise(preterm = mean(pct_preterm), .groups = "drop") |>
  ggplot(aes(x = year, y = preterm, color = state)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ state) +
  guides(color = "none")


# ------------------------------------------------------------
# 3. MAKE IT PRESENTABLE:  labs()  +  a theme
# ------------------------------------------------------------
ggplot(births_2023, aes(x = pct_prenatal, y = pct_lbw, color = state)) +
  geom_point(size = 3) +
  labs(
    title    = "Prenatal care and low birth weight, 2023",
    subtitle = "Each point is one county",
    x        = "% with adequate prenatal care",
    y        = "% low birth weight",
    color    = "State",
    caption  = "Source: simulated MCH data"
  ) +
  theme_minimal(base_size = 14)

# Save any plot to a file for your slides/report:
#   ggsave("prenatal_vs_lbw.png", width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------
# 4. A NICE TABLE WITH gt
# ------------------------------------------------------------
births_2023 |>
  filter(county %in% c("Ada", "Canyon", "Sedgwick",
                       "Johnson", "Yellowstone", "Flathead")) |>
  select(state, county, pct_lbw, pct_preterm, pct_prenatal) |>
  gt() |>
  tab_header(
    title    = "County Birth Outcomes, 2023",
    subtitle = "Selected counties across Idaho, Kansas, and Montana"
  ) |>
  cols_label(
    state = "State", county = "County",
    pct_lbw = "% LBW", pct_preterm = "% Preterm", pct_prenatal = "% Prenatal"
  ) |>
  fmt_number(columns = starts_with("pct"), decimals = 1) |>
  tab_source_note("Source: Simulated MCH surveillance data (teaching example)")


# ============================================================
# 5. YOUR TURN  --  GUIDED EXERCISE
# ============================================================
# GOAL: Build a bar chart of the mean % PRETERM birth by state
#       for 2023, then give it real titles.
#
# Fill in the blanks (____) and run it:
#
# births_2023 |>
#   group_by(____) |>
#   summarise(preterm = mean(____)) |>
#   ggplot(aes(x = state, y = ____)) +
#   geom_col(fill = "#2F5597") +
#   labs(title = "____",
#        x = "State",
#        y = "% preterm birth")
#
# STRETCH GOALS (pick one):
#   * Reorder the bars from highest to lowest:
#       aes(x = reorder(state, -preterm), y = preterm)
#   * Flip it sideways by adding:  + coord_flip()
#   * Add the number on each bar:
#       + geom_text(aes(label = round(preterm, 1)), vjust = -0.4)
#
# ---- Solution ----
births_2023 |>
  group_by(state) |>
  summarise(preterm = mean(pct_preterm)) |>
  ggplot(aes(x = reorder(state, -preterm), y = preterm)) +
  geom_col(fill = "#2F5597") +
  geom_text(aes(label = round(preterm, 1)), vjust = -0.4) +
  labs(title = "Mean preterm birth rate by state, 2023",
       x = "State", y = "% preterm birth") +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# CHEAT SHEET
#   geom_col()      bars from values you computed
#   geom_line()     trends over time
#   geom_point()    scatter / relationships
#   geom_boxplot()  distributions
#   geom_segment()  the sticks in a lollipop
#   facet_wrap(~x)  small multiples, one panel per group
#   aes(color=, fill=)   map a column to color
#   labs(), theme_minimal()   polish
#   ggsave()        export for slides
# ------------------------------------------------------------
