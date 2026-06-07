#################################
#### Final Project — Working Script
#### The Low Income Household Cost Burden:
#### Energy, Food, and Labor Equity in the U.S.
#### Vincent Alulu | PID 59024801
#### Spring 2026
#################################

library(methods)
library(shiny)
library(bslib)
library(bsicons)
library(fredr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(scales)
library(lubridate)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# ---------------------------------------------------------------------------
# Part 1: Pull all data from FRED
# ---------------------------------------------------------------------------

# Unemployment rate
urate <- fredr(
  series_id         = "UNRATE",
  observation_start = as.Date("2000-01-01")
)

# Headline CPI inflation (12-month % change)
cpi <- fredr(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Energy CPI (12-month % change)
energy_cpi <- fredr(
  series_id         = "CPIENGSL",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Federal minimum wage (nominal)
min_wage <- fredr(
  series_id         = "FEDMINNFRWG",
  observation_start = as.Date("2000-01-01")
)

# CPI level — for deflating nominal minimum wage to real terms
cpi_level <- fredr(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("2000-01-01")
)

# Gasoline prices (weekly — will convert to monthly)
gasoline <- fredr(
  series_id         = "GASREGCOVW",
  observation_start = as.Date("2000-01-01")
)

# NBER recession indicator
usrec <- fredr(
  series_id         = "USREC",
  observation_start = as.Date("2000-01-01")
)

# Wage growth — Leisure & Hospitality (lowest paid major sector)
wage_leisure <- fredr(
  series_id         = "CES7000000003",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Food at home CPI (12-month % change)
food_home <- fredr(
  series_id         = "CPIUFDSL",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Food away from home CPI (12-month % change)
food_away <- fredr(
  series_id         = "CUUR0000SEFV",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# PPI Farm products (12-month % change)
ppi_farm <- fredr(
  series_id         = "WPU01",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# PPI Grains (12-month % change)
ppi_grains <- fredr(
  series_id         = "WPU0111",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Unemployment — less than high school diploma
unemp_lths <- fredr(
  series_id         = "LNS14027659",
  observation_start = as.Date("2000-01-01")
)

# Unemployment — bachelor's degree and above
unemp_ba <- fredr(
  series_id         = "LNS14027689",
  observation_start = as.Date("2000-01-01")
)

# Wage growth — all private employees (12-month % change)
wage_all <- fredr(
  series_id         = "CES0500000003",
  observation_start = as.Date("2000-01-01"),
  units             = "pc1"
)

# Check all pulled
cat("Unemployment:", nrow(urate), "\n")
cat("CPI inflation:", nrow(cpi), "\n")
cat("Energy CPI:", nrow(energy_cpi), "\n")
cat("Min wage:", nrow(min_wage), "\n")
cat("Gasoline:", nrow(gasoline), "\n")
cat("USREC:", nrow(usrec), "\n")
cat("Wage leisure:", nrow(wage_leisure), "\n")
cat("Food at home:", nrow(food_home), "\n")
cat("Food away:", nrow(food_away), "\n")
cat("PPI farm:", nrow(ppi_farm), "\n")
cat("PPI grains:", nrow(ppi_grains), "\n")
cat("Unemp LTHS:", nrow(unemp_lths), "\n")
cat("Unemp BA:", nrow(unemp_ba), "\n")
cat("Wage all:", nrow(wage_all), "\n")

# ---------------------------------------------------------------------------
# Part 2: Data preparation
# ---------------------------------------------------------------------------

# Real minimum wage — deflate nominal by CPI rebased to 2009
# 2009 = last time federal minimum wage was raised
cpi_2009_avg <- cpi_level %>%
  mutate(year = format(date, "%Y")) %>%
  filter(year == "2009") %>%
  summarise(avg = mean(value, na.rm = TRUE)) %>%
  pull(avg)

real_min_wage <- min_wage %>%
  inner_join(
    cpi_level %>% select(date, cpi = value),
    by = "date"
  ) %>%
  mutate(real_min_wage = value / cpi * cpi_2009_avg)

# Convert gasoline from weekly to monthly average
gasoline_monthly <- gasoline %>%
  mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(date) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# Add 12-month trailing moving averages to all series
energy_cpi <- energy_cpi %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

gasoline_monthly <- gasoline_monthly %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

wage_leisure <- wage_leisure %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

food_home <- food_home %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

food_away <- food_away %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

ppi_farm <- ppi_farm %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

ppi_grains <- ppi_grains %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

unemp_lths <- unemp_lths %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

unemp_ba <- unemp_ba %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

wage_all <- wage_all %>%
  arrange(date) %>%
  mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right"))

# Education unemployment gap
# Difference between unemployment for less than HS vs bachelor's degree
# Widens during recessions — shows disproportionate impact on low-income workers
unemp_gap <- unemp_lths %>%
  select(date, lths = value) %>%
  inner_join(
    unemp_ba %>% select(date, ba = value),
    by = "date"
  ) %>%
  mutate(
    gap   = lths - ba,
    ma_12 = rollmean(gap, 12, fill = NA, align = "right")
  )

# ---------------------------------------------------------------------------
# Low-Income SAHM Rule
# ---------------------------------------------------------------------------
# Original SAHM rule (Claudia Sahm, 2019): recession starts when the 3-month
# average unemployment rate rises 0.5 percentage points above its 12-month minimum.
# Here we apply the same logic to workers WITHOUT a high school diploma —
# the most vulnerable education group — to detect low-income labor market downturns
# specifically, rather than the broader economy.
# Triggers when sahm_lths > 0.5 — signals a low-income labor market downturn.

unemp_lths_sahm <- unemp_lths %>%
  arrange(date) %>%
  mutate(
    # 3-month rolling average — smooths month-to-month noise
    roll_avg_3m  = rollmean(value, 3, fill = NA, align = "right"),
    # 12-month rolling minimum — the recent low point in unemployment
    roll_min_12m = rollapply(value, 12, min, fill = NA, align = "right"),
    # SAHM indicator = how much unemployment has risen above its recent low
    sahm_lths    = roll_avg_3m - roll_min_12m
  )

cat("SAHM computed. Latest value:",
    round(tail(unemp_lths_sahm$sahm_lths, 1), 2), "\n")
cat("Trigger threshold: 0.5 — currently",
    ifelse(tail(unemp_lths_sahm$sahm_lths, 1) > 0.5,
           "TRIGGERED ⚠️", "not triggered ✅"), "\n")

# ---------------------------------------------------------------------------
# Get latest values for stat cards
# ---------------------------------------------------------------------------
latest_urate   <- urate %>% filter(date == max(date)) %>% pull(value)
latest_cpi     <- cpi %>% filter(date == max(date)) %>% pull(value)
latest_energy  <- energy_cpi %>% filter(date == max(date)) %>% pull(value)
latest_real_mw <- real_min_wage %>% filter(date == max(date)) %>% pull(real_min_wage)
latest_date    <- max(urate$date)

# Check computed values
cat("Latest unemployment:", latest_urate, "\n")
cat("Latest CPI inflation:", round(latest_cpi, 2), "\n")
cat("Latest energy CPI:", round(latest_energy, 2), "\n")
cat("Latest real min wage:", round(latest_real_mw, 2), "\n")

# ---------------------------------------------------------------------------
# Part 3: Save all RDS files to app folder
# ---------------------------------------------------------------------------
saveRDS(urate,             "alulu_final_project/urate.RDS")
saveRDS(cpi,               "alulu_final_project/cpi.RDS")
saveRDS(cpi_level,         "alulu_final_project/cpi_level.RDS")
saveRDS(energy_cpi,        "alulu_final_project/energy_cpi.RDS")
saveRDS(min_wage,          "alulu_final_project/min_wage.RDS")
saveRDS(real_min_wage,     "alulu_final_project/real_min_wage.RDS")
saveRDS(gasoline_monthly,  "alulu_final_project/gasoline_monthly.RDS")
saveRDS(usrec,             "alulu_final_project/usrec.RDS")
saveRDS(wage_leisure,      "alulu_final_project/wage_leisure.RDS")
saveRDS(food_home,         "alulu_final_project/food_home.RDS")
saveRDS(food_away,         "alulu_final_project/food_away.RDS")
saveRDS(ppi_farm,          "alulu_final_project/ppi_farm.RDS")
saveRDS(ppi_grains,        "alulu_final_project/ppi_grains.RDS")
saveRDS(unemp_lths,        "alulu_final_project/unemp_lths.RDS")
saveRDS(unemp_ba,          "alulu_final_project/unemp_ba.RDS")
saveRDS(wage_all,          "alulu_final_project/wage_all.RDS")
saveRDS(unemp_gap,         "alulu_final_project/unemp_gap.RDS")
saveRDS(unemp_lths_sahm,   "alulu_final_project/unemp_lths_sahm.RDS")

cat("All series saved successfully!\n")
list.files("alulu_final_project")