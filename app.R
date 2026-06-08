#################################
#### Final Project — The Low Income Household Cost Burden:
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

# ---------------------------------------------------------------------------
# Auto-update feature — re-pulls from FRED if data is older than 30 days
# ---------------------------------------------------------------------------
rds_path     <- "urate.RDS"
needs_update <- TRUE

if (file.exists(rds_path)) {
  last_modified <- file.mtime(rds_path)
  days_since    <- as.numeric(Sys.time() - last_modified, units = "days")
  needs_update  <- days_since > 30
}

if (needs_update) {
  message("Data is older than 30 days — pulling fresh data from FRED...")
  
  fredr_set_key("a7c7d875cc66831aa2e3cb49ed7746cd")
  
  urate        <- fredr("UNRATE",        observation_start = as.Date("2000-01-01"))
  cpi          <- fredr("CPIAUCSL",      observation_start = as.Date("2000-01-01"), units = "pc1")
  energy_cpi   <- fredr("CPIENGSL",      observation_start = as.Date("2000-01-01"), units = "pc1")
  min_wage     <- fredr("FEDMINNFRWG",   observation_start = as.Date("2000-01-01"))
  cpi_level    <- fredr("CPIAUCSL",      observation_start = as.Date("2000-01-01"))
  gasoline     <- fredr("GASREGCOVW",    observation_start = as.Date("2000-01-01"))
  usrec        <- fredr("USREC",         observation_start = as.Date("2000-01-01"))
  wage_leisure <- fredr("CES7000000003", observation_start = as.Date("2000-01-01"), units = "pc1")
  food_home    <- fredr("CPIUFDSL",      observation_start = as.Date("2000-01-01"), units = "pc1")
  food_away    <- fredr("CUUR0000SEFV",  observation_start = as.Date("2000-01-01"), units = "pc1")
  ppi_farm     <- fredr("WPU01",         observation_start = as.Date("2000-01-01"), units = "pc1")
  ppi_grains   <- fredr("WPU0111",       observation_start = as.Date("2000-01-01"), units = "pc1")
  unemp_lths   <- fredr("LNS14027659",   observation_start = as.Date("2000-01-01"))
  unemp_ba     <- fredr("LNS14027689",   observation_start = as.Date("2000-01-01"))
  wage_all     <- fredr("CES0500000003", observation_start = as.Date("2000-01-01"), units = "pc1")
  
  # Data prep
  cpi_2009_avg <- cpi_level %>%
    mutate(year = format(date, "%Y")) %>%
    filter(year == "2009") %>%
    summarise(avg = mean(value, na.rm = TRUE)) %>%
    pull(avg)
  
  real_min_wage <- min_wage %>%
    inner_join(cpi_level %>% select(date, cpi = value), by = "date") %>%
    mutate(real_min_wage = value / cpi * cpi_2009_avg)
  
  gasoline_monthly <- gasoline %>%
    mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
    group_by(date) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  add_ma <- function(df, col = "value") {
    df %>% arrange(date) %>%
      mutate(ma_12 = rollmean(.data[[col]], 12, fill = NA, align = "right"))
  }
  
  energy_cpi       <- add_ma(energy_cpi)
  gasoline_monthly <- add_ma(gasoline_monthly)
  wage_leisure     <- add_ma(wage_leisure)
  food_home        <- add_ma(food_home)
  food_away        <- add_ma(food_away)
  ppi_farm         <- add_ma(ppi_farm)
  ppi_grains       <- add_ma(ppi_grains)
  unemp_lths       <- add_ma(unemp_lths)
  unemp_ba         <- add_ma(unemp_ba)
  wage_all         <- add_ma(wage_all)
  
  unemp_gap <- unemp_lths %>%
    select(date, lths = value) %>%
    inner_join(unemp_ba %>% select(date, ba = value), by = "date") %>%
    mutate(gap   = lths - ba,
           ma_12 = rollmean(gap, 12, fill = NA, align = "right"))
  
  # Low-Income SAHM Rule
  unemp_lths_sahm <- unemp_lths %>%
    arrange(date) %>%
    mutate(
      roll_avg_3m  = rollmean(value, 3,  fill = NA, align = "right"),
      roll_min_12m = rollapply(value, 12, min, fill = NA, align = "right"),
      sahm_lths    = roll_avg_3m - roll_min_12m
    )
  
  # Save all RDS
  saveRDS(urate,            "urate.RDS")
  saveRDS(cpi,              "cpi.RDS")
  saveRDS(cpi_level,        "cpi_level.RDS")
  saveRDS(energy_cpi,       "energy_cpi.RDS")
  saveRDS(min_wage,         "min_wage.RDS")
  saveRDS(real_min_wage,    "real_min_wage.RDS")
  saveRDS(gasoline_monthly, "gasoline_monthly.RDS")
  saveRDS(usrec,            "usrec.RDS")
  saveRDS(wage_leisure,     "wage_leisure.RDS")
  saveRDS(food_home,        "food_home.RDS")
  saveRDS(food_away,        "food_away.RDS")
  saveRDS(ppi_farm,         "ppi_farm.RDS")
  saveRDS(ppi_grains,       "ppi_grains.RDS")
  saveRDS(unemp_lths,       "unemp_lths.RDS")
  saveRDS(unemp_ba,         "unemp_ba.RDS")
  saveRDS(wage_all,         "wage_all.RDS")
  saveRDS(unemp_gap,        "unemp_gap.RDS")
  saveRDS(unemp_lths_sahm,  "unemp_lths_sahm.RDS")
  
  message("Data updated successfully!")
  
} else {
  urate            <- readRDS("urate.RDS")
  cpi              <- readRDS("cpi.RDS")
  cpi_level        <- readRDS("cpi_level.RDS")
  energy_cpi       <- readRDS("energy_cpi.RDS")
  min_wage         <- readRDS("min_wage.RDS")
  real_min_wage    <- readRDS("real_min_wage.RDS")
  gasoline_monthly <- readRDS("gasoline_monthly.RDS")
  usrec            <- readRDS("usrec.RDS")
  wage_leisure     <- readRDS("wage_leisure.RDS")
  food_home        <- readRDS("food_home.RDS")
  food_away        <- readRDS("food_away.RDS")
  ppi_farm         <- readRDS("ppi_farm.RDS")
  ppi_grains       <- readRDS("ppi_grains.RDS")
  unemp_lths       <- readRDS("unemp_lths.RDS")
  unemp_ba         <- readRDS("unemp_ba.RDS")
  wage_all         <- readRDS("wage_all.RDS")
  unemp_gap        <- readRDS("unemp_gap.RDS")
  unemp_lths_sahm  <- readRDS("unemp_lths_sahm.RDS")
  
  message(paste("Data is current —", round(days_since, 1), "days since last update."))
}

# ---------------------------------------------------------------------------
# Compute headline values for stat cards
# ---------------------------------------------------------------------------
latest_urate   <- urate %>% filter(date == max(date)) %>% pull(value)
latest_cpi     <- cpi %>% filter(date == max(date)) %>% pull(value)
latest_energy  <- energy_cpi %>% filter(date == max(date)) %>% pull(value)
latest_real_mw <- real_min_wage %>% filter(date == max(date)) %>% pull(real_min_wage)
latest_date    <- max(urate$date)
data_freshness <- format(file.mtime("urate.RDS"), "%d %b %Y")

# ---------------------------------------------------------------------------
# Build recession bands for geom_rect
# ---------------------------------------------------------------------------
rec_bands <- usrec %>%
  arrange(date) %>%
  mutate(
    in_recession = as.integer(round(value)) == 1L,
    new_segment  = in_recession != lag(in_recession, default = FALSE)
  ) %>%
  mutate(segment_id = cumsum(new_segment)) %>%
  filter(in_recession) %>%
  group_by(segment_id) %>%
  summarise(
    xmin = min(date),
    xmax = max(date) %m+% months(1L),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Compute Household Affordability Gap Index
# ---------------------------------------------------------------------------
affordability_gap <- food_home %>%
  select(date, food = value) %>%
  inner_join(energy_cpi %>% select(date, energy = value), by = "date") %>%
  inner_join(wage_leisure %>% select(date, wage = value), by = "date") %>%
  mutate(
    gap_index = ((food + energy) / 2) - wage,
    ma_12     = rollmean(gap_index, 12, fill = NA, align = "right")
  )

# ---------------------------------------------------------------------------
# Methodology notes — shown under each plot in light grey
# ---------------------------------------------------------------------------
note_energy <- "Energy CPI = 12-month % change in the U.S. Consumer Price Index for All Energy (CPIENGSL, FRED). This index covers all household energy expenditures including gasoline, electricity, natural gas, and heating oil which is the full basket of energy costs a typical household faces. Green dashed line = 12-month % change in Leisure & Hospitality wages (CES7000000003), the lowest paid major sector, used as a low-income wage proxy. Navy line = 12-month trailing moving average. Red shading = energy rising faster than wages (falling behind). Green shading = wages rising faster than energy prices (gaining ground)."

note_gasoline <- "Gasoline price = weekly U.S. regular gasoline retail price (GASREGCOVW, FRED), converted to monthly average. Navy line = 12-month trailing moving average. Grey bars = NBER recession periods."

note_food <- "Food prices (bread, rice, meat, pasta bought at supermarkets, market stores ) at home and food prices away from home (restaurants, fastfood, cafeterias) = 12-month % change in respective CPI components (FRED). PPI Farm Products and PPI Grains = 12-month % change in producer prices (WPU01, WPU0111) - upstream price pressures that typically feed into consumer food prices with a lag. Navy line = 12-month trailing moving average. Grey bars = NBER recession periods."

note_labor_unemp <- "Unemployment rates by education level sourced from BLS via FRED (LNS14027659, LNS14027689). Navy line = 12-month trailing moving average. Grey bars = NBER recession periods."

note_labor_gap <- "Education Unemployment Gap = unemployment rate for workers without a high school diploma minus the rate for bachelor's degree holders. Measures structural labor market inequality. Widens sharply during recessions - low-education workers are hit first and hardest. Navy line = 12-month trailing moving average."

note_labor_wage <- "Wage growth = 12-month % change in average hourly earnings (FRED). Leisure & Hospitality (CES7000000003) is the lowest paid major sector - used as a proxy for low-income wage trends. All Private (CES0500000003) shown for comparison. Navy line = 12-month trailing moving average."
note_real_min_wage <- "Real Minimum Wage = nominal federal minimum wage (FEDMINNFRWG) deflated by CPI (CPIAUCSL), rebased so that 2009 = 100 (the last year the federal minimum wage was raised). Shows the erosion of purchasing power over time in constant 2009 dollars."
note_sahm <- "Low-Income SAHM Indicator = 3-month average of less-than-HS unemployment minus its 12-month minimum (Sahm, 2019). Adapted here to target workers without a high school diploma — the most economically vulnerable education group — rather than the full labor force. Triggers above 0.5 percentage points, signaling a low-income labor market downturn. Notable spikes: 2001 recession (~2pp), 2008-09 financial crisis (~6pp), and 2020 COVID-19 shock (~14pp) — each far exceeding the national SAHM threshold, confirming that low-income workers experience recessions with much greater severity than the average worker. Red dashed line = 0.5pp trigger threshold. Grey bars = NBER recession periods."
note_burden <- "Household Affordability Gap Index = (Food at Home CPI + Energy CPI) / 2 − Leisure & Hospitality Wage Growth. All three components are 12-month % changes, making the arithmetic directly comparable in percentage points. Weighted average (÷2) prevents energy spikes from dominating the index. Red bars = falling behind. Green bars = gaining ground. Navy line = 12-month trailing moving average."

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "The Low Income Household Cost Burden",
  bg    = "#1F4E79",
  
  header = tags$head(
    tags$style(HTML("
      .bslib-sidebar-layout > .sidebar {
        background-color: #1F4E79 !important;
        color: white !important;
      }
      .bslib-sidebar-layout > .sidebar p,
      .bslib-sidebar-layout > .sidebar label,
      .bslib-sidebar-layout > .sidebar .help-block,
      .bslib-sidebar-layout > .sidebar h5,
      .bslib-sidebar-layout > .sidebar h6 {
        color: #E3F2FD !important;
      }
      .bslib-sidebar-layout > .sidebar hr {
        border-color: rgba(255,255,255,0.2) !important;
      }
      .bslib-sidebar-layout > .collapse-toggle {
        color: white !important;
      }
      .navbar-nav .nav-item:nth-child(1) .nav-link {
        background: linear-gradient(135deg, #005A8E, #0099CD);
        border-radius: 6px; margin: 4px 2px;
        color: white !important; font-weight: 600;
      }
      .navbar-nav .nav-item:nth-child(2) .nav-link {
        background: linear-gradient(135deg, #C45800, #E87722);
        border-radius: 6px; margin: 4px 2px;
        color: white !important; font-weight: 600;
      }
      .navbar-nav .nav-item:nth-child(3) .nav-link {
        background: linear-gradient(135deg, #006B54, #00A878);
        border-radius: 6px; margin: 4px 2px;
        color: white !important; font-weight: 600;
      }
      .navbar-nav .nav-item:nth-child(4) .nav-link {
        background: linear-gradient(135deg, #4B0082, #7B2FBE);
        border-radius: 6px; margin: 4px 2px;
        color: white !important; font-weight: 600;
      }
      .navbar-nav .nav-item:nth-child(5) .nav-link {
        background: linear-gradient(135deg, #7A5900, #C4900A);
        border-radius: 6px; margin: 4px 2px;
        color: white !important; font-weight: 600;
      }
      .navbar-nav .nav-item .nav-link.active {
        opacity: 1 !important;
        transform: scale(1.05);
        box-shadow: 0 4px 12px rgba(0,0,0,0.3) !important;
        font-weight: 800 !important;
      }
      .navbar-nav .nav-item .nav-link:not(.active) {
        opacity: 0.75;
      }
      .navbar-nav .nav-item .nav-link:hover {
        opacity: 1 !important;
        transform: scale(1.03);
        box-shadow: 0 2px 8px rgba(0,0,0,0.2) !important;
      }
      .btn-download {
        background-color: transparent !important;
        border: 1px solid rgba(255,255,255,0.4) !important;
        color: #E3F2FD !important;
        font-size: 11px !important;
        padding: 4px 10px !important;
        border-radius: 4px !important;
        width: 100%;
        margin-top: 4px;
      }
      .btn-download:hover {
        background-color: rgba(255,255,255,0.1) !important;
        border-color: white !important;
      }
      .method-note {
        color: #999;
        font-size: 11px;
        line-height: 1.6;
        margin-top: 8px;
        padding: 8px 12px;
        border-left: 2px solid #e0e0e0;
      }
      /* Methodology accordion styling */
.accordion-button {
  font-size: 12px !important;
  color: #999 !important;
  background-color: #fafafa !important;
  padding: 6px 12px !important;
}
.accordion-button:not(.collapsed) {
  color: #555 !important;
  background-color: #f0f0f0 !important;
  box-shadow: none !important;
}
.accordion-body {
  font-size: 11px !important;
  color: #999 !important;
  line-height: 1.6 !important;
  padding: 8px 12px !important;
  border-left: 2px solid #e0e0e0;
}
      /* Inline plot note styling */
      #food_note_inline, #labor_note_inline, #energy_note_inline {
        color: #555;
        font-size: 13px;
        margin-bottom: 8px;
        display: block;
        padding: 0 4px;
      }
    "))
  ),
  
  # ── TAB 1: HOME ──────────────────────────────────────────────────────
  nav_panel(
    title = "🏠 Home",
    
    card(
      card_header("About This Dashboard"),
      card_body(
        p("This dashboard explores the growing financial pressure facing low-income
        households in the United States. It focuses on how rising food and energy prices,
        combined with wages that have not kept pace with inflation, are reducing
        purchasing power and making everyday essentials less affordable. The project
        brings together trends that are often studied separately to show their combined
        effect on household well-being over time. Through interactive visualizations,
        users can compare changes in wages, inflation, energy costs, and household
        spending burdens across different income groups. The dashboard is designed for
          policy analysts, NGO researchers, journalists, and students interested in
          economic inequality and cost-of-living challenges. It is especially relevant
          for organizations such as Center for Equitable Energy and Economic Policy (CEEEP)
          that work on energy affordability, labor equity, and economic policy
          for vulnerable households."),
        div(
          style = "display:flex; gap:20px; align-items:center; margin-top:8px;",
          p(paste("📅 Data through:", format(latest_date, "%B %Y")),
            style = "color:#888; font-size:12px; margin:0;"),
          p(paste("🔄 Last refreshed:", data_freshness),
            style = "color:#888; font-size:12px; margin:0;"),
          p("⏱ Auto-updates monthly",
            style = "color:#888; font-size:12px; margin:0;")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title    = "Unemployment Rate",
        value    = paste0(latest_urate, "%"),
        showcase = bs_icon("people"),
        theme    = "primary"
      ),
      value_box(
        title    = "CPI Inflation (12-month)",
        value    = paste0(round(latest_cpi, 1), "%"),
        showcase = bs_icon("graph-up"),
        theme    = "danger"
      ),
      value_box(
        title    = "Energy CPI (12-month)",
        value    = paste0(round(latest_energy, 1), "%"),
        showcase = bs_icon("lightning-charge"),
        theme    = "warning"
      ),
      value_box(
        title    = "Real Min Wage (2009 $)",
        value    = paste0("$", round(latest_real_mw, 2)),
        showcase = bs_icon("cash-stack"),
        theme    = "success"
      )
    ),
    
    card(
      card_header("Why This Matters"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          
          div(
            style = "border:1px solid #e0e0e0; border-radius:8px; overflow:hidden; height:100%;",
            div(style = "background:#546E7A; padding:12px 16px; display:flex; align-items:center; gap:10px;",
                tags$span("⚡", style = "font-size:20px;"),
                tags$h5("Energy Burden", style = "color:white; font-weight:700; margin:0;")),
            div(style = "padding:16px;",
                tags$p("Low-income households spend 6–8% of their income on energy,
                compared to just 1–2% for high-income households. As energy
                prices rise, the burden falls disproportionately on households
                least able to absorb the cost.",
                       style = "color:#555; font-size:14px; line-height:1.6;"))
          ),
          
          div(
            style = "border:1px solid #e0e0e0; border-radius:8px; overflow:hidden; height:100%;",
            div(style = "background:#546E7A; padding:12px 16px; display:flex; align-items:center; gap:10px;",
                tags$span("🛒", style = "font-size:20px;"),
                tags$h5("Food Prices", style = "color:white; font-weight:700; margin:0;")),
            div(style = "padding:16px;",
                tags$p("Food inflation hits low-income households hardest because they
                spend a larger share of income on groceries. Since 2020, restaurant
                prices have risen faster than grocery prices.",
                       style = "color:#555; font-size:14px; line-height:1.6;"))
          ),
          
          div(
            style = "border:1px solid #e0e0e0; border-radius:8px; overflow:hidden; height:100%;",
            div(style = "background:#546E7A; padding:12px 16px; display:flex; align-items:center; gap:10px;",
                tags$span("💼", style = "font-size:20px;"),
                tags$h5("Wage Stagnation", style = "color:white; font-weight:700; margin:0;")),
            div(style = "padding:16px;",
                tags$p("The federal minimum wage has remained at $7.25 since 2009,
                while inflation has steadily eroded its purchasing power.
                In real terms, minimum wage workers earn substantially less today
                than they did fifteen years ago.",
                       style = "color:#555; font-size:14px; line-height:1.6;"))
          )
        )
      )
    )
  ), # end Home nav_panel
  
  # ── TAB 2: ENERGY BURDEN ─────────────────────────────────────────────
  nav_panel(
    title = "⚡ Energy Burden",
    
    layout_sidebar(
      sidebar = sidebar(
        helpText("Track energy and gasoline price inflation over time."),
        
        selectInput(
          inputId  = "energy_series",
          label    = "Select Series:",
          choices  = c(
            "Energy CPI (12-month % change)" = "energy_cpi",
            "Gasoline Price ($/gallon)"       = "gasoline"
          ),
          selected = "energy_cpi"
        ),
        
        dateRangeInput(
          inputId = "energy_dates",
          label   = "Date Range:",
          start   = as.Date("2000-01-01"),
          end     = max(energy_cpi$date)
        ),
        
        checkboxInput("show_ma",    "Show 12-Month Moving Average", value = TRUE),
        checkboxInput("log_scale",  "Log Scale", value = FALSE),
        hr(),
        tags$p(style = "font-size:11px; color:#90CAF9; line-height:1.6;",
               "📋 Data from FRED. Please cite FRED when using downloaded data."),
        downloadButton("download_energy", "⬇ Download CSV", class = "btn-download"),
        hr(),
        tags$p(style = "font-size:11px; color:#888;",
               paste("📅 Data through:", format(latest_date, "%B %Y")), tags$br(),
               paste("🔄 Last refreshed:", data_freshness), tags$br(),
               "⏱ Auto-updates monthly")
      ),
      
      div(
        style = "display:flex; justify-content:flex-end; align-items:center; gap:24px; padding:6px 12px; margin-bottom:4px; background:#f8f9fa; border-radius:6px; font-size:13px; color:#555;",
        div(bs_icon("calendar3"), " ", textOutput("energy_date_range", inline = TRUE)),
        div(bs_icon("graph-up"),  " ", textOutput("energy_series_label", inline = TRUE))
      ),
      hr(),
      card(
        card_header("Energy Price Trends"),
        card_body(
          textOutput("energy_note_inline"),
          plotOutput("energy_plot", height = "450px"),
          accordion(
            open = FALSE,
            accordion_panel(
              title = "📐 Methodology: Click to expand and access the construction method used",
              textOutput("energy_note")
            )
          )
        )
      )
    )
  ), # end Energy nav_panel
  
  # ── TAB 3: FOOD ECONOMICS ────────────────────────────────────────────
  nav_panel(
    title = "🛒 Food Economics",
    
    layout_sidebar(
      sidebar = sidebar(
        helpText("Track food price inflation at the consumer and producer level."),
        
        selectInput(
          inputId  = "food_series",
          label    = "Select Series:",
          choices  = c(
            "Food at Home CPI (% change)"    = "food_home",
            "Food Away from Home (% change)" = "food_away",
            "PPI Farm Products (% change)"   = "ppi_farm",
            "PPI Grains (% change)"          = "ppi_grains"
          ),
          selected = "food_home"
        ),
        
        dateRangeInput("food_dates", "Date Range:",
                       start = as.Date("2000-01-01"),
                       end   = max(food_home$date)),
        checkboxInput("food_show_ma",    "Show 12-Month Moving Average", value = TRUE),
        checkboxInput("food_log_scale",  "Log Scale", value = FALSE),
        hr(),
        tags$p(style = "font-size:11px; color:#90CAF9; line-height:1.6;",
               "📋 Data from FRED. Please cite FRED when using downloaded data."),
        downloadButton("download_food", "⬇ Download CSV", class = "btn-download"),
        hr(),
        tags$p(style = "font-size:11px; color:#888;",
               paste("📅 Data through:", format(latest_date, "%B %Y")), tags$br(),
               paste("🔄 Last refreshed:", data_freshness), tags$br(),
               "⏱ Auto-updates monthly")
      ),
      
      div(
        style = "display:flex; justify-content:flex-end; align-items:center; gap:24px; padding:6px 12px; margin-bottom:4px; background:#f8f9fa; border-radius:6px; font-size:13px; color:#555;",
        div(bs_icon("calendar3"), " ", textOutput("food_date_range", inline = TRUE)),
        div(bs_icon("cart3"),     " ", textOutput("food_series_label", inline = TRUE))
      ),
      hr(),
      card(
        card_header("Food Price Trends"),
        card_body(
          textOutput("food_note_inline"),
          plotOutput("food_plot", height = "450px"),
          accordion(
            open = FALSE,
            accordion_panel(
              title = "📐 Methodology: Click to expand and access the construction method used",
              p(note_food, style = "color:#999; font-size:11px; line-height:1.6;")
            )
          )
        )
      )
    )
  ), # end Food nav_panel
  
  # ── TAB 4: LABOR MARKETS ─────────────────────────────────────────────
  nav_panel(
    title = "💼 Labor Markets",
    
    layout_sidebar(
      sidebar = sidebar(
        helpText("Explore wage growth and unemployment by education level."),
        
        selectInput(
          inputId  = "labor_series",
          label    = "Select Series:",
          choices  = c(
            "Unemployment — Less than High School" = "unemp_lths",
            "Unemployment — Bachelor's & Above"    = "unemp_ba",
            "Education Unemployment Gap"            = "unemp_gap",
            "Wage Growth — Leisure & Hospitality"  = "wage_leisure",
            "Wage Growth — All Private"            = "wage_all",
            "Real Minimum Wage (2009 $)"           = "real_min_wage",
            "Low-Income SAHM Indicator"            = "sahm_lths"
          ),
          selected = "unemp_gap"
        ),
        
        dateRangeInput("labor_dates", "Date Range:",
                       start = as.Date("2000-01-01"),
                       end   = max(unemp_lths$date)),
        checkboxInput("labor_show_ma",   "Show 12-Month Moving Average", value = TRUE),
        checkboxInput("labor_log_scale", "Log Scale", value = FALSE),
        hr(),
        tags$p(style = "font-size:11px; color:#90CAF9; line-height:1.6;",
               "📋 Data from FRED. Please cite FRED when using downloaded data."),
        downloadButton("download_labor", "⬇ Download CSV", class = "btn-download"),
        hr(),
        tags$p(style = "font-size:11px; color:#888;",
               paste("📅 Data through:", format(latest_date, "%B %Y")), tags$br(),
               paste("🔄 Last refreshed:", data_freshness), tags$br(),
               "⏱ Auto-updates monthly")
      ),
      
      div(
        style = "display:flex; justify-content:flex-end; align-items:center; gap:24px; padding:6px 12px; margin-bottom:4px; background:#f8f9fa; border-radius:6px; font-size:13px; color:#555;",
        div(bs_icon("calendar3"), " ", textOutput("labor_date_range", inline = TRUE)),
        div(bs_icon("briefcase"), " ", textOutput("labor_series_label", inline = TRUE))
      ),
      hr(),
      card(
        card_header("Labor Market Trends"),
        card_body(
          textOutput("labor_note_inline"),
          plotOutput("labor_plot", height = "450px"),
          accordion(
            open = FALSE,
            accordion_panel(
              title = "📐 Methodology: Click to expand and access the construction method used",
              textOutput("labor_note")
            )
          )
        )
      )
    )
  ), # end Labor nav_panel
  
  # ── TAB 5: THE COST BURDEN ───────────────────────────────────────────
  nav_panel(
    title = "📉 The Cost Burden",
    
    layout_sidebar(
      sidebar = sidebar(
        helpText("Tracks the combined cost burden of food and energy inflation relative to low-income wage growth."),
        
        dateRangeInput("burden_dates", "Date Range:",
                       start = as.Date("2000-01-01"),
                       end   = max(affordability_gap$date)),
        checkboxInput("burden_show_ma", "Show 12-Month Moving Average", value = TRUE),
        hr(),
        tags$p(style = "font-size:11px; color:#90CAF9; line-height:1.6;",
               "📋 Data from FRED. Please cite FRED when using downloaded data."),
        downloadButton("download_burden", "⬇ Download CSV", class = "btn-download"),
        hr(),
        tags$p(style = "font-size:11px; color:#888;",
               paste("📅 Data through:", format(latest_date, "%B %Y")), tags$br(),
               paste("🔄 Last refreshed:", data_freshness), tags$br(),
               "⏱ Auto-updates monthly")
      ),
      
      div(
        style = "display:flex; justify-content:flex-end; align-items:center; gap:24px; padding:6px 12px; margin-bottom:4px; background:#f8f9fa; border-radius:6px; font-size:13px; color:#555;",
        div(bs_icon("calendar3"), " ", textOutput("burden_date_range", inline = TRUE))
      ),
      hr(),
      card(
        card_header("Household Affordability Gap Index"),
        card_body(
          p("When the index is positive (red), food and energy costs are rising
      faster than wages, consequnetly,  low-income households are falling behind.
      When negative (green), wages are outpacing costs.",
            style = "color:#555; font-size:13px; margin-bottom:8px;"),
          plotOutput("burden_plot", height = "420px"),
          accordion(
            open = FALSE,
            accordion_panel(
              title = "📐 Methodology: Click to expand and access the construction method used",
              p(note_burden, style = "color:#999; font-size:11px; line-height:1.6;")
            )
          )
        )
      )
    )
  ) # end Cost Burden nav_panel
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output) {
  
  # ── ENERGY TAB ───────────────────────────────────────────────────────
  output$energy_date_range <- renderText({
    paste(format(input$energy_dates[1], "%b %Y"),
          "to", format(input$energy_dates[2], "%b %Y"))
  })
  
  output$energy_series_label <- renderText({
    if (input$energy_series == "energy_cpi") "Energy CPI (12-month % change)"
    else "Gasoline Price ($/gallon)"
  })
  
  # Dynamic methodology note for energy tab
  output$energy_note <- renderText({
    if (input$energy_series == "energy_cpi") note_energy else note_gasoline
  })
  
  # Inline note below Energy Price Trends title
  output$energy_note_inline <- renderText({
    if (input$energy_series == "energy_cpi") {
      "When energy prices rise faster than wages (red shading), low-income households lose purchasing power. When wages outpace energy prices (green shading), households gain ground."
    } else {
      "Weekly U.S. regular gasoline retail price converted to monthly average. Gasoline is the most visible energy cost for low-income households — price spikes at the pump are felt immediately in household budgets."
    }
  })
  
  output$energy_plot <- renderPlot({
    
    if (input$energy_series == "energy_cpi") {
      df <- energy_cpi; y_col <- "value"
      y_label <- "12-Month % Change"
      title   <- "U.S. Energy CPI vs. Leisure & Hospitality Wage Growth"
      color   <- "firebrick"
    } else {
      df <- gasoline_monthly; y_col <- "value"
      y_label <- "Price per Gallon (USD)"
      title   <- "U.S. Regular Gasoline Price"
      color   <- "darkorange"
    }
    
    df <- df %>% filter(date >= input$energy_dates[1], date <= input$energy_dates[2])
    rec_filtered <- rec_bands %>% filter(xmax > input$energy_dates[1], xmin < input$energy_dates[2])
    
    p <- ggplot(df, aes(x = date, y = .data[[y_col]])) +
      geom_rect(data = rec_filtered, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey70", alpha = 0.4) +
      geom_line(color = color, linewidth = 0.8, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)
    
    if (input$energy_series == "energy_cpi") {
      wage_filtered <- wage_leisure %>%
        filter(date >= input$energy_dates[1], date <= input$energy_dates[2])
      shade_data <- df %>% select(date, energy = value) %>%
        inner_join(wage_filtered %>% select(date, wage = value), by = "date")
      
      p <- p +
        geom_ribbon(data = shade_data, inherit.aes = FALSE,
                    aes(x = date, ymin = wage, ymax = pmax(energy, wage)),
                    fill = "firebrick", alpha = 0.15) +
        geom_ribbon(data = shade_data, inherit.aes = FALSE,
                    aes(x = date, ymin = pmin(energy, wage), ymax = wage),
                    fill = "darkgreen", alpha = 0.15) +
        geom_line(data = wage_filtered, aes(x = date, y = value),
                  color = "darkgreen", linewidth = 0.8,
                  linetype = "dashed", inherit.aes = FALSE)
    }
    
    if (input$show_ma && "ma_12" %in% names(df))
      p <- p + geom_line(aes(y = ma_12), color = "navy", linewidth = 1.1)
    
    if (input$log_scale) p <- p + scale_y_log10(labels = comma)
    else p <- p + scale_y_continuous(labels = comma, n.breaks = 8)
    
    subtitle_text <- if (input$energy_series == "energy_cpi") {
      if (input$show_ma) "Red = Energy CPI | Green dashed = Wage growth | Red shading = falling behind | Green shading = gaining ground | Navy = 12-month MA | Grey = NBER recessions"
      else "Red = Energy CPI | Green dashed = Wage growth | Red shading = falling behind | Green shading = gaining ground | Grey = NBER recessions"
    } else {
      if (input$show_ma) "Navy = 12-month MA | Grey = NBER recessions"
      else "Grey = NBER recessions"
    }
    
    p + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(title = title, subtitle = subtitle_text, x = "Date", y = y_label,
           caption = "Source: FRED, Federal Reserve Bank of St. Louis. Shaded areas = NBER recessions.") +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey50", size = 10),
            plot.caption  = element_text(color = "grey60", size = 9),
            axis.text.x   = element_text(angle = 45, hjust = 1))
  })
  
  output$download_energy <- downloadHandler(
    filename = function() paste0("energy_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- if (input$energy_series == "energy_cpi") {
        energy_cpi %>% select(date, value, ma_12) %>%
          rename(energy_cpi_pct_change = value, ma_12_month = ma_12)
      } else {
        gasoline_monthly %>% select(date, value, ma_12) %>%
          rename(gasoline_price_usd = value, ma_12_month = ma_12)
      }
      writeLines(c("# Source: Federal Reserve Bank of St. Louis (FRED)",
                   "# URL: https://fred.stlouisfed.org",
                   "# Please cite FRED when using this data.",
                   "# Downloaded via: The Low Income Household Cost Burden Dashboard",
                   paste0("# Downloaded on: ", Sys.Date()), ""), file)
      write.csv(df, file, row.names = FALSE, append = TRUE)
    }
  )
  
  # ── FOOD TAB ─────────────────────────────────────────────────────────
  output$food_date_range <- renderText({
    paste(format(input$food_dates[1], "%b %Y"), "to",
          format(input$food_dates[2], "%b %Y"))
  })
  
  # Inline note below Food Price Trends title
  output$food_note_inline <- renderText({
    switch(input$food_series,
           "food_home"  = "Grocery price inflation — food purchased at supermarkets and stores. Spikes above zero indicate consumers are paying more for staples like bread, meat, dairy, and vegetables.",
           "food_away"  = "Restaurant and fast food price inflation. Has risen faster than grocery prices since 2020, driven by higher labor and input costs in the food service sector.",
           "ppi_farm"   = "Producer prices paid to farmers — a leading indicator. When farm prices spike, consumer grocery prices typically follow within 4–8 weeks.",
           "ppi_grains" = "Producer prices for grain commodities. Grain price shocks caused by droughts or supply disruptions feed directly into bread, pasta, and cereal prices at the consumer level."
    )
  })
  
  output$food_series_label <- renderText({
    switch(input$food_series,
           "food_home"  = "Food at Home CPI (% change)",
           "food_away"  = "Food Away from Home (% change)",
           "ppi_farm"   = "PPI Farm Products (% change)",
           "ppi_grains" = "PPI Grains (% change)")
  })
  
  output$food_plot <- renderPlot({
    df_info <- switch(input$food_series,
                      "food_home"  = list(df = food_home,  color = "#2E7D32",
                                          title = "Food at Home CPI — 12-Month % Change",       ylab = "12-Month % Change"),
                      "food_away"  = list(df = food_away,  color = "#388E3C",
                                          title = "Food Away from Home CPI — 12-Month % Change", ylab = "12-Month % Change"),
                      "ppi_farm"   = list(df = ppi_farm,   color = "#795548",
                                          title = "PPI Farm Products — 12-Month % Change",       ylab = "12-Month % Change"),
                      "ppi_grains" = list(df = ppi_grains, color = "#A1887F",
                                          title = "PPI Grains — 12-Month % Change",              ylab = "12-Month % Change")
    )
    
    df <- df_info$df %>% filter(date >= input$food_dates[1], date <= input$food_dates[2])
    rec_filtered <- rec_bands %>% filter(xmax > input$food_dates[1], xmin < input$food_dates[2])
    
    p <- ggplot(df, aes(x = date, y = value)) +
      geom_rect(data = rec_filtered, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey70", alpha = 0.4) +
      geom_line(color = df_info$color, linewidth = 0.8, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)
    
    if (input$food_show_ma && "ma_12" %in% names(df))
      p <- p + geom_line(aes(y = ma_12), color = "navy", linewidth = 1.1)
    
    if (input$food_log_scale) p <- p + scale_y_log10(labels = comma)
    else p <- p + scale_y_continuous(labels = comma, n.breaks = 8)
    
    p + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(title    = df_info$title,
           subtitle = if(input$food_show_ma) "Navy = 12-month MA | Grey = NBER recessions" else "Grey = NBER recessions",
           x = "Date", y = df_info$ylab,
           caption = "Source: FRED, Federal Reserve Bank of St. Louis. Shaded areas = NBER recessions.") +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey50", size = 10),
            plot.caption  = element_text(color = "grey60", size = 9),
            axis.text.x   = element_text(angle = 45, hjust = 1))
  })
  
  output$download_food <- downloadHandler(
    filename = function() paste0("food_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- switch(input$food_series,
                   "food_home"  = food_home  %>% select(date, value, ma_12) %>% rename(food_at_home_cpi_pct = value, ma_12_month = ma_12),
                   "food_away"  = food_away  %>% select(date, value, ma_12) %>% rename(food_away_cpi_pct = value, ma_12_month = ma_12),
                   "ppi_farm"   = ppi_farm   %>% select(date, value, ma_12) %>% rename(ppi_farm_pct = value, ma_12_month = ma_12),
                   "ppi_grains" = ppi_grains %>% select(date, value, ma_12) %>% rename(ppi_grains_pct = value, ma_12_month = ma_12)
      )
      writeLines(c("# Source: Federal Reserve Bank of St. Louis (FRED)",
                   "# URL: https://fred.stlouisfed.org",
                   "# Please cite FRED when using this data.",
                   "# Downloaded via: The Low Income Household Cost Burden Dashboard",
                   paste0("# Downloaded on: ", Sys.Date()), ""), file)
      write.csv(df, file, row.names = FALSE, append = TRUE)
    }
  )
  
  # ── LABOR TAB ────────────────────────────────────────────────────────
  output$labor_date_range <- renderText({
    paste(format(input$labor_dates[1], "%b %Y"), "to",
          format(input$labor_dates[2], "%b %Y"))
  })
  
  # Inline note below Labor Market Trends title
  output$labor_note_inline <- renderText({
    switch(input$labor_series,
           "unemp_lths"    = "Unemployment rate for workers without a high school diploma — the most economically vulnerable group. Always higher than the national average and rises sharply in every recession.",
           "unemp_ba"      = "Unemployment rate for workers with a bachelor's degree or above. Consistently the lowest of any education group, showing the strong labor market protection that higher education provides.",
           "unemp_gap"     = "Gap between less-than-HS and bachelor's unemployment rates. Widens dramatically in every recession — low-education workers are always hit first and recover last.",
           "wage_leisure"  = "12-month wage growth in Leisure and Hospitality — the lowest paid major private sector. Used as a proxy for low-income wage trends. Compare with the Energy tab to see periods of squeeze.",
           "wage_all"      = "12-month wage growth for all private employees — shown for comparison. The gap between this and Leisure and Hospitality wages reveals structural wage inequality across sectors.",
           "real_min_wage" = "Federal minimum wage deflated by CPI and rebased to 2009 dollars. The flat nominal wage of $7.25 has lost ~35% of its purchasing power since 2009 as inflation accumulated while Congress did not act.",
           "sahm_lths"     = "Low-Income SAHM Indicator: triggers above the 0.5pp red dashed threshold, signaling a labor market downturn specifically for less-than-HS workers. Triggered at ~2pp (2001), ~6pp (2008), and ~14pp (2020) — far exceeding the national threshold each time."
    )
  })
  
  output$labor_series_label <- renderText({
    switch(input$labor_series,
           "unemp_lths"    = "Unemployment — Less than High School",
           "unemp_ba"      = "Unemployment — Bachelor's & Above",
           "unemp_gap"     = "Education Unemployment Gap",
           "wage_leisure"  = "Wage Growth — Leisure & Hospitality",
           "wage_all"      = "Wage Growth — All Private",
           "real_min_wage" = "Real Minimum Wage (2009 $)",
           "sahm_lths"     = "Low-Income SAHM Indicator")
  })
  
  # Dynamic methodology note for labor tab
  output$labor_note <- renderText({
    switch(input$labor_series,
           "unemp_lths"    = note_labor_unemp,
           "unemp_ba"      = note_labor_unemp,
           "unemp_gap"     = note_labor_gap,
           "wage_leisure"  = note_labor_wage,
           "wage_all"      = note_labor_wage,
           "real_min_wage" = note_real_min_wage,
           "sahm_lths"     = note_sahm)
  })
  
  output$labor_plot <- renderPlot({
    
    df_info <- switch(input$labor_series,
                      "unemp_lths" = list(
                        df = unemp_lths, color = "#C62828",
                        title = "Unemployment Rate — Less Than High School Diploma",
                        ylab  = "Unemployment Rate (%)"),
                      "unemp_ba" = list(
                        df = unemp_ba, color = "#1565C0",
                        title = "Unemployment Rate — Bachelor's Degree and Above",
                        ylab  = "Unemployment Rate (%)"),
                      "unemp_gap" = list(
                        df = unemp_gap %>% rename(value = gap), color = "#6A1B9A",
                        title = "Education Unemployment Gap (Less than HS minus Bachelor's)",
                        ylab  = "Percentage Point Gap"),
                      "wage_leisure" = list(
                        df = wage_leisure, color = "#E65100",
                        title = "Wage Growth — Leisure & Hospitality (12-Month % Change)",
                        ylab  = "12-Month % Change"),
                      "wage_all" = list(
                        df = wage_all, color = "#2E7D32",
                        title = "Wage Growth — All Private Employees (12-Month % Change)",
                        ylab  = "12-Month % Change"),
                      "real_min_wage" = list(
                        df = real_min_wage %>%
                          select(date, real_min_wage) %>%
                          rename(value = real_min_wage) %>%
                          mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right")),
                        color = "#1565C0",
                        title = "Real Federal Minimum Wage (2009 Dollars)",
                        ylab  = "USD (2009 dollars)"),
                      "sahm_lths" = list(
                        df = unemp_lths_sahm %>%
                          select(date, sahm_lths) %>%
                          rename(value = sahm_lths) %>%
                          mutate(ma_12 = rollmean(value, 12, fill = NA, align = "right")),
                        color = "#B71C1C",
                        title = "Low-Income SAHM Indicator — Less Than High School Unemployment",
                        ylab  = "Percentage Points above 12-month minimum")
    )
    
    df <- df_info$df %>%
      filter(date >= input$labor_dates[1], date <= input$labor_dates[2])
    rec_filtered <- rec_bands %>%
      filter(xmax > input$labor_dates[1], xmin < input$labor_dates[2])
    
    p <- ggplot(df, aes(x = date, y = value)) +
      geom_rect(data = rec_filtered, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey70", alpha = 0.4) +
      geom_line(color = df_info$color, linewidth = 0.8, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)
    
    # Add SAHM threshold line when SAHM is selected
    if (input$labor_series == "sahm_lths") {
      p <- p + geom_hline(
        yintercept = 0.5,
        linetype   = "dashed",
        color      = "firebrick",
        linewidth  = 0.8
      ) +
        annotate("text", x = min(df$date, na.rm = TRUE),
                 y = 0.6, label = "0.5 trigger threshold",
                 color = "firebrick", size = 3.5, hjust = 0)
    }
    
    if (input$labor_show_ma && "ma_12" %in% names(df))
      p <- p + geom_line(aes(y = ma_12), color = "navy", linewidth = 1.1)
    
    if (input$labor_log_scale) p <- p + scale_y_log10(labels = comma)
    else p <- p + scale_y_continuous(labels = comma, n.breaks = 8)
    
    p + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(title    = df_info$title,
           subtitle = if(input$labor_show_ma) "Navy = 12-month MA | Grey = NBER recessions" else "Grey = NBER recessions",
           x = "Date", y = df_info$ylab,
           caption = "Source: FRED, Federal Reserve Bank of St. Louis. Shaded areas = NBER recessions.") +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey50", size = 10),
            plot.caption  = element_text(color = "grey60", size = 9),
            axis.text.x   = element_text(angle = 45, hjust = 1))
  })
  
  output$download_labor <- downloadHandler(
    filename = function() paste0("labor_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- switch(input$labor_series,
                   "unemp_lths"    = unemp_lths    %>% select(date, value, ma_12) %>% rename(unemp_less_than_hs = value, ma_12_month = ma_12),
                   "unemp_ba"      = unemp_ba      %>% select(date, value, ma_12) %>% rename(unemp_bachelors = value, ma_12_month = ma_12),
                   "unemp_gap"     = unemp_gap     %>% select(date, gap, ma_12)   %>% rename(education_gap_pp = gap, ma_12_month = ma_12),
                   "wage_leisure"  = wage_leisure  %>% select(date, value, ma_12) %>% rename(wage_leisure_pct = value, ma_12_month = ma_12),
                   "wage_all"      = wage_all      %>% select(date, value, ma_12) %>% rename(wage_all_pct = value, ma_12_month = ma_12),
                   "real_min_wage" = real_min_wage %>% select(date, real_min_wage) %>% rename(real_min_wage_2009usd = real_min_wage),
                   "sahm_lths"     = unemp_lths_sahm %>% select(date, sahm_lths) %>% rename(sahm_low_income = sahm_lths)
      )
      writeLines(c("# Source: Federal Reserve Bank of St. Louis (FRED)",
                   "# URL: https://fred.stlouisfed.org",
                   "# Please cite FRED when using this data.",
                   "# Downloaded via: The Low Income Household Cost Burden Dashboard",
                   paste0("# Downloaded on: ", Sys.Date()), ""), file)
      write.csv(df, file, row.names = FALSE, append = TRUE)
    }
  )
  
  # ── COST BURDEN TAB ──────────────────────────────────────────────────
  output$burden_date_range <- renderText({
    paste(format(input$burden_dates[1], "%b %Y"), "to",
          format(input$burden_dates[2], "%b %Y"))
  })
  
  output$burden_plot <- renderPlot({
    df <- affordability_gap %>%
      filter(date >= input$burden_dates[1], date <= input$burden_dates[2])
    rec_filtered <- rec_bands %>%
      filter(xmax > input$burden_dates[1], xmin < input$burden_dates[2])
    
    p <- ggplot(df, aes(x = date, y = gap_index)) +
      geom_rect(data = rec_filtered, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey70", alpha = 0.3) +
      geom_col(aes(fill = gap_index > 0), alpha = 0.6, width = 20) +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "darkgreen"),
                        guide  = "none") +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.6)
    
    if (input$burden_show_ma)
      p <- p + geom_line(aes(y = ma_12), color = "navy",
                         linewidth = 1.2, na.rm = TRUE)
    
    p + scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_y_continuous(labels = function(x) paste0(x, " pp"), n.breaks = 8) +
      labs(title    = "Household Affordability Gap Index",
           subtitle = if(input$burden_show_ma) {
             "Red bars = falling behind | Green bars = gaining ground | Navy = 12-month MA | Grey = NBER recessions"
           } else {
             "Red bars = falling behind | Green bars = gaining ground | Grey = NBER recessions"
           },
           x = "Date", y = "Percentage Points",
           caption = "Index = (Food Inflation + Energy Inflation) / 2 − Leisure & Hospitality Wage Growth. Source: FRED.") +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey50", size = 10),
            plot.caption  = element_text(color = "grey60", size = 9),
            axis.text.x   = element_text(angle = 45, hjust = 1))
  })
  
  output$download_burden <- downloadHandler(
    filename = function() paste0("affordability_gap_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- affordability_gap %>%
        select(date, gap_index, ma_12) %>%
        rename(affordability_gap_pp = gap_index, ma_12_month = ma_12)
      writeLines(c("# Source: Federal Reserve Bank of St. Louis (FRED)",
                   "# URL: https://fred.stlouisfed.org",
                   "# Index = (Food Inflation + Energy Inflation) / 2 - Leisure & Hospitality Wage Growth",
                   "# Please cite FRED when using this data.",
                   "# Downloaded via: The Low Income Household Cost Burden Dashboard",
                   paste0("# Downloaded on: ", Sys.Date()), ""), file)
      write.csv(df, file, row.names = FALSE, append = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)