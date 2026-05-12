# ============================================================
#  US County-Level Rent vs. Income — Cost of Living Explorer
#  Source: US Census ACS 2020-2024 5-Year Estimates (API)
# ============================================================

r_version <- paste(R.version[["major"]], sub("\\..*", "", R.version[["minor"]]), sep = ".")
personal_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", r_version)
dir.create(personal_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(personal_lib, .libPaths()))

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(DT)
library(jsonlite)
library(DBI)
library(RSQLite)

census_api_url <- function(year, include_key = TRUE) {
  base_url <- paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/acs5?get=NAME,B25064_001E,B19013_001E&for=county:*"
  )

  key <- Sys.getenv("CENSUS_API_KEY", unset = Sys.getenv("CENSUS_KEY", unset = ""))
  if (include_key && nzchar(key)) {
    paste0(base_url, "&key=", utils::URLencode(key, reserved = TRUE))
  } else {
    base_url
  }
}

fetch_acs_county_data <- function(year) {
  url <- census_api_url(year)

  raw <- tryCatch(
    fromJSON(url),
    error = function(e) {
      message("Census API request failed for ", year, ": ", conditionMessage(e))
      message("Using local fallback data from acs_county.json.")
      fromJSON("acs_county.json")
    }
  )
  df <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
  names(df) <- raw[1, ]

  df <- df %>%
    mutate(
      median_rent    = as.numeric(B25064_001E),
      median_income  = as.numeric(B19013_001E),
      geoid          = paste0(state, county),
      county         = sub(" County$", "", sub(",.*", "", NAME)),
      state          = sub(".*, ", "", NAME),
      annual_rent    = median_rent * 12,
      rent_to_income = round((annual_rent / median_income) * 100, 1)
    ) %>%
    filter(median_rent > 0, median_income > 0) %>%
    select(geoid, county, state, median_rent, median_income,
           annual_rent, rent_to_income)

  df
}

# Pull distinct state list for the filter dropdown (from ACS API)
temp_df <- fetch_acs_county_data(2022)  # Use 2022 as reference year
all_states <- sort(unique(temp_df$state))
available_years <- 2020:2024

energy_columns <- c(
  "geoid",
  "county",
  "state",
  "electricity_rate",
  "natural_gas_rate",
  "energy_burden",
  "solar_potential",
  "ev_chargers"
)

initialize_energy_database <- function(csv_path = "energy_costs.csv", db_path = "energy.db") {
  if (!file.exists(csv_path)) {
    stop("Missing ", csv_path, ". Expected columns: ", paste(energy_columns, collapse = ", "))
  }

  energy_df <- read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    colClasses = c(geoid = "character")
  )

  missing_cols <- setdiff(energy_columns, names(energy_df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ", csv_path, ": ", paste(missing_cols, collapse = ", "))
  }

  energy_df <- energy_df %>%
    select(all_of(energy_columns)) %>%
    mutate(
      geoid            = as.character(geoid),
      county           = as.character(county),
      state            = as.character(state),
      electricity_rate = as.numeric(electricity_rate),
      natural_gas_rate = as.numeric(natural_gas_rate),
      energy_burden    = as.numeric(energy_burden),
      solar_potential  = as.numeric(solar_potential),
      ev_chargers      = as.integer(ev_chargers)
    )

  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS county_energy (
      geoid            TEXT PRIMARY KEY,
      county           TEXT NOT NULL,
      state            TEXT NOT NULL,
      electricity_rate REAL NOT NULL,
      natural_gas_rate REAL NOT NULL,
      energy_burden    REAL NOT NULL,
      solar_potential  REAL NOT NULL,
      ev_chargers      INTEGER NOT NULL
    )
  ")

  insert_sql <- "
    INSERT OR REPLACE INTO county_energy (
      geoid, county, state, electricity_rate, natural_gas_rate,
      energy_burden, solar_potential, ev_chargers
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  "

  dbWithTransaction(con, {
    dbExecute(con, "DELETE FROM county_energy")
    for (i in seq_len(nrow(energy_df))) {
      dbExecute(con, insert_sql, params = unname(as.list(energy_df[i, energy_columns])))
    }
  })

  invisible(energy_df)
}

read_county_energy <- function(db_path = "energy.db") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  dbReadTable(con, "county_energy") %>%
    mutate(
      geoid            = as.character(geoid),
      electricity_rate = as.numeric(electricity_rate),
      natural_gas_rate = as.numeric(natural_gas_rate),
      energy_burden    = as.numeric(energy_burden),
      solar_potential  = as.numeric(solar_potential),
      ev_chargers      = as.integer(ev_chargers)
    )
}

initialize_energy_database()

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body  { font-family: 'Segoe UI', sans-serif; background: #f4f6f8; }
    .well { background: #fff; border: 1px solid #ddd; border-radius:6px; }
    h2    { color: #2c3e50; }
    .mbox { background:#fff; border-radius:8px; padding:12px 16px;
            border-left:5px solid #3498db; margin-bottom:10px;
            box-shadow:0 1px 3px rgba(0,0,0,.06); }
    .mbox h5 { margin:0 0 2px 0; color:#888; font-size:11px;
               text-transform:uppercase; letter-spacing:.5px; }
    .mbox p  { margin:0; font-size:20px; font-weight:bold; color:#2c3e50; }
    .sql-box { background:#1e1e1e; color:#d4d4d4; font-family:monospace;
               font-size:11px; padding:10px 12px; border-radius:5px;
               white-space:pre-wrap; margin-top:6px; }
  "))),

  titlePanel(div(
    h2("US County Rent vs. Income — Cost of Living Explorer"),
    p(style = "color:#888; margin-top:-8px; font-size:13px;",
      "3,212 counties | 2020-2024 ACS 5-Year Estimates | Census API backend")
  )),

  tabsetPanel(
    tabPanel(
      "Rent Burden",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("year_filter", "Select Year:",
                      choices  = as.character(available_years),
                      selected = "2022"),
          selectInput("state_filter", "Filter by State:",
                      choices  = c("All States", all_states),
                      selected = "All States"),
          sliderInput("top_n", "Top N burdened counties:",
                      min = 5, max = 40, value = 15, step = 5),
          hr(),
          h5(style = "font-weight:bold; margin-bottom:4px;", "API Query"),
          uiOutput("api_display"),
          hr(),
          tags$p(style = "font-size:11px; color:#aaa;",
            "Data: US Census ACS 2020-2024 (B25064, B19013).",
            tags$br(),
            "Fetched from Census API when available; local cache used if the API requires a key.")
        ),

        mainPanel(
          width = 9,
          fluidRow(
            column(3, uiOutput("box_counties")),
            column(3, uiOutput("box_rent")),
            column(3, uiOutput("box_income")),
            column(3, uiOutput("box_ratio"))
          ),
          br(),
          fluidRow(
            column(6, plotlyOutput("scatter", height = "380px")),
            column(6, plotlyOutput("bar_top", height = "380px"))
          ),
          hr(),
          DTOutput("tbl")
        )
      )
    ),
    tabPanel(
      "Energy Costs",
      br(),
      fluidRow(
        column(6, plotlyOutput("energy_electricity_bar", height = "420px")),
        column(6, plotlyOutput("energy_burden_scatter", height = "420px"))
      ),
      hr(),
      DTOutput("energy_tbl")
    )
  )
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------
server <- function(input, output, session) {

  year_cache <- reactiveVal(list())

  # Reactive: load ACS data for the selected year and state filter
  county_data <- reactive({
    year <- as.integer(input$year_filter)
    cached <- year_cache()

    if (!is.null(cached[[as.character(year)]])) {
      df <- cached[[as.character(year)]]
    } else {
      df <- fetch_acs_county_data(year)
      cached[[as.character(year)]] <- df
      year_cache(cached)
    }

    if (input$state_filter != "All States") {
      df <- df %>% filter(state == input$state_filter)
    }

    df <- df %>%
      arrange(desc(rent_to_income), desc(median_rent), desc(median_income))

    last_sql(paste0("Census API request: ", census_api_url(year, include_key = FALSE)))

    df
  })

  last_sql <- reactiveVal("")

  county_energy_data <- reactive({
    read_county_energy()
  })

  # Show the live API query in the sidebar
  output$api_display <- renderUI({
    div(class = "sql-box", last_sql())
  })

  # Summary stats from the selected year/state data
  stats <- reactive({
    df <- county_data()
    req(nrow(df) > 0)

    df %>%
      summarise(
        n = n(),
        avg_rent = round(mean(median_rent)),
        avg_income = round(mean(median_income)),
        avg_ratio = round(mean(rent_to_income), 1),
        pct_burdened = round(100.0 * sum(if_else(rent_to_income >= 30, 1, 0)) / n(), 0)
      )
  })

  # Metric boxes
  output$box_counties <- renderUI({
    s <- stats(); req(nrow(s) > 0)
    div(class="mbox", tags$h5("Counties"), tags$p(scales::comma(s$n)))
  })
  output$box_rent <- renderUI({
    s <- stats(); req(nrow(s) > 0)
    div(class="mbox", tags$h5("Avg Monthly Rent"),
        tags$p(scales::dollar(s$avg_rent)))
  })
  output$box_income <- renderUI({
    s <- stats(); req(nrow(s) > 0)
    div(class="mbox", tags$h5("Avg Household Income"),
        tags$p(scales::dollar(s$avg_income)))
  })
  output$box_ratio <- renderUI({
    s <- stats(); req(nrow(s) > 0)
    col <- if (s$avg_ratio >= 30) "#e74c3c" else "#27ae60"
    div(class="mbox", style=paste0("border-left-color:", col, ";"),
        tags$h5("Avg Rent Burden"),
        tags$p(style=paste0("color:", col, ";"),
               paste0(s$avg_ratio, "% (", s$pct_burdened, "% of counties >30%)")))
  })

  # Scatter plot: income vs rent, colored by burden
  output$scatter <- renderPlotly({
    df <- county_data(); req(nrow(df) > 0)
    df$tip <- paste0(
      "<b>", df$county, ", ", df$state, "</b><br>",
      "Rent: ",   scales::dollar(df$median_rent),   "/mo<br>",
      "Income: ", scales::dollar(df$median_income), "<br>",
      "Burden: ", df$rent_to_income, "%"
    )

    p <- ggplot(df, aes(x = median_income, y = median_rent,
                        color = rent_to_income, text = tip)) +
      geom_point(alpha = 0.5, size = 1.3) +
      geom_smooth(method = "lm", se = FALSE, color = "gray40",
                  linetype = "dashed", linewidth = 0.6) +
      scale_color_gradient2(low="#2166ac", mid="#ffffbf", high="#d73027",
                            midpoint=30, name="Burden %") +
      scale_x_continuous(labels = scales::label_dollar(scale=1e-3, suffix="k")) +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = "Rent vs. Household Income",
           x = "Median Household Income", y = "Median Monthly Rent") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation="v", x=1.02, y=0.5))
  })

  # Bar: top N most burdened counties
  output$bar_top <- renderPlotly({
    n <- input$top_n
    df <- county_data() %>%
      arrange(desc(rent_to_income), desc(median_rent), desc(median_income)) %>%
      slice_head(n = n)

    req(nrow(df) > 0)

    df$label <- paste0(df$county, ", ", df$state)
    df$tip <- paste0(
      "<b>", df$label, "</b><br>",
      "Burden: ", df$rent_to_income, "%<br>",
      "Rent: ",   scales::dollar(df$median_rent),   "/mo<br>",
      "Income: ", scales::dollar(df$median_income)
    )

    p <- ggplot(df, aes(x = reorder(label, rent_to_income),
                        y = rent_to_income, fill = rent_to_income, text = tip)) +
      geom_col(width = 0.75) +
      geom_hline(yintercept = 30, linetype = "dashed",
                 color = "#2c3e50", linewidth = 0.7) +
      scale_fill_gradient2(low="#2166ac", mid="#ffffbf", high="#d73027",
                           midpoint=30, guide="none") +
      coord_flip() +
      labs(title = paste("Top", n, "Most Rent-Burdened Counties"),
           x = NULL, y = "Rent-to-Income (%)") +
      theme_minimal(base_size = 10) +
      theme(plot.title  = element_text(face = "bold"),
            axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  # Sortable data table (full query result)
  output$tbl <- renderDT({
    df <- county_data(); req(nrow(df) > 0)
    df %>%
      transmute(
        County             = paste0(county, ", ", state),
        `Monthly Rent`     = scales::dollar(median_rent),
        `Annual Rent`      = scales::dollar(annual_rent),
        `Household Income` = scales::dollar(median_income),
        `Rent Burden (%)`  = rent_to_income
      ) %>%
      datatable(
        rownames = FALSE,
        filter   = "top",
        options  = list(pageLength = 12, scrollX = TRUE,
                        order = list(list(4, "desc")),
                        columnDefs = list(list(type = 'num', targets = 4)))
      ) %>%
      formatStyle(
        "Rent Burden (%)",
        background         = styleColorBar(c(0, 80), "#f8d7da"),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle(
        "Rent Burden (%)",
        color = styleInterval(c(25, 30, 35),
                              c("#155724","#856404","#721c24","#491217"))
      )
  })

  # Bar chart: electricity rates by county
  output$energy_electricity_bar <- renderPlotly({
    df <- county_energy_data() %>%
      arrange(electricity_rate)

    req(nrow(df) > 0)

    df$label <- paste0(df$county, ", ", df$state)
    df$tip <- paste0(
      "<b>", df$label, "</b><br>",
      "Electricity: ", scales::dollar(df$electricity_rate, accuracy = 0.001), "/kWh<br>",
      "Natural Gas: ", scales::dollar(df$natural_gas_rate, accuracy = 0.01), "/therm<br>",
      "Energy Burden: ", df$energy_burden, "%"
    )

    p <- ggplot(df, aes(x = reorder(label, electricity_rate),
                        y = electricity_rate, text = tip)) +
      geom_col(fill = "#3498db", width = 0.75) +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar_format(accuracy = 0.001)) +
      labs(title = "Electricity Rate by County",
           x = NULL, y = "Electricity Rate ($/kWh)") +
      theme_minimal(base_size = 10) +
      theme(plot.title  = element_text(face = "bold"),
            axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  # Scatter plot: energy burden vs electricity rate
  output$energy_burden_scatter <- renderPlotly({
    df <- county_energy_data()
    req(nrow(df) > 0)

    df$label <- paste0(df$county, ", ", df$state)
    df$tip <- paste0(
      "<b>", df$label, "</b><br>",
      "Energy Burden: ", df$energy_burden, "%<br>",
      "Electricity: ", scales::dollar(df$electricity_rate, accuracy = 0.001), "/kWh<br>",
      "Solar Potential: ", df$solar_potential, "<br>",
      "EV Chargers: ", scales::comma(df$ev_chargers)
    )

    p <- ggplot(df, aes(x = electricity_rate, y = energy_burden,
                        color = solar_potential, size = ev_chargers, text = tip)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous(labels = scales::dollar_format(accuracy = 0.001)) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_color_gradient(low = "#2166ac", high = "#d73027",
                           name = "Solar Potential") +
      scale_size_continuous(name = "EV Chargers", range = c(4, 10)) +
      labs(title = "Energy Burden vs. Electricity Rate",
           x = "Electricity Rate ($/kWh)", y = "Energy Burden (%)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text")
  })

  # Sortable data table (all energy fields)
  output$energy_tbl <- renderDT({
    df <- county_energy_data()
    req(nrow(df) > 0)

    datatable(
      df,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 12, scrollX = TRUE,
                      order = list(list(5, "desc")))
    ) %>%
      formatCurrency(c("electricity_rate", "natural_gas_rate"),
                     currency = "$", digits = 3) %>%
      formatRound(c("energy_burden", "solar_potential"), digits = 1) %>%
      formatRound("ev_chargers", digits = 0) %>%
      formatStyle(
        "energy_burden",
        background         = styleColorBar(c(0, max(df$energy_burden, na.rm = TRUE)), "#d7ebff"),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })
}

shinyApp(ui, server)
