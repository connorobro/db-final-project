# ============================================================
#  US County-Level Rent vs. Income — Cost of Living Explorer
#  Source: US Census ACS 2020-2024 5-Year Estimates (API)
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(DT)
library(jsonlite)
library(DBI)
library(RPostgres)

state_lookup <- data.frame(
  state = c(state.name, "District of Columbia", "Puerto Rico"),
  state_abbr = c(state.abb, "DC", "PR"),
  stringsAsFactors = FALSE
)

census_api_key <- Sys.getenv("CENSUS_API_KEY", unset = "")
use_census_api <- nzchar(census_api_key)

fetch_acs_county_data <- function(year) {
  if (!use_census_api && year == 2022 && file.exists("acs_county.json")) {
    raw <- fromJSON("acs_county.json")
    df <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
    names(df) <- c("NAME", "B25064_001E", "B19013_001E", "state", "county")
  } else if (use_census_api) {
    url <- paste0(
      "https://api.census.gov/data/",
      year,
      "/acs/acs5?get=NAME,B25064_001E,B19013_001E&for=county:*&key=",
      census_api_key
    )

    raw <- fromJSON(url)
    df <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
    names(df) <- raw[1, ]
  } else {
    stop("No Census API key found and no local 2022 data available.")
  }

  df <- df %>%
    mutate(
      state_fips     = state,
      county_fips    = county,
      median_rent    = as.numeric(B25064_001E),
      median_income  = as.numeric(B19013_001E),
      geoid          = paste0(state_fips, county_fips),
      county         = sub(" County$", "", sub(",.*", "", NAME)),
      state          = sub(".*, ", "", NAME),
      annual_rent    = median_rent * 12,
      rent_to_income = round((annual_rent / median_income) * 100, 1),
      state_abbr     = state_lookup$state_abbr[match(state, state_lookup$state)],
      county_sequence = as.integer(substr(geoid, 3, 5))
    ) %>%
    filter(median_rent > 0, median_income > 0) %>%
    select(year = year, geoid, state_fips, county_fips, county, state, state_abbr,
           county_sequence, median_rent, median_income, annual_rent, rent_to_income)

  df
}

load_jobmarket_data <- function(path = "county_wages_jobmarket_2023.csv") {
  read.csv(path, stringsAsFactors = FALSE,
           colClasses = c(county_fips = "character")) %>%
    mutate(
      geoid = sprintf("%05d", as.integer(county_fips)),
      job_geoid = geoid,
      county_sequence = as.integer(substr(geoid, 3, 5)),
      job_year = year,
      job_median_household_income = median_household_income
    ) %>%
    select(
      job_geoid,
      county_sequence,
      job_county_name = county_name,
      state_abbr,
      job_year,
      num_establishments,
      total_employment,
      total_wages,
      taxable_wages,
      avg_weekly_wage,
      avg_annual_pay,
      employment_yoy_pct_change,
      wage_yoy_pct_change,
      location_quotient_employment,
      unemployment_rate,
      job_median_household_income
    )
}

# PostgreSQL helper functions

db_connect <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = "university",
    host     = "localhost",
    port     = 5432,
    user     = "postgres",
    password = "postgres"
  )
}

fetch_acs_county_data_db <- function(year, state_filter = "All States") {
  if (state_filter != "All States") {
    sql <- "SELECT geoid, state_fips, county_fips, county, state, median_rent, median_income, annual_rent, rent_to_income
            FROM cost_of_living
            WHERE year = $1 AND state = $2
            ORDER BY rent_to_income DESC, median_rent DESC, median_income DESC"
    dbGetQuery(db_con, sql, params = list(year, state_filter))
  } else {
    sql <- "SELECT geoid, state_fips, county_fips, county, state, median_rent, median_income, annual_rent, rent_to_income
            FROM cost_of_living
            WHERE year = $1
            ORDER BY rent_to_income DESC, median_rent DESC, median_income DESC"
    dbGetQuery(db_con, sql, params = list(year))
  }
}

fetch_comparison_data_db <- function(year, state_filter = "All States") {
  if (state_filter != "All States") {
    sql <- "SELECT c.geoid, c.state_fips, c.county_fips, c.county, c.state,
                     c.median_rent, c.median_income, c.annual_rent, c.rent_to_income,
                     c.state_abbr, c.county_sequence,
                     j.job_geoid, j.job_county_name, j.state_abbr AS job_state_abbr,
                     j.job_year, j.num_establishments, j.total_employment, j.total_wages,
                     j.taxable_wages, j.avg_weekly_wage, j.avg_annual_pay,
                     j.employment_yoy_pct_change, j.wage_yoy_pct_change,
                     j.location_quotient_employment, j.unemployment_rate,
                     j.job_median_household_income
            FROM cost_of_living c
            INNER JOIN job_market j
              ON c.state_abbr = j.state_abbr
             AND c.county_sequence = j.county_sequence
            WHERE c.year = $1 AND c.state = $2
            ORDER BY c.rent_to_income DESC, j.avg_annual_pay DESC"
    dbGetQuery(db_con, sql, params = list(year, state_filter))
  } else {
    sql <- "SELECT c.geoid, c.state_fips, c.county_fips, c.county, c.state,
                     c.median_rent, c.median_income, c.annual_rent, c.rent_to_income,
                     c.state_abbr, c.county_sequence,
                     j.job_geoid, j.job_county_name, j.state_abbr AS job_state_abbr,
                     j.job_year, j.num_establishments, j.total_employment, j.total_wages,
                     j.taxable_wages, j.avg_weekly_wage, j.avg_annual_pay,
                     j.employment_yoy_pct_change, j.wage_yoy_pct_change,
                     j.location_quotient_employment, j.unemployment_rate,
                     j.job_median_household_income
            FROM cost_of_living c
            INNER JOIN job_market j
              ON c.state_abbr = j.state_abbr
             AND c.county_sequence = j.county_sequence
            WHERE c.year = $1
            ORDER BY c.rent_to_income DESC, j.avg_annual_pay DESC"
    dbGetQuery(db_con, sql, params = list(year))
  }
}

check_db_available <- function(con) {
  if (is.null(con)) return(FALSE)
  ok <- tryCatch({
    dbGetQuery(con, "SELECT 1 FROM cost_of_living LIMIT 1")
    dbGetQuery(con, "SELECT 1 FROM job_market LIMIT 1")
    TRUE
  }, error = function(e) {
    warning("Database schema unavailable: ", e$message)
    FALSE
  })
  if (!ok) {
    dbDisconnect(con)
    return(FALSE)
  }
  TRUE
}

# Attempt to use PostgreSQL if configured

db_con <- tryCatch(db_connect(), error = function(e) {
  warning("Could not connect to PostgreSQL: ", e$message)
  NULL
})
db_available <- check_db_available(db_con)

if (db_available) {
  all_states <- sort(dbGetQuery(db_con, "SELECT DISTINCT state FROM cost_of_living ORDER BY state")$state)
  available_years <- dbGetQuery(db_con, "SELECT DISTINCT year FROM cost_of_living ORDER BY year")$year
  job_data <- NULL
} else {
  temp_df <- fetch_acs_county_data(2022)
  all_states <- sort(unique(temp_df$state))
  available_years <- if (use_census_api) 2020:2024 else 2022
  job_data <- load_jobmarket_data()
}

housing_metric_choices <- c(
  "Monthly Rent" = "median_rent",
  "Household Income" = "median_income",
  "Annual Rent" = "annual_rent",
  "Rent Burden" = "rent_to_income"
)

job_metric_choices <- c(
  "Average Annual Pay" = "avg_annual_pay",
  "Average Weekly Wage" = "avg_weekly_wage",
  "Unemployment Rate" = "unemployment_rate",
  "Total Employment" = "total_employment",
  "Business Establishments" = "num_establishments",
  "Employment YoY Change" = "employment_yoy_pct_change",
  "Wage YoY Change" = "wage_yoy_pct_change",
  "Employment Location Quotient" = "location_quotient_employment",
  "Job Market Household Income" = "job_median_household_income"
)

metric_label <- function(metric, choices) {
  names(choices)[match(metric, unname(choices))]
}

percent_metrics <- c(
  "rent_to_income",
  "unemployment_rate",
  "employment_yoy_pct_change",
  "wage_yoy_pct_change"
)

currency_metrics <- c(
  "median_rent",
  "median_income",
  "annual_rent",
  "avg_weekly_wage",
  "avg_annual_pay",
  "total_wages",
  "taxable_wages",
  "job_median_household_income"
)

format_metric_values <- function(x, metric) {
  if (metric %in% currency_metrics) {
    return(scales::dollar(round(x)))
  }
  if (metric %in% percent_metrics) {
    return(paste0(round(x, 1), "%"))
  }
  if (metric == "location_quotient_employment") {
    return(round(x, 2))
  }
  scales::comma(round(x))
}

axis_label_for <- function(metric) {
  if (metric %in% currency_metrics) {
    return(scales::dollar)
  }
  if (metric %in% percent_metrics) {
    return(function(x) paste0(x, "%"))
  }
  if (metric == "location_quotient_employment") {
    return(function(x) round(x, 2))
  }
  scales::comma
}

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

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("year_filter", "Select Year:",
                  choices  = as.character(available_years),
                  selected = "2023"),
      selectInput("state_filter", "Filter by State:",
                  choices  = c("All States", all_states),
                  selected = "All States"),
      sliderInput("top_n", "Top N burdened counties:",
                  min = 5, max = 40, value = 15, step = 5),
      hr(),
      h5(style = "font-weight:bold; margin-bottom:4px;", "Comparison Metrics"),
      selectInput("housing_metric", "ACS metric:",
                  choices = housing_metric_choices,
                  selected = "rent_to_income"),
      selectInput("job_metric", "Job-market metric:",
                  choices = job_metric_choices,
                  selected = "avg_annual_pay"),
      hr(),
      h5(style = "font-weight:bold; margin-bottom:4px;", "API Query"),
      uiOutput("api_display"),
      hr(),
      tags$p(style = "font-size:11px; color:#aaa;",
        "Data: US Census ACS 2020-2024 (B25064, B19013).",
        tags$br(),
        "Uses PostgreSQL when available; otherwise fetches live from the Census API.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Cost Overview",
          br(),
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
        ),
        tabPanel(
          "Job Market Compare",
          br(),
          fluidRow(
            column(3, uiOutput("box_joined_counties")),
            column(3, uiOutput("box_metric_correlation")),
            column(3, uiOutput("box_avg_job_metric")),
            column(3, uiOutput("box_job_year"))
          ),
          br(),
          fluidRow(
            column(7, plotlyOutput("job_scatter", height = "430px")),
            column(5, plotlyOutput("job_corr_bar", height = "430px"))
          ),
          hr(),
          DTOutput("job_tbl")
        )
      )
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

    if (db_available) {
      df <- fetch_acs_county_data_db(year, input$state_filter)
      last_sql(paste0("SQL query: cost_of_living year=", year,
                      if (input$state_filter != "All States")
                        paste0(" state=", input$state_filter) else ""))
    } else {
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

      if (use_census_api) {
        last_sql(paste0(
          "Census API request: https://api.census.gov/data/",
          year,
          "/acs/acs5?get=NAME,B25064_001E,B19013_001E&for=county:*&key=",
          census_api_key
        ))
      } else {
        last_sql("Local ACS dataset: acs_county.json (2022)")
      }
    }

    df
  })

  last_sql <- reactiveVal("")

  # Show the live SQL/API query in the sidebar
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

  comparison_data <- reactive({
    if (db_available) {
      df <- fetch_comparison_data_db(as.integer(input$year_filter), input$state_filter)
    } else {
      acs_for_join <- county_data() %>%
        left_join(state_lookup, by = "state") %>%
        filter(!is.na(state_abbr)) %>%
        group_by(state_abbr) %>%
        arrange(geoid, .by_group = TRUE) %>%
        mutate(county_sequence = row_number()) %>%
        ungroup()

      df <- acs_for_join %>%
        inner_join(job_data, by = c("state_abbr", "county_sequence")) %>%
        arrange(desc(rent_to_income), desc(avg_annual_pay))
    }

    req(nrow(df) > 0)
    df
  })

  selected_correlation <- reactive({
    df <- comparison_data()
    housing_metric <- input$housing_metric
    job_metric <- input$job_metric
    complete_rows <- complete.cases(df[[housing_metric]], df[[job_metric]])

    if (sum(complete_rows) < 2) {
      return(NA_real_)
    }

    cor(df[[housing_metric]][complete_rows],
        df[[job_metric]][complete_rows],
        use = "complete.obs")
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

  output$box_joined_counties <- renderUI({
    df <- comparison_data()
    div(class="mbox", tags$h5("Matched Counties"),
        tags$p(scales::comma(nrow(df))))
  })

  output$box_metric_correlation <- renderUI({
    cor_value <- selected_correlation()
    col <- if (is.na(cor_value)) "#888" else if (cor_value >= 0) "#2166ac" else "#d73027"
    label <- if (is.na(cor_value)) "n/a" else round(cor_value, 2)

    div(class="mbox", style=paste0("border-left-color:", col, ";"),
        tags$h5("Correlation"),
        tags$p(style=paste0("color:", col, ";"), label))
  })

  output$box_avg_job_metric <- renderUI({
    df <- comparison_data()
    job_metric <- input$job_metric
    job_label <- metric_label(job_metric, job_metric_choices)
    avg_value <- mean(df[[job_metric]], na.rm = TRUE)

    div(class="mbox", tags$h5(paste("Avg", job_label)),
        tags$p(format_metric_values(avg_value, job_metric)))
  })

  output$box_job_year <- renderUI({
    df <- comparison_data()
    div(class="mbox", tags$h5("Job Data Year"),
        tags$p(paste(unique(df$job_year), collapse = ", ")))
  })

  output$job_scatter <- renderPlotly({
    df <- comparison_data()
    housing_metric <- input$housing_metric
    job_metric <- input$job_metric
    housing_label <- metric_label(housing_metric, housing_metric_choices)
    job_label <- metric_label(job_metric, job_metric_choices)

    df$housing_value <- df[[housing_metric]]
    df$job_value <- df[[job_metric]]
    df$tip <- paste0(
      "<b>", df$county, ", ", df$state, "</b><br>",
      "ACS ", housing_label, ": ",
      format_metric_values(df$housing_value, housing_metric), "<br>",
      "2023 ", job_label, ": ",
      format_metric_values(df$job_value, job_metric), "<br>",
      "Rent burden: ", df$rent_to_income, "%"
    )

    p <- ggplot(df, aes(x = housing_value, y = job_value,
                        color = rent_to_income, text = tip)) +
      geom_point(alpha = 0.55, size = 1.4) +
      geom_smooth(method = "lm", se = FALSE, color = "gray35",
                  linetype = "dashed", linewidth = 0.6) +
      scale_color_gradient2(low="#2166ac", mid="#ffffbf", high="#d73027",
                            midpoint=30, name="Burden %") +
      scale_x_continuous(labels = axis_label_for(housing_metric)) +
      scale_y_continuous(labels = axis_label_for(job_metric)) +
      labs(title = paste("ACS", housing_label, "vs. 2023", job_label),
           x = paste("ACS", housing_label, input$year_filter),
           y = paste("2023", job_label)) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation="v", x=1.02, y=0.5))
  })

  output$job_corr_bar <- renderPlotly({
    df <- comparison_data()
    housing_metric <- input$housing_metric
    housing_label <- metric_label(housing_metric, housing_metric_choices)

    corr_df <- data.frame(
      metric = names(job_metric_choices),
      variable = unname(job_metric_choices),
      stringsAsFactors = FALSE
    )

    corr_df$corr <- vapply(corr_df$variable, function(metric) {
      complete_rows <- complete.cases(df[[housing_metric]], df[[metric]])
      if (sum(complete_rows) < 2) {
        return(NA_real_)
      }
      cor(df[[housing_metric]][complete_rows],
          df[[metric]][complete_rows],
          use = "complete.obs")
    }, numeric(1))

    corr_df <- corr_df %>%
      filter(!is.na(corr))

    req(nrow(corr_df) > 0)

    corr_df$tip <- paste0(
      "<b>", corr_df$metric, "</b><br>",
      "Correlation with ", housing_label, ": ",
      round(corr_df$corr, 3)
    )

    p <- ggplot(corr_df, aes(x = reorder(metric, corr),
                             y = corr, fill = corr, text = tip)) +
      geom_col(width = 0.72) +
      geom_hline(yintercept = 0, color = "gray45", linewidth = 0.5) +
      coord_flip() +
      scale_fill_gradient2(low="#d73027", mid="#f7f7f7", high="#2166ac",
                           midpoint=0, limits=c(-1, 1), guide="none") +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = paste("Job-Market Correlations with", housing_label),
           x = NULL, y = "Pearson correlation") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  output$job_tbl <- renderDT({
    df <- comparison_data()
    req(nrow(df) > 0)

    table_df <- df %>%
      transmute(
        County = paste0(county, ", ", state),
        `ACS Year` = as.integer(input$year_filter),
        `Monthly Rent` = median_rent,
        `ACS Household Income` = median_income,
        `Rent Burden (%)` = rent_to_income,
        `2023 Avg Annual Pay` = avg_annual_pay,
        `2023 Avg Weekly Wage` = avg_weekly_wage,
        `2023 Unemployment (%)` = unemployment_rate,
        `2023 Employment` = total_employment,
        `2023 Establishments` = num_establishments,
        `2023 Location Quotient` = location_quotient_employment
      )

    datatable(
      table_df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, scrollX = TRUE,
                     order = list(list(4, "desc")))
    ) %>%
      formatCurrency(
        c("Monthly Rent", "ACS Household Income",
          "2023 Avg Annual Pay", "2023 Avg Weekly Wage"),
        currency = "$", digits = 0
      ) %>%
      formatRound(c("Rent Burden (%)", "2023 Unemployment (%)"), digits = 1) %>%
      formatRound("2023 Location Quotient", digits = 2) %>%
      formatRound(c("2023 Employment", "2023 Establishments"), digits = 0) %>%
      formatStyle(
        "Rent Burden (%)",
        background = styleColorBar(c(0, 80), "#f8d7da"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
}

shinyApp(ui, server)
