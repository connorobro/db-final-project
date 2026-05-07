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

fetch_acs_county_data <- function(year) {
  url <- paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/acs5?get=NAME,B25064_001E,B19013_001E&for=county:*"
  )

  raw <- fromJSON(url)
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
        "Fetched live from Census API — all charts query live from API.")
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

    last_sql(paste0(
      "Census API request: https://api.census.gov/data/",
      year,
      "/acs/acs5?get=NAME,B25064_001E,B19013_001E&for=county:*"
    ))

    df
  })

  last_sql <- reactiveVal("")

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
}

shinyApp(ui, server)
