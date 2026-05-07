# ============================================================
#  US County-Level Rent vs. Income — Cost of Living Explorer
#  Database: PostgreSQL (university db, cost_of_living table)
#  Source:   2022 ACS 5-Year Estimates (Census Bureau API)
#  All data queries use SQL via DBI / RPostgres
# ============================================================

library(shiny)
library(DBI)
library(RPostgres)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(DT)

# ------------------------------------------------------------------
# DB connection (PostgreSQL — university database)
# ------------------------------------------------------------------
db_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = "university",
    host     = "localhost",
    port     = 5432,
    user     = "postgres",
    password = "postgres"
  )
}

# Pull distinct state list for the filter dropdown (SQL query)
con  <- db_con()
all_states <- dbGetQuery(con, "SELECT DISTINCT state FROM cost_of_living ORDER BY state")$state
dbDisconnect(con)

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
      "3,212 counties | 2022 ACS 5-Year Estimates | PostgreSQL backend")
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("state_filter", "Filter by State:",
                  choices  = c("All States", all_states),
                  selected = "All States"),
      sliderInput("top_n", "Top N burdened counties:",
                  min = 5, max = 40, value = 15, step = 5),
      hr(),
      h5(style = "font-weight:bold; margin-bottom:4px;", "Last SQL Query"),
      uiOutput("sql_display"),
      hr(),
      tags$p(style = "font-size:11px; color:#aaa;",
        "Data: US Census ACS 2022 (B25064, B19013).",
        tags$br(),
        "Stored in PostgreSQL — all charts query live from DB.")
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

  # Reactive: run SQL query against PostgreSQL based on state filter
  county_data <- reactive({
    con <- db_con()
    on.exit(dbDisconnect(con))

    if (input$state_filter == "All States") {
      sql <- "
        SELECT geoid, county, state,
               median_rent, median_income,
               annual_rent, rent_to_income
        FROM cost_of_living
        ORDER BY rent_to_income DESC"
    } else {
      sql <- paste0("
        SELECT geoid, county, state,
               median_rent, median_income,
               annual_rent, rent_to_income
        FROM cost_of_living
        WHERE state = '", input$state_filter, "'
        ORDER BY rent_to_income DESC")
    }

    last_sql(sql)
    dbGetQuery(con, sql)
  })

  last_sql <- reactiveVal("")

  # Show the live SQL in the sidebar
  output$sql_display <- renderUI({
    div(class = "sql-box", last_sql())
  })

  # Summary stats via SQL aggregate query
  stats <- reactive({
    con <- db_con()
    on.exit(dbDisconnect(con))

    if (input$state_filter == "All States") {
      where <- ""
    } else {
      where <- paste0("WHERE state = '", input$state_filter, "'")
    }

    sql <- paste0("
      SELECT
        COUNT(*)                        AS n,
        ROUND(AVG(median_rent))         AS avg_rent,
        ROUND(AVG(median_income))       AS avg_income,
        ROUND(AVG(rent_to_income), 1)   AS avg_ratio,
        ROUND(100.0 * SUM(CASE WHEN rent_to_income >= 30 THEN 1 ELSE 0 END)
              / COUNT(*), 0)            AS pct_burdened
      FROM cost_of_living ", where)

    dbGetQuery(con, sql)
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

  # Bar: top N most burdened counties (SQL query with LIMIT)
  output$bar_top <- renderPlotly({
    con <- db_con()
    on.exit(dbDisconnect(con))

    n <- input$top_n
    where <- if (input$state_filter == "All States") ""
             else paste0("WHERE state = '", input$state_filter, "'")

    sql <- paste0("
      SELECT county || ', ' || state AS label,
             rent_to_income, median_rent, median_income
      FROM cost_of_living
      ", where, "
      ORDER BY rent_to_income DESC
      LIMIT ", n)

    df <- dbGetQuery(con, sql)
    req(nrow(df) > 0)

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
                        order = list(list(4, "desc")))
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
