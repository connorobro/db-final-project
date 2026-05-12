library(jsonlite)
library(DBI)
library(RPostgres)
library(dplyr)

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
    stop("No Census API key found and no local data available for year ", year)
  }

  df <- df %>%
    mutate(
      state_fips     = state,
      county_fips    = county,
      median_rent    = as.integer(B25064_001E),
      median_income  = as.integer(B19013_001E),
      geoid          = paste0(state_fips, county_fips),
      county         = sub(" County$", "", sub(",.*", "", NAME)),
      state          = sub(".*, ", "", NAME),
      annual_rent    = median_rent * 12,
      rent_to_income = round((annual_rent / median_income) * 100, 1),
      state_abbr     = state_lookup$state_abbr[match(state, state_lookup$state)],
      county_sequence = as.integer(substr(geoid, 3, 5))
    ) %>%
    filter(median_rent > 0, median_income > 0, !is.na(state_abbr)) %>%
    mutate(year = year) %>%
    select(year, geoid, state_fips, county_fips, county, state, state_abbr,
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

years <- if (use_census_api) 2020:2023 else 2022
acs_data <- bind_rows(lapply(years, function(year) {
  message("Fetching ACS data for year ", year)
  fetch_acs_county_data(year)
}))

cat("Rows in acs_data:", nrow(acs_data), "\n")

job_data <- load_jobmarket_data()

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "university",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# Create the ACS table with a composite primary key for year + geoid.
# Additional fields are included for filtering and SQL joins.
dbExecute(con, "DROP TABLE IF EXISTS cost_of_living")
dbExecute(con, "
  CREATE TABLE cost_of_living (
    year            INTEGER      NOT NULL,
    geoid           VARCHAR(5)   NOT NULL,
    state_fips      VARCHAR(2)   NOT NULL,
    county_fips     VARCHAR(3)   NOT NULL,
    county          VARCHAR(100) NOT NULL,
    state           VARCHAR(50)  NOT NULL,
    state_abbr      VARCHAR(2)   NOT NULL,
    county_sequence INTEGER      NOT NULL,
    median_rent     INTEGER      NOT NULL,
    median_income   INTEGER      NOT NULL,
    annual_rent     NUMERIC       NOT NULL,
    rent_to_income  NUMERIC(5,1) NOT NULL,
    PRIMARY KEY (year, geoid)
  )
")

dbWriteTable(con, "cost_of_living", acs_data, append = TRUE, row.names = FALSE)

# Create a job-market table for faster joins and future filtering.
dbExecute(con, "DROP TABLE IF EXISTS job_market")
dbExecute(con, "
  CREATE TABLE job_market (
    job_geoid                    VARCHAR(5)   PRIMARY KEY,
    county_sequence              INTEGER      NOT NULL,
    job_county_name              VARCHAR(150),
    state_abbr                   VARCHAR(2)   NOT NULL,
    job_year                     INTEGER      NOT NULL,
    num_establishments           INTEGER,
    total_employment             INTEGER,
    total_wages                  BIGINT,
    taxable_wages                BIGINT,
    avg_weekly_wage              NUMERIC,
    avg_annual_pay               NUMERIC,
    employment_yoy_pct_change    NUMERIC,
    wage_yoy_pct_change          NUMERIC,
    location_quotient_employment NUMERIC,
    unemployment_rate            NUMERIC,
    job_median_household_income  NUMERIC
  )
")

dbWriteTable(con, "job_market", job_data, append = TRUE, row.names = FALSE)

dbExecute(con, "CREATE INDEX idx_cost_of_living_year_state ON cost_of_living(year, state)")
dbExecute(con, "CREATE INDEX idx_cost_of_living_year_abbr_sequence ON cost_of_living(year, state_abbr, county_sequence)")
dbExecute(con, "CREATE INDEX idx_job_market_state_county ON job_market(state_abbr, county_sequence)")
dbExecute(con, "CREATE INDEX idx_job_market_year ON job_market(job_year)")

n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM cost_of_living")$n
cat("Inserted", n, "rows into cost_of_living\n")

n_job <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM job_market")$n
cat("Inserted", n_job, "rows into job_market\n")

dbDisconnect(con)
