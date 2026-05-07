library(jsonlite)
library(DBI)
library(RPostgres)
library(dplyr)

# Parse Census JSON (array of arrays)
raw <- fromJSON("C:/Users/conno/OneDrive/Desktop/db final project/acs_county.json")
df  <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
names(df) <- c("county_name", "median_rent", "median_income", "state_fips", "county_fips")

df <- df %>%
  mutate(
    median_rent   = as.numeric(median_rent),
    median_income = as.numeric(median_income),
    geoid         = paste0(state_fips, county_fips),
    county        = sub(",.*", "", county_name),
    state         = trimws(sub(".*,", "", county_name))
  ) %>%
  filter(median_rent > 0, median_income > 0) %>%
  mutate(
    annual_rent    = median_rent * 12,
    rent_to_income = round((annual_rent / median_income) * 100, 1)
  ) %>%
  select(geoid, county, state, median_rent, median_income, annual_rent, rent_to_income)

cat("Rows to insert:", nrow(df), "\n")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "university",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

dbExecute(con, "DROP TABLE IF EXISTS cost_of_living")
dbExecute(con, "
  CREATE TABLE cost_of_living (
    geoid          VARCHAR(5)   PRIMARY KEY,
    county         VARCHAR(100) NOT NULL,
    state          VARCHAR(50)  NOT NULL,
    median_rent    INTEGER      NOT NULL,
    median_income  INTEGER      NOT NULL,
    annual_rent    INTEGER      NOT NULL,
    rent_to_income NUMERIC(5,1) NOT NULL
  )
")

dbWriteTable(con, "cost_of_living", df, append = TRUE, row.names = FALSE)
n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM cost_of_living")$n
cat("Inserted", n, "rows into cost_of_living\n")

dbDisconnect(con)
