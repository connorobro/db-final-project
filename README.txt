county_wages_jobmarket_2023.csv
================================
Source structure mirrors: BLS Quarterly Census of Employment & Wages (QCEW)
                          + USDA ERS County-Level Data Sets
Year: 2023 | Rows: 3,143 (one per U.S. county) | Columns: 15

COLUMN REFERENCE
----------------
county_fips                   5-digit FIPS code — standard join key for housing/census datasets
county_name                   County name + state abbreviation
state_abbr                    2-letter state code
year                          Reference year (2023)
num_establishments            Number of covered business establishments (annual avg)
total_employment              Total covered employment — annual average headcount
total_wages                   Total annual wages paid to all employees ($)
taxable_wages                 Taxable portion of annual wages ($)
avg_weekly_wage               Average weekly wage per employee ($)
avg_annual_pay                Average annual pay per employee ($)
employment_yoy_pct_change     Year-over-year employment % change
wage_yoy_pct_change           Year-over-year avg annual pay % change
location_quotient_employment  Employment LQ vs. U.S. national avg (>1 = above-avg concentration)
unemployment_rate             County unemployment rate (%)
median_household_income       Median household income ($)

JOINING TO HOUSING DATA
-----------------------
Use county_fips (zero-padded to 5 chars) to join with:
  - Zillow Research Data  (county_fips field)
  - Census ACS housing tables (GEOID field, first 5 chars)
  - FHFA House Price Index by county (fips field)
  - Redfin market data (region_id mapped via FIPS)

MODELLING NOTES
---------------
- Lag wages/employment by 1 year when predicting future prices
- Log-transform: total_employment, total_wages, num_establishments, median_household_income
- employment_yoy_pct_change and wage_yoy_pct_change are strong leading indicators
- location_quotient > 1.5 typically signals economically specialised (boom/bust risk) counties
