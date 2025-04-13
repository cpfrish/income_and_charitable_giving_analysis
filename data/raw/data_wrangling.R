library(tidyverse)

#Load IRS 2022 ZipCode level Statistics of Income data set
irs <- read_csv("irs_no_agi_raw.csv")
#Load HUD Crosswalk file which maps Zips -> Counties including allocation weights
hud <- readxl::read_xlsx("zip_county_122022.xlsx")

#filter out US territories in HUD
hud <- hud |>
  filter(!USPS_ZIP_PREF_STATE %in% c("AS", "GU", "MP", "PR", "VI"))

#rename to join with irs
hud <- hud |>
  rename(ZIPCODE = ZIP, county_fips = COUNTY) |>
  rename_all(tolower) |>
  mutate(state_fips_hud = substr(county_fips, 1, 2))


#check how many Zips span multiple counties (require allocation)
hud |>
  group_by(zipcode) |>
  summarise(county_count = n_distinct(county_fips)) |>
  filter(county_count > 1) |>
  arrange(desc(county_count))

#looks like 11K one to many zip -> county allocations we need to perform

#clean up irs keeping target variables
irs <- irs |>
  mutate(
    zipcode = ZIPCODE,
    state_fips_irs = STATEFIPS,
    total_charitable_contb = A19700,
    agi = A00100,
    num_of_returns = N1,
    net_investment_income_tax = A85300,
    net_cap_gains_less_loss = A01000,
    num_returns_net_cap_gains_less_loss = N01000,
    num_returns_rent_and_royalty = N25870,
    total_rent_and_royalty = A25870,
    ord_div_amount = A00600,
    num_returns_ord_dividens = N00600,
    state = STATE,
    total_tax_payments_amount = A10600,
    salaries_and_wages_amnt = A00200,
    num_salaries_and_wages = N00200,
    zip_region = substr(ZIPCODE, 1, 3),
    .keep = "none"
  )  |>
  filter(!zipcode %in% c('00000', '99999')) 


#join
county_level_irs_data <- left_join(irs, hud, by = "zipcode")

# check for NAs
county_level_irs_data |> filter(is.na(county_fips)) #none that is good

#check allocation again, slightly less at 10.6K
county_level_irs_data |>
  group_by(zipcode) |>
  summarise(county_count = n_distinct(county_fips)) |>
  filter(county_count > 1) |>
  arrange(desc(county_count))

#sense check comparing state_fips between each data source

county_level_irs_data <- county_level_irs_data |>
  mutate(
    state_fips_match_strict = case_when(
      # Both NA -> considered a match here
      is.na(state_fips_irs) & is.na(state_fips_hud) ~ TRUE,
      # One NA, one not -> Mismatch
      is.na(state_fips_irs) | is.na(state_fips_hud) ~ FALSE,
      # Both have values and are equal -> Match
      state_fips_irs == state_fips_hud            ~ TRUE,
      # Both have values and are unequal -> Mismatch
      TRUE                                        ~ FALSE
    )
  )

# Count the explicit matches and mismatches
print(county_level_irs_data |> count(state_fips_match_strict))

# Count NAs
print(county_level_irs_data |> count(is.na(state_fips_irs), is.na(state_fips_hud)))

# View only the rows definitively flagged as mismatches
print(
  county_level_irs_data |>
    filter(!state_fips_match_strict) |>
    select(zipcode, county_fips, state_fips_irs, state_fips_hud)
)

#Looks like an error in the HUD crosswalk file where ZIP 20736 is being
#attributed to Yuma County AZ but should be in MD. The other mismatch is expected
#based on methodology of HUD Crosswalk 

# filtering out that row before allocation and aggregation
combined_irs_hud_filtered <- county_level_irs_data |>
  filter(!(zipcode == "20736" & county_fips == "04025"))

#Select rows we are interested in, and reorganize chr vars to front of DF

combined_irs_hud_filtered <- combined_irs_hud_filtered |>
  select(!state_fips_match_strict)

combined_irs_hud_filtered <- combined_irs_hud_filtered |>
  relocate(where(is.character), .before = 1)

#save this verison before allocation
write.csv(combined_irs_hud_filtered, "interim_irs_hud_combined.csv")

## Step 2: Now we make use of the allocation ratio columns (specifically RES_RATIO)

allocated_data <- combined_irs_hud_filtered |>
  mutate(# Multiply each IRS variable by the Residential allocation ratio
    across(
      c(
        agi,
        num_of_returns,
        total_charitable_contb,
        net_investment_income_tax,
        net_cap_gains_less_loss,
        num_returns_net_cap_gains_less_loss,
        num_returns_rent_and_royalty,
        total_rent_and_royalty,
        ord_div_amount,
        num_returns_ord_dividens,
        total_tax_payments_amount,
        salaries_and_wages_amnt,
        num_salaries_and_wages
      ),
      ~ . * res_ratio,
      .names = "{.col}_allocated"
    ))

#Now we aggregate by county level


county_level_irs_data <- allocated_data |>
  # Group by the unique county identifier
  group_by(county_fips) |>
  # Summarise by summing the allocated values for each county
  summarise(
    # Count and Main Predictor
    total_returns = sum(num_of_returns_allocated, na.rm = TRUE),
    total_agi_amount = sum(agi_allocated, na.rm = TRUE),
    
    # Salaries and Wages
    total_salaries_wages_amount = sum(salaries_and_wages_amnt_allocated, na.rm = TRUE),
    total_salaries_wages_returns = sum(num_salaries_and_wages_allocated, na.rm = TRUE),
    
    # Ordinary Dividends
    total_ordinary_dividends_amount = sum(ord_div_amount_allocated, na.rm = TRUE),
    total_ordinary_dividends_returns = sum(num_returns_ord_dividens_allocated, na.rm = TRUE),
    
    # Net Capital Gains less Loss
    total_net_capital_gains_amount = sum(net_cap_gains_less_loss_allocated, na.rm = TRUE),
    total_net_capital_gains_returns = sum(num_returns_net_cap_gains_less_loss_allocated, na.rm = TRUE),
    
    # (Outcome) Charitable Contributions
    total_charitable_contributions_amount = sum(total_charitable_contb_allocated, na.rm = TRUE),
    
    # Net Investment Income Tax
    total_net_investment_income_tax_amount = sum(net_investment_income_tax_allocated, na.rm = TRUE),
    
    # Rent and Royalty Income
    total_rent_royalty_amount = sum(total_rent_and_royalty_allocated, na.rm = TRUE),
    total_rent_royalty_returns = sum(num_returns_rent_and_royalty_allocated, na.rm = TRUE),
    
    # Total Tax Payments
    total_tax_payments_amount = sum(total_tax_payments_amount_allocated, na.rm = TRUE),
    
    # Keep the state FIPS (should be unique per county_fips)
    state_fips = first(state_fips_hud),
    # Use the state FIPS from the HUD data context
    
    .groups = 'drop' # Ungroup after summarising
  ) 


#Proportion Normalization
county_level_irs_data <- county_level_irs_data |>
  mutate(
    #Total return normalizations
    mean_agi_per_return = ifelse(total_returns > 0, total_agi_amount / total_returns, 0),
    mean_charitable_contributation_per_return = ifelse(
      total_returns > 0,
      total_charitable_contributions_amount / total_returns,
      0
    ),
    mean_tax_payment_per_return = ifelse(total_returns > 0, total_tax_payments_amount / total_returns, 0),
    
    #Specific return normalizations
    mean_salary_per_salary_return = ifelse(
      total_salaries_wages_returns > 0,
      total_salaries_wages_amount / total_salaries_wages_returns,
      0
    ),
    
    
    mean_dividend_per_dividend_return = ifelse(
      total_ordinary_dividends_returns > 0,
      total_ordinary_dividends_amount / total_ordinary_dividends_returns,
      0
    ),
    
    
    mean_capgain_per_capgain_return = ifelse(
      total_net_capital_gains_returns > 0,
      total_net_capital_gains_amount / total_net_capital_gains_returns,
      0
    ),
    
    
    mean_rent_royalty_per_rent_royalty_return = ifelse(
      total_rent_royalty_returns > 0,
      total_rent_royalty_amount / total_rent_royalty_returns,
      0
    )
  )
         
#Check the new data set
summary(county_level_irs_data)

# type and sample of obs
str(county_level_irs_data)

#similar to str
glimpse(county_level_irs_data)


#sample of dataframe
head(county_level_irs_data)



## Money amounts in thousands of dollars ##


write_csv(county_level_irs_data, "interim_irs_county.csv")
