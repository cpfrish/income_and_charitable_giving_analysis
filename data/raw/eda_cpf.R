library(tidyverse)
library(janitor)
#library(tigris)
#library(sf)

df <- read_csv("irs_no_agi_raw.csv")

df |> 
  distinct(STATEFIPS)

#Model: AGI 

#Money amounts in thousands of dollars
#num return etc. are not

subset <- df |> 
  mutate(
    zipcode = ZIPCODE,
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
    zip_region = substr(ZIPCODE, 1,3),
    .keep = "none"
  )  |> 
  filter(!zipcode %in% c('00000','99999')) |> 
  mutate(agi_per_return = agi/num_of_returns)
  

cols_to_factor <- c("zipcode", "state", "zip_region")

subset <- subset |> 
  mutate(across(all_of(cols_to_factor), as.factor))



glimpse(subset)
str(subset)
summary(subset)



subset |> 
  summarize(avg_n_returns = mean(num_of_returns)) # 5579

subset |> 
  ggplot(aes(x = num_of_returns)) +
  geom_histogram(bins = 100)

subset |>
  select(num_of_returns) |> 
  slice_max(num_of_returns, n = 10)

subset |> 
  select(num_of_returns) |> 
  slice_min(num_of_returns, n = 10)






