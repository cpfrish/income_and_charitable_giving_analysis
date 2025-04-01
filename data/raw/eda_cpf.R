library(tidyverse)
library(janitor)

df <- read_csv("irs_agi_raw.csv")

df |> 
  distinct(STATEFIPS)

subset <- df |> 
  mutate(
    agi = A00100,
    net_cap_gains_less_loss = A01000,
    rent_and_royalty = A25870,
    total_charitable_contb = A19700,
  ) |> 
  select(
    STATE, zipcode, agi_stub, agi, net_cap_gains_less_loss, rent_and_royalty, 
    total_charitable_contb
  )

subset <- subset |> 
  rename(state = STATE)

subset <- subset |> 
  filter(!zipcode %in% c('00000','99999')) 


glimpse(subset)
str(subset)
summary(subset)

subset |> 
count(zipcode, sort = TRUE) |> 
  filter(!n == 6)
