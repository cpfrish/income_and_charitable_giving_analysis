library(tidyverse)
library(moments)

county <- read_csv("interim_irs_county.csv")

summary(county$total_returns)
skewness(county$total_returns) #12.56
#Data is significantly right skewed


# --- Define Group Parameters ---

# 1. Define the lower bound threshold of number of returns
#    Choosing 1000 here as an example
lower_bound_threshold <- 1000

# 2. Define the quartile breakpoints based on your summary statistics
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#      2    4534   11339   49301   30542 4566771
q1_breakpoint <- 4534
median_breakpoint <- 11339
q3_breakpoint <- 30542




# --- Data Processing ---

# Create a new data frame with the filter and the new group column
county_data_grouped <- county %>%
  
  # Step 1: Apply the lower bound filter first
  filter(total_returns >= lower_bound_threshold) %>%
  
  # Step 2: Create new grouping column
  mutate(
    return_group = case_when(
      # Group 1: Smallest group (between lower bound and Q1)
      total_returns >= lower_bound_threshold &
        total_returns < q1_breakpoint ~ paste0("Smallest 25% (<", q1_breakpoint, ")"),
      
      # Group 2: Between Q1 and Median
      total_returns >= q1_breakpoint &
        total_returns < median_breakpoint ~ paste0("25%-50% (", q1_breakpoint, "-", median_breakpoint, ")"),
      
      # Group 3: Between Median and Q3
      total_returns >= median_breakpoint &
        total_returns < q3_breakpoint ~ paste0("50%-75% (", median_breakpoint, "-", q3_breakpoint, ")"),
      
      # Group 4: Largest group (Q3 and above)
      total_returns >= q3_breakpoint ~ paste0("Largest 25% (>=", q3_breakpoint, ")"),
      
      # Group X: failsafe for any unexpected cases (should ideally not be triggered)
      TRUE ~ "Error/Check Data"
      
    )
  ) %>%
  
  # Step 3 Convert the new group column to an ordered factor
  # This helps to plot or model based on these groups, ensuring correct order.
  mutate(return_group = factor(
    return_group,
    levels = c(
      paste0("Smallest 25% (<", q1_breakpoint, ")"),
      paste0("25%-50% (", q1_breakpoint, "-", median_breakpoint, ")"),
      paste0("50%-75% (", median_breakpoint, "-", q3_breakpoint, ")"),
      paste0("Largest 25% (>=", q3_breakpoint, ")"),
      "Error/Check Data" # Include error level if it exists
    ),
    ordered = TRUE
  ))


# --- Verification ---

# View the first few rows of the new data frame with the group column
print(head(county_data_grouped))

# Check the number of counties in each group
print(table(county_data_grouped$return_group))

# Check the range of 'total_returns' within each group to ensure it worked correctly
county_data_grouped %>%
  group_by(return_group) %>%
  summarise(
    count = n(),
    min_returns = min(total_returns),
    max_returns = max(total_returns)
  ) %>%
  print()

#write new csv with groups
write_csv(county_data_grouped, "interim_county_irs_grouped.csv")