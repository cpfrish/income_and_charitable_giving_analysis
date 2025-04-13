

df <- read.csv("../interim/interim_irs_county.csv")


# Set a seed for reproducibility
set.seed(123)

# Determine the number of rows
n_rows <- nrow(df)
# Calculate the size of the exploration and confirmation sets (30%/70%)
exploration_size <- floor(0.3 * n_rows)
confirmation_size <- n_rows - exploration_size


# Randomly sample indices for the exploration set
exploration_indices <- sample(1:n_rows, size = exploration_size, replace = FALSE)

# Create the exploration and confirmation data frames
exploration_df <- df[exploration_indices, ]
confirmation_df <- df[-exploration_indices, ]


