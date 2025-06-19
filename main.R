library(tidyverse)
library(boot)

cast_to_num <- function(s) {
  ans <- ifelse(s=='Yes', 1, ifelse(s=='No', 0, ifelse(s=='--', NA, s)))
  return(ans)
}

standardize_num <- function(x) {
  ans <- ifelse(x==-1, 0, x)
  return(ans)
}

d <- read_csv('ECTEL2025 Transcript Scores GPT4, GPT-4o and GPT-turbo - ECTEL25-Simplified-Scores -- FIXED FINAL FINAL.csv') %>%
  mutate_if(is.character, cast_to_num) %>%
  select(-`Transcript File`) %>%
  mutate_all(as.numeric) %>%
  mutate_all(standardize_num)

# d_tmp <- read_csv('ECTEL2025 Transcript Scores GPT4, GPT-4o and GPT-turbo - ECTEL25-Simplified-Scores -- FINAL - V2.csv') %>%
#   mutate_if(is.character, cast_to_num)
# 
# d_tmp %>%
#   filter((`praise_gpt4-eval`==1) & (`praise_learnlm-eval`==0)) %>%
#   pull(`Transcript File`)
# 
# d_tmp %>%
#   filter((`praise_gpt4-eval`==0) & (`praise_learnlm-eval`==1)) %>%
#   pull(`Transcript File`)
# 
# d_tmp %>%
#   filter((`errors_gpt4-eval`==1) & (`errors_learnlm-eval`==0)) %>%
#   pull(`Transcript File`)
# 
# d_tmp %>%
#   filter((`errors_gpt4-eval`==0) & (`errors_learnlm-eval`==1)) %>%
#   pull(`Transcript File`)

score_model <- function(human_present, human_eval, model_present, model_eval) {
  case_when(
    human_present == 1 & model_present == 1 & (human_eval == model_eval) ~ 2,
    human_present == 1 & model_present == 1 & (human_eval != model_eval) ~ 1,
    human_present == model_present ~ 2, # Both disagree that the relevant move is present
    TRUE ~ 0
  )
}

# Recognize
score_model <- function(human_present, human_eval, model_present, model_eval) {
  case_when(
    human_present == model_present ~ 1,
    TRUE ~ 0
  )
}

add_model_scores <- function(df) {
  df %>%
    mutate(
      score_gpt4 = score_model(`human-present`, `human-eval`, `gpt4-present`, `gpt4-eval`),
      score_gpt4o = score_model(`human-present`, `human-eval`, `gpt4o-present`, `gpt4o-eval`),
      score_gpt4turbo = score_model(`human-present`, `human-eval`, `gpt4turbo-present`, `gpt4turbo-eval`),
      score_gemini = score_model(`human-present`, `human-eval`, `gemini-present`, `gemini-eval`),
      score_learnlm = score_model(`human-present`, `human-eval`, `learnlm-present`, `learnlm-eval`)
    ) %>%
    summarise(
      total_score_gpt4 = sum(score_gpt4, na.rm = TRUE)/.5, # Recognition
      total_score_gpt4o = sum(score_gpt4o, na.rm = TRUE)/.5,
      total_score_gpt4turbo = sum(score_gpt4turbo, na.rm = TRUE)/.5,
      total_score_gemini = sum(score_gemini, na.rm = TRUE)/.5,
      total_score_learnlm = sum(score_learnlm, na.rm = TRUE)/.5
    )
}

## Praise

# bootstrap_scores <- function(data, indices) {
#   sampled_data <- data[indices, ]
#   result <- sampled_data %>%
#     select(matches('praise')) %>%
#     rename_with(~ gsub('^praise_', '', .x)) %>%
#     add_model_scores()
#   as.numeric(result)
# }
# 
# # Run bootstrap
# set.seed(123)
# boot_results <- boot(
#   data = d,
#   statistic = bootstrap_scores,
#   R = 10000
# )
# 
# # Get 95% confidence intervals
# boot_results
# boot.ci(boot_results, type = "perc", index = 1)  # gpt4
# boot.ci(boot_results, type = "perc", index = 2)  # gpt4o
# boot.ci(boot_results, type = "perc", index = 3)  # gpt4turbo
# boot.ci(boot_results, type = "perc", index = 4)  # gemini
# boot.ci(boot_results, type = "perc", index = 5)  # learnlm

## Errors

bootstrap_scores <- function(data, indices) {
  sampled_data <- data[indices, ]
  result <- sampled_data %>%
    select(matches('errors')) %>%
    rename_with(~ gsub('^errors_', '', .x)) %>%
    add_model_scores()
  as.numeric(result)
}

# Run bootstrap
set.seed(123)
boot_results <- boot(
  data = d,
  statistic = bootstrap_scores,
  R = 10000
)

# Get 95% confidence intervals
boot_results
boot.ci(boot_results, type = "perc", index = 1)  # gpt4
boot.ci(boot_results, type = "perc", index = 2)  # gpt4o
boot.ci(boot_results, type = "perc", index = 3)  # gpt4turbo
boot.ci(boot_results, type = "perc", index = 4)  # gemini
boot.ci(boot_results, type = "perc", index = 5)  # learnlm

# Function to simulate random predictions and evaluate using the same score_model
simulate_chance_scores <- function(data, indices) {
  sampled_data <- data[indices, ] %>%
    select(`human-present`, `human-eval`) %>%
    mutate(
      chance_present = sample(c(0, 1), size = n(), replace = TRUE),
      chance_eval = ifelse(chance_present == 1, sample(c(0, 1), size = n(), replace = TRUE), NA)
    ) %>%
    mutate(
      score_chance = score_model(`human-present`, `human-eval`, chance_present, chance_eval)
    )
  
  sum(sampled_data$score_chance, na.rm = TRUE)
}

set.seed(123)
boot_chance_praise <- boot(
  data = d %>% select(matches('praise_')) %>%
    rename_with(~ gsub('^praise_', '', .x)),
  statistic = simulate_chance_scores,
  R = 10000
)

boot_chance_praise
boot.ci(boot_chance_praise, type = "perc")

set.seed(123)
boot_chance_errors <- boot(
  data = d %>% select(matches('errors_')) %>%
    rename_with(~ gsub('^errors_', '', .x)),
  statistic = simulate_chance_scores,
  R = 10000
)

boot_chance_errors
boot.ci(boot_chance_errors, type = "perc")

