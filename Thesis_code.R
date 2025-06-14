install.packages("jsonlite")
install.packages("geosphere")
install.packages("dplyr")
install.packages("readr")
install.packages("lubridate")
install.packages("tidytext")
install.packages("textdata")
install.packages("ggplot2")
install.packages("syuzhet")
install.packages("tidyr")
install.packages("fixest")
install.packages("broom")
install.packages("purrr")
install.packages("stringr")
install.packages("kableExtra")

library(jsonlite)
library(geosphere)
library(dplyr)
library(readr)
library(lubridate)
library(tidytext)
library(textdata)
library(ggplot2)
library(tidyr)
library(fixest)
library(broom)
library(purrr)
library(stringr)
library(knitr)
library(kableExtra)

# Read JSON dataset
#con <- file("yelp_academic_dataset_business.json", "r")
#full_business <- stream_in(con)
#con_review <- file("yelp_academic_dataset_review.json", "r")
#full_review <- stream_in(con_review)

# Check the most appeared state
state_counts <- table(full_business$state)
sorted_states <- sort(state_counts, decreasing = TRUE)
top_states <- head(sorted_states, 5)

# Plot the state counts
barplot(top_states,
        main = "Top 5 Most Appeared States",
        xlab = "State",
        ylab = "Count",
        col = "skyblue",
        border = "white",
        las = 1)


# Divide business groups depending on the distance 
# from the Nashville Waffle House shooting on 2018-04-22
shooting_incident <- c(-86.6393, 36.0522)
full_business$distance_km <- mapply(function(lat, lon) {
  distHaversine(c(lon, lat), shooting_incident) / 1000
}, full_business$latitude, full_business$longitude) 
#head(full_business$distance_km)

# Insert levels of distance for the business dataset
business_between_2_50km <- full_business %>% 
  filter(distance_km >= 2, distance_km <= 50)

# Simplify the variables within the business dataset
business_between_2_50km <- business_between_2_50km %>%
  select(business_id, name, address, city, state, postal_code, latitude, 
         longitude, stars, review_count, is_open, distance_km)

# All dividing into half months to rule out the possibility of insufficient sample size and have more robust results,
# where always only 1 half month in the time period after shooting incidence


# First method: 2 half months in "before"
# Insert review periods for the review dataset
filtered_reviews_between_2_50km_hm <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km_hm, "filtered_reviews_between_2_50km_hm.csv", row.names = FALSE)

filtered_reviews_between_2_50km_hm <- filtered_reviews_between_2_50km_hm %>%
  filter(date >= as_datetime("2018-03-22"),
         date <= as_datetime("2018-05-07"))

# Separate the review dataset into half months
filtered_reviews_between_2_50km_hm <- filtered_reviews_between_2_50km_hm %>%
  mutate(review_half_months = case_when(
    date >= as_datetime("2018-03-22") & date < as_datetime("2018-04-07") ~ "first half month",
    date >= as_datetime("2018-04-07") & date < as_datetime("2018-04-22") ~ "second half month",
    date >= as_datetime("2018-04-22") & date <= as_datetime("2018-05-07") ~ "third half month"
  ))

# Simplify the variables within the review dataset
filtered_reviews_between_2_50km_hm <- filtered_reviews_between_2_50km_hm %>%
  select(business_id, stars, text, date, review_half_months)

write.csv(filtered_reviews_between_2_50km_hm, "filtered_reviews_between_2_50km_hm.csv", row.names = FALSE)

# Add LIWC sentimental variables to the filtered reviews
LIWC_filtered_reviews_hm <- read.csv("LIWC-22 Results - filtered_reviews_between_2_50k___ - LIWC Analysis_hm.csv", stringsAsFactors = FALSE)

# Separate the review dataset into before and after periods
LIWC_filtered_reviews_hm <- LIWC_filtered_reviews_hm %>%
  mutate(review_period = case_when(
    review_half_months == "first half month" | review_half_months == "second half month" ~ "before",
    review_half_months == "third half month" ~ "after"
  ))

# Keep only businesses with both before and after reviews
eligible_ids_hm <- LIWC_filtered_reviews_hm %>%
  group_by(business_id) %>%
  filter(n_distinct(review_period) == 2) %>%
  pull(business_id) %>%
  unique()

LIWC_filtered_reviews_hm <- LIWC_filtered_reviews_hm %>%
  filter(business_id %in% eligible_ids_hm)

# Aggregate the sub-dimension LIWC variables into the main dimensions
LIWC_filtered_reviews_hm <- LIWC_filtered_reviews_hm %>%
  mutate(
    States = need + want + acquire + lack + fulfill + fatigue,
    Motive = reward + risk + curiosity + allure,
    Timeorientation = time + focuspast + focuspresent + focusfuture
  )

# Model with distance_km (continuous)
# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_hm <- LIWC_filtered_reviews_hm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")

write.csv(LIWC_filtered_reviews_hm, "LIWC_filtered_reviews_hm.csv", row.names = FALSE)

# Define LIWC variable names
liwc_vars <- c("WC", "Analytic", "Clout", "Authentic", "Tone", "AllPunc", "Drives", 
               "Cognition", "Affect", "Social", "Culture", "Lifestyle", "Physical", 
               "Perception", "States", "Motive", "Timeorientation", "stars")


liwc_means %>%
  arrange(desc(abs_diff)) %>%
  head(10)

# Summarize by business_id and review_period (before/after)
liwc_start_hm <- which(names(LIWC_filtered_reviews_hm) == "WC")
liwc_end_hm <- which(names(LIWC_filtered_reviews_hm) == "Timeorientation")
full_liwc_vars_hm <- names(LIWC_filtered_reviews_hm)[liwc_start_hm:liwc_end_hm]

aggregated_LIWC_reviews_hm <- LIWC_filtered_reviews_hm %>%
  group_by(business_id, review_period) %>%
  summarise(
    # average all the LIWC variables
    across(all_of(full_liwc_vars_hm), mean, na.rm = TRUE),
    # average the stars and add distance in km
    across(stars, mean, na.rm = TRUE),
    across(distance_km, mean, na.rm = TRUE),
    # choose the "half month" that appears the most for each business_id
    review_half_months = names(which.max(table(review_half_months))),
    # Combine the "text" as one paragraph for the before and after of each business_id
    combined_text = str_c(text, collapse = " "),
    .groups = "drop"
  )

# Make sure predictors are factors
# aggregated_LIWC_reviews$review_period <- as.factor(LIWC_filtered_reviews$distance_km)
# LIWC_filtered_reviews$distance_km <- as.numeric(LIWC_filtered_reviews$distance_km)

# Convert review_period into a dummy variable: after = 1, before = 0
aggregated_LIWC_reviews_hm <- aggregated_LIWC_reviews_hm %>%
  mutate(review_period_dummy = ifelse(review_period == "after", 1, 0))

# Replace all NAN with 0
aggregated_LIWC_reviews_hm <- aggregated_LIWC_reviews_hm %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%  
  mutate(across(everything(), ~ replace_na(., 0)))

# Reverse the Distance Variable
aggregated_LIWC_reviews_hm <- aggregated_LIWC_reviews_hm %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

write.csv(aggregated_LIWC_reviews_hm, "aggregated_LIWC_reviews_hm.csv", row.names = FALSE)

# Calculate before/after means and absolute difference
liwc_means_hm <- aggregated_LIWC_reviews_hm %>%
  group_by(review_period) %>%
  summarise(across(all_of(liwc_vars), mean, na.rm = TRUE)) %>%
  pivot_longer(-review_period, names_to = "LIWC_variable", values_to = "mean") %>%
  pivot_wider(names_from = review_period, values_from = mean) %>%
  mutate(abs_diff = abs(before - after)) %>%
  arrange(desc(abs_diff))

# Plot top 10 most changed variables
liwc_means_hm %>%
  slice_max(abs_diff, n=10) %>%
  pivot_longer(cols = c(before, after), names_to = "period", values_to = "mean") %>%
  ggplot(aes(x = reorder(LIWC_variable, -mean), y = mean, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 LIWC Variables by Absolute Mean Difference",
       x = "LIWC Variable", y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Run the Fixed effect model on stars
model_fe_hm_stars <- feols(
  stars ~ review_period_dummy:proximity | business_id + review_half_months,
  data = aggregated_LIWC_reviews_hm
)
results_fe_hm_stars <- broom::tidy(model_fe_hm_stars) %>%
  dplyr::mutate(dependent = "stars")
# p > 0.05, thus stars is insignificant

# Run the Fixed effect model
models_fe_hm <- purrr::map(liwc_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_half_months")),        
        data = aggregated_LIWC_reviews_hm) })

table(aggregated_LIWC_reviews_hm$proximity, aggregated_LIWC_reviews_hm$review_period)

names(models_fe_hm) <- liwc_vars

# Tidy and extract significant interaction terms
results_fe_hm <- purrr::map2(models_fe_hm, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

interaction_results_fe_hm <- dplyr::filter(results_fe_hm, grepl("review_period_dummy.*:proximity", term))

# Print or save results
print(interaction_results_fe_hm)
write.csv(interaction_results_fe_hm, "liwc_fixed_effects_results_hm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_hm <- interaction_results_fe_hm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)

write.csv(significant_vars_fe_hm, "liwc_fixed_effects_significant_results_hm.csv", row.names = FALSE)

# Visualizations of top 15 significant LIWC variables
# Sort by absolute effect size and keep top 15 for clarity
top_vars_hm <- significant_vars_fe_hm %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:15)

# No plot because there is no significant variable to show

interaction_results_fe_hm %>%
  kable("latex", booktabs = TRUE, digits = 4,
        col.names = c("Interaction Term", "Estimate", "Std. Error", "t Statistic", "p-value", "LIWC Variable")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Second method: 3 half months in "before"
# Insert review periods for the review dataset
filtered_reviews_between_2_50km_3hm <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km_3hm, "filtered_reviews_between_2_50km_3hm.csv", row.names = FALSE)

filtered_reviews_between_2_50km_3hm <- filtered_reviews_between_2_50km_3hm %>%
  filter(date >= as_datetime("2018-03-07"),
         date <= as_datetime("2018-05-07"))

# Separate the review dataset into before and after periods
filtered_reviews_between_2_50km_3hm <- filtered_reviews_between_2_50km_3hm %>%
  mutate(review_half_months = case_when(
    date >= as_datetime("2018-03-07") & date < as_datetime("2018-03-22") ~ "first half month",
    date >= as_datetime("2018-03-22") & date < as_datetime("2018-04-07") ~ "second half month",
    date >= as_datetime("2018-04-07") & date < as_datetime("2018-04-22") ~ "third half month",
    date >= as_datetime("2018-04-22") & date <= as_datetime("2018-05-07") ~ "fourth half month"
  ))

# Simplify the variables within the review dataset
filtered_reviews_between_2_50km_3hm <- filtered_reviews_between_2_50km_3hm %>%
  select(business_id, stars, text, date, review_half_months)

write.csv(filtered_reviews_between_2_50km_3hm, "filtered_reviews_between_2_50km_3hm.csv", row.names = FALSE)

# Add LIWC sentimental variables to the filtered reviews
LIWC_filtered_reviews_3hm <- read.csv("LIWC-22 Results - filtered_reviews_between_2_50k___ - LIWC Analysis_3hm.csv", stringsAsFactors = FALSE)

# Separate the review dataset into before and after periods
LIWC_filtered_reviews_3hm <- LIWC_filtered_reviews_3hm %>%
  mutate(review_period = case_when(
    review_half_months == "first half month" | review_half_months == "second half month" | review_half_months == "third half month" ~ "before",
    review_half_months == "fourth half month" ~ "after"
  ))

# Keep only businesses with both before and after reviews
eligible_ids_3hm <- LIWC_filtered_reviews_3hm %>%
  group_by(business_id) %>%
  filter(n_distinct(review_period) == 2) %>%
  pull(business_id) %>%
  unique()

LIWC_filtered_reviews_3hm <- LIWC_filtered_reviews_3hm %>%
  filter(business_id %in% eligible_ids_3hm)

# Aggregate the sub-dimension LIWC variables into the main dimensions
LIWC_filtered_reviews_3hm <- LIWC_filtered_reviews_3hm %>%
  mutate(
    States = need + want + acquire + lack + fulfill + fatigue,
    Motive = reward + risk + curiosity + allure,
    Timeorientation = time + focuspast + focuspresent + focusfuture
  )

# Model with distance_km (continuous)
# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_3hm <- LIWC_filtered_reviews_3hm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")

write.csv(LIWC_filtered_reviews_3hm, "LIWC_filtered_reviews_3hm.csv", row.names = FALSE)

aggregated_LIWC_reviews_3hm <- LIWC_filtered_reviews_3hm %>%
  group_by(business_id, review_period) %>%
  summarise(
    # average all the LIWC variables
    across(all_of(full_liwc_vars_hm), mean, na.rm = TRUE),
    # average the stars and add distance in km
    across(stars, mean, na.rm = TRUE),
    across(distance_km, mean, na.rm = TRUE),
    # choose the "half month" that appears the most for each business_id
    review_half_months = names(which.max(table(review_half_months))),
    # Combine the "text" as one paragraph for the before and after of each business_id
    combined_text = str_c(text, collapse = " "),
    .groups = "drop"
  )

# Make sure predictors are factors
# aggregated_LIWC_reviews$review_period <- as.factor(LIWC_filtered_reviews$distance_km)
# LIWC_filtered_reviews$distance_km <- as.numeric(LIWC_filtered_reviews$distance_km)

# Convert review_period into a dummy variable: after = 1, before = 0
aggregated_LIWC_reviews_3hm <- aggregated_LIWC_reviews_3hm %>%
  mutate(review_period_dummy = ifelse(review_period == "after", 1, 0))

# Replace all NAN with 0
aggregated_LIWC_reviews_3hm <- aggregated_LIWC_reviews_3hm %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%  
  mutate(across(everything(), ~ replace_na(., 0)))

# Reverse the Distance Variable
aggregated_LIWC_reviews_3hm <- aggregated_LIWC_reviews_3hm %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

write.csv(aggregated_LIWC_reviews_3hm, "aggregated_LIWC_reviews_3hm.csv", row.names = FALSE)

# Calculate before/after means and absolute difference
liwc_means_3hm <- aggregated_LIWC_reviews_3hm %>%
  group_by(review_period) %>%
  summarise(across(all_of(liwc_vars), mean, na.rm = TRUE)) %>%
  pivot_longer(-review_period, names_to = "LIWC_variable", values_to = "mean") %>%
  pivot_wider(names_from = review_period, values_from = mean) %>%
  mutate(abs_diff = abs(before - after)) %>%
  arrange(desc(abs_diff))

# Plot top 10 most changed variables
liwc_means_3hm %>%
  slice_max(abs_diff, n=10) %>%
  pivot_longer(cols = c(before, after), names_to = "period", values_to = "mean") %>%
  ggplot(aes(x = reorder(LIWC_variable, -mean), y = mean, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 LIWC Variables by Absolute Mean Difference",
       x = "LIWC Variable", y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Run the Fixed effect model on stars
model_fe_3hm_stars <- feols(
  stars ~ review_period_dummy:proximity | business_id + review_half_months,
  data = aggregated_LIWC_reviews_3hm
)
results_fe_3hm_stars <- broom::tidy(model_fe_3hm_stars) %>%
  dplyr::mutate(dependent = "stars")
# p > 0.05, thus stars is insignificant

# Run the Fixed effect model on LIWC variables
models_fe_3hm <- purrr::map(liwc_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_half_months")),        
        data = aggregated_LIWC_reviews_3hm) })

table(aggregated_LIWC_reviews_3hm$proximity, aggregated_LIWC_reviews_3hm$review_period)

names(models_fe_3hm) <- liwc_vars

# Tidy and extract significant interaction terms
results_fe_3hm <- purrr::map2(models_fe_3hm, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

interaction_results_fe_3hm <- dplyr::filter(results_fe_3hm, grepl("review_period_dummy.*:proximity", term))

# Print or save results
print(interaction_results_fe_3hm)
write.csv(interaction_results_fe_3hm, "liwc_fixed_effects_results_3hm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_3hm <- interaction_results_fe_3hm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)

write.csv(significant_vars_fe_3hm, "liwc_fixed_effects_significant_results_3hm.csv", row.names = FALSE)

# Visualizations of top 15 significant LIWC variables
# Sort by absolute effect size and keep top 15 for clarity
top_vars_3hm <- significant_vars_fe_3hm %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:15)

# Plot the results
ggplot(top_vars_3hm, aes(x = reorder(dependent, estimate), y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "grey90", midpoint = 0) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "LIWC Variables Affected by Mass Shootings",
    subtitle = "Interaction Effect: Review Period × Proximity (Fixed Effects Model)",
    x = "LIWC Variable",
    y = "Estimate"
  ) +
  theme_minimal()

interaction_results_fe_3hm %>%
  kable("latex", booktabs = TRUE, digits = 4,
        col.names = c("Interaction Term", "Estimate", "Std. Error", "t Statistic", "p-value", "LIWC Variable")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Third method: 4 half months in "before"
# Insert review periods for the review dataset
filtered_reviews_between_2_50km_4hm <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km_4hm, "filtered_reviews_between_2_50km_4hm.csv", row.names = FALSE)

filtered_reviews_between_2_50km_4hm <- filtered_reviews_between_2_50km_4hm %>%
  filter(date >= as_datetime("2018-02-22"),
         date <= as_datetime("2018-05-07"))

# Separate the review dataset into before and after periods
filtered_reviews_between_2_50km_4hm <- filtered_reviews_between_2_50km_4hm %>%
  mutate(review_half_months = case_when(
    date >= as_datetime("2018-02-22") & date < as_datetime("2018-03-07") ~ "first half month",
    date >= as_datetime("2018-03-07") & date < as_datetime("2018-03-22") ~ "second half month",
    date >= as_datetime("2018-03-22") & date < as_datetime("2018-04-07") ~ "third half month",
    date >= as_datetime("2018-04-07") & date < as_datetime("2018-04-22") ~ "fourth half month",
    date >= as_datetime("2018-04-22") & date <= as_datetime("2018-05-07") ~ "fifth half month"
  ))

# Simplify the variables within the review dataset
filtered_reviews_between_2_50km_4hm <- filtered_reviews_between_2_50km_4hm %>%
  select(business_id, stars, text, date, review_half_months)

write.csv(filtered_reviews_between_2_50km_4hm, "filtered_reviews_between_2_50km_4hm.csv", row.names = FALSE)

# Add LIWC sentimental variables to the filtered reviews
LIWC_filtered_reviews_4hm <- read.csv("LIWC-22 Results - filtered_reviews_between_2_50k___ - LIWC Analysis_4hm.csv", stringsAsFactors = FALSE)

# Separate the review dataset into before and after periods
LIWC_filtered_reviews_4hm <- LIWC_filtered_reviews_4hm %>%
  mutate(review_period = case_when(
    review_half_months == "first half month" | review_half_months == "second half month" | review_half_months == "third half month" | review_half_months == "fourth half month" ~ "before",
    review_half_months == "fifth half month" ~ "after"
  ))

# Keep only businesses with both before and after reviews
eligible_ids_4hm <- LIWC_filtered_reviews_4hm %>%
  group_by(business_id) %>%
  filter(n_distinct(review_period) == 2) %>%
  pull(business_id) %>%
  unique()

LIWC_filtered_reviews_4hm <- LIWC_filtered_reviews_4hm %>%
  filter(business_id %in% eligible_ids_4hm)

# Aggregate the sub-dimension LIWC variables into the main dimensions
LIWC_filtered_reviews_4hm <- LIWC_filtered_reviews_4hm %>%
  mutate(
    States = need + want + acquire + lack + fulfill + fatigue,
    Motive = reward + risk + curiosity + allure,
    Timeorientation = time + focuspast + focuspresent + focusfuture
  )

# Model with distance_km (continuous)
# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_4hm <- LIWC_filtered_reviews_4hm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")

write.csv(LIWC_filtered_reviews_4hm, "LIWC_filtered_reviews_4hm.csv", row.names = FALSE)

aggregated_LIWC_reviews_4hm <- LIWC_filtered_reviews_4hm %>%
  group_by(business_id, review_period) %>%
  summarise(
    # average all the LIWC variables
    across(all_of(full_liwc_vars_hm), mean, na.rm = TRUE),
    # average the stars and add distance in km
    across(stars, mean, na.rm = TRUE),
    across(distance_km, mean, na.rm = TRUE),
    # choose the "half month" that appears the most for each business_id
    review_half_months = names(which.max(table(review_half_months))),
    # Combine the "text" as one paragraph for the before and after of each business_id
    combined_text = str_c(text, collapse = " "),
    .groups = "drop"
  )

# Make sure predictors are factors
# aggregated_LIWC_reviews$review_period <- as.factor(LIWC_filtered_reviews$distance_km)
# LIWC_filtered_reviews$distance_km <- as.numeric(LIWC_filtered_reviews$distance_km)

# Convert review_period into a dummy variable: after = 1, before = 0
aggregated_LIWC_reviews_4hm <- aggregated_LIWC_reviews_4hm %>%
  mutate(review_period_dummy = ifelse(review_period == "after", 1, 0))

# Replace all NAN with 0
aggregated_LIWC_reviews_4hm <- aggregated_LIWC_reviews_4hm %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%  
  mutate(across(everything(), ~ replace_na(., 0)))

# Reverse the Distance Variable
aggregated_LIWC_reviews_4hm <- aggregated_LIWC_reviews_4hm %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

write.csv(aggregated_LIWC_reviews_4hm, "aggregated_LIWC_reviews_4hm.csv", row.names = FALSE)

# Calculate before/after means and absolute difference
liwc_means_4hm <- aggregated_LIWC_reviews_4hm %>%
  group_by(review_period) %>%
  summarise(across(all_of(liwc_vars), mean, na.rm = TRUE)) %>%
  pivot_longer(-review_period, names_to = "LIWC_variable", values_to = "mean") %>%
  pivot_wider(names_from = review_period, values_from = mean) %>%
  mutate(abs_diff = abs(before - after)) %>%
  arrange(desc(abs_diff))

# Plot top 10 most changed variables
liwc_means_4hm %>%
  slice_max(abs_diff, n=10) %>%
  pivot_longer(cols = c(before, after), names_to = "period", values_to = "mean") %>%
  ggplot(aes(x = reorder(LIWC_variable, -mean), y = mean, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 LIWC Variables by Absolute Mean Difference",
       x = "LIWC Variable", y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Run the Fixed effect model on stars
model_fe_4hm_stars <- feols(
  stars ~ review_period_dummy:proximity | business_id + review_half_months,
  data = aggregated_LIWC_reviews_4hm
)
results_fe_4hm_stars <- broom::tidy(model_fe_4hm_stars) %>%
  dplyr::mutate(dependent = "stars")
# p > 0.05, thus stars is insignificant

# Run the Fixed effect model on LIWC variables
models_fe_4hm <- purrr::map(liwc_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_half_months")),        
        data = aggregated_LIWC_reviews_4hm) })

table(aggregated_LIWC_reviews_4hm$proximity, aggregated_LIWC_reviews_4hm$review_period)

names(models_fe_4hm) <- liwc_vars

# Tidy and extract significant interaction terms
results_fe_4hm <- purrr::map2(models_fe_4hm, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

interaction_results_fe_4hm <- dplyr::filter(results_fe_4hm, grepl("review_period_dummy.*:proximity", term))

# Print or save results
print(interaction_results_fe_4hm)
write.csv(interaction_results_fe_4hm, "liwc_fixed_effects_results_4hm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_4hm <- interaction_results_fe_4hm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)

write.csv(significant_vars_fe_4hm, "liwc_fixed_effects_significant_results_4hm.csv", row.names = FALSE)

# Visualizations of top 15 significant LIWC variables
# Sort by absolute effect size and keep top 15 for clarity
top_vars_4hm <- significant_vars_fe_4hm %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:15)

# Plot the results
ggplot(top_vars_4hm, aes(x = reorder(dependent, estimate), y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "grey90", midpoint = 0) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "LIWC Variables Affected by Mass Shootings",
    subtitle = "Interaction Effect: Review Period × Proximity (Fixed Effects Model)",
    x = "LIWC Variable",
    y = "Estimate"
  ) +
  theme_minimal()

interaction_results_fe_4hm %>%
  kable("latex", booktabs = TRUE, digits = 4,
        col.names = c("Interaction Term", "Estimate", "Std. Error", "t Statistic", "p-value", "LIWC Variable")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



#####
# Fourth method: dividing into 3 months (before: 1 half month; after: 2 half months)
# Insert review periods for the review dataset
filtered_reviews_between_2_50km_rvhm <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km_rvhm, "filtered_reviews_between_2_50km_rvhm.csv", row.names = FALSE)

filtered_reviews_between_2_50km_rvhm <- filtered_reviews_between_2_50km_rvhm %>%
  filter(date >= as_datetime("2018-04-07"),
         date <= as_datetime("2018-05-22"))

# Separate the review dataset into before and after periods
filtered_reviews_between_2_50km_rvhm <- filtered_reviews_between_2_50km_rvhm %>%
  mutate(review_half_months = case_when(
    date >= as_datetime("2018-04-07") & date < as_datetime("2018-04-22") ~ "first half month",
    date >= as_datetime("2018-04-22") & date < as_datetime("2018-05-07") ~ "second half month",
    date >= as_datetime("2018-05-07") & date <= as_datetime("2018-05-22 23:59:59") ~ "third half month"
  ))

# Simplify the variables within the review dataset
filtered_reviews_between_2_50km_rvhm <- filtered_reviews_between_2_50km_rvhm %>%
  select(business_id, stars, text, date, review_half_months)

write.csv(filtered_reviews_between_2_50km_rvhm, "filtered_reviews_between_2_50km_rvhm.csv", row.names = FALSE)

# Add LIWC sentimental variables to the filtered reviews
LIWC_filtered_reviews_rvhmm <- read.csv("LIWC-22 Results - filtered_reviews_between_2_50k___ - LIWC Analysis_rvhm.csv", stringsAsFactors = FALSE)

# Separate the review dataset into before and after periods
LIWC_filtered_reviews_rvhmm <- LIWC_filtered_reviews_rvhmm %>%
  mutate(review_period = case_when(
    review_half_months == "first half month"  ~ "before",
    review_half_months == "second half month" | review_half_months == "third half month" ~ "after"
  ))

# Keep only businesses with both before and after reviews
eligible_ids_rvhm <- LIWC_filtered_reviews_rvhmm %>%
  group_by(business_id) %>%
  filter(n_distinct(review_period) == 2) %>%
  pull(business_id) %>%
  unique()

LIWC_filtered_reviews_rvhmm <- LIWC_filtered_reviews_rvhmm %>%
  filter(business_id %in% eligible_ids_rvhm)

# Model with distance_km (continuous)
# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_rvhmm <- LIWC_filtered_reviews_rvhmm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")

write.csv(LIWC_filtered_reviews_rvhmm, "LIWC_filtered_reviews_rvhmm.csv", row.names = FALSE)

# Summarize by business_id and review_period (before/after)
liwc_start_rvhm <- which(names(LIWC_filtered_reviews_rvhmm) == "WC")
liwc_end_rvhm <- which(names(LIWC_filtered_reviews_rvhmm) == "Exclam")
full_liwc_vars_rvhm <- names(LIWC_filtered_reviews_rvhmm)[liwc_start_rvhm:liwc_end_rvhm]

aggregated_LIWC_reviews_rvhm <- LIWC_filtered_reviews_rvhmm %>%
  group_by(business_id, review_period) %>%
  summarise(
    # average all the LIWC variables
    across(all_of(full_liwc_vars_rvhm), mean, na.rm = TRUE),
    # average the stars and add distance in km
    across(stars, mean, na.rm = TRUE),
    across(distance_km, mean, na.rm = TRUE),
    # choose the "half month" that appears the most for each business_id
    review_half_months = names(which.max(table(review_half_months))),
    # Combine the "text" as one paragraph for the before and after of each business_id
    combined_text = str_c(text, collapse = " "),
    .groups = "drop"
  )

# Make sure predictors are factors
# aggregated_LIWC_reviews$review_period <- as.factor(LIWC_filtered_reviews$distance_km)
# LIWC_filtered_reviews$distance_km <- as.numeric(LIWC_filtered_reviews$distance_km)

# Convert review_period into a dummy variable: after = 1, before = 0
aggregated_LIWC_reviews_rvhm <- aggregated_LIWC_reviews_rvhm %>%
  mutate(review_period_dummy = ifelse(review_period == "after", 1, 0))

# Replace all NAN with 0
aggregated_LIWC_reviews_rvhm <- aggregated_LIWC_reviews_rvhm %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%  
  mutate(across(everything(), ~ replace_na(., 0)))

# Reverse the Distance Variable
aggregated_LIWC_reviews_rvhm <- aggregated_LIWC_reviews_rvhm %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

write.csv(aggregated_LIWC_reviews_rvhm, "aggregated_LIWC_reviews_rvhm.csv", row.names = FALSE)

# Run the Fixed effect model
models_fe_rvhm <- purrr::map(liwc_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_half_months")),        
        data = aggregated_LIWC_reviews_rvhm) })

table(aggregated_LIWC_reviews_rvhm$proximity, aggregated_LIWC_reviews_rvhm$review_period)

names(models_fe_rvhm) <- liwc_vars

# Tidy and extract significant interaction terms
results_fe_rvhm <- purrr::map2(models_fe_rvhm, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

interaction_results_fe_rvhm <- dplyr::filter(results_fe_rvhm, grepl("review_period_dummy.*:proximity", term))

# Print or save results
print(interaction_results_fe_rvhm)
write.csv(interaction_results_fe_rvhm, "liwc_fixed_effects_results_rvhm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_rvhm <- interaction_results_fe_rvhm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)

write.csv(significant_vars_fe_rvhm, "liwc_fixed_effects_significant_results_rvhm.csv", row.names = FALSE)

# Visualizations of top significant LIWC variables
top_vars_rvhm <- significant_vars_fe_rvhm %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:15)

# No plot because there is no significant variable to show

# Fifth method: dividing into 3 months (before: 1 month; after: 2 months)
# Insert review periods for the review dataset
filtered_reviews_between_2_50km_rvfm <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km_rvfm, "filtered_reviews_between_2_50km_rvfm.csv", row.names = FALSE)

filtered_reviews_between_2_50km_rvfm <- filtered_reviews_between_2_50km_rvfm %>%
  filter(date >= as_datetime("2018-03-22"),
         date <= as_datetime("2018-06-22"))

# Separate the review dataset into before and after periods
filtered_reviews_between_2_50km_rvfm <- filtered_reviews_between_2_50km_rvfm %>%
  mutate(review_months = case_when(
    date >= as_datetime("2018-03-22") & date < as_datetime("2018-04-22") ~ "first month",
    date >= as_datetime("2018-04-22") & date < as_datetime("2018-05-22") ~ "second month",
    date >= as_datetime("2018-05-22") & date <= as_datetime("2018-06-22 23:59:59") ~ "third month"
  ))

# Simplify the variables within the review dataset
filtered_reviews_between_2_50km_rvfm <- filtered_reviews_between_2_50km_rvfm %>%
  select(business_id, stars, text, date, review_months)

write.csv(filtered_reviews_between_2_50km_rvfm, "filtered_reviews_between_2_50km_rvfm.csv", row.names = FALSE)

# Add LIWC sentimental variables to the filtered reviews
LIWC_filtered_reviews_rvfm <- read.csv("LIWC-22 Results - filtered_reviews_between_2_50k___ - LIWC Analysis_rvfm.csv", stringsAsFactors = FALSE)

# Separate the review dataset into before and after periods
LIWC_filtered_reviews_rvfm <- LIWC_filtered_reviews_rvfm %>%
  mutate(review_period = case_when(
    review_months == "first month" ~ "before",
    review_months == "second month" | review_months == "third month" ~ "after"
  ))

# Keep only businesses with both before and after reviews
eligible_ids_rvfm <- LIWC_filtered_reviews_rvfm %>%
  group_by(business_id) %>%
  filter(n_distinct(review_period) == 2) %>%
  pull(business_id) %>%
  unique()

LIWC_filtered_reviews_rvfm <- LIWC_filtered_reviews_rvfm %>%
  filter(business_id %in% eligible_ids_rvfm)

# Model with distance_km (continuous)
# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_rvfm <- LIWC_filtered_reviews_rvfm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")

write.csv(LIWC_filtered_reviews_rvfm, "LIWC_filtered_reviews_rvfm.csv", row.names = FALSE)

# Summarize by business_id and review_period (before/after)
liwc_start_rvfm <- which(names(LIWC_filtered_reviews_rvfm) == "WC")
liwc_end_rvfm <- which(names(LIWC_filtered_reviews_rvfm) == "Exclam")
full_liwc_vars_rvfm <- names(LIWC_filtered_reviews_rvfm)[liwc_start_rvfm:liwc_end_rvfm]

aggregated_LIWC_reviews_rvfm <- LIWC_filtered_reviews_rvfm %>%
  group_by(business_id, review_period) %>%
  summarise(
    # average all the LIWC variables
    across(all_of(full_liwc_vars_rvfm), mean, na.rm = TRUE),
    # average the stars and add distance in km
    across(stars, mean, na.rm = TRUE),
    across(distance_km, mean, na.rm = TRUE),
    # choose the "month" that appears the most for each business_id
    review_months = names(which.max(table(review_months))),
    # Combine the "text" as one paragraph for the before and after of each business_id
    combined_text = str_c(text, collapse = " "),
    .groups = "drop"
  )

# Make sure predictors are factors
# aggregated_LIWC_reviews$review_period <- as.factor(LIWC_filtered_reviews$distance_km)
# LIWC_filtered_reviews$distance_km <- as.numeric(LIWC_filtered_reviews$distance_km)

# Convert review_period into a dummy variable: after = 1, before = 0
aggregated_LIWC_reviews_rvfm <- aggregated_LIWC_reviews_rvfm %>%
  mutate(review_period_dummy = ifelse(review_period == "after", 1, 0))

# Replace all NAN with 0
aggregated_LIWC_reviews_rvfm <- aggregated_LIWC_reviews_rvfm %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%  
  mutate(across(everything(), ~ replace_na(., 0)))

# Reverse the Distance Variable
aggregated_LIWC_reviews_rvfm <- aggregated_LIWC_reviews_rvfm %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

write.csv(aggregated_LIWC_reviews_rvfm, "aggregated_LIWC_reviews_rvfm.csv", row.names = FALSE)

# Run the Fixed effect model
models_fe_rvfm <- purrr::map(liwc_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_months")),        
        data = aggregated_LIWC_reviews_rvfm) })

table(aggregated_LIWC_reviews_rvfm$proximity, aggregated_LIWC_reviews_rvfm$review_period)

names(models_fe_rvfm) <- liwc_vars

# Tidy and extract significant interaction terms
results_fe_rvfm <- purrr::map2(models_fe_rvfm, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

interaction_results_fe_rvfm <- dplyr::filter(results_fe_rvfm, grepl("review_period_dummy.*:proximity", term))

# Print or save results
print(interaction_results_fe_rvfm)
write.csv(interaction_results_fe_rvfm, "liwc_fixed_effects_results_rvfm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_rvfm <- interaction_results_fe_rvfm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)

write.csv(significant_vars_fe_rvfm, "liwc_fixed_effects_significant_results_rvfm.csv", row.names = FALSE)

# Visualizations of top significant LIWC variables
top_vars_rvfm <- significant_vars_fe_rvfm %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:15)

# Plot the results
ggplot(top_vars_rvfm, aes(x = reorder(dependent, estimate), y = estimate, fill = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "grey90", midpoint = 0) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "LIWC Variables Affected by Mass Shootings",
    subtitle = "Interaction Effect: Review Period × Proximity (Fixed Effects Model)",
    x = "LIWC Variable",
    y = "Estimate"
  ) +
  theme_minimal()


# Draft of code and not related
# Stars
# Run fixed effects regression for stars
model_fe_stars <- feols(
  stars ~ review_period_dummy:proximity | business_id + review_months,
  data = aggregated_LIWC_reviews_fm
)

# Tidy up the result and add the variable name manually
results_fe_stars <- broom::tidy(model_fe_stars) %>%
  dplyr::mutate(dependent = "stars")

interaction_results_fe_stars <- dplyr::filter(results_fe_stars, grepl("review_period_dummy.*:proximity", term))

# I found a problem in explaining the interaction effect review_period x distance_km
# It now reflects the positive affect of distance increasing on the sentiment level
# but what if we does the opposite (like to see how the sentiment is affected as the distance decreases)
# shall I separate the negative and positive sentiment?

tone_vars <- c("tone_pos", "tone_neg")

models_fe_tone <- purrr::map(tone_vars, function(var) {  
  feols(as.formula(paste0(var, " ~ review_period_dummy:distance_km | business_id + review_period_dummy")),        
        data = aggregated_LIWC_reviews) })

results_fe_tone <- purrr::map2(models_fe_tone, tone_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

# Reverse the Distance Variable
aggregated_LIWC_reviews <- aggregated_LIWC_reviews %>%
  mutate(proximity = max(distance_km, na.rm = TRUE) - distance_km)

# Update the model
models_fe_prox <- purrr::map(liwc_vars, function(var) {
  feols(as.formula(paste0(var, " ~ review_period_dummy:proximity | business_id + review_period_dummy")),
        data = aggregated_LIWC_reviews)
})

# Tidy and extract significant interaction terms
results_fe_prox <- purrr::map2(models_fe_prox, liwc_vars, function(mod, name) {
  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
}) %>%
  dplyr::bind_rows()

# Add a time variable to divide all the dates into 3 separate months:
#filtered_reviews_between_2_50km <- filtered_reviews_between_2_50km %>%
#  mutate(review_months = case_when(
#    date >= as_datetime("2018-02-22") & date < as_datetime("2018-03-22") ~ "first month",
#    date >= as_datetime("2018-03-22") & date < as_datetime("2018-04-22") ~ "second month",
#    date >= as_datetime("2018-04-22") & date <= as_datetime("2018-05-22 23:59:59") ~ "third month"
#  ))




#business_between_2_50km <- business_between_2_50km %>%
#  mutate(distance_level = case_when(
#    distance_km <= 2 ~ 5,
#    distance_km > 2 & distance_km <= 5 ~ 4,
#    distance_km > 5 & distance_km <= 10 ~ 3,
#    distance_km > 10 & distance_km <= 20 ~ 2,
#    distance_km > 20 & distance_km <= 50 ~ 1
#  ))

# Model with Distance_level (discretizing)
# Add Distance_Level varaible to LIWC_filtered_reviews
# LIWC_filtered_reviews <- LIWC_filtered_reviews %>%
# left_join(business_between_2_50km %>% select(business_id, distance_level),
#             by = "business_id")

# Make sure predictors are factors
# LIWC_filtered_reviews$review_period <- as.factor(LIWC_filtered_reviews$review_period)
# LIWC_filtered_reviews$distance_level <- as.factor(LIWC_filtered_reviews$distance_level)

# Run fixed effects regression for each LIWC variable
# models_fe <- purrr::map(liwc_vars, function(var) {  
#   feols(as.formula(paste0(var, " ~ review_period:distance_level")),        
#         data = LIWC_filtered_reviews) }) 

# table(LIWC_filtered_reviews$distance_level, LIWC_filtered_reviews$review_period)


# names(models_fe) <- liwc_vars

# Tidy and extract significant interaction terms
# results_fe <- purrr::map2(models_fe, liwc_vars, function(mod, name) {
#  broom::tidy(mod) %>% dplyr::mutate(dependent = name)
# }) %>%
# dplyr::bind_rows()

# interaction_results_fe <- dplyr::filter(results_fe, grepl("review_period.*:distance_level", term))

# Print or save results
# print(interaction_results_fe)
# write.csv(interaction_results_fe, "liwc_fixed_effects_results.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
# significant_vars_fe <- interaction_results_fe %>%
#  dplyr::filter(p.value < 0.05) %>%
#  dplyr::select(dependent, term, estimate, std.error, p.value)

# LIWC variable
# Run fixed effects regression for each LIWC variable
# Dividing DRIVES variable
#feols(
#  Drives ~ affiliation + achieve + power | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing COGNITION variable
#feols(
#  Cognition ~ allnone + cogproc + memory | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# memory appears as an insignificant variable
# Dividing cogproc variable further
#feols(
#  cogproc ~ insight + cause + discrep + tentat + certitude + differ | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing AFFECT variable
#feols(
#  Affect ~ tone_pos + tone_neg + emotion + swear | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# Dividing emotion variable further
#feols(
#  emotion ~ emo_pos + emo_neg | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# Dividng emo_neg variable even further
#feols(
#  emo_neg ~ emo_anx + emo_anger + emo_sad | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing SOCIAL variable
#feols(
#  Social ~ socbehav + socrefs | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# Dividing socbehav variable further
#feols(
#  socbehav ~ prosocial + polite + conflict + moral + comm | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# Dividing socrefs variable further
#feols(
#  socrefs ~ family + friend + female + male | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing CULTURE variable
#feols(
#  Culture ~ politic + ethnicity + tech | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing LIFESTYLE variable
#feols(
#  Lifestyle ~ leisure + home + work + money + relig | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing PHYSICAL variable
#feols(
#  Physical ~ health + substances + sexual + food + death | business_id + review_period,
#  data = LIWC_filtered_reviews
#)
# Dividing health variable further
#feols(
#  health ~ illness + wellness + mental | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

# Dividing PERCEPTION variable
#feols(
#  Perception ~ attention + motion + space + visual + auditory + feeling | business_id + review_period,
#  data = LIWC_filtered_reviews
#)

