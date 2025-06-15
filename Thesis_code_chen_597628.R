# Install packages
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

# Read packages
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

# Read JSON datasets downloaded from https://business.yelp.com/data/resources/open-dataset/
con <- file("yelp_academic_dataset_business.json", "r")
full_business <- stream_in(con)
con_review <- file("yelp_academic_dataset_review.json", "r")
full_review <- stream_in(con_review)

#----- Part 1: Prepare the cleaned business and review dataset before applying different methods-----#
# Check the most appeared state to help to find a suitable mass shooting
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

# Insert a new variable called "distance_km" to help to indicate the distance in km
# from the Nashville Waffle House shooting which happened on 2018-04-22
shooting_incident <- c(-86.6393, 36.0522)
full_business$distance_km <- mapply(function(lat, lon) {
  distHaversine(c(lon, lat), shooting_incident) / 1000
}, full_business$latitude, full_business$longitude) 
head(full_business$distance_km)

# Put 50km as the upper bound for the distance from the business to the shooting
business_between_2_50km <- full_business %>% 
  filter(distance_km >= 2, distance_km <= 50)

# Simplify the variables within the business dataset
business_between_2_50km <- business_between_2_50km %>%
  select(business_id, name, address, city, state, postal_code, latitude, 
         longitude, stars, review_count, is_open, distance_km)
write.csv(business_between_2_50km, "business_between_2_50km", row.names = FALSE)

# Only keep the reviews that are from the businesses that are kept in the business_between_2_50km
filtered_reviews_between_2_50km <- semi_join(full_review, business_between_2_50km, by = "business_id")
write.csv(filtered_reviews_between_2_50km, "filtered_reviews_between_2_50km.csv", row.names = FALSE)


# All dividing into half months to rule out the possibility of insufficient sample size and have more robust results,
# where always only 1 half month in the time period after shooting incidence


#-----Part 2.1: First method as the main analysis with 2 half months in "before the mass shooting"-----#
filtered_reviews_between_2_50km_hm <- filtered_reviews_between_2_50km %>%
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

# Add LIWC sentimental variables to the filtered reviews, processed by the LIWC-22 Analysis tool bought from https://www.liwc.app/buy
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

# Add distance_km varaible to LIWC_filtered_reviews
LIWC_filtered_reviews_hm <- LIWC_filtered_reviews_hm %>%
  left_join(business_between_2_50km %>% select(business_id, distance_km),
            by = "business_id")
write.csv(LIWC_filtered_reviews_hm, "LIWC_filtered_reviews_hm.csv", row.names = FALSE)

# Define LIWC variable names and calculate their means
liwc_vars <- c("WC", "Analytic", "Clout", "Authentic", "Tone", "AllPunc", "Drives", 
               "Cognition", "Affect", "Social", "Culture", "Lifestyle", "Physical", 
               "Perception", "States", "Motive", "Timeorientation", "stars")
liwc_means %>%
  arrange(desc(abs_diff)) %>%
  head(10)

# Prepare for the aggregation step below
liwc_start_hm <- which(names(LIWC_filtered_reviews_hm) == "WC")
liwc_end_hm <- which(names(LIWC_filtered_reviews_hm) == "Timeorientation")
full_liwc_vars_hm <- names(LIWC_filtered_reviews_hm)[liwc_start_hm:liwc_end_hm]

# Aggregate the review dataset so that each business only has one "before" review output and one "after" review output
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

# Reverse the Distance Variable to Proximity
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

# Print results
print(interaction_results_fe_hm)
write.csv(interaction_results_fe_hm, "liwc_fixed_effects_results_hm.csv", row.names = FALSE)

# Filter significant results (p < 0.05)
significant_vars_fe_hm <- interaction_results_fe_hm %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::select(dependent, term, estimate, std.error, p.value)
write.csv(significant_vars_fe_hm, "liwc_fixed_effects_significant_results_hm.csv", row.names = FALSE)

# Generate a summary table for the interaction results with all the statistic characteristics
interaction_results_fe_hm %>%
  kable("latex", booktabs = TRUE, digits = 4,
        col.names = c("Interaction Term", "Estimate", "Std. Error", "t Statistic", "p-value", "LIWC Variable")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



