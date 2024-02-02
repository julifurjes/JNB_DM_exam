# Setting the working directory
setwd('/work/JNB exam/The_good_stuff')

install.packages("pacman")
pacman::p_load(tidyverse, R2jags, dplyr, readxl, parallel, polspline)
set.seed(1989) #(Taylor's version)

##### LOAD DATA #####

# Load data in as df data frame
df <- read_excel("/work/JNB exam/data.xlsx")

##### Preparing data #####

# Defining a percentage for the cut-off rate for classifying healthy and unhealthy scores
top_percentage <- 30

# Empty dataframe for outputs
calc_scores <- data.frame(matrix(nrow = nrow(df), ncol = 0))

# Defining function for calculating the cut-off number
find_percentile_cutoff <- function(numbers, percentile) {
  # Sort the list in ascending order
  sorted_numbers <- sort(numbers, decreasing = TRUE)
  
  # Calculate the index for the bottom of the top percentile
  index <- ceiling(length(sorted_numbers) * (percentile/100))
  
  # Get the value at the calculated index
  value <- sorted_numbers[index]
  
  return(value)
}

## Openness ##

# Openness questions: Q36-39P + Q40-14P (+ normalizing scores)
df <- df %>%
  mutate(
    Q40P = ifelse(!is.na(Q40P), 6 - Q40P, NA),
    Q41P = ifelse(!is.na(Q41P), 6 - Q41P, NA),
    mean_openness = rowMeans(.[, c("Q36P", "Q37P", "Q38P", "Q39P", "Q40P", "Q41P")], na.rm = TRUE),
    sum_openness = rowSums(.[, c("Q36P", "Q37P", "Q38P", "Q39P", "Q40P", "Q41P")], na.rm = TRUE))

# Calculating mean and sd of openness
mean(df$sum_openness)
sd(df$sum_openness)

# Saving the sum in the new df
calc_scores$sum_open <- df$sum_openness

# Calculating the cut-off point
openness_cut_off <- find_percentile_cutoff(calc_scores$sum_open, top_percentage)

partner_openness_table <- calc_scores %>%
  group_by(openness = ifelse(sum_open >= openness_cut_off, "Open", "Not Open")) %>%
  summarize(count = n())
print(partner_openness_table)

# Making it binary
calc_scores$bin_open <- ifelse(calc_scores$sum_open >= openness_cut_off, 1, 0)
number_of_people_open <- sum(calc_scores$bin_open)

# Check the first few rows to verify
head(calc_scores$bin_open)
plot(calc_scores$bin_open)

## Relationship quality ##

calc_scores$sum_rel <- rowSums(df[, c("Q57P", "Q58P", "Q59P", "Q60P")], na.rm = TRUE)

# Calculating mean and sd of relationship quality
mean(calc_scores$sum_rel)
sd(calc_scores$sum_rel)

# Calculating the cut-off point
rel_cut_off <- find_percentile_cutoff(calc_scores$sum_rel, top_percentage)

relationship_table <- calc_scores %>%
  group_by(relationship = ifelse(sum_rel >= rel_cut_off, "Healthy Relationship", "Unhealthy Relationship")) %>%
  summarize(count = n())
print(relationship_table)

# Create a binary parameter for relationship quality
calc_scores$bin_rel <- ifelse(calc_scores$sum_rel >= rel_cut_off, 1, 0)

## Mother ##

# Mother attachment scores
# Reversing the scale for questions Q18M-22M to make all the questions be on the same scale, then add the average scores to the df
df <- df %>%
  mutate(
    Q18M = ifelse(!is.na(Q18M), 6 - Q18M, NA),
    Q19M = ifelse(!is.na(Q19M), 6 - Q19M, NA),
    Q20M = ifelse(!is.na(Q20M), 6 - Q20M, NA),
    Q21M = ifelse(!is.na(Q21M), 6 - Q21M, NA),
    Q22M = ifelse(!is.na(Q22M), 6 - Q22M, NA),
    avg_score_M = rowMeans(df[, c("Q14M", "Q15M", "Q16M", "Q17M", "Q18M", "Q19M", "Q20M", "Q21M", "Q22M")], na.rm = TRUE),
    sum_M = rowSums(df[, c("Q14M", "Q15M", "Q16M", "Q17M", "Q18M", "Q19M", "Q20M", "Q21M", "Q22M")], na.rm = TRUE)
  )

# Calculating the cut-off point
mom_cut_off <- find_percentile_cutoff(df$sum_M, top_percentage)

# Testing mommy issues
mommy_issues_table <- df %>%
  group_by(mother_attachment = ifelse(sum_M >= mom_cut_off, "Healthy mom (att)", "Unhealthy mom (att)")) %>%
  summarize(count = n())
print(mommy_issues_table)

# Calculating mean and sd of mother attachment
mean(df$sum_M)
sd(df$sum_M)

# Mother perception scores
# Good and bad adjectives are the same for mother and father
# Define good and bad descriptions by their numbers
good_parent_adjectives <- c(1, 2, 5, 6, 7, 11, 12, 14, 17, 20)
bad_parent_adjectives <- c(3, 4, 8, 9, 10, 13, 15, 16, 18, 19)

# Initialize a vector to hold ratios for good vs bad ip scores
good_to_bad_ratios_M <- numeric(nrow(df))

# Loop through each response and calculate ratios
for (i in 1:nrow(df)) {
  # Split the responses (adjectives) into individual numbers
  adjectives <- as.numeric(unlist(strsplit(df$Q65M[i], split = ",")))
  
  # Count how many good and bad scores there are
  good_count_M <- sum(adjectives %in% good_parent_adjectives)
  bad_count_M <- sum(adjectives %in% bad_parent_adjectives)
  
  # Calculate the ratio of good answers - so 1 means all adjectives are good, 0 means all adjectives are bad ()the higher the ratio the better
  ratio_M <- good_count_M / (good_count_M + bad_count_M)
  
  # Store the ratio
  good_to_bad_ratios_M[i] <- ratio_M
}

# Adding the ratios to df
df$mom_perception <- good_to_bad_ratios_M

# Calculating mean and sd of mother perception
mean(df$mom_perception)
sd(df$mom_perception)

# Calculating the cut-off point
mom_cut_off <- find_percentile_cutoff(df$mom_perception, top_percentage)

# Testing mommy issues
mommy_perc_table <- df %>%
  group_by(mother_perc = ifelse(mom_perception >= mom_cut_off, "Healthy mom (perc)", "Unhealthy mom (perc)")) %>%
  summarize(count = n())
print(mommy_perc_table)

# Creating percentage of the attachment scores
mom_att_ratio <- df$sum_M / 45 # (total score)

# Merging the mother attachment and mother perception
# It's 9:1 because of the number of questions
weighted_average <- (mom_att_ratio * 9 + df$mom_perception * 1) / (9 + 1)

# Calculating the cut-off point
mom_cut_off <- find_percentile_cutoff(weighted_average, top_percentage)

# Create a binary parameter for mommy issues
calc_scores$bin_mom <- ifelse(weighted_average >= mom_cut_off, 1, 0)

## Father ##

# Same normalization method for questions Q29-33, for father attachment scores
df <- df %>%
  mutate(
    Q29F = ifelse(!is.na(Q29F), 6 - Q29F, NA),
    Q30F = ifelse(!is.na(Q30F), 6 - Q30F, NA),
    Q31F = ifelse(!is.na(Q31F), 6 - Q31F, NA),
    Q32F = ifelse(!is.na(Q32F), 6 - Q32F, NA),
    Q33F = ifelse(!is.na(Q33F), 6 - Q33F, NA),
    avg_score_F = rowMeans(df[, c("Q25F", "Q26F", "Q27F", "Q28F", "Q29F", "Q30F", "Q31F", "Q32F", "Q33F")], na.rm = TRUE),
    sum_F = rowSums(df[, c("Q25F", "Q26F", "Q27F", "Q28F", "Q29F", "Q30F", "Q31F", "Q32F", "Q33F")], na.rm = TRUE)
    
  )

# Calculating the cut-off point
dad_cut_off <- find_percentile_cutoff(df$sum_F, top_percentage)

# Testing daddy issues
daddy_issues_table <- df %>%
  group_by(father_attachment = ifelse(sum_F >= dad_cut_off, "Healthy dad (att)", "Unhealthy dad (att)")) %>%
  summarize(count = n())
print(daddy_issues_table)

# Calculating mean and sd of father attachment
mean(df$sum_F)
sd(df$sum_F)

# Father perception scores
# Good and bad adjectives are the same

# Initialize a vector to hold ratios for good vs bad ip scores
good_to_bad_ratios_F <- numeric(nrow(df))

# Loop through each response and calculate ratios
for (i in 1:nrow(df)) {
  # Split the responses (adjectives) into individual numbers
  adjectives <- as.numeric(unlist(strsplit(df$Q66F[i], split = ",")))
  
  # Count how many good and bad scores there are
  good_count_F <- sum(adjectives %in% good_parent_adjectives)
  bad_count_F <- sum(adjectives %in% bad_parent_adjectives)
  
  # Calculate the ratio of good answers - so 1 means all adjectives are good, 0 means all adjectives are bad ()the higher the ratio the better
  ratio_F <- good_count_F / (good_count_F + bad_count_F)
  
  # Store the ratio
  good_to_bad_ratios_F[i] <- ratio_F
}

# Adding the ratios to df
df$dad_perception <- good_to_bad_ratios_F

# Calculating mean and sd of father perception in childhood
mean(df$dad_perception, na.rm = TRUE)
sd(df$dad_perception, na.rm = TRUE)

# Calculating the cut-off point
dad_cut_off <- find_percentile_cutoff(df$dad_perception, top_percentage)

# Testing daddy issues
daddy_perc_table <- df %>%
  group_by(father_perc = ifelse(dad_perception >= dad_cut_off, "Healthy dad (perc)", "Unhealthy dade (perc)")) %>%
  summarize(count = n())
print(daddy_perc_table)

# Creating percentage of the attachment scores
dad_att_ratio <- df$sum_F / 45 # (total score)

# Merging the mother attachment and mother perception
# It's 9:1 because of the number of questions
weighted_average <- (dad_att_ratio * 9 + df$dad_perception * 1) / (9 + 1)

# Calculating the cut-off point
dad_cut_off <- find_percentile_cutoff(weighted_average, top_percentage)

# Create a binary parameter for daddy issues
calc_scores$bin_dad <- ifelse(weighted_average >= dad_cut_off, 1, 0)

## Interparental ##

# Define good and bad descriptions by their numbers
good_ip_adjectives <- c(1, 2, 6, 7, 11, 12)
bad_ip_adjectives <- c(3, 4, 5, 8, 9, 10)

# Initialize a vector to hold ratios for good vs bad ip scores
good_to_bad_ratios <- numeric(nrow(df))

# Loop through each response and calculate ratios
for (i in 1:nrow(df)) {
  # Split the responses (adjectives) into individual numbers
  adjectives <- as.numeric(unlist(strsplit(df$Q67R[i], split = ",")))
  
  # Count how many good and bad scores there are
  good_count <- sum(adjectives %in% good_ip_adjectives)
  bad_count <- sum(adjectives %in% bad_ip_adjectives)
  
  # Calculate the ratio of good answers - so 1 means all adjectives are good, 0 means all adjectives are bad ()the higher the ratio the better
  ratio <- good_count / (good_count + bad_count)
  
  # Store the ratio
  good_to_bad_ratios[i] <- ratio
}

# Adding the ratios to df
calc_scores$sum_ip <- good_to_bad_ratios

# Defining threshold for interparental perception health
ip_cut_off <- find_percentile_cutoff(calc_scores$sum_ip, top_percentage)

#testing interparental health issues
ip_health_table <- calc_scores %>%
  group_by(ip_ratio = ifelse(sum_ip >= ip_cut_off, "Healthy IP", "Unhealthy IP")) %>%
  summarize(count = n())
print(ip_health_table)

# Create a binary parameter for ip
calc_scores$bin_ip <- ifelse(calc_scores$sum_ip >= ip_cut_off, 1, 0)

#calculating mean and sd of ratios
mean(calc_scores$sum_ip, na.rm = TRUE)
sd(calc_scores$sum_ip, na.rm = TRUE)

## Removing NANs ##

calc_scores <- na.omit(calc_scores)

print(calc_scores)

# Check for NANs
has_nan <- any(is.nan(as.matrix(calc_scores)))
print(has_nan)

# Print length
print(nrow(calc_scores))

# Save calc_scores df
write.csv(calc_scores, "/work/JNB exam/calc_scores.csv", row.names=TRUE)

