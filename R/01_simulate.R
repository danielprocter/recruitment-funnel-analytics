# Recruitment Funnel Analytics - Data Simulation
# Author: Daniel Procter
# Date: 18/09/2025
# Purpose: Generate a synthetic dataset of applicants moving through recruitment stages (application -> screening -> interview -> offer -> hire).

# Install and load required packages (install if missing)
packages <- c("tidyverse")
installed <- packages %in% row.names(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
lapply(packages, library, character.only = TRUE)

# Set seed so results are reproducible
set.seed(123)

# Candidate IDs
n_cand <- 2500
cand_id <- sprintf("C%04d", seq(1, n_cand))

# Recruiters (5 in total)
rec_id <- c("R1","R2","R3","R4","R5")

# Job roles available
job_roles <- c("Data Analyst", "HR Specialist", "Software Engineer", "Marketing Associate")

# Funnel stages
stages <- c("Application","Screening","Interview","Offer","Hired")

# Office locations
locations <- c("London", "Manchester", "Bristol", "Edinburgh")

# Candidate sources
sources <- c("LinkedIn", "Job board", "Careers page")

# Distributions for recruiter, job role, location, and source
rec_prob <- c(0.35, 0.15, 0.15, 0.20, 0.15)
job_prob <- c(0.40, 0.30, 0.20, 0.10)
location_prob <- c(0.40, 0.25, 0.15, 0.20)
source_prob <- c(0.50, 0.30, 0.20)

# Base candidate table
df <- tibble(
  cand_id = cand_id,
  rec_id = sample(rec_id, size = n_cand, replace = TRUE, prob = rec_prob),
  job_role = sample(job_roles, size = n_cand, replace = TRUE, prob = job_prob),
  location = sample(locations, size = n_cand, replace = TRUE, prob = location_prob),
  source = sample(sources, size = n_cand, replace = TRUE, prob = source_prob)
)


# Candidate test scores (influenced by source)
df <- df %>%
  rowwise() %>%
  mutate(score = case_when(
    source == "LinkedIn"     ~ rnorm(1, mean = 70, sd = 2.5),
    source == "Job board"    ~ rnorm(1, mean = 55, sd = 15),
    source == "Careers page" ~ rnorm(1, mean = 60, sd = 12)
  )) %>%
  ungroup() %>%
  mutate(score = pmin(pmax(round(score), 0), 100))


# Funnel progression
# Everyone has applied
df <- df %>%
  mutate(application = 1)

# Screening probability (based on score, with small adjustment for source)
df <- df %>% 
  mutate(prob_screening = case_when(
    score < 40 ~ 0.10,
    score <= 70 ~ 0.70,
    score > 70 ~ 0.90) +
      case_when(source == "LinkedIn" ~ 0.05,
                source == "Job board" ~ -0.10,
                TRUE ~ 0))

# Screening outcome (1 = passed, 0 = failed)
df <- df %>%
  mutate(screening = ifelse(runif(n()) < prob_screening, 1, 0))

# Interview probability (only if screened, based on score)
df <- df %>%
  mutate(prob_interview = ifelse(screening == 1,
                                 case_when(score < 40 ~ 0.05,
                                           score <= 70 ~ 0.40,
                                           score > 70 ~ 0.65),
                                 NA_real_))

# Interview outcome
df <- df %>%
  mutate(interview = ifelse(!is.na(prob_interview) & runif(n()) < prob_interview, 1, 0))

# Offer probability (only if interviewed, based on score)
df <- df %>%
  mutate(prob_offer = ifelse(interview == 1,
                             case_when(score < 40 ~ 0.02,
                                       score <= 70 ~ 0.25,
                                       score > 70 ~ 0.35),
                             NA_real_))

# Offer outcome
df <- df %>%
  mutate(offer = ifelse(!is.na(prob_offer) & runif(n()) < prob_offer, 1, 0))

# Hire probability (only if offered, depends on recruiter)
df <- df %>%
  mutate(prob_hired = ifelse(offer == 1,
                             case_when(rec_id == "R2" ~ 0.60,  # weaker closer
                                       rec_id == "R4" ~ 0.95,  # strong closer
                                       TRUE ~ 0.80),
                             NA_real_))

# Hire outcome
df <- df %>%
  mutate(hired = ifelse(!is.na(prob_hired) & runif(n()) < prob_hired, 1, 0))

# Removing probability columns
df_clean <- df %>%
  select(-starts_with("prob_"))

# Saving clean dataset
write_csv(df_clean, "../data/simulated_funnel_data.csv")
