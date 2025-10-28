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

# Recruiters
rec_id <- c("R1","R2","R3","R4","R5")

# Job roles
job_roles <- c("Data Analyst", "HR Specialist", "Software Engineer", "Marketing Associate")

# Funnel stages
stages <- c("Application","Screening","Interview","Offer","Hired")

# Candidate sources
sources <- c("LinkedIn", "Job board", "Careers page")

# Candidate overall distributions for recruiter
rec_prob <- c(0.24, 0.19, 0.18, 0.21, 0.18)
names(rec_prob) <- rec_id

# Candidate source distributions for each recruiter
rec_source_probs <- list(
  R1 = c("LinkedIn" = 0.70, "Job board" = 0.20, "Careers page" = 0.10),
  R2 = c("LinkedIn" = 0.55, "Job board" = 0.30, "Careers page" = 0.15),
  R3 = c("LinkedIn" = 0.45, "Job board" = 0.35, "Careers page" = 0.20),
  R4 = c("LinkedIn" = 0.30, "Job board" = 0.55, "Careers page" = 0.15),
  R5 = c("LinkedIn" = 0.50, "Job board" = 0.20, "Careers page" = 0.30)
)

# Role distributions for each recruiter
rec_role_probs <- list(
  R1 = c("Data Analyst" = 0.35, "HR Specialist" = 0.15, "Software Engineer" = 0.35, "Marketing Associate" = 0.15),
  R2 = c("Data Analyst" = 0.20, "HR Specialist" = 0.40, "Software Engineer" = 0.20, "Marketing Associate" = 0.20),
  R3 = c("Data Analyst" = 0.30, "HR Specialist" = 0.25, "Software Engineer" = 0.25, "Marketing Associate" = 0.20),
  R4 = c("Data Analyst" = 0.25, "HR Specialist" = 0.20, "Software Engineer" = 0.30, "Marketing Associate" = 0.25),
  R5 = c("Data Analyst" = 0.35, "HR Specialist" = 0.10, "Software Engineer" = 0.40, "Marketing Associate" = 0.15)
)

# Base candidate table
df <- tibble(
  cand_id = cand_id,
  rec_id  = sample(rec_id, size = n_cand, replace = TRUE, prob = rec_prob)
)

# Sample source and role within recruiter
df <- df %>%
  group_by(rec_id) %>%
  mutate(source = sample(names(rec_source_probs[[first(rec_id)]]),
                      size = n(), replace = TRUE,
                      prob = unname(rec_source_probs[[first(rec_id)]])),
         job_role = sample(names(rec_role_probs[[first(rec_id)]]),
                      size = n(), replace = TRUE,
                      prob = unname(rec_role_probs[[first(rec_id)]]))) %>%
  ungroup()

# Candidate test scores (influenced by source)
df <- df %>%
  rowwise() %>%
  mutate(score = case_when(
    source == "LinkedIn"     ~ rnorm(1, mean = 70, sd = 12),
    source == "Job board"    ~ rnorm(1, mean = 55, sd = 12),
    source == "Careers page" ~ rnorm(1, mean = 60, sd = 12)
  )) %>%
  ungroup() %>%
  mutate(score = pmin(pmax(round(score), 0), 100))

# Funnel progression
# Everyone has applied
df <- df %>% mutate(application = 1)

# Screening probability (based on score, small adjustment for source)
df <- df %>% 
  mutate(prob_screening = case_when(
    score < 40  ~ 0.10,
    score <= 70 ~ 0.70,
    score > 70  ~ 0.90) + 
      case_when(source == "LinkedIn"  ~  0.05,
                source == "Job board" ~ -0.10,
                TRUE ~  0.00))

# Screening outcome
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
                             case_when(rec_id == "R1" ~ 0.95, # very strong closer
                                       rec_id == "R2" ~ 0.80, # strong closer
                                       rec_id == "R3" ~ 0.60, # average closer
                                       rec_id == "R4" ~ 0.45, # weak closer
                                       rec_id == "R5" ~ 0.35), # very weak closer
                             NA_real_))

# Hire outcome
df <- df %>%
  mutate(hired = ifelse(!is.na(prob_hired) & runif(n()) < prob_hired, 1, 0))

# Remove probability columns
df_clean <- df %>%
  select(-starts_with("prob_"))

# Save dataset
write_csv(df_clean, "data/simulated_funnel_data.csv")
