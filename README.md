# Recruitment Funnel Analytics (Simulated Data)

This project simulates a dataset of **2,500 job applicants** progressing through a recruitment funnel:

**Application to Screening to Interview to Offer to Hired**

The project includes data simulation, exploratory analysis, and predictive modelling of hiring outcomes.

---

## Project Structure

- **Simulation script**  
  `R/simulate.R`

- **Analytical report (source)**  
  `R/report.Rmd`

- **Rendered report (local)**  
  `report/report.html`

- **Rendered report (GitHub Pages)**  
  `docs/report.html`

---

## Purpose

To provide a realistic, end-to-end example of recruitment funnel analytics, including:

- Funnel drop-off rates across stages  
- Recruiter-level performance differences  
- Role, location, and source comparisons  
- Impact of candidate test scores on progression and hiring  
- Predictive modelling (logistic regression, decision tree, random forest)

---

## How to Use

1. **Regenerate the simulated data**
   ```r
   source("R/simulate.R")
