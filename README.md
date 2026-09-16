# Student BMI Intervention Analysis with Linear Mixed Models

This project evaluates three approaches for preventing excess weight among university students using repeated BMI measurements and linear mixed-effects models.

## Objective

The study follows 1,000 students across six measurement periods and asks whether BMI trajectories differ between three interventions:

1. an information booklet;
2. group sessions, a booklet, and sports support;
3. the second intervention plus a mobile application.

The analysis adjusts for baseline BMI, age, gender, smoking, and physical activity.

## Methods

- exploratory analysis of longitudinal BMI measurements and missingness;
- progressive comparison of six mixed-effects models;
- random intercepts and random time slopes at student level;
- likelihood-ratio tests, AIC, and BIC for model selection;
- Type III tests for fixed effects;
- marginal and conditional predictions;
- residual and Q-Q diagnostic plots.

## Main results

The model with student-specific random intercepts and time slopes was selected. Its AIC was 20,482.23, substantially lower than the random-intercept model's AIC of 25,159.34.

| Effect | Result |
|---|---|
| Time | Significant (`p < 0.001`) |
| Intervention group | Significant (`p < 0.001`) |
| Time × group | Significant (`p < 0.001`) |
| Age | Significant (`p = 0.016`) |
| Physical activity | Significant (`p = 0.009`) |
| Smoking | Not significant (`p = 0.858`) |

BMI trajectories differed significantly across interventions, and the combined intervention with mobile-app support produced the largest BMI reduction.

## Repository structure

```text

├── README.md
├── analysis.R
├── data/
│   └── student_bmi.csv
├── install_packages.R
└── report.pdf
```

The public data file has been renamed to remove the student number, and the exported row-index column has been removed. Student identifiers are synthetic study IDs used to model repeated measurements.

## Reproduce

From the project directory, run:

```r
source("install_packages.R")
source("analysis.R")
```

## Author

Yassine Zeamari — LSTAT2210, *Linear Mixed Models*, UCLouvain (2026).

This project is published for educational and portfolio purposes.


> **Project archive:** Large binary artifacts are available in the [original portfolio folder](https://github.com/yassinedkk/LDAT2M/tree/main/portfolio/student-bmi-mixed-models).
