# Problem Set 1 — Predicting Income  
**Big Data & Machine Learning for Applied Economics**  
Universidad de los Andes — 2022-2  

This repository contains the solution to **Problem Set 1: Predicting Income**, using the **GEIH 2018 Bogotá** sample.

---

## 🎯 Objective

Construct and evaluate models to predict **individual income** using observed characteristics:

- Restrict the sample to **employed individuals aged 18+**
- Estimate and interpret econometric specifications (age–earnings profile, gender gap)
- Evaluate predictive performance (train/test, additional models, influence statistics, LOOCV)

---

## 📁 Repository Structure

```
├── document/                  # Contains the final document in PDF format
├── scripts/                   # R / RMarkdown scripts for scraping, cleaning, and modeling
├── stores/                    # Datasets used (.rds / .csv format)
├── views/                     # Exported tables and figures
├── GEIH_2018_4.rds            # Main dataset used in the analysis
├── Problem_Set_1.pdf          # Final report
├── Problem_Set_1.R            # Main script (end-to-end workflow)
├── .gitignore
└── README.md
```

---

## 🧪 What we did

- Scraped/downloaded the GEIH 2018 Bogotá sample data from the course website.
- Filtered the sample to employed individuals aged 18+.
- Performed feature engineering (recoding variables and creating model-ready features).
- Handled missing data using documented rules (dropping, recoding, and/or imputation depending on variable).
- Estimated an age–earnings profile with a quadratic specification and visualized the implied profile.
- Used bootstrap resampling to compute confidence intervals (including for peak age).
- Replicated the conditional gender gap using the Frisch–Waugh–Lovell (FWL) decomposition and bootstrapped standard errors.
- Split data into training (70%) and testing (30%) sets and compared predictive performance across models.
- Computed LOOCV errors for top-performing models and compared against test-set performance.
- Computed influence statistics on the test sample to identify observations with high prediction impact (potential anomalies).

---

## 📦 Packages used (from `Problem_Set_1.R`)

- boot
- car
- caret
- cowplot
- pacman
- rvest
- skimr
- stargazer
- tidyverse

---

## 🔁 Reproducibility

1. Open `Problem_Set_1.R` (main script).
2. Ensure any required data files are available (see `stores/` and `GEIH_2018_4.rds`).
3. Run the script from top to bottom to reproduce:
   - data acquisition (scraping)  
   - cleaning + sample restriction  
   - descriptive outputs (tables/figures saved under `views/`)  
   - model estimation + inference  
   - prediction exercises and validation

The final report is included as `Problem_Set_1.pdf`.

---

## 📄 Final Deliverable

The final report (`Problem_Set_1.pdf`) includes:

- Cleanly formatted tables and figures  
- Interpretation of results  
- Model comparisons  
- Discussion of predictive performance  

---

## 📜 License

MIT License
