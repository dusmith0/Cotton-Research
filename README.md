# 🌾 Climate Stress & Early-Stage Modeling on West Texas Cotton Abandonment

![R](https://img.shields.io/badge/Language-R-blue.svg)
![Machine Learning](https://img.shields.io/badge/Model-Random%20Forest-green.svg)
![GLMnet](https://img.shields.io/badge/Method-Quasi--Binomial%20Elastic%20Net-orange.svg)
![Validation](https://img.shields.io/badge/Validation-Sliding--Window%20CV%20(42--Fold)-purple.svg)

An applied spatio-temporal machine learning and econometric study evaluating whether early-season climate stressors (May–June) can reliably predict regional cotton abandonment in West Texas without sacrificing model accuracy compared to full-season aggregates.

---

## 📌 Executive Summary

Crop abandonment in the Northern High and Low Plains of West Texas averages **21.62% annually** (reaching extremes up to **92.14%** in Terry County in 2022), compared to a U.S. national agricultural crop average of just 1–3%. 

This project evaluates the predictive utility of disaggregating seasonal climate stressors across three critical crop growth stages—**First Flower (May–June)**, **Last Flower (July–August)**, and **Last Boll (September–October)**—against full-season aggregates using historical data from 1968 to 2025 across 8 high-producing Texas counties[cite: 1].

### Key Findings
* **Non-Linear Superiority:** Non-linear Random Forest significantly outperformed regularized parametric models, dropping the prediction error (**RMSE from 0.2221 down to 0.1605**—a **27.7% improvement in loss**)[cite: 1].
* **Early-Warning Viability:** Paired Wilcoxon Signed-Rank tests proved that using early-season climate metrics (Season 1) incurs **no statistically significant loss in predictive accuracy** compared to full-season aggregate models ($p = 0.230$)[cite: 1].
* **Local Dominance:** LASSO feature selection ($\alpha = 0.9$) demonstrated that **County Fixed Effects strongly overpowered individual climate variables**, highlighting that local groundwater depletion, soil management, and financial assets play a major role alongside climate stress[cite: 1].

---

## 🛠️ Data & Feature Engineering

The dataset combines panel weather observation data from NOAA and agricultural yield/abandonment statistics from USDA-NASS spanning 1968–2025 across 8 core cotton-producing counties (Hale, Crosby, Lamb, Gaines, Lubbock, Randall, Dawson, Terry)[cite: 1].

* **Target Variable:** Total Abandonment Percentage continuous and bounded $y \in [0, 1]$[cite: 1].
* **Growth Stages Disaggregated:**
  * **Season 1 (S1):** First Flower (May – June)[cite: 1]
  * **Season 2 (S2):** Last Flower (July – August)[cite: 1]
  * **Season 3 (S3):** Last Boll (September – October)[cite: 1]
* **Engineered Climate Stressors:**
  * Cumulative Growing Degree Days (GDD) & Daily GDD Departures[cite: 1]
  * Extreme Heat Days ($T_{MAX} \ge 102^\circ\text{F} / 39^\circ\text{C}$)[cite: 1]
  * Precipitation ($\text{PRCP}$), Temperature Extremes ($\text{TMAX}, \text{TMIN}$), and Hail Frequency ($\text{WT05}$)[cite: 1]
  * Hydro-thermal interaction terms ($\text{PRCP} \times \text{TMAX}$)[cite: 1]

---

## 📐 Methodological Workflow & Validation

To respect the temporal autocorrelation of historical climate data and prevent data leakage, models were evaluated using a custom **42-fold sliding-window cross-validation pipeline** (10-year training window shifted annually to predict 1-year forward test windows)[cite: 1].
### Analytical & Modeling Methods
1. **Trend & Interaction Significance:** Non-parametric **Mann-Kendall Tests** combined with **False Discovery Rate (FDR)** alpha-inflation controls to establish significant long-term climate shifts[cite: 1].
2. **Parametric Benchmarks:** Regularized **Quasi-Binomial GLMs (Elastic Net, $\alpha=0.3$ & $\alpha=0.9$)** with Logit link functions to handle continuous bounded proportion target data while controlling extreme VIF multicollinearity (VIFs up to 67.45)[cite: 1].
3. **Non-Parametric Predictive Modeling:** **Random Forests (`ranger`)** to capture non-linear tipping points, cubic curves, and multi-variable climate interactions[cite: 1].
4. **Statistical Hypothesis Testing:** Non-parametric **Paired Wilcoxon Signed-Rank Tests** with Bonferroni p-value corrections ($\alpha = 0.0125$) across cross-validation folds[cite: 1].

---

## 📊 Performance Comparison

All models were evaluated across multiple loss metrics calculated on out-of-fold predictions[cite: 1]:

| Model | RMSE | Weighted RMSE | Log-Loss | Weighted Log-Loss | Wilcoxon $p$-value vs Baseline | 95% CI (RMSE) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **Total Aggregated (Baseline)** | 0.2221 | 0.2088 | 0.5566 | 0.5422 | — | (0.192, 0.226)[cite: 1] |
| **Seasonal Disaggregated** | 0.2399 | 0.2259 | 0.6870 | 0.6631 | 0.230 | (0.208, 0.243)[cite: 1] |
| **GDD Disaggregated** | 0.2391 | 0.2258 | 0.6597 | 0.6365 | 0.563 | (0.208, 0.243)[cite: 1] |
| **GDD Departure** | 0.2406 | 0.2284 | 0.6959 | 0.6749 | 0.228 | (0.211, 0.246)[cite: 1] |
| **Random Forest** | **0.1605** | **0.1476** | **0.4922** | **0.4754** | **< 0.001** | **(0.148, 0.173)**[cite: 1] |

---

## 💡 Practical Implications

From an agricultural risk management and crop insurance perspective (e.g., USDA-RMA adjusters, agricultural lenders), these results prove that **early-season climate splits (May–June) provide strong early warning indicators for midseason abandonment without incurring a statistically significant accuracy penalty**[cite: 1]. 

Decision-makers do not need to wait for full-season aggregated climate data to estimate regional risk exposure[cite: 1].

---

## 📂 Repository Structure
---

## 🛠️ How to Reproduce

1. **Clone the repository:**
   ```bash
   git clone [https://github.com/your-username/west-texas-cotton-abandonment.git](https://github.com/your-username/west-texas-cotton-abandonment.git)
   cd west-texas-cotton-abandonment

   install.packages(c("tidyverse", "glmnet", "ranger", "betareg", " Kendall", "pROC"))
   source("src/03_models.R")
