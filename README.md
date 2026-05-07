# 📊 Financial Literacy & Household Indebtedness: A Methodological Pilot
### *Econometric Identification & Policy Simulation Infrastructure*

## 📝 Project Overview
This repository contains the **Pilot Study** phase of an ongoing research project investigating the causal link between financial literacy and household indebtedness in Canada. Using provincial-level aggregates ($N=49$), this phase focuses on building a robust, reproducible pipeline to handle endogeneity and small-sample bias.

## 📂 Project Structure
- **01-04_Scripts.R**: Data acquisition, cleaning, and econometric modeling (OLS, IV, Firth-Logit).
- **05_shiny_app.R**: Interactive Policy Simulator for fiscal ROI testing.
- **Master-Orchestrator.R**: Single-point execution script for full pipeline reproduction.
- [cite_start]**outputs/**: Automated generation of LaTeX tables, diagnostic plots, and logs[cite: 423].

## 🧪 Pilot Study Methodology
1. [cite_start]**Causal Identification**: Utilizing **Two-Stage Least Squares (2SLS)** and **LIML** to address endogeneity (Wu-Hausman $p < 0.001$)[cite: 421].
2. [cite_start]**Probabilistic Risk**: Modeling "Critical Over-Indebtedness" via **Firth-Robust Logistic Regression** to mitigate small-sample separation[cite: 413, 417].
3. [cite_start]**Policy Simulation**: A Monte Carlo-based dashboard to estimate the impact of literacy interventions on provincial debt risk[cite: 413].

## ⚠️ Current Findings & Pilot Limitations
- [cite_start]**Sample Power**: Current provincial aggregates ($N=49$) show a near-zero marginal effect, suggesting the need for higher-granularity data.
- [cite_start]**Identification**: Identification is confirmed, though instruments show borderline weakness ($p=0.065$), addressed via LIML estimators[cite: 421].
- **Roadmap**: The next phase will transition from provincial aggregates to **household-level microdata** to increase statistical power and resolve latent protective effects.

## 👤 Author
**Romuald Guédé, **
[GitHub: romualdguede](https://github.com/romualdguede)