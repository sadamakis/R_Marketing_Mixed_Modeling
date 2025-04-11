# Marketing Mixed Modeling using R
This repository contains implementations of marketing mixed modeling codes for various tasks, which optimize marketing campaigns. 

## Introduction

This repository contains a collection of R scripts designed for Marketing Mix Modeling (MMM) to optimize marketing campaigns.

**The toolkit comprises the following key components:**

1.  **Time Series Modeling:** Accounts for seasonality and adstock (carryover effect of advertising). The following model variations are included in separate scripts:

    * **`Marketing_Mixed_Modeling - Method 1.R`:**
        * Model formula: `y ~ x + t + s`
        * Seasonality is modeled using binary variables for each month.

    * **`Marketing_Mixed_Modeling - Method 2 - Residual Regression.R`:**
        * **Decomposition:** Time series decomposition using `y ~ t + s`.
        * **Regression:** Independent variables are regressed on the residuals: `y.res ~ x + t + s`.

    * **`Marketing_Mixed_Modeling - Method 3 - Residual Decompose.R`:**
        * **Decomposition:** Time series decomposition using the `decompose()` function in R.
        * **Regression:** Independent variables are regressed on the residuals (random component of the decomposition): `y.res ~ x + t + s`.

2.  **Stationarity and Seasonality Tests:** Scripts to assess the time series data for stationarity and the presence of seasonality.

3.  **Collinearity Checks:** Tools to identify multicollinearity among independent variables using correlation analysis and Variance Inflation Factor (VIF).

4.  **Linear Regression and Optimization:** Scripts to fit linear regression models and optimize rate parameters (e.g., adstock decay rates).

5.  **Model Performance Evaluation:** Functions and scripts to evaluate the performance of the fitted MMM models using relevant metrics.

6.  **Contribution Reports:** Generation of reports to assess the contribution of each component within the model (e.g., individual marketing channels, seasonality, trend).

7.  **ROI Calculation:** Scripts to calculate the Return on Investment (ROI) for marketing activities based on the model results.

## Key Contents

* **src:** Codes that execute the MMM development.

## Purpose

This repository serves as a collection of practical implementations and examples demonstrating different approaches to MMM problems. It can be used for:

* **Learning and understanding:** Providing clear and concise code examples for developing MMMs.
* **Experimentation:** Offering a platform to test and compare different algorithms on various datasets.
* **Reference:** Serving as a quick reference for implementing common machine learning tasks.

## Contributions

Contributions, including bug fixes, new implementations, and improvements to existing code, are welcome! Please refer to the [CONTRIBUTING.md](CONTRIBUTING.md) file for guidelines.

## License

Your use of the SAS_Scorecard_Suite repository is governed by the terms and conditions outlined in the [LICENSE.md](LICENSE.md) file of this repository. By proceeding, you acknowledge and agree to these terms.

**Keywords:** time series models, marketing mixed models, adstock, optimization, model decomposition, Fourier transform, ROI, R.
