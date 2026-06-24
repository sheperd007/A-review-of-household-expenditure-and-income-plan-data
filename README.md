# Household Income Decile Classification (Iran, 1398 / 2019–20)

Predicting whether an Iranian urban household falls into a low- or high-income economic
decile from national **Household Expenditure & Income Survey** data, to support
subsidy-eligibility decisions. An end-to-end R pipeline that cleans and imputes raw
survey data, engineers an income target, balances the classes, selects features with a
random forest, and benchmarks four classifiers — **logistic regression, decision tree,
multi-layer perceptron, and k-nearest neighbours**.

**Tech stack:** R · tidyverse · randomForest · rpart · neuralnet · class (kNN) · caret · ROSE · pROC

---

## Overview

Under Iranian subsidy-reform law, cash subsidies are withheld from the upper income
deciles and targeted at the lower ones. This project frames that policy as a **binary
classification** problem on the official *Household Expenditure and Income Plan* survey
for the fiscal year **1398** (Iranian calendar; roughly 2019–2020), covering urban
households in the provinces of **Hamedan, Ilam, Kermanshah, and Kurdistan**.

Two complementary targets are derived from total household income and modelled
separately:

- **`upquantile`** — household above the **70th income percentile** (high-income).
- **`downquantile`** — household below the **30th income percentile** (low-income, i.e.
  the subsidy-eligible group).

Raw survey records group household information into four areas:

- Social characteristics of household members
- Dwelling, facilities, and major durable goods
- Food and non-food expenditure
- Household income

The shipped dataset contains **68 variables**; the study population is roughly **2,188
urban households** after restricting to the four provinces of interest.

## What's inside

| Path | Description |
|------|-------------|
| `code.R` | Full analysis pipeline: cleaning, imputation, feature engineering, class balancing, random-forest feature selection, and the four classifiers (logistic regression, decision tree, MLP, kNN) with ROC comparison. |
| `DataHD98_6.csv` | Raw survey extract (68 columns) for the selected provinces. |
| `معرفي فايل خام هزينه و درامد 98.pdf` | Official documentation describing the raw expenditure/income data file. |
| `پرسشنامه طرح هزينه و درامد خانوارسال 98.pdf` | The survey questionnaire for the 1398 household plan. |
| `راهنما.docx` | Supplementary guide / notes. |
| `LICENSE` | Apache License 2.0. |

## Methods / Approach

The pipeline in `code.R` proceeds as follows:

1. **Data cleaning** — drop columns with more than 40% missing values and zero-variance
   columns that carry no information.
2. **Imputation** — domain-aware fixes for education fields, mode imputation by group
   for categorical variables (e.g. employment type, building material), zero-fill for
   structural missings (subsidy receipt), and **random-forest imputation** for the
   remaining continuous variables; negative free-income values are corrected.
3. **Target engineering** — build total household income (`Daramad`) from wage,
   miscellaneous, and subsidy components, then derive the 70th- and 30th-percentile
   binary labels.
4. **Exploratory analysis** — density plots of income across employment, education, and
   household-status groups.
5. **Train / test / validation split** — 70% train, with the remainder split evenly into
   test and validation sets.
6. **Class balancing** — oversampling of the minority class with **ROSE** (`ovun.sample`).
7. **Feature selection** — a **random forest** ranks features by mean-decrease-in-Gini;
   variables above an importance threshold are retained.
8. **Modelling & comparison** — four classifiers are trained and tuned on the selected
   features:
   - **Logistic regression** (`glm`, binomial/logit)
   - **Decision tree** (`rpart`, with a grid over `minsplit` / `minbucket` / `maxdepth`)
   - **Multi-layer perceptron** (`neuralnet`, range-scaled inputs, several hidden-layer
     sizes)
   - **k-nearest neighbours** (`class::knn`, swept over a range of *k*)
9. **Evaluation** — confusion matrices via `caret` and **ROC / AUC** curves
   (`pROC`) overlaying all four models for the 70th-percentile target.

## How to run

The pipeline is a single R script. Install the required packages, point the script at the
CSV, and run it.

```r
install.packages(c(
  "readr", "tidyverse", "plyr", "dplyr", "imputeMissings",
  "ggplot2", "hrbrthemes", "viridis", "ROSE", "pROC",
  "randomForest", "caret", "rpart", "rpart.plot", "neuralnet", "class"
))
```

Update the data path near the top of `code.R` to your local copy:

```r
# in code.R
Data <- as.data.frame(read_csv("DataHD98_6.csv"))
```

Then run the script:

```bash
Rscript code.R
```

or open `code.R` in RStudio and run it interactively to inspect the plots
(density plots, the decision tree, and the ROC comparison).

## Results

The script trains all four classifiers and compares them with confusion matrices and
overlaid ROC/AUC curves for the high-income (70th-percentile) target, plus separate
runs for the low-income (30th-percentile) target. Metrics are computed at runtime from
the train/test/validation splits; re-run `code.R` to reproduce the confusion matrices
and ROC plot. No fixed metric values are stored in this repository.

## Data source

Household Expenditure and Income Survey, fiscal year **1398** (Statistical Centre of
Iran), urban households of Hamedan, Ilam, Kermanshah, and Kurdistan provinces. See the
included PDFs for the official raw-file documentation and the survey questionnaire.

## License

Released under the **Apache License 2.0** — see [`LICENSE`](LICENSE).
