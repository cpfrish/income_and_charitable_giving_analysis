# Income & Charitable Giving — Are Higher Earners More Generous?

An OLS regression study of the relationship between income and the *proportion*
of income given to charity, testing the popular "U-shaped generosity"
hypothesis. Final project for UC Berkeley MIDS w203 (Statistics for Data
Science).

**Authors:** Fatema Alsaleh · Colin Frishberg · WooJung Kim

📄 **Read the final report:** [`reports/Lab2_Assignment_final.pdf`](reports/Lab2_Assignment_final.pdf)

## Question & data

Research agrees richer households give more *dollars*, but is inconsistent on
whether they give a larger *share*. We model charitable-giving proportion
against Adjusted Gross Income (AGI) using county-aggregated **2022 IRS
Statistics of Income** data.

## Method

- OLS with linear, quadratic, and logarithmic AGI specifications
- Robust standard errors (`sandwich` + `lmtest`); full CLM assumption diagnostics
- Regression tables via `stargazer`; reproducible environment via `renv`

## Result

The data reject the U-shape: we find a **J-shaped positive relationship** —
giving proportion grows non-linearly with mean AGI. The preferred model (AGI
terms plus dividend, capital-gains, and rent/royalty income) explains **78.5%
of the variance** in charitable contributions per return (adjusted R² = 0.785).

## Repository layout

- `reports/` — final report (Quarto source + rendered PDF), references
- `notebooks/` — per-author modeling notebooks (`modeling_*.Rmd`)
- `data/` — raw and processed IRS SOI extracts
- `peer_review/` — course peer-review artifacts

## Reproduce

```r
# in the project root
renv::restore()
# then render reports/Lab2_Assignment_final.qmd with Quarto
```
