# Sleep Health Data Regression Analysis

## Overview
This project was developed as part of a statistical modeling course at Ben-Gurion University. The goal was to analyze sleep health metrics, build linear regression models, and manually derive OLS estimators to verify software outputs.

## Tools & Technologies
- **Language:** R
- **Packages:** dplyr

## Key Analysis Steps
- Built simple and multiple linear regression models.
- Performed hypothesis testing (t-test, joint Wald test).
- Conducted model diagnostics and analyzed interaction effects.

## Results

The analysis used 374 complete observations and modeled sleep duration (hours) using physical activity, age, stress level, sleep quality, and resting heart rate.

### Multiple linear regression

The five-predictor model explained 80.25% of the observed variation in sleep duration (R² = 0.8025; adjusted R² = 0.7998). Holding the other included predictors constant:

| Predictor | Estimate | p-value | Interpretation |
| --- | ---: | ---: | --- |
| Physical activity (per minute/day) | 0.001555 | 0.122 | The estimated association was positive, but was not statistically significant at the 0.05 level. Its 95% confidence interval included zero (approximately −0.000418 to 0.003528 hours per activity minute). |
| Age (per year) | −0.010507 | 0.0000215 | Higher age was associated with slightly shorter sleep duration. |
| Stress level (per scale point) | −0.077579 | 0.00231 | Higher stress was associated with shorter sleep duration. |
| Sleep quality (per scale point) | 0.575852 | < 2 × 10⁻¹⁶ | Higher sleep-quality scores were strongly associated with longer sleep duration. |
| Resting heart rate (per BPM) | 0.026875 | 0.0000384 | Higher resting heart rate was associated with longer sleep duration in this fitted model. |

A joint Wald test rejected the null that stress level and resting heart rate were both unrelated to sleep duration. The residual plots were reported as broadly consistent with linearity and constant variance, with no prominent influential outliers.

### Robustness check: activity by age

Adding a physical-activity-by-age interaction produced a statistically significant interaction term (p < 0.001), indicating that the activity–sleep association differs by age in this model. The main activity coefficient in an interaction model is conditional on the age reference value used in the model.

### Interpretation and limitations

In the model without an interaction, physical activity was not independently associated with sleep duration at the 0.05 significance level after adjustment for the other predictors. This is an observational analysis, so the estimates describe conditional associations and do not establish that changing activity, stress, sleep quality, or heart rate causes a change in sleep duration. The dataset includes 374 observations and a limited set of covariates; unmeasured factors and reverse causality may remain.
