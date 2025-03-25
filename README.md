# 🚗 Car Insurance Pricing Using Data Science

## Project Overview

This project applies advanced data science techniques to model and estimate automobile insurance premiums. Given the uncertainty involved in insurance pricing, especially when predicting claims costs and claim frequency, statistical learning methods provide robust tools to predict accurately and effectively.

The goal is to calculate the pure premium, considering drivers' profiles and historical data.

## 📚 Project Structure

### 1. Data Management
- **Data Dictionary:** Clear definition of variables used.
- **Initial Data Exploration:** Basic statistics and visual exploration.
- **Data Cleaning:** Correcting inconsistencies, missing values, and categorization.
- **Feature Engineering:** Creating meaningful features for better predictions.

### 2. Modeling the Average Claim Cost
- **Regression Models:**
  - Log-normal Regression
  - Gamma Regression
- **Penalized Regression Models:**
  - Ridge Regression (Selected Model ✅)
  - Elastic Net Regression
- **Statistical Learning Models:**
  - CART
  - Random Forest
  - XGBoost
  
The best-performing model based on Root Mean Squared Error (RMSE) was **Ridge Regression**.

### 3. Classification: Predicting Claim Occurrence
- **Logistic Regression**
- **Stepwise Regression**
- **Penalized Regression:** Ridge, Lasso, Elastic Net
- **Naive Bayes Classifier**
- **Decision Trees:** CART and Random Forest
- **Gradient Boosting:** XGBoost (Selected Model ✅)

The best-performing classification model based on precision and AUC (Area Under the Curve) was **XGBoost**.

## 🚩 Key Results
| Model Type | Selected Model | Performance Metric | Result |
|------------|----------------|--------------------|--------|
| Regression | Ridge Regression | RMSE | Lowest among tested |
| Classification | XGBoost | Accuracy & AUC | Highest accuracy (77.32%) and AUC (73.27%) |

## 🔧 Tools & Technologies
- **R** (XGBoost, glmnet, CART, Random Forest)

## 📌 Conclusion
This project highlights the effectiveness of combining classical statistical methods and modern machine learning approaches for insurance pricing. It underlines the importance of continuous feature engineering, exploratory data analysis, and methodical model comparison to optimize predictive performance.

**Important:** A good model is always enhanced by domain knowledge, expert experience, and careful human judgment.

