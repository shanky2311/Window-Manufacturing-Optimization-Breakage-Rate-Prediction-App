# 🪟 Window Breakage Rate Optimization App

This is a Shiny dashboard app built to **predict and minimize glass window breakage rates** in a manufacturing process. The application combines machine learning, data preprocessing, and UI interactivity to support business decisions.

## 🚀 Features
- **Descriptive Analytics**: View summaries, trends, and correlations in manufacturing data.
- **Predictive Analytics**: Predict breakage rate using a linear regression model.
- **Prescriptive Analytics**: Optimize key machine settings to reduce window breakage.

## 📦 Dataset
- File: `Window_Manufacturing.xlsx`
- Columns include: Breakage Rate, Window Size, Glass Thickness, Ambient Temp, Cut Speed, Edge Deletion Rate, Supplier Info, and more.

## 🔧 Methods & Techniques
- Missing value imputation using `mice` (CART).
- Dummy variable creation for categorical features.
- Correlation analysis to drop highly correlated and linear combination features.
- Normalization via `caret::preProcess`.
- Linear regression model with RMSE calculation for model evaluation.
- Optimization of input parameters via `optim()`.

## 📊 Libraries Used
- **UI/UX**: `shiny`, `shinydashboard`, `DT`
- **EDA & Plots**: `ggplot2`, `reshape2`
- **Modeling**: `caret`, `mice`
- **Optimization**: `optim()` (L-BFGS-B method)

## 🖥️ How to Run
1. Install R and required packages.
2. Clone this repo:
   ```bash
   git clone https://github.com/yourusername/window-breakage-prediction-app.git
   cd window-breakage-prediction-app
