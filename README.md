# Case-Study---Online-Sales-Platform-Data-Insights
Main Tasks:

**SQL**: Data collation, merging and extraction

**R**: Data Cleaning, Merging External Data, Multiple Imputation by Chained Equations, Statistical Analysis, One-to-One Relationship Analysis, Visualization, Linear Regression Modeling


This project mainly uses SQL and R for analysis, focuses on analyzing beachwear sales performance and identifying key factors influencing conversion rates.


## 📝 Objectives  
We aimed to answer the research question:  

> **What independent variables affect the conversion rate in the beachwear category at Wehkamp?**  

Key analysis areas:  
- **WHO**: Demographics  
- **WHAT**: Product and Behavior  
- **WHEN**: Time  
- **WHERE**: Geography  

---

## 📊 Data Sources  
1. **Internal Wehkamp Data**:  
   - Clickstream, customer sessions, product views, and order details.  
2. **External Data**:  
   - Weather Data (KNMI)  
   - COVID-19 Cases (Johns Hopkins CSSE)  
   - Google Trends Data (Wehkamp search interest).  

---

## ⚙️ Tools and Libraries  
### **Programming Languages**:  
- **R**: Data Analysis, Visualization, and Regression Analysis  
- **SQL**: Data Extraction and Cleaning  

### **Libraries**:  
- `dplyr`, `ggplot2`, `lubridate`, `mice`, `car`, `bit64`, `RPostgres`, `pscl`, `gmodels`  

---

## 🔍 Key Analyses  
1. **Data Integration**:  
   - Merged internal Wehkamp data with external weather, COVID-19, and Google Trends data.  
2. **Data Cleaning**:  
   - Imputed missing values using MICE.  
   - Outlier detection and removal using Mahalanobis Distance.  
3. **Statistical Analysis**:  
   - Chi-square tests for categorical variables.  
   - Logistic and Linear regression models to identify significant factors.  
4. **Visualization**:  
   - Created charts, scatter plots, and trends to communicate findings.  

---

## 🚀 Project Highlights  
- Analyzed **500,000+ records** to derive actionable insights.  
- Identified key drivers of sales conversions:  
   - **Weather (mean temperature)** correlates positively with sales.  
   - **Income, age, and education** significantly impact conversion rates.  
   - **Excessive product views** negatively correlate with conversions.  
- Built predictive models to identify and explain significant features.  

---

## 📁 Repository Contents  
- **`R Code.R`**: R scripts for data cleaning, analysis, and visualization.  
- **`SQL code.ipynb`**: SQL queries for data extraction and integration.  
- **`Final_Report.pdf`**: Final report detailing the methodology, analysis, and recommendations.  
- **`assignment requirement.pdf`**: Project instructions and requirements.  
