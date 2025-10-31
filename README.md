# Business Intelligence System for Altadis Imperial Brands Distribution Channel

## Project Overview
This Master's Thesis project involved the design and development of an end-to-end Business Intelligence (BI) system for **Altadis Imperial Brands**, a leading tobacco company. The goal was to transform raw operational data into strategic insights to optimize their distribution channel management.

The project covers the full BI cycle: data extraction, cleaning, exploratory analysis, predictive modeling, customer segmentation, and interactive dashboard creation.

## 📊 Business Objectives
- Analyze sales and delivery patterns across the distribution network.
- Predict weekly product demand to optimize logistics and inventory.
- Segment retail points for targeted commercial strategies.
- Identify key geographical and product performance indicators.

## 🛠️ Tools & Technologies
- **Data Analysis & Modeling:** R (tidyverse, forecast, cluster)
- **Data Visualization:** Power BI
- **Data Processing:** Excel, Power Query
- **Methods:** ETL, EDA, Time Series Forecasting (ARIMA), Clustering (K-Means)

## 📁 Project Structure
TFM-ImperialBrands/
├── 📄 README.md # Project overview (this file)
├── 📄 Trabajo Fin de Máster.pdf # Full thesis document (Spanish)
└── 📁 project/ # All code and dashboard files
├── 📄 01_data_cleaning.R # Data preprocessing and ETL
└── 📄 dashboard.pbix # Power BI interactive dashboards

## 🔍 Key Results & Insights

### Predictive Modeling
- **ARIMA Model for Deliveries:** Achieved **90% forecast accuracy** (MAPE: 10.2%) in predicting weekly delivery volumes, providing a reliable basis for logistics planning.

### Customer Segmentation
- **K-Means Clustering:** Identified **3 distinct store clusters** based on sales performance, stock-outs, and engagement levels:
  - **Cluster 1:** High-performance stores (high sales, high engagement)
  - **Cluster 2:** Medium-performance stores with growth potential
  - **Cluster 3:** Low-performance stores requiring strategic review

### Data Visualization
- **Interactive Power BI Dashboards:** Created two comprehensive dashboards for:
  - Sales performance analysis by province, zone, and product type
  - Product portfolio performance and geographical distribution

## 📈 How to Navigate This Repository
1.  **R Script File:** Code used on this project.
2.  **Power BI Files:** Open `.pbix` files with Power BI Desktop to explore the interactive dashboards.

## 🎯 Business Impact
The insights generated from this BI system enable:
- **Improved Demand Planning:** Using delivery predictions to optimize inventory levels
- **Personalized Commercial Strategies:** Tailoring approaches based on store segments
- **Geographical Optimization:** Identifying high-potential regions and underperforming areas
- **Product Portfolio Management:** Adjusting product offerings based on consumption patterns
