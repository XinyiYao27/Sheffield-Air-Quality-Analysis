# Quantifying the Detectability of Anthropogenic Signals in Diurnal Air-Pollution Patterns
**Student Name:** Xinyi Yao 
**Module:** IJC437 Introduction to Data Science

## 1. Project Overview (Executive Summary)
This project investigates whether machine learning models can detect human activity signals (specifically traffic and heating) from air quality data in Sheffield. By comparing NO2 and PM2.5 patterns, we found that Random Forest models can accurately classify weekends for NO2 (traffic-driven) but fail for PM2.5, suggesting different underlying pollution sources.

## 2. Key Findings
- **NO2**: Highly predictable weekend drops (Accuracy: 86.4%), confirming traffic as the main source.
- **PM2.5**: No distinct weekend pattern, suggesting dominance of residential heating.
- **Model Performance**: Random Forest significantly outperformed GLM, proving the non-linear nature of air pollution.

## 3. Repository Structure
- `Code/`: Contains the R code for this project: Data Pre-processing -- Weather Normalisation -- EDA and Visualisation -- Classification Models.
- `Data/`: Datasets used in this module.
- `Results/`: Output visualisations.

## 4. How to Run
1. Clone this repository.
2. Run the code in Rstudio.

