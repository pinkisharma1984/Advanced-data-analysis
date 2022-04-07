# Advanced-data-analysis

This project sought to explore and analyze potential factors contributing to global food insecurity by aggregating data from seven datasets provided by the Food and Agriculture Organization of the United Nations website(fao.org).(https://www.fao.org/faostat/en/#data). The aggregated raw data consisted of 33 variables and 2240 observations with each observation representing a country and year in the range of 2010-2019.

# Techniques: 
### Exploratory Analysis

### Data Cleaning and Preprocessing :
The variables described information that could reasonably affect a country’s food insecurity, such as; population, GDP, political stability, food production, consumption, supply, geographic region, access to water and public sanitation, inflation, and climate change. The dataset was not cleaned, so as part of preprocessing, we removed NAs from each of the individual columns and removed rows that had misleading 0 and character values.  

### Used to analyze the data is Linear Discriminant Analysis
To find variables that would easily separate whether the country is politically stable or not.

### Ordinal Logistic regression
Used the proportional odds logistic regression technique to predict multi-class ordered variables. Our aim is to predict whether a country is politically stable or not based on the independent variables.
