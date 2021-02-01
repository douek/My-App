# My-App
Beta version - 
Shiny (R) application for interactive Exploratory Data Analysis. 

https://ayeletd12.shinyapps.io/EDA_app/

## Gamma version with bugs fixed and new layout coming soon! ## 

support in 

input - User uploads his CSV data
High-level information - data shape, list of features, data types, and variables summary.

Missing Values - Visualisation and Treatment

  Visualization - for each feature (X) the proportion of missing values (Y) while red color is for observations with missing values and blue for complete ones.
  
  Treatment - the treatment of the missing value can be obtained in one of these two ways: (this is not necessary for the next steps)
    1. Omit - omits all rows with missing values. not recommended in case of many rows containing missing values 
    2. Assign a specific value - whether a constant, like 0 or -9999, or a statistic, like mean or median, the value assigns to all numeric feature while for categorial assign "Missing value" label.  
    
Feature Selection - in this section the user chooses the features to the model and the final dataset.
  
  1. Target variable - user chooses a variable, can examine the distribution by univariate plot, examine as a factor. 
  2. X Features - the user chooses a feature and can examine the distribution of the data, gets a summary. the user can examine transformation as log, square (will be extended in the future) to numeric features. 
  
Outliers - in this section the user can deep in his search for outlier and integrity issues in the data in two ways:
  1. applying a condition on a specific variable - choosing a requested condition and press go. 
  2. visual choosing - the user chooses from the graph the wanted observations by double-clicking it.
  
  in both cases, a table with the relevant observation will upload below and the user can add examine combination with more filters to the other variables. the user can choose rows from the table by clicking on them and can omit them from the dataset. 
  
Save - the user can download the final dataset. 
  future element - fitting a model.


