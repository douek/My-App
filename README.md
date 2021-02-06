# My-App
Shiny (R) application for interactive Exploratory Data Analysis. 

https://ayeletd12.shinyapps.io/EDA_app/

## Gamma version with bugs fixed and new layout coming soon! ## 

### Input
Users can upload raw data as CSV format and receive high-level information like data shape, list of features, data types, and variables summary.


## Missing Values 

Visualization - Visualize the proportions of missing values (Y) for each feature (X). Observation of missing values (red) and complete ones (blue).

Treatment 
* Omit all rows with missing values (not recommended in case of many rows containing missing values ).
* Assign a specific value - whether a constant, like 0 or -9999, or a statistic, like mean or median. The value assigned to all numeric features while for categorial assign "Missing value" label.


### Feature Selection 
In this section, the user chooses the features of the model and the final dataset.

* Target variable - examine the distribution by univariate plot and examine factor of a chosen variable.
* X Features - examine the distribution of the data, examine transformation as log and square to numeric features (will be extended in the future).

  
### Outliers
In this section, the users can deepen the search for outlier and integrity issues in the data:

* Conditional filtering - Applying a condition on a specific variable
* Visual filtering - choose the wanted observations from the graph by double-clicking it.

In both options, a table with the relevant observation will appear below and the user can add an examination combination with more filters to the other variables. The user can choose rows from the table by clicking on them and can omit them from the dataset.
 
### Save 
 
The user can download the final dataset

### comming soon
*  Fitting a model
