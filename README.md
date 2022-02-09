# ExploringCrimeForecastPerformance_STARMA_ARIMA
 
This repository was created for my master's thesis, "Exploring the predictive performance of autoregressive space-time models at different temporal resolutions."

The study models STARMA and ARIMA models for three types of crimes (all, violent, and property) in five time periods (weekly, monthly, quarterly, semiannual, and annual). The models are then compared with their error metrics (MSE, RMSE, and R2), the mapped residuals of the model, and the LISA maps of the model residuals.

The study area is New York City and the spatial resolution is the police precincts. The shape files of the precincts can be found in the folder: Input_Data.

The NYPD Arrests Data (Historic) from 2006-2020 is the crime data for the study. Since the file is too large to upload to this repository, you can download it from the website where the dataset is published and updated annually:
https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u
