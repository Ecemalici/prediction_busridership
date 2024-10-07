#-----Data Acquisition------------------------
#-----Section-01-Set & Get Work Directory-----

# Setting the working directory before processing any work or tests in R.
# This is useful for saving graphs, plots, images, and data in the set directory.
setwd(dirname(file.choose()))

# Getting the path directory to confirm the working directory.
getwd()

#-----Section-02-Data Frame & Data Set-----
# Loading and reading the data from the CSV file into a new data frame.
DS7010.Data <- read.csv("CTA_2014_2024.csv", stringsAsFactors = FALSE)

#-----Data Exploration-----------------
#-----Section-03-Previewing Data & Descriptive Statistics-----
# Explore the dataset to understand its structure, size, and basic statistics.
# Inspect the top rows of the dataset to get a general overview.
head(DS7010.Data)

# The counterpart of head() is tail(), which prints the last rows of the dataset.
tail(DS7010.Data)

# Display the structure of the data frame to understand the types of variables.
str(DS7010.Data)

# Summary statistics to understand the central tendency and variability of the dataset.
summary(DS7010.Data)

#-----Section 04- Data Pre-processing-------------------------
#-----Section 04-01-Checking for missing values---------------

# Checking for missing data in the entire data-set.

# Check for any missing values in the dataset.
# This helps identify any potential data quality issues.
missing_data <- DS7010.Data
apply(missing_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# Mapping up missing values by column using miss-map.
library(Amelia)
missmap(missing_data, margins = c(10, 5), col = c("black", "grey"), legend = TRUE,
        main = "Fig.: Missingness Map of DS7010")

# Remove the temporary missing_data variable from the environment.
rm(missing_data)

#-------- Section 04-02- Converting data to Date format---------
library(dplyr)

# Convert the Month column to Date format using the zoo library.
DS7010.Data <- DS7010.Data %>%
  mutate(YearMonth=as.Date(DS7010.Data$date, format = "%m/%d/%Y"))

##----Section 05-Data Visualization------------------------
# Load necessary libraries for data manipulation and visualization.
library(dplyr)
library(ggplot2)
library(highcharter)

# ------ Distribution of monthly ridership total-------
# Visualize rides total over time.
month_rides_total <- DS7010.Data %>%
  group_by(YearMonth) %>%
  summarise(Total = sum(rides, na.rm = TRUE))

# Highchart visualization for rides total over time.
hchart(month_rides_total, 'spline', hcaes(YearMonth, Total)) %>%
  hc_title(text = "Monthly Bus Rides Total Over Time") %>%
  hc_add_theme(hc_theme_gridlight())

# ------ Distribution of ridership total by bus routes-------
# Visualize the distribution of ridership total by bus routes.
month_route_total <- DS7010.Data %>%
  group_by(YearMonth, route) %>%
  summarise(Total = sum(rides, na.rm = TRUE), .groups = 'drop') 

# Highchart visualization for ridership total by bus routes (pie chart).
hchart(month_route_total, 'pie', hcaes(x = route , y = Total, color = route)) %>%
  hc_title(text = "Distribution by Route Type") %>%
  hc_add_theme(hc_theme_gridlight())

# Highchart visualization for bus routes distribution (column chart).
hchart(month_route_total, 'column', hcaes(x = route, y = Total, group = route)) %>%
  hc_title(text = "Distribution by Bus Routes") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_legend(align = 'right', float = TRUE)
 
# Highchart visualization for distribution by routes by month.
hchart(month_route_total, 'column', hcaes(x = YearMonth, y = Total, 
                                          group = route)) %>%
  hc_title(text = "Distribution by Routes by Month") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_legend(align = 'right', float = TRUE)

# --------------Distribution of Day Types--------------
# Visualize the distribution of day types.
month_daytype_total <- DS7010.Data %>%
  group_by(YearMonth, daytype) %>%
  summarise(Total = sum(rides, na.rm = TRUE), .groups = 'drop') 

# Highchart visualization for day type distribution (pie chart).
hchart(daytype_total, 'pie', hcaes(x = daytype , y = Total, color = daytype)) %>%
  hc_title(text = "Distribution by Day Types") %>%
  hc_add_theme(hc_theme_gridlight())

# Highchart visualization for day type distribution (column chart).
hchart(daytype_total, 'column', hcaes(x = daytype, y = Total, group = Total)) %>%
  hc_title(text = "Distribution by Day Types") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_legend(align = 'right', float = TRUE)

# Highchart visualization for distribution by day type by month (line chart).
hchart(month_daytype_total, "line", hcaes(YearMonth, Total, color = daytype, 
                                          group = daytype)) %>%
  hc_title(text = "Distribution by Day Type by Month") %>%
  hc_yAxis(title = list(text = "Day Types")) %>%
  hc_add_theme(hc_theme_google())

#-------------Section-05-01-Visualization by using maps----------
# Visualize distribution by geometry using the CTA_bus_routes shapefile.
# Load required libraries
library(sf)
library(tmap)
library(ggplot2)
library(dplyr)
library(maptiles)
library(spdep)     # For spatial dependence
library(spatstat)

# Load the shapefile for CTA bus routes
CTA_BusRoutes <- st_read("CTA_BusRoutes_shapefile/CTA_BusRoutes.shp", quiet = TRUE)

# Load the background shapefile
background_shapefile <- st_read("Boundaries - Neighborhoods/geo_export_38319620-8ca8-4ce5-86d4-dcb5c81a8a12.shp", 
                                quiet = TRUE)

# Ensure both shapefiles are in the same CRS
if (st_crs(CTA_BusRoutes) != st_crs(background_shapefile)) {
  background_shapefile <- st_transform(background_shapefile, st_crs(CTA_BusRoutes))
}

# Join the route data with the shapefile
CTA_BusRoutes <- left_join(CTA_BusRoutes, route_total, by = c("ROUTE" = "route"))

# Replace NA values in the Counts column with 0
CTA_BusRoutes$Total[is.na(CTA_BusRoutes$Total)] <- 0

# Switch to plot mode for tmap
tmap_mode("plot")

# Plot with background shapefile
ggplot() +
  # Plot the background layer first
  geom_sf(data = background_shapefile, fill = "lightgrey", color = "white") +
  # Overlay the bus routes with a color scale based on 'Total'
  geom_sf(data = CTA_BusRoutes, aes(color = Total), size = 1) +
  # Use a color gradient for the routes based on the 'Total' field
  scale_color_gradient(low = "lightblue", high = "red") +
  # Minimal theme for better visualization
  theme_minimal() +
  # Add titles and captions
  labs(title = "Chicago Bus Routes by Total Ridership",
       caption = "Data source: CTA") +
  # Adjust legend position and appearance
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# Another plot with better visualization of the routes

# Plot bus routes using a similar approach
ggplot() +
  # Plot the background layer first
  geom_sf(data = background_shapefile, fill = "lightgrey", color = "white") +
  # Use a color scale to differentiate between the routes
  scale_color_manual(values = rainbow(n = length(unique(CTA_BusRoutes$ROUTE)))) +
  # Plot the bus routes with segments (each route as a separate line)
  geom_sf(data = CTA_BusRoutes, aes(color = as.factor(ROUTE)), size = 1, 
          alpha = 0.8) +
  # Clean up the theme for better presentation
  theme_void() +
  # Add titles and captions
  labs(title = "Chicago Bus Routes by Total Ridership",
       caption = "Data source: CTA") +
  # Adjust legend position and appearance
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))


#-----------Section 05-02-Saving CSV files for QGIS-----------
#to save data frame or table into csv
{
  file_path <- "route_total.csv"
  write.csv(route_total, file = file_path,row.names = FALSE)
  rm(file_path)
  }
{
  file_path <- "month_route_total.csv"
  write.csv(month_route_total, file = file_path,row.names = FALSE)
  rm(file_path)
}

# ---------Time Series Modelling----------------
# Load required libraries
library(tseries)
library(forecast)
library(ggplot2)
library(gridExtra)  # For arranging plots
library(tibble)
library(tidyr)
library(zoo)
library(prophet)

#--------- Section 06-Data Preparation --------
# Convert to time-series object
start_date <- c(as.numeric(format(min(month_rides_total$YearMonth), "%Y")), 
                as.numeric(format(min(month_rides_total$YearMonth), "%m")))
ts_data <- ts(month_rides_total$Total, start = start_date, frequency = 365)

# Plot the time series data
plot(ts_data, main = "Monthly Bus Ridership Over Time", ylab = "Bus Ridership", 
     xlab = "Time")

# Split Data into Training and Testing Sets
# Determine the split point (80% training and 20% testing)
split_point <- round(0.8 * nrow(month_rides_total))

# Create the training and testing sets
rides.tr <- month_rides_total[1:split_point, ]
rides.te <- month_rides_total[(split_point + 1):nrow(month_rides_total), ]

# Convert `rides.tr` and `rides.te` to time series
rides.tr <- ts(rides.tr$Total, start = c(2016, 1), frequency = 365)
rides.te <- ts(rides.te$Total, start = c(2014 + (split_point / 365), 1), frequency = 365)

#--------- Section 07: Decomposition and Plotting ---------

# STL Decomposition
decomp <- stl(rides.tr, s.window = "periodic")
plot(decomp)

# Plot trend, seasonal, and remainder components
plot(decomp$time.series[, "trend"], type = "l", col = "red", main = "Trend Component")
lines(decomp$time.series[, "seasonal"], col = "blue")
lines(decomp$time.series[, "remainder"], col = "green")

# Seasonally Adjusted Data
seasonal_adj <- seasadj(decomp)
plot(rides.tr, col = "gray", main = "Seasonally Adjusted Bus Ridership")
lines(seasonal_adj, col = "red")

#--------- Section 08: Stationarity Test and Differencing ---------

# Perform Augmented Dickey-Fuller test to check for stationarity
adf_result <- adf.test(rides.tr)
print(adf_result)

# Differencing and plotting differenced time series
diff_te <- diff(rides.te)
plot(diff_te, main = "Differenced Time Series")

#--------- Section 09:Forecasting Models ---------

# Simple Forecasting Methods
mean_forecast <- meanf(rides.te, h = 30)
naive_forecast <- naive(rides.te, h = 30)
snaive_forecast <- snaive(rides.te, h = 30)
summary(mean_forecast)
summary(naive_forecast)
summary(snaive_forecast)

# Plot simple forecasts
plot(mean_forecast, main = "Mean Forecast")
plot(naive_forecast, main = "Naive Forecast")
plot(snaive_forecast, main = "Seasonal Naive Forecast")

# ARIMA Model
fit_arima <- auto.arima(rides.tr)
summary(fit_arima)
forecast_arima <- forecast(fit_arima, h = 30)
plot(forecast_arima)

# Residual Analysis for ARIMA Model
par(mfrow = c(2, 2))
plot(residuals(fit_arima), main = "Residuals of ARIMA Model", ylab = "Residuals")
abline(h = 0, col = "red")
acf(residuals(fit_arima), main = "ACF of Residuals (ARIMA Model)")
pacf(residuals(fit_arima), main = "PACF of Residuals (ARIMA Model)")

# Prophet Model
prophet_data <- data.frame(ds = month_rides_total$YearMonth, y = month_rides_total$Total)
model_prophet <- prophet(prophet_data)
summary(model_prophet)
future <- make_future_dataframe(model_prophet, periods = 30)
forecast_prophet <- predict(model_prophet, future)
plot(model_prophet, forecast_prophet)

# Exponential Smoothing State Space Model (ETS)
ets_model <- ets(rides.tr)
summary(ets_model)
forecast_ets <- forecast(ets_model, h = 30)
plot(forecast_ets, main = "ETS Forecast")

# Seasonal ARIMA Model
sarima_model <- auto.arima(rides.tr, seasonal = TRUE)
summary(sarima_model)
forecast_sarima <- forecast(sarima_model, h = 30)
plot(forecast_sarima, main = "Seasonal ARIMA Forecast")

# Reset plotting area
par(mfrow = c(1, 1))

#--------- Section 10- Model Accuracy and Evaluation ---------

# Calculate RMSE and MAE
rmse <- function(error) {
  sqrt(mean(error^2))
}

mae <- function(error) {
  mean(abs(error))
}

# Calculate forecast errors and evaluate model performance
n_test <- length(rides.te)
start_index <- max(1, n_test - 29)
end_index <- n_test
test_window <- rides.te[start_index:end_index]
errors <- snaive_forecast$mean - test_window

print(paste("RMSE:", rmse(errors)))
print(paste("MAE:", mae(errors)))

#-----Section 19-Clean Up------------------------------------------
# remove all variables from the environment
rm(list=ls())
