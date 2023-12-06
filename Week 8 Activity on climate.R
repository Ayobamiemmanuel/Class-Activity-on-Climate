library(lme4)
library('tidyverse')
library('broom')
library(ggplot2)
library(Matrix)
library(dplyr)


clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

unique(clim$altitude)
unique(clim$p_mean)

# Remove non-numeric characters and replace with an empty string
clim$altitude <- as.numeric(gsub("[^0-9.]", "", clim$altitude))
clim$p_mean <- as.numeric(gsub("[^0-9.]", "", clim$p_mean))

clim$altitude <- as.numeric(clim$altitude)
clim$p_mean <- as.numeric(clim$p_mean)


install.packages("raster")
install.packages("sp")
install.packages("maps")
library(raster)
library(sp)
library(maps)

G1 <- raster::getData(country = "France", level = 1)
G1

ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()


# Exercise 1

climfrar <- clim[1:34, ]

#Linear Model
names(climfrar)

#Model
model <- lm(t_mean ~ altitude+lat+lon, data = climfrar)
summary(model)

# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.76492 -0.32755  0.04337  0.24787  2.58927 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7308 on 30 degrees of freedom
# Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
# F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12

#Interpretation
# The linear regression model indicates that when all predictor variables are zero, 
# the average temperature (t_mean) is estimated to be 37.27. 
#As altitude increases by one unit, the average temperature decreases by about 0.0064 units. 
#Similarly, for each degree increase in latitude, the average temperature drops by approximately 0.5340 units. 
#However, the effect of longitude on temperature is not statistically significant, suggesting it may not have a meaningful linear relationship with temperature.
# The model performs well, explaining about 83% of the variability in temperature.


#Exercise 2

model2 <- lm(t_mean ~ altitude+lat, data = climfrar)
summary(model2)

# Call:
#   lm(formula = t_mean ~ altitude + lat, data = climfrar)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.79206 -0.27571 -0.00556  0.30536  2.71871 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#   altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#   lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7268 on 31 degrees of freedom
# Multiple R-squared:  0.8292,	Adjusted R-squared:  0.8182 
# F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12


pred_t_mean <- predict(model2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
pred_t_mean

# Create a dataframe from the results
prediction_table <- data.frame(station = c("Mont-Ventoux", "Pic-du-midi"),
                               Predicted_Mean = pred_t_mean[, "fit"],
                               Lower_Bound = pred_t_mean[, "lwr"],
                               Upper_Bound = pred_t_mean[, "upr"])
prediction_table


# prediction_table
# Location Predicted_Mean Lower_Bound Upper_Bound
# 1 Mont-Ventoux       6.165713    3.792341    8.539085
# 2  Pic-du-midi      -3.447331   -8.347964    1.453302

#The predictions and confidence intervals suggest the expected temperature ranges for Mont-Ventoux and Pic-du-midi. 
#For Mont-Ventoux, the model predicts a mean temperature around 6.17 units, with a 95% confidence that the actual temperature falls between 3.79 and 8.54 units. 
#On the other hand, for Pic-du-midi, the model predicts a mean temperature of approximately -3.45 units, with a 95% confidence interval ranging from -8.35 to 1.45 units. 
#These intervals provide a level of certainty about where the actual temperatures are likely to fall. 
#For instance, while we expect Mont-Ventoux to be moderately warm, Pic-du-midi's temperature could be cooler, with a wider range of possibilities.



# A dataframe with measured means
measured_means <- clim %>%
  filter(station %in% c("Mont-Ventoux", "Pic-du-Midi")) %>%
  group_by(station) %>%
  summarise(measured_mean = mean(t_mean))

# Combine measured means with predicted means and intervals
comparison_table <- cbind(measured_means, prediction_table)

comparison_table
# station measured_mean      station Predicted_Mean Lower_Bound Upper_Bound
# 1 Mont-Ventoux           3.6 Mont-Ventoux       6.165713    3.792341    8.539085
# 2  Pic-du-Midi          -1.2  Pic-du-midi      -3.447331   -8.347964    1.453302

#Comparison:

#Mont-Ventoux:
  
  #The measured mean temperature (3.6) falls within the 95% prediction interval (3.79, 8.54). 
  #The predicted mean (6.17) is within the interval, indicating a reasonable agreement between the measured and predicted values.
#Pic-du-Midi:
  
#The measured mean temperature (-1.2) falls within the 95% prediction interval (-8.35, 1.45). 
#The predicted mean (-3.45) is within the interval, suggesting alignment between the measured and predicted values.

#Exercise 3
install.packages("scatterplot3d")
library(scatterplot3d)

scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,))
scatter_3d$plane3d(model2)

summary(model2)
#The results indicate that, on average, the starting temperature is around 37.91 degrees when both altitude and latitude are zero. 
#As altitude increases, the model predicts a decrease in temperature, with approximately a 0.0063-degree drop for every one-unit increase in altitude. 
#Similarly, moving south (decreasing latitude) is associated with a decrease in temperature, with about a 0.55-degree drop for every one-degree decrease in latitude. 
#All three factors—Intercept, Altitude, and Latitude—are deemed highly significant, suggesting that the observed relationships are unlikely to be random.