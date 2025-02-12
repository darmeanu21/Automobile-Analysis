#importing and cleaning the data
data <- read.csv("C:/Users/emili/Desktop/imports-85.data", header=FALSE)
data[data == "?"] <- NA
cleaned_data <- na.omit(data)
View(cleaned_data)

library(dplyr) #for renaming columns
cleaned_data <- cleaned_data %>%
  rename(
    fuel = V4,
    fuel_system = V18,
    highway_MPG = V25,
    engine_size = V17
  )

#combining MFI and MPFI into the same category since they basically are the same thing
table(cleaned_data$fuel, cleaned_data$fuel_system) 
cleaned_data$fuel_system_combined <- cleaned_data$fuel_system
cleaned_data$fuel_system_combined[cleaned_data$fuel_system_combined == "mfi"] <- "mpfi"
table(cleaned_data$fuel_system_combined)

attach(cleaned_data)


#fitting the OLS model
model <- lm(highway_MPG ~ engine_size * factor(fuel_system_combined), data = cleaned_data)
summary(model)

cleaned_data$residuals <- resid(model)

library(ggplot2)
library(gridExtra)

#creating the diagnostic plots
#QQ plot
plot1 <- ggplot(cleaned_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#Residuals vs Fitted Values
plot2 <- ggplot(cleaned_data, aes(x = fitted, y = residuals, color = fuel_system_combined)) +
  geom_point(alpha = 0.7) +  # Scatter points with transparency
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Horizontal line at 0
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals",
    color = "Fuel System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "right"               # Position the legend on the right
  )

#Residuals vs Engine Size
plot3 <- ggplot(cleaned_data, aes(x = engine_size, y = residuals, color = fuel_system_combined)) +
  geom_point(alpha = 0.7) +  # Scatter points with transparency
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Horizontal line at 0
  labs(
    title = "Residuals vs Engine Size",
    x = "Engine Size",
    y = "Residuals",
    color = "Fuel System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "right"               # Position the legend on the right
  )
#Histogram of Residuals
plot4 <- ggplot(cleaned_data, aes(x = residuals)) +
  geom_histogram(
    bins = 30,                # Number of bins (adjust as needed)
    fill = "red",            # Fill color
    color = "black",          # Outline color
    alpha = 0.7               # Transparency
  ) +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
#Boxplot of Residuals
plot5 <- ggplot(cleaned_data, aes(y = residuals)) +
  geom_boxplot(fill = "red", color = "black", alpha = 0.7) +  
  labs(
    title = "Box-and-Whisker Plot of Residuals",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
#Plots arranged in a 2x3 grid
install.packages("gridExtra")
grid.arrange(
  plot1, plot2, plot3,          
  plot4, plot5, textGrob(""),  
  ncol = 3,                     
  nrow = 2,                     
  layout_matrix = rbind(
    c(1, 2, 3),                 
    c(4, 5, 6)                  
  )
)
#Breusch Pagan test
library(lmtest)
bptest(model)
shapiro.test(residuals)



library(MASS)
# Perform Box-Cox transformation
boxcox_result <- boxcox(model, lambda = seq(-2, 2, by = 0.1),
                        main = "Box-Cox Transformation for Highway MPG")
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
cat("Optimal lambda:", optimal_lambda, "\n")

# If the optimal lambda is close to 0, use a log transformation:
if (abs(optimal_lambda) < 0.1) {
  cleaned_data$highway_MPG_trans <- log(cleaned_data$highway_MPG)
  cat("Log transformation applied to highway_MPG.\n")
} else {
  cleaned_data$highway_MPG_trans <- (cleaned_data$highway_MPG^optimal_lambda - 1) / optimal_lambda
  cat("Box-Cox transformation applied with lambda =", optimal_lambda, "\n")
}

# Refitting the model with the transformed Y values
model_trans <- lm(highway_MPG_trans ~ engine_size * factor(fuel_system_combined), data = cleaned_data)
summary(model_trans)

cleaned_data$residuals_trans <- resid(model_trans)
cleaned_data$fitted_trans <- model_trans$fitted.values


#diagnostic plots for the transformed Y values
#Engine Size vs Highway MPG transformed
plot5 <- ggplot(cleaned_data, aes(x = engine_size, y = highway_MPG_trans, color = fuel_system_combined)) +
  geom_point(alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, size = 1) +  
  labs(
    title = "Engine Size vs Highway MPG by Fuel System Type",
    x = "Engine Size",
    y = "Highway MPG",
    color = "Fuel System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
#QQ Plot for log model
plot6 <- ggplot(cleaned_data, aes(sample = residuals_trans)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Log Model Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
#Residual vs Fitted Values for log model
plot7 <- ggplot(cleaned_data, aes(x = fitted_trans, y = residuals_trans, color = fuel_system_combined)) +
  geom_point(alpha = 0.7) +  # Scatter points with transparency
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) + 
  labs(
    title = "Residuals vs Fitted Values (Transformed Model)",
    x = "Fitted Values",
    y = "Residuals",
    color = "Fuel System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "right"               
  )
#Residuals vs Engine Size log model
plot8 <- ggplot(cleaned_data, aes(x = engine_size, y = residuals_trans, color = fuel_system_combined)) +
  geom_point(alpha = 0.7) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Horizontal line at 0
  labs(
    title = "Residuals vs Engine Size (Transformed Model)",
    x = "Fitted Values",
    y = "Engine Size",
    color = "Fuel System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right"               
  )
#histogram for log transformed
plot9 <- ggplot(cleaned_data, aes(x = residuals_trans)) +
  geom_histogram(
    bins = 30,                
    fill = "red",            # Fill color
    color = "black",          # Outline color
    alpha = 0.7               # Transparency
  ) +
  labs(
    title = "Histogram of Transformed Residuals",
    x = "Residuals (Transformed Model)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
#boxplot log model
plot10 <- ggplot(cleaned_data, aes(y = residuals_trans)) +
  geom_boxplot(fill = "red", color = "black", alpha = 0.7) +  # Box plot with styling
  labs(
    title = "Box-and-Whisker Plot of Transformed Residuals",
    y = "Residuals (Transformed Model)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
# Arrange plots in a 2x3 grid
grid.arrange(
  plot6, plot7, plot8,          
  plot9, plot10, textGrob(""),  
  ncol = 3,                     
  nrow = 2,                     
  layout_matrix = rbind(
    c(1, 2, 3),                 
    c(4, 5, 6)                
  )
)

#various test
bptest(model_trans)
shapiro.test(cleaned_data$residuals_trans)


#fitting robust model
robust_model <- rlm(highway_MPG_trans ~ engine_size + factor(fuel_system_combined), data = cleaned_data)
summary(robust_model)
cleaned_data$residuals_robust <- resid(robust_model)

#testing robust model
shapiro.test(cleaned_data$residuals_robust)
bptest(robust_model)

library(MASS)
library(leaps)
library(caret)

set.seed(123)

k <- 10 # Number of folds 

folds <- createFolds(cleaned_data$highway_MPG_trans, k = k, list = TRUE)

rmse_values_robust <- c()
mae_values_robust <- c()

for (i in 1:k) {

  training <- cleaned_data[-folds[[i]], ]
  testing <- cleaned_data[folds[[i]], ]
  
  # Fit the robust regression model
  robust_model <- rlm(
    highway_MPG_trans ~ engine_size + factor(fuel_system_combined),
    data = training
  )
  
  predictions <- predict(robust_model, newdata = testing)
  
  rmse <- sqrt(mean((testing$highway_MPG_trans - predictions)^2))  
  mae <- mean(abs(testing$highway_MPG_trans - predictions))        
  

  rmse_values_robust[i] <- rmse
  mae_values_robust[i] <- mae
}


average_rmse_robust <- mean(rmse_values_robust)
average_mae_robust <- mean(mae_values_robust)


cat("Average RMSE:", round(average_rmse_robust, 3), "\n")
cat("Average MAE:", round(average_mae_robust, 3), "\n")



#k-fold validation for log model
rmse_values <- c()
mae_values <- c()


for (i in 1:k) {
  training <- cleaned_data[-folds[[i]], ]
  testing <- cleaned_data[folds[[i]], ]
  
  ols_model <- lm(
    highway_MPG_trans ~ engine_size + factor(fuel_system_combined),
    data = training
  )
  
  predictions <- predict(ols_model, newdata = testing)
  
  rmse <- sqrt(mean((testing$highway_MPG_trans - predictions)^2)) 
  mae <- mean(abs(testing$highway_MPG_trans - predictions))     
  
  rmse_values[i] <- rmse
  mae_values[i] <- mae
}

average_rmse <- mean(rmse_values)
average_mae <- mean(mae_values)

cat("Average RMSE (OLS):", round(average_rmse, 3), "\n")
cat("Average MAE (OLS):", round(average_mae, 3), "\n")

write.csv(cleaned_data, "data.csv", row.names = FALSE)
