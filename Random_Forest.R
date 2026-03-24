library(dplyr)
library(ranger)
library(caret)
library(sf)
library(spdep)

#----------------------------
# 1. Load and prepare data
#----------------------------

forecast_data <- readRDS("crime_monthly_v5.rds")

forecast_data <- forecast_data %>%
  mutate(
    area_f  = as.factor(area),
    month_f = as.factor(month_f)
  )

rf_data <- forecast_data %>%
  filter(!is.na(crime_lag1)) %>%
  select(y, area_f, month_f, population, time_id, crime_lag1)

#----------------------------
# 2. Split data
#----------------------------
# Main training and test sets
train_rf <- rf_data %>% filter(time_id %in% 1:56)
test_rf  <- rf_data %>% filter(time_id %in% 57:59)

# Internal split for tuning
train_tune <- rf_data %>% filter(time_id %in% 1:53)
valid_tune <- rf_data %>% filter(time_id %in% 54:56)


#----------------------------
# 3. Recursive forecasting function
#----------------------------
recursive_rf_predict <- function(model, new_data) {
  
  # Month 1
  data_1 <- new_data %>% filter(time_id == min(time_id))
  pred_1 <- predict(model, data = data_1)$predictions
  
  # Month 2
  data_2 <- new_data %>% filter(time_id == sort(unique(time_id))[2])
  data_2$crime_lag1 <- pred_1
  pred_2 <- predict(model, data = data_2)$predictions
  
  # Month 3
  data_3 <- new_data %>% filter(time_id == sort(unique(time_id))[3])
  data_3$crime_lag1 <- pred_2
  pred_3 <- predict(model, data = data_3)$predictions
  
  c(pred_1, pred_2, pred_3)
}

#----------------------------
# 4. Hyperparameter grid
#----------------------------
p <- 4   # predictors: area_f, month_f, population, crime_lag1

rf_grid <- expand.grid(
  mtry = c(2, 3, 4),
  min_n = c(5, 10, 20),
  trees = c(500, 1000, 1500)
)

rf_grid$RMSE <- NA_real_
rf_grid$MAE  <- NA_real_
rf_grid$R2   <- NA_real_


#----------------------------
# 5. Grid search on validation period
#----------------------------
set.seed(123)

for (i in seq_len(nrow(rf_grid))) {
  
  cat("Fitting model", i, "of", nrow(rf_grid), "\n")
  
  rf_fit <- ranger(
    y ~ area_f + month_f + crime_lag1 + population,
    data = train_tune,
    num.trees = rf_grid$trees[i],
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min_n[i],
    importance = "permutation",
    seed = 123
  )
  
  valid_pred <- recursive_rf_predict(rf_fit, valid_tune)
  
  rf_grid$RMSE[i] <- sqrt(mean((valid_tune$y - valid_pred)^2))
  rf_grid$MAE[i]  <- mean(abs(valid_tune$y - valid_pred))
  rf_grid$R2[i]   <- cor(valid_tune$y, valid_pred)^2
}

# View tuning results
rf_grid <- rf_grid %>% arrange(RMSE)
print(rf_grid)

# Best hyperparameters
best_params <- rf_grid %>% slice(1)
best_params

#----------------------------
# 6. Refit on full training set
#----------------------------
rf_model <- ranger(
  y ~ area_f + month_f + crime_lag1 + population,
  data = train_rf,
  num.trees = best_params$trees,
  mtry = best_params$mtry,
  min.node.size = best_params$min_n,
  importance = "permutation",
  seed = 123
)

#----------------------------
# 7. Recursive forecast on test period
#----------------------------
rf_pred <- recursive_rf_predict(rf_model, test_rf)

test_rf$rf_pred <- rf_pred

#----------------------------
# 8. Test accuracy
#----------------------------
rmse <- sqrt(mean((test_rf$y - rf_pred)^2))
mae  <- mean(abs(test_rf$y - rf_pred))
r2   <- cor(test_rf$y, rf_pred)^2

c(RMSE = rmse, MAE = mae, R2 = r2)

sort(rf_model$variable.importance, decreasing = TRUE)

#----------------------------
# 9. Actual vs Predicted 
#----------------------------
plot_df <- test_rf %>%
  filter(time_id %in% c(57, 58, 59)) %>%
  mutate(
    time_id = factor(
      time_id,
      levels = c(57, 58, 59),
      labels = c("2023 September", "2023 October", "2023 November")
    )
  )

stats_df <- plot_df %>%
  group_by(time_id) %>%
  summarise(
    RMSE = sqrt(mean((y - rf_pred)^2, na.rm = TRUE)),
    MAE  = mean(abs(y - rf_pred), na.rm = TRUE),
    R2   = cor(y, rf_pred, use = "complete.obs")^2,
    label = paste0(
      "R² = ", round(R2, 2), "\n",
      "MAE = ", round(MAE, 2), "\n",
      "RMSE = ", round(RMSE, 2)
    ),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = y, y = rf_pred)) +
  geom_point(alpha = 0.3, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  facet_wrap(~ time_id, ncol = 3) +
  geom_text(
    data = stats_df,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.1,
    size = 3.2
  ) +
  labs(
    x = "Actual monthly crime count",
    y = "Prediction from random forest model"
  ) +
  theme_minimal()

#----------------------------
# 10. Variable Importance  
#----------------------------

#check importance of variables
sort(rf_model$variable.importance, decreasing = TRUE)

#variable importance plot
var_imp_df <- data.frame(
  variable = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
)

var_imp_df <- var_imp_df %>%
  arrange(desc(importance))

ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Importance (Random Forest)",
    x = "Variable",
    y = "Permutation importance"
  ) +
  theme_minimal()
