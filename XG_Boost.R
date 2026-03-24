library(dplyr)
library(Matrix)
library(xgboost)
library(ggplot2)
#--------------------------------------------------
# 1. Load and prepare data
#--------------------------------------------------
forecast_data <- readRDS("crime_monthly_v5.rds")

forecast_data <- forecast_data %>%
  filter(!is.na(crime_lag1)) %>%
  mutate(
    month_f = as.factor(month_f),
    area    = as.factor(area)
  )

feat_cols <- c("month_f", "area", "population", "crime_lag1")

#--------------------------------------------------
# 2. Split data
#--------------------------------------------------
xg_train_df <- forecast_data %>%
  filter(time_id <= 53)

xg_val_df <- forecast_data %>%
  filter(time_id %in% 54:56)

xg_test_df <- forecast_data %>%
  filter(time_id %in% 57:59)

#--------------------------------------------------
# 3. Create design matrices
# Use combined data to ensure identical columns
#--------------------------------------------------
all_df <- bind_rows(
  xg_train_df %>% mutate(dataset = "train"),
  xg_val_df   %>% mutate(dataset = "val"),
  xg_test_df  %>% mutate(dataset = "test")
)

X_all <- sparse.model.matrix(~ month_f + area + population + crime_lag1 - 1, data = all_df)

n_train <- nrow(xg_train_df)
n_val   <- nrow(xg_val_df)
n_test  <- nrow(xg_test_df)

X_train <- X_all[1:n_train, ]
X_val   <- X_all[(n_train + 1):(n_train + n_val), ]
X_test  <- X_all[(n_train + n_val + 1):(n_train + n_val + n_test), ]

y_train <- xg_train_df$y
y_val   <- xg_val_df$y
y_test  <- xg_test_df$y

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval   <- xgb.DMatrix(data = X_val, label = y_val)

#--------------------------------------------------
# 4. Hyperparameter grid
#--------------------------------------------------
xgb_grid <- expand.grid(
  eta = c(0.03, 0.05, 0.1),
  max_depth = c(4, 6, 8),
  min_child_weight = c(3, 5, 10),
  subsample = c(0.7, 0.8),
  colsample_bytree = c(0.7, 0.8),
  stringsAsFactors = FALSE
)

xgb_grid$best_iter <- NA_integer_
xgb_grid$RMSE <- NA_real_
xgb_grid$MAE  <- NA_real_
xgb_grid$R2   <- NA_real_

#--------------------------------------------------
# 5. Grid search
#--------------------------------------------------
set.seed(123)

for (i in seq_len(nrow(xgb_grid))) {
  
  cat("Fitting model", i, "of", nrow(xgb_grid), "\n")
  
  params <- list(
    booster = "gbtree",
    objective = "count:poisson",
    eval_metric = "poisson-nloglik",
    eta = xgb_grid$eta[i],
    max_depth = xgb_grid$max_depth[i],
    min_child_weight = xgb_grid$min_child_weight[i],
    subsample = xgb_grid$subsample[i],
    colsample_bytree = xgb_grid$colsample_bytree[i]
  )
  
  fit <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 5000,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  pred_val <- predict(fit, dval)
  
  xgb_grid$best_iter[i] <- fit$best_iteration
  xgb_grid$RMSE[i] <- sqrt(mean((y_val - pred_val)^2))
  xgb_grid$MAE[i]  <- mean(abs(y_val - pred_val))
  xgb_grid$R2[i]   <- cor(y_val, pred_val)^2
}

# View tuning results
xgb_grid <- xgb_grid %>% arrange(RMSE)
print(xgb_grid)

best_params <- xgb_grid[which.min(xgb_grid$RMSE), ]

#--------------------------------------------------
# 6. Refit final model using time_id <= 56
#--------------------------------------------------
xg_train_final_df <- forecast_data %>%
  filter(time_id <= 56)

xg_train_final_df <- xg_train_final_df %>%
  mutate(
    month_f = as.factor(month_f),
    area    = as.factor(area)
  )

# rebuild matrix using train_final + test to keep columns aligned
all_final_df <- bind_rows(
  xg_train_final_df %>% mutate(dataset = "train_final"),
  xg_test_df        %>% mutate(dataset = "test")
)

X_all_final <- sparse.model.matrix(~ month_f + area + population + crime_lag1 - 1, data = all_final_df)

n_train_final <- nrow(xg_train_final_df)

X_train_final <- X_all_final[1:n_train_final, ]
X_test_full   <- X_all_final[(n_train_final + 1):nrow(X_all_final), ]

y_train_final <- xg_train_final_df$y

dtrain_final <- xgb.DMatrix(data = X_train_final, label = y_train_final)

final_params <- list(
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik",
  eta = best_params$eta,
  max_depth = best_params$max_depth,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree
)

set.seed(123)
xgb_model <- xgb.train(
  params = final_params,
  data = dtrain_final,
  nrounds = best_params$best_iter,
  verbose = 0
)

#--------------------------------------------------
# 7. Recursive multi-step forecasting
#--------------------------------------------------

# month 57
xg_test_df_1 <- xg_test_df %>% filter(time_id == 57)

tmp1 <- bind_rows(
  xg_train_final_df %>% select(all_of(feat_cols)),
  xg_test_df_1 %>% select(all_of(feat_cols))
)

X_tmp1 <- sparse.model.matrix(~ month_f + area + population + crime_lag1 - 1, data = tmp1)
X_test_1 <- X_tmp1[(nrow(tmp1) - nrow(xg_test_df_1) + 1):nrow(tmp1), ]

dtest_1 <- xgb.DMatrix(data = X_test_1)
pred_1 <- predict(xgb_model, dtest_1)

# month 58
xg_test_df_2 <- xg_test_df %>% filter(time_id == 58)
xg_test_df_2$crime_lag1 <- pred_1

tmp2 <- bind_rows(
  xg_train_final_df %>% select(all_of(feat_cols)),
  xg_test_df_2 %>% select(all_of(feat_cols))
)

X_tmp2 <- sparse.model.matrix(~ month_f + area + population + crime_lag1 - 1, data = tmp2)
X_test_2 <- X_tmp2[(nrow(tmp2) - nrow(xg_test_df_2) + 1):nrow(tmp2), ]

dtest_2 <- xgb.DMatrix(data = X_test_2)
pred_2 <- predict(xgb_model, dtest_2)

# month 59
xg_test_df_3 <- xg_test_df %>% filter(time_id == 59)
xg_test_df_3$crime_lag1 <- pred_2

tmp3 <- bind_rows(
  xg_train_final_df %>% select(all_of(feat_cols)),
  xg_test_df_3 %>% select(all_of(feat_cols))
)

X_tmp3 <- sparse.model.matrix(~ month_f + area + population + crime_lag1 - 1, data = tmp3)
X_test_3 <- X_tmp3[(nrow(tmp3) - nrow(xg_test_df_3) + 1):nrow(tmp3), ]

dtest_3 <- xgb.DMatrix(data = X_test_3)
pred_3 <- predict(xgb_model, dtest_3)

# combine predictions
pred <- c(pred_1, pred_2, pred_3)

#--------------------------------------------------
# 8. Test accuracy
#--------------------------------------------------
RMSE <- sqrt(mean((y_test - pred)^2))
MAE  <- mean(abs(y_test - pred))
R2   <- cor(y_test, pred)^2

c(RMSE = RMSE, MAE = MAE, R2 = R2)

#--------------------------------------------------
# 9. Actual vs Predicted Plot
#--------------------------------------------------
xg_test_df$xg_pred = pred 

plot_df <- xg_test_df %>%
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
    RMSE = sqrt(mean((y - xg_pred)^2, na.rm = TRUE)),
    MAE  = mean(abs(y - xg_pred), na.rm = TRUE),
    R2   = cor(y, xg_pred, use = "complete.obs")^2,
    label = paste0(
      "R² = ", round(R2, 2), "\n",
      "MAE = ", round(MAE, 2), "\n",
      "RMSE = ", round(RMSE, 2)
    ),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = y, y = xg_pred)) +
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

#--------------------------------------------------
# 10. Feature Importance
#--------------------------------------------------

imp <- xgb.importance(model = xgb_model)
xgb.plot.importance(imp)

xgb.plot.importance(imp[1:10])

imp %>%
  mutate(group = ifelse(grepl("area", Feature), "area",
                        ifelse(grepl("month_f", Feature), "month",
                               Feature))) %>%
  group_by(group) %>%
  summarise(total_gain = sum(Gain)) %>%
  arrange(desc(total_gain))



imp_grouped <- imp %>%
  mutate(
    group = case_when(
      grepl("^area", Feature) ~ "Area",
      grepl("^month_f", Feature) ~ "Month",
      Feature == "population" ~ "Population",
      Feature == "crime_lag1" ~ "Lagged Crime",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(group) %>%
  summarise(Gain = sum(Gain)) %>%
  arrange(desc(Gain))

imp_grouped <- imp_grouped %>%
  mutate(perc = Gain / sum(Gain))

ggplot(imp_grouped, aes(x = reorder(group, perc), y = perc)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Relative Feature Importance (XGBoost)",
    x = "Feature Group",
    y = "Percentage Contribution"
  ) +
  theme_minimal()
