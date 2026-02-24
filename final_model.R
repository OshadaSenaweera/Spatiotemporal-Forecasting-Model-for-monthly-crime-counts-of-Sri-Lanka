library(INLA)        # install.packages("INLA", repos=c(getOption("repos"), INLA='https://inla.r-inla-download.org/R/stable'))
library(ggplot2)
library(Metrics)
library(dplyr)
library(sf)
library(spdep)
library(tmap)
################################################################################

crime_monthly <- readRDS("crime_monthly_v3.rds")
g <- inla.read.graph("adm3.adj") #read the created graph

crime_monthly$month_f = as.factor(crime_monthly$month)
crime_monthly$year_f = as.factor(crime_monthly$year)
crime_monthly$st_id <- as.integer(interaction(crime_monthly$area_id, crime_monthly$time_id, 
                                              drop = TRUE))

crime_monthly <- crime_monthly %>%
  arrange(time_id, area_id)

crime_monthly <- crime_monthly %>%
  filter(time_id != 60)

# lagged term

crime_monthly <- crime_monthly %>%
  arrange(area_id, year, month) %>%   # VERY important
  group_by(area_id) %>%
  mutate(
    crime_lag1 = lag(y, 1),
    crime_lag2 = lag(y, 2),
    crime_lag3 = lag(y, 3)
  ) %>%
  ungroup()

crime_monthly <- crime_monthly %>%
  arrange(time_id, area_id)

saveRDS(crime_monthly, "crime_monthly_v4.rds")

################################################################################

crime_monthly <- readRDS("crime_monthly_v4.rds")
g <- inla.read.graph("adm3.adj") #read the created graph

################################################################################
forecast_data <- crime_monthly %>%
  mutate(y_n = ifelse(time_id %in% 57:59, NA, y))

acf(crime_monthly$y, lag.max = 24)
pacf(crime_monthly$y, lag.max = 24)

################################################################################

formula_m1 <- y_n ~ 1 + month_f + f(time_id, model="ar1") + f(st_id,model="iid")+
                    f(area_id2, model = "besag", graph = g, group = time_id,
                                     control.group = list(model = "ar1"))
  
sptm_m1 <- inla(
  formula_m1,
  family = "poisson",       #zeroinflatedpoisson1 #zeroinflatednbinomial1
  data = forecast_data,
  offset = log(E),
  control.predictor = list(compute = TRUE,link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  # safer numerical settings:
  control.inla = list(
    strategy = "simplified.laplace"),       # gaussian, simplified.laplace (default) or laplace
  control.mode = list(restart = TRUE),
  verbose = TRUE
)

#saveRDS(sptm_m1, file = "final_model_2.rds")
#sptm_m1 <- readRDS("final_model.rds")

saveRDS(sptm_m1, file = "f3_model.rds")

saveRDS(sptm_m1, file = "f4_model.rds")

sptm_m1 <- readRDS("f3_model.rds")
summary(sptm_m1)

#Model validation - check PIT and CPO values 
sum(sptm_m1$cpo$failure)  #no failiures is expected
sum(log(sptm_m1$cpo$cpo)) # can be used to compare competitive models in terms of prediction 
# performance (larger values denote a better fitting)
hist(sptm_m1$cpo$pit, main="PIT histogram",xlab="PIT values", breaks =100) #should be uniform

#hist(sptm_m1$cpo$cpo, breaks =100, xlab = "CPO values") # cpo larger the better 



#get other metrics
#crime_monthly$model_pred <- sptm_m1$summary.fitted.values$mean #get posterior mean of each prediction
#summary(crime_monthly$model_pred)

forecast_data$model_pred <-  sptm_m1$summary.fitted.values$mean
summary(forecast_data$model_pred)

test_forecast <- forecast_data %>%
  filter(time_id %in% 57:59)

library(Metrics)

rmse <- rmse(test_forecast$y, test_forecast$model_pred)
mae  <- mae(test_forecast$y, test_forecast$model_pred)

c(RMSE = rmse, MAE = mae)
correlation_value   <- cor(test_forecast$y, test_forecast$model_pred)

#forecast_data$crime_lag1-forecast_data$crime_lag1

test_forecast$avg = (test_forecast$crime_lag1+test_forecast$crime_lag2+test_forecast$crime_lag3)/3

rmse <- rmse(test_forecast$y, test_forecast$avg)
mae  <- mae(test_forecast$y, test_forecast$avg)

c(RMSE = rmse, MAE = mae)
correlation_value   <- cor(test_forecast$y, test_forecast$crime_lag1)

forecast_data$residuals = forecast_data$y-forecast_data$model_pred
# Plotting residuals temporally 
temporal_resid <- forecast_data %>%
  group_by(time_id) %>%
  summarise(resid_mean = mean(residuals))

ggplot(temporal_resid, aes(x = time_id, y = resid_mean)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Month index", y = "Residual total",
       title = "Temporal pattern of residuals") +
  theme_minimal()
















cpo <- sptm_m1$cpo$cpo
pit<- sptm_m1$cpo$pit

fail_cpo <- sptm_m1$cpo$failure
sum(fail_cpo)

ok <- is.finite(cpo)& (fail == 0)          #& (cpo > 0)
loo_log_score <- sum(log(cpo[ok]))
loo_log_score 
sum(ok)

monthly_avg <- crime_monthly %>%
  group_by(month) %>%
  summarise(month_avg=mean(y))





ggplot(crime_monthly, aes(x = y, y = model_pred)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual montly crime count",
    y = "Predicted mean",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

correlation_value   <- cor(crime_monthly$y, crime_monthly$model_pred)

crime_monthly$residuals = crime_monthly$y - crime_monthly$model_pred
summary(crime_monthly$residuals)

# plotting residuals with fitted value
ggplot(crime_monthly, aes(x = model_pred, y = residuals)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted count", y = "Residual", title = "Residuals vs Predicted") +
  theme_minimal()

path <- "C:/Users/oshad/OneDrive/Documents/R/lka_admin_boundaries"
adm3_sf <- st_read(file.path(path, "lka_admin3.shp"))  # DS Division

adm3_sf <- adm3_sf %>% 
  rename(area = adm3_name)  #remove unwanted columns and rename area column

# Ensure order and geometry validity
adm3_sf <- adm3_sf %>%
  st_make_valid() %>%
  arrange(area)

adm3_sf$area_id = 1:339

spatial_resid <- crime_monthly %>%
  group_by(area_id) %>%
  summarise(resid_mean = mean(residuals))

adm3_resid <- adm3_sf %>%
  left_join(spatial_resid, by = "area_id")

tm_shape(adm3_resid) +
  tm_polygons("resid_mean", palette = "RdBu", style = "quantile",
              title = "Mean Residuals (Observed - Predicted)")+
  tm_layout(title = "Spatial distribution of model residuals")


# Plotting residuals temporally 
temporal_resid <- crime_monthly %>%
  group_by(time_id) %>%
  summarise(resid_mean = sum(residuals))

ggplot(temporal_resid, aes(x = time_id, y = resid_mean)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Month index", y = "Residual total",
       title = "Temporal pattern of residuals") +
  theme_minimal()

################################################################################
nsim <- 100   # number of posterior samples

samp <- inla.posterior.sample(
  n = nsim,
  result = sptm_m1
)

n_obs <- nrow(crime_monthly)

eta_mat <- matrix(NA, nrow = nsim, ncol = n_obs)

for (i in seq_len(nsim)) {
  eta_mat[i, ] <- samp[[i]]$latent[
    grep("^Predictor", rownames(samp[[i]]$latent))
    , 1]
}

mu_mat <- exp(eta_mat)

yrep_mat <- matrix(
  rpois(nsim * n_obs, lambda = as.vector(mu_mat)),
  nrow = nsim,
  ncol = n_obs
)

yrep_mean <- colMeans(yrep_mat)
crime_monthly$model_pred <- yrep_mean

crime_monthly$residuals_n = crime_monthly$y - crime_monthly$model_pred
















c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)


sum(sptm_m1 $cpo$failure)  #no failiures is expected
sum(log(sptm_m1 $cpo$cpo)) # can be used to compare competitive models in terms of prediction 
# performance (larger values denote a better fitting)

hist(sptm_m1 $cpo$pit, main="PIT histogram",xlab="PIT values", breaks =100) #should be uniform
hist(sptm_m1 $cpo$cpo, breaks =100, xlab = "CPO values") #CPO>0.5 should not exsist 

summary(sptm_m1 )





crime_monthly$model_pred = sptm_m1$summary.fitted.values$mean



rmse_value <- rmse(crime_monthly$y, crime_monthly$model_pred)
mae_value  <- mae(crime_monthly$y, crime_monthly$model_pred)
r2_value   <- cor(crime_monthly$y, crime_monthly$model_pred)

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

crime_monthly$residuals = crime_monthly$y - crime_monthly$model_pred
crime_monthly$pearson_resid <- (crime_monthly$y - crime_monthly$model_pred)/sqrt(crime_monthly$model_pred)


hist(crime_monthly$pearson_resid,breaks = 100,main = "Histogram of residuals", xlab = "residuals")


# Plotting residuals temporally 
temporal_resid <- crime_monthly %>%
  group_by(time_id) %>%
  summarise(resid_mean = mean(residuals))

ggplot(temporal_resid, aes(x = time_id, y = resid_mean)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Week index", y = "Residual total",
       title = "Temporal pattern of residuals") +
  theme_minimal()



























# plotting residuals with fitted value
ggplot(crime_monthly, aes(x = model_pred, y = residuals)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted count", y = "Residual", title = "Residuals vs Predicted") +
  theme_minimal()


path <- "C:/Users/oshad/OneDrive/Documents/R/lka_admin_boundaries"
adm3_sf <- st_read(file.path(path, "lka_admin3.shp"))  # DS Division



adm3_sf <- adm3_sf %>% 
  rename(area = adm3_name)  #remove unwanted columns and rename area column

# Ensure order and geometry validity
adm3_sf <- adm3_sf %>%
  st_make_valid() %>%
  arrange(area)

adm3_sf$area_id = 1:339

spatial_resid <- crime_monthly %>%
  group_by(area_id) %>%
  summarise(resid_median = median(residuals))


adm3_resid <- adm3_sf %>%
  left_join(spatial_resid, by = "area_id")


tm_shape(adm3_resid) +
  tm_polygons("resid_median", palette = "RdBu", style = "quantile",
              title = "Mean Residuals (Observed - Predicted)") 


# Plotting residuals temporally 
temporal_resid <- crime_monthly %>%
  group_by(time_id) %>%
  summarise(resid_mean = mean(pearson_resid))

ggplot(temporal_resid, aes(x = time_id, y = resid_mean)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Week index", y = "Residual total",
       title = "Temporal pattern of residuals") +
  theme_minimal()


st_effect <- sptm_m1$summary.random$area_id[, "mean"]

N <- length(unique(crime_monthly$area_id2))
T <- length(unique(crime_monthly$time_id))
length(st_effect) == N * T   # should be TRUE

st_mat <- matrix(st_effect, nrow = N, ncol = T)

RR_mat <- exp(st_mat)

crime_monthly$st_effect <- st_mat[cbind(crime_monthly$area_id,crime_monthly$time_id)]

crime_monthly$st_RR <- exp(crime_monthly$st_effect)

library(reshape2)

df_long <- melt(st_mat)
colnames(df_long) <- c("area", "time", "effect")

ggplot(df_long, aes(time, area, fill = effect)) +
  geom_raster() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  labs(
    fill = "log RR",
    x = "Time",
    y = "Area"
  ) +
  theme_minimal()

times_all <- sort(unique(crime_monthly$time_id))
times_50  <- times_all[round(seq(1, length(times_all), length.out = 50))]


anim_df <- crime_monthly %>%
  filter(time_id %in% times_50) %>%
  select(area_id, time_id, st_effect) %>%
  mutate(st_RR = exp(st_effect))  # optional, easier to interpret

map_anim <- adm3_sf %>%
  left_join(anim_df, by = "area_id")


tmap_mode("plot")

m <- tm_shape(map_anim) +
  tm_polygons(
    "st_effect",                 # or "st_RR"
    palette = "-RdBu",
    style   = "cont",
    midpoint = 0,                # if using st_effect (log RR)
    title = "Spatio-temporal effect (log RR)"
  ) +
  tm_borders(lwd = 0.3, col = "grey30") +
  tm_layout(frame = FALSE, legend.outside = TRUE) +
  tm_facets(along = "time_id", free.scales = FALSE)

# Create GIF
tmap_animation(
  m,
  filename = "st_effect_animation.gif",
  width = 1200, height = 900,
  delay = 40   # ms between frames; increase to slow down
)

