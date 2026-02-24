library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

crime_dataset_SL <- readRDS("crime_dataset_SL.rds")
crime_monthly <- readRDS("crime_monthly_v4.rds")

unique(crime_dataset_SL$crime_type)

crime_dataset_SL$report_delay = crime_dataset_SL$reported_date-crime_dataset_SL$cr_start_date_chnged
str(crime_dataset_SL)
  
crime_dataset_SL$delay_days <- as.numeric(crime_dataset_SL$report_delay)
summary(crime_dataset_SL$delay_days)

ggplot(crime_dataset_SL, aes(x = delay_days)) +
  geom_histogram(binwidth = 5) +
  labs(
    x = "Reporting Delay (Days)",
    y = "Frequency"
  ) +
  theme_minimal()

crime_dataset_SL <- crime_dataset_SL %>%
  mutate(delay_cat = case_when(
    delay_days <= 1 ~ "Within 1 day",
    delay_days <= 7  ~ "Within 1 week",
    delay_days <= 31 ~ "1 week-1 month",
    delay_days <= 90 ~ "1 month-3 month",
    delay_days <= 365 ~ "3 months-1 year",
    delay_days > 365  ~ "> 1 year"
  ))

crime_dataset_SL$delay_cat <- factor(
  crime_dataset_SL$delay_cat,
  levels = c("Within 1 day", "Within 1 week", "1 week-1 month",
             "1 month-3 month", "3 months-1 year","> 1 year")
)

ggplot(crime_dataset_SL, aes(x = delay_cat)) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Delay Time",
    y = "Number of Incidents"
  ) +
  theme_minimal()

ggplot(crime_dataset_SL, aes(x = delay_cat)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill = "darkred") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Reporting Delay",
    y = "Percentage of crime records"
  ) +
  theme_minimal()

crime_dataset_SL_n <- read_excel("crime_incidents_SL_new.xlsx", sheet = "Sheet1")
unique(crime_dataset_SL_n$type)

library(forcats)

crime_dataset_SL_n %>%
  mutate(type = fct_rev(fct_infreq(type))) %>%  # reverse order
  ggplot(aes(x = type)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill = "darkred") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(
    x = "Crime Type",
    y = "Percentage of crime records"
  ) +
  theme_minimal()


crime_monthly <- readRDS("crime_monthly_v4.rds")
str(crime_monthly)

national_monthly <- crime_monthly %>%
  group_by(year, month) %>%
  summarise(total_crime = sum(y), .groups = "drop") %>%
  arrange(year, month)

national_monthly$date <- as.Date(
  paste(national_monthly$year, national_monthly$month, "01", sep = "-")
)

ggplot(national_monthly, aes(x = date, y = total_crime)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(
    x = "Month",
    y = "Total Crime Count"
  ) +
  theme_minimal()

district_monthly <- crime_monthly %>%
  group_by(district, year, month) %>%
  summarise(total_crime = sum(y), .groups = "drop")

district_monthly$date <- as.Date(
  paste(district_monthly$year, district_monthly$month, "01", sep = "-")
)

ggplot(district_monthly, aes(x = date, y = total_crime)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ district, scales = "free_y") +
  labs(
    title = "Monthly Crime Counts by District",
    x = "Month",
    y = "Crime Count"
  ) +
  theme_minimal()

################################################################################

area_avg <- crime_monthly %>%
  group_by(area) %>%
  summarise(
    avg_monthly_crime = mean(y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_monthly_crime))


library(sf)
library(spdep)

path <- "C:/Users/oshad/OneDrive/Documents/R/lka_admin_boundaries"
adm3 <- st_read(file.path(path, "lka_admin3.shp"))  # DS Division
adm3 <- adm3 %>% select(area=adm3_name,
                        province_1=adm1_name,
                        district_1=adm2_name,
                        center_lat,center_lon, geometry)%>%
                    arrange(area)

adm3_joined <- adm3 %>%
  left_join(area_avg, by = "area")

######spatial autocorrelation

# Ensure geometry is valid
adm3_joined <- st_make_valid(adm3_joined)

# Create neighbour list (queen contiguity)
nb <- poly2nb(adm3_joined,queen = TRUE)


# manually adding edges
dym <- c(st_distance(adm3_joined[c(48),], adm3_joined))
sort(dym)[1:12]
closest = order(dym)[1:3]
nb_1 <- addlinks1(nb, from = closest[1], to = closest[2:3])


dym2 <- c(st_distance(adm3_joined[c(118),], adm3_joined))
closest = order(dym2)[1:3]
nb_2 <- addlinks1(nb_1, from = closest[1], to = closest[3])

dym3 <- c(st_distance(adm3_joined[c(66),], adm3_joined))
closest = order(dym3)[1:3]
nb_3 <- addlinks1(nb_2, from = closest[2], to = closest[3])

# Save the graph as INLA graph for modelling
#nb2INLA("adm3.adj", nb_3)

plot(st_geometry(adm3_joined), border="red")


# Convert to spatial weights
lw <- nb2listw(nb_3, style = "W", zero.policy = TRUE)


# Global Moran's test

moran_result <- moran.test(adm3_joined$avg_monthly_crime, lw, zero.policy = TRUE)
moran_result

moran_over_time <- crime_monthly %>%
  group_by(time_id) %>%
  summarise(
    moran_I = moran.test(y, lw, zero.policy = TRUE)$estimate[1],
    p_value = moran.test(y, lw, zero.policy = TRUE)$p.value,
    .groups = "drop"
  )

ggplot(moran_over_time, aes(x = time_id, y = moran_I)) +
  geom_line(color = "darkred") +
  labs(
    title = "Monthly Global Moran's I",
    x = "Time",
    y = "Moran's I"
  ) +
  theme_minimal()

################################################################################
#local Moran

local_moran <- localmoran(
  adm3_joined$avg_monthly_crime,
  lw,
  zero.policy = TRUE
)

adm3_joined$Ii        <- local_moran[,1]  # Local Moran's I
adm3_joined$Z.Ii      <- local_moran[,4]
adm3_joined$p_value   <- local_moran[,5]

lag_y <- lag.listw(lw, adm3_joined$avg_monthly_crime, zero.policy = TRUE)


y_mean <- mean(adm3_joined$avg_monthly_crime, na.rm = TRUE)

adm3_joined$cluster <- "Not significant"

adm3_joined$cluster[
  adm3_joined$avg_monthly_crime > y_mean &
    lag_y > y_mean &
    adm3_joined$p_value < 0.05
] <- "High-High"

adm3_joined$cluster[
  adm3_joined$avg_monthly_crime < y_mean &
    lag_y < y_mean &
    adm3_joined$p_value < 0.05
] <- "Low-Low"

adm3_joined$cluster[
  adm3_joined$avg_monthly_crime > y_mean &
    lag_y < y_mean &
    adm3_joined$p_value < 0.05
] <- "High-Low"

adm3_joined$cluster[
  adm3_joined$avg_monthly_crime < y_mean &
    lag_y > y_mean &
    adm3_joined$p_value < 0.05
] <- "Low-High"

adm3_joined$cluster <- factor(
  adm3_joined$cluster,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant")
)

library(ggplot2)

ggplot(adm3_joined) +
  geom_sf(aes(fill = cluster), color = "grey50", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "High-High" = "#b2182b",
      "Low-Low" = "#2166ac",
      "High-Low" = "#ef8a62",
      "Low-High" = "#67a9cf",
      "Not significant" = "grey90"
    ),
    name = "LISA Cluster"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )



# Standardise variable
z <- scale(adm3_joined$avg_monthly_crime)

# Spatial lag
lag_z <- lag.listw(lw, z, zero.policy = TRUE)

plot(z, lag_z,
     xlab = "Standardized Crime",
     ylab = "Spatial Lag of Crime",
     main = "Moran Scatter Plot")

abline(lm(lag_z ~ z), col = "red")
abline(h = 0, v = 0, lty = 2)


#############temporal autocorretation

national_ts <- crime_monthly %>%
  group_by(year, month) %>%
  summarise(total_crime = sum(y), .groups = "drop") %>%
  arrange(year, month)

national_series <- ts(national_ts$total_crime,
                      start = c(2019, 1),
                      frequency = 12)


Box.test(national_series,
         lag = 12,
         type = "Ljung-Box")


acf(national_series,
    lag.max = 24,
    main = "ACF: National Monthly Crime Counts")



library(forecast)
library(ggplot2)

ggAcf(national_series, lag.max = 24) +
  labs(title = "") +
  theme_minimal()



###############################################################################


# Convert to spatial weights
#lw <- nb2listw(nb, style = "W", zero.policy = TRUE)






# Standardise variable
z <- scale(adm3_joined$avg_monthly_crime)

# Spatial lag
lag_z <- lag.listw(lw, z, zero.policy = TRUE)

plot(z, lag_z,
     xlab = "Standardized Crime",
     ylab = "Spatial Lag of Crime",
     main = "Moran Scatter Plot")

abline(lm(lag_z ~ z), col = "red")
abline(h = 0, v = 0, lty = 2)













library(tmap)

tmap_mode("plot")

tm_shape(adm3_joined) +
  tm_polygons(
    "avg_monthly_crime",
    palette = "Reds",
    title = "Average Monthly Crime"
  ) +
  tm_layout(
    main.title = "Average Monthly Crime Count by DS Division",
    legend.outside = TRUE
  )


tm_shape(adm3_joined) +
  tm_polygons(
    "avg_monthly_crime",
    palette = "Reds",        # darker = higher (often clearer)
    style = "cont",
    title = "Avg Monthly Crime"
  ) +
  tm_layout(
    main.title = "Average Monthly Crime Count by DS Division",
    main.title.size = 1.1,
    legend.outside = TRUE,
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.frame = FALSE,
    frame = FALSE,
    bg.color = "white"
  )

any(is.na(adm3_joined$avg_monthly_crime))


adm3_clean <- adm3_joined %>%
  filter(!is.na(avg_monthly_crime))

library(ggplot2)
library(viridis)  # install.packages("viridis") if needed

p <- ggplot(adm3_joined) +
  geom_sf(aes(fill = avg_monthly_crime), color = "grey50", linewidth = 0.15) +
  scale_fill_gradientn(
    colours = c(
      "#f6efe9",   # very light beige
      "#fdd0c2",   # light pink
      "#fc9272",   # soft coral
      "#ef3b2c",   # medium red
      "#99000d"    # deep red
    ),
    name = "Average Monthly Crime Count"
  ) +
  coord_sf(datum = NA) +  # removes axes/grid
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.height = unit(10, "mm"),
    legend.key.width  = unit(5, "mm"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.margin = margin(8, 8, 8, 8)
  )

p

################################################################################

crime_dataset_SL <- readRDS("crime_dataset_SL.rds")
str(crime_dataset_SL)

crime_dataset_SL_new <- crime_dataset_SL %>%
  filter(!(year == 2023 & month == 12))

unique(crime_dataset_SL_new$crime_type)

crime_dataset_SL_new <- crime_dataset_SL_new %>%
  mutate(crime_type = case_when(
    crime_type == "Public Order Crimes" ~ "Property Crimes",
    crime_type == "Firearms/Weapons Crimes" ~ "Violent Crimes",
    TRUE ~ crime_type
  ))


monthly_by_type <- crime_dataset_SL_new %>%
  group_by(year, month, crime_type) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(date)


ggplot(monthly_by_type, aes(x = date, y = crime_count, color = crime_type)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Month",
    y = "Crime Count",
    color = "Crime Type"
  ) +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.8, "cm")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
  

ggplot(monthly_by_type,
       aes(x = date, y = crime_count, color = crime_type)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Reds") +
  labs(
    x = "Month",
    y = "Crime Count",
    color = "Crime Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

################################################################################

crime_monthly <- readRDS("crime_monthly_v4.rds")
str(crime_monthly)

month_avg_national <- crime_monthly %>%
  group_by(year, month) %>%
  summarise(total_y = sum(y, na.rm = TRUE), .groups = "drop") %>%
  group_by(month) %>%
  summarise(avg_total_y = mean(total_y, na.rm = TRUE), .groups = "drop") %>%
  mutate(month = factor(month, levels = 1:12))

ggplot(month_avg_national, aes(x = month, y = avg_total_y)) +
  geom_col(fill = "darkred") +
  labs(
    x = "Month",
    y = "Average crime count"
  ) +
  theme_minimal()

################################################################################
crime_weekly <- readRDS("crime_weekly_new_v2.rds")

police_stations <- crime_weekly %>%
  slice(1:339) %>%
  select(area, num_police_stations)



################################################################################

library(sf)
library(tmap)
library(rgeoda)
library(geodaData)
library(RColorBrewer)
guerry <- geodaData::guerry
