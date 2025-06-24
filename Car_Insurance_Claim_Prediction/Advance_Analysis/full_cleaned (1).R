# Load necessary libraries
library(dplyr)
library(rlang)

# Step 1: Load the dataset
train_data <- read.csv("C:/Users/Hiruni/OneDrive/Desktop/full_set.csv")

# Step 2: Check unique values in each column
sapply(train_data, function(x) length(unique(x)))

# Step 3: Convert specified columns to factors
train_data$is_claim <- as.factor(train_data$is_claim)
train_data$model <- as.factor(train_data$model)

##divide area cluster based on population density using quantile method
quantiles <- quantile(train_data$population_density, probs = c(0.33, 0.67), na.rm = TRUE)

# Recategorize area_cluster based on population density
train_data <- train_data %>%
  mutate(area_cluster = case_when(
    population_density <= quantiles[1] ~ "Low",
    population_density > quantiles[1] & population_density <= quantiles[2] ~ "Medium",
    population_density > quantiles[2] ~ "High"
  ))

# Convert to factor for easier plotting
train_data$area_cluster <- factor(train_data$area_cluster, levels = c("Low", "Medium", "High"))

# Check the distribution
table(train_data$area_cluster)



#Remove unnecessary columns
train_data <- train_data %>%
  select(-c('length', 'width', 'gross_weight', 'height','population_density','policy_id','make','segment','fuel_type',
            'engine_type',	'airbags',	'rear_brakes_type',	'cylinder',	'transmission_type'	,
            'gear_box','steering_type','ncap_rating','max_torque','max_power','is_esc','is_adjustable_steering','is_tpms','is_parking_sensors','is_parking_camera',
            'is_front_fog_lights','is_rear_window_wiper',
            'is_rear_window_washer','is_rear_window_defogger',
            'is_brake_assist',	'is_power_door_locks',
            'is_central_locking',	'is_power_steering','is_driver_seat_height_adjustable',
            'is_day_night_rear_view_mirror','is_ecw','is_speed_alert','displacement','turning_radius'
            
  ))

# Check the structure and summary of the dataset
str(train_data)
summary(train_data)

# Step 5: Check for missing values and duplicates
sum(is.na(train_data))        # Check for missing values
sum(duplicated(train_data))   # Check for duplicates

#Save the cleaned dataset
write.csv(train_data, "C:/Users/Hiruni/OneDrive/Desktop/full_cleaned.csv", row.names = FALSE)

# Verify the cleaned dataset
head(train_data)
str(train_data)
