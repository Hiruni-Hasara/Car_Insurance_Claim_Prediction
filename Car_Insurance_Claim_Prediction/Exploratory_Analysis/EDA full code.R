##############Data preprocessing
# Load necessary libraries
library(dplyr)
library(rlang)

# Step 1: Load the dataset
train_data <- read.csv("C:/Users/Hiruni/OneDrive/Desktop/train.csv")

# Step 2: Check unique values in each column
sapply(train_data, function(x) length(unique(x)))

# Step 3: Convert specified columns to factors
train_data$is_claim <- as.factor(train_data$is_claim)
train_data$ncap_rating <- as.factor(train_data$ncap_rating)
train_data$cylinder <- as.factor(train_data$cylinder)
train_data$gear_box <- as.factor(train_data$gear_box)
train_data$airbags <- as.factor(train_data$airbags)
train_data$make <- as.factor(train_data$make)

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


library(DescTools)  # For Cramér's V
#######cramer's v for segment vs height,width,gross weight,length for identify which are redundant variables.(find which are highly associated with segment)
# Convert numerical variables to categories
train_data$length_category <- cut(train_data$length, breaks=3, labels=c("Short", "Medium", "Long"))
train_data$width_category <- cut(train_data$width, breaks=3, labels=c("Narrow", "Medium", "Wide"))
train_data$height_category <- cut(train_data$height, breaks=3, labels=c("Low", "Medium", "High"))
train_data$weight_category <- cut(train_data$gross_weight, breaks=3, labels=c("Light", "Medium", "Heavy"))

# Compute Cramér’s V for each variable
cramer_length <- CramerV(train_data$segment, train_data$length_category)
cramer_width <- CramerV(train_data$segment, train_data$width_category)
cramer_height <- CramerV(train_data$segment, train_data$height_category)
cramer_weight <- CramerV(train_data$segment, train_data$weight_category)

# Print results
print(c(cramer_length, cramer_width, cramer_height, cramer_weight))




###we plot model vs car specifications plots to identify the relationships
library(ggplot2)
library(dplyr)

# List of categorical variables (excluding 'model' itself)
categorical_vars <- c('make','segment','fuel_type',
                      'engine_type',	'airbags',	'rear_brakes_type',	'cylinder',	'transmission_type'	,
                      'gear_box','steering_type','ncap_rating','max_torque','max_power','is_esc','is_adjustable_steering','is_tpms','is_parking_sensors','is_parking_camera',
                      'is_front_fog_lights','is_rear_window_wiper',
                      'is_rear_window_washer','is_rear_window_defogger',
                      'is_brake_assist',	'is_power_door_locks',
                      'is_central_locking',	'is_power_steering','is_driver_seat_height_adjustable',
                      'is_day_night_rear_view_mirror','is_ecw','is_speed_alert')

# Loop through categorical variables and plot grouped bar charts
for (var in categorical_vars) { 
  print(
    ggplot(train_data, aes(x = model, fill = get(var))) +
      geom_bar(position = "dodge", color = "black") +  # Dodge for side-by-side bars
      labs(title = paste("Chart of", var, "vs Model"), x = "Model", y = "Count", fill = var) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability
  )
}

###here we got that each variable in car specifications relates to the model.
##Ex:if a car which belongs to the model 1 has front fog lights ,then all the cars of model 1 has front fog lights.
##then we ddid cramers v for confirm this.
#######cramer's v for model vs car specification variables for identify which are redundant variables.(find which are highly associated with model)

train_data$displacement <- as.factor(train_data$displacement)
train_data$turning_radius <- as.factor(train_data$turning_radius)



# Compute Cramér’s V between model and car specifications
car_specifications<-c('make','segment','fuel_type',
                      'engine_type',	'airbags',	'rear_brakes_type',	'cylinder',	'transmission_type'	,
                      'gear_box','steering_type','ncap_rating','max_torque','max_power','is_esc','is_adjustable_steering','is_tpms','is_parking_sensors','is_parking_camera',
                      'is_front_fog_lights','is_rear_window_wiper',
                      'is_rear_window_washer','is_rear_window_defogger',
                      'is_brake_assist',	'is_power_door_locks',
                      'is_central_locking',	'is_power_steering','is_driver_seat_height_adjustable',
                      'is_day_night_rear_view_mirror','is_ecw','is_speed_alert','displacement','turning_radius')

cramer_results <- sapply(train_data[, car_specifications], function(x) CramerV(train_data$model, x))

# View results
print(cramer_results)


#Remove unnecessary columns
train_data <- train_data %>%
  select(-c('length', 'width', 'gross_weight', 'height','population_density','policy_id','make','segment','fuel_type',
            'engine_type',	'airbags',	'rear_brakes_type',	'cylinder',	'transmission_type'	,
            'gear_box','steering_type','ncap_rating','max_torque','max_power','is_esc','is_adjustable_steering','is_tpms','is_parking_sensors','is_parking_camera',
            'is_front_fog_lights','is_rear_window_wiper',
            'is_rear_window_washer','is_rear_window_defogger',
            'is_brake_assist',	'is_power_door_locks',
            'is_central_locking',	'is_power_steering','is_driver_seat_height_adjustable',
            'is_day_night_rear_view_mirror','is_ecw','is_speed_alert','displacement','turning_radius','length_category','width_category','height_category','weight_category'))

# Check the structure and summary of the dataset
str(train_data)
summary(train_data)

# Step 5: Check for missing values and duplicates
sum(is.na(train_data))        # Check for missing values
sum(duplicated(train_data))   # Check for duplicates

#Save the cleaned dataset
write.csv(train_data, "C:/Users/Hiruni/OneDrive/Desktop/train_cleaned.csv", row.names = FALSE)

# Verify the cleaned dataset
head(train_data)





#############Univariate Analysis
###univariate for categorical variables
library(ggplot2)
library(dplyr)
train_data=read.csv("C:/Users/Hiruni/OneDrive/Desktop/train_cleaned.csv")
sum(is.na(train_data)) 

categorical_vars <- c('area_cluster','model','is_claim')

##bar charts
# Loop through categorical variables and plot bar charts with global proportions
for (var in categorical_vars) {
  # Compute proportions manually
  prop_df <- as.data.frame(prop.table(table(train_data[[var]]))) 
  colnames(prop_df) <- c("Category", "Proportion")
  
  print(
    ggplot(prop_df, aes(x = Category, y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", color = "black") +  # Identity because we precomputed proportions
      geom_text(aes(label = scales::percent(Proportion)), 
                vjust = -0.5, size = 4, color = "black") +  # Show proportions as percentages
      labs(title = paste("Proportion of", var), x = var, y = "Proportion") +
      scale_fill_brewer(palette = "Set2") +  # Different colors for bars
      theme_minimal()
  )
}

###pie charts
# Loop through categorical variables and plot pie charts
for (var in categorical_vars) {
  # Compute proportions manually
  prop_df <- as.data.frame(prop.table(table(train_data[[var]]))) 
  colnames(prop_df) <- c("Category", "Proportion")
  
  # Plot pie chart using ggplot
  print(
    ggplot(prop_df, aes(x = "", y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +  # Create pie chart by polar coordinates
      geom_text(aes(label = scales::percent(Proportion)), 
                position = position_stack(vjust = 0.5), size = 4, color = "white") +  # Display percentage labels
      labs(title = paste("Proportion of", var)) +
      scale_fill_brewer(palette = "Set2") +  # Set color palette
      theme_void()  # Clean the plot background
  )
}

####univariate for quantitative variables
# List of quantitative variables
quantitative_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder", 
                       )  # Replace with actual names

# Plot histograms and boxplots for each quantitative variable
for (var in quantitative_vars) {
  
  # Histogram
  print(
    ggplot(train_data, aes(x = get(var))) +
      geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
      labs(title = paste("Histogram for", var), x = var, y = "Count") +
      theme_minimal()
  )
  
  # Boxplot
  print(
    ggplot(train_data, aes(y = get(var))) +
      geom_boxplot(fill = "lightgreen", color = "black") +  # Boxplot
      labs(title = paste("Boxplot for", var), y = var) +
      theme_minimal()
  )
}





############Bivariate Analysis
###Bivariate for categorical data
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)
train_data=read.csv("C:/Users/Hiruni/OneDrive/Desktop/train_cleaned.csv")


train_data$is_claim <- factor(train_data$is_claim, levels = c(0, 1), labels = c("No", "Yes"))


# List of categorical variables
categorical_vars <- c('model','area_cluster')

# Loop through each categorical variable and plot side-by-side proportion bar charts
for (var in categorical_vars) {
  train_data[[var]] <- factor(train_data[[var]])  # Ensure categorical variable is a factor
  
  # Create summary data frame where proportions sum to 1 within each category
  summary_data <- train_data %>%
    group_by(!!sym(var), is_claim) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(!!sym(var)) %>%
    mutate(proportion = count / sum(count))  # Normalize within each category
  
  # Generate side-by-side proportion bar chart
  ggplot(summary_data, aes_string(x = var, y = "proportion", fill = "is_claim")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +  # Side-by-side bars
    geom_text(aes(label = percent(proportion, accuracy = 0.1)), 
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3, color = "black") +  # Labels on top
    labs(title = paste("Proportion of", var, "by Claim Status"),
         x = var,
         y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("#EE2E31", "#4DD091"), labels = c("No", "Yes")) +
    theme_minimal() -> p
  
  print(p)  # Display the plot
}

###bivariate for quantitative variables
quantitative_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder", 
                       ) # Replace with actual variables

for (var in quantitative_vars) {
  summary_stats <- train_data %>%
    group_by(is_claim) %>%
    summarise(
      Median = median(.data[[var]], na.rm = TRUE),
      Mean = mean(.data[[var]], na.rm = TRUE),
      IQR_Lower = quantile(.data[[var]], 0.25, na.rm = TRUE),
      IQR_Upper = quantile(.data[[var]], 0.75, na.rm = TRUE)
    )
  
  p <- ggplot(train_data, aes_string(x = "is_claim", y = var, fill = "is_claim")) +
    geom_boxplot() +
    
    
    
    
    # Add Mean, Median, and IQR values above the plot
    annotate("text", x = 1, y = max(train_data[[var]], na.rm = TRUE) * 1.05, 
             label = paste0("Median: ", round(summary_stats$Median[1], 2),
                            "\nIQR: [", round(summary_stats$IQR_Lower[1], 2), ", ", 
                            round(summary_stats$IQR_Upper[1], 2), "]"),
             color = "red", size = 4, hjust = 0) +
    
    annotate("text", x = 2, y = max(train_data[[var]], na.rm = TRUE) * 1.05, 
             label = paste0("Median: ", round(summary_stats$Median[2], 2),
                            "\nIQR: [", round(summary_stats$IQR_Lower[2], 2), ", ", 
                            round(summary_stats$IQR_Upper[2], 2), "]"),
             color = "green", size = 4, hjust = 0) +
    
    # Labels and theme
    labs(title = paste("Distribution of", var, "by Claim Status"),
         x = "Claim Status",
         y = var) +
    scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

#####Heatmap
library(ggplot2)
library(corrplot)

# List of quantitative variables
quantitative_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder")
# Subset the data to include only the quantitative variables
quant_data <- train_data[, quantitative_vars]

# Compute the correlation matrix
cor_matrix <- cor(quant_data, use = "complete.obs")

# Plot heatmap using corrplot
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "upper", 
         order = "hclust", 
         tl.cex = 0.8, 
         tl.col = "black", 
         number.cex = 0.7, 
         addCoef.col = "black")

#### Multivariate Analysis 
### FAMD

# Load necessary libraries
library(FactoMineR)  # For performing FAMD
library(factoextra)  # For visualizing results
library(data.table)  # For data manipulation

# Load the dataset
##data <- fread("train_cleaned1.csv")  # Reading the dataset from a CSV file

# Select relevant variables for FAMD analysis
new_data <- train_data[ , c("policy_tenure", "age_of_car", "age_of_policyholder", "area_cluster", "model")]

# Preview the first few rows of the selected data
head(new_data)

# Perform FAMD analysis
res.famd <- FAMD(new_data, graph = FALSE)  # ncp = 10 specifies up to 10 dimensions to be retained

# Summarize the FAMD results
summary(res.famd)

# Visualize the variable factor map (representation of variables in the factor space)
fviz_famd_var(res.famd, repel = TRUE)  # repel = TRUE avoids label overlap for better readability

# Get the description of the dimensions
dimdesc(res.famd)

# Visualize the scree plot (to assess the percentage of variance explained by each dimension)
fviz_screeplot(res.famd)

# Visualize the contribution of variables to the first dimension (Dim 1)
fviz_contrib(res.famd, "var", axes = 1)

# Visualize the contribution of variables to the second dimension (Dim 2)
fviz_contrib(res.famd, "var", axes = 2)

# Visualize the quantitative variables on the factor map (with uniform color for variables)
fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")

# Visualize the quantitative variables on the factor map with a gradient color scale
# Gradient based on cos2 values (quality of representation of the variables)
fviz_famd_var(res.famd, "quanti.var", 
              col.var = "cos2",  # Color based on cos2 values
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Gradient color scheme
              repel = TRUE)  # Avoid label overlap



