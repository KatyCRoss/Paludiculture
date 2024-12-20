# Introduction ####
# This script was started on the 15/10/24 by Katy Ross used for preparing and visualising the data as part of the P3 R&D Paludiculture fellowship and report "Potential GHG emission savings from Paludiculture" for the UK Governments Department for Environment Food and Rural Affairs.

# Library area ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(pcaMethods)
library(stringr)
library(PMCMRplus)
library(car)
library(purrr)
library(paletteer)
library(forcats)
library(MASS)

# Data import ####
# Read in the Emissions dataset
Emissions <- read.csv("Emissions tables.csv", na.strings = "na") %>%
  mutate(WTD_mean = as.numeric(WTD_mean))%>%
  mutate(Common_name = as.factor(Common_name))%>%
  mutate(Scientific_name = as.factor(Scientific_name))%>%
  mutate(Land_use = as.factor(Land_use))%>%
  mutate(Former_land_use = as.factor(Former_land_use))%>%
  mutate(WTD_se = as.numeric(WTD_se))%>%
  mutate(WTD_u_95_CI = as.numeric(WTD_u_95_CI))%>%
  mutate(WTD_l_95_CI = as.numeric(WTD_l_95_CI))%>%
  mutate(CO2_t_ha_yr = as.numeric(CO2_t_ha_yr))%>%
  mutate(CO2_se = as.numeric(CO2_se))%>%
  mutate(CO2_u_95 = as.numeric(CO2_u_95))%>%
  mutate(CO2_l_95 = as.numeric(CO2_l_95))%>%
  mutate(CH4_t_CO2e_ha_yr = as.numeric(CH4_t_CO2e_ha_yr))%>%
  mutate(CH4_se = as.numeric(CH4_se))%>%
  mutate(CH4_u_95 = as.numeric(CH4_u_95))%>%
  mutate(CH4_l_95 = as.numeric(CH4_l_95))%>%
  mutate(N2O_t_CO2e_ha_yr = as.numeric(N2O_t_CO2e_ha_yr))%>%
  mutate(N2O_se = as.numeric(N2O_se))%>%
  mutate(N2O_u_95 = as.numeric(N2O_u_95))%>%
  mutate(N2O_l_95 = as.numeric(N2O_l_95))%>%
  mutate(DOC_t_CO2e_ha_yr = as.numeric(DOC_t_CO2e_ha_yr))%>%
  mutate(DOC_se = as.numeric(DOC_se))%>%
  mutate(DOC_u_95 = as.numeric(DOC_u_95))%>%
  mutate(DOC_l_95 = as.numeric(DOC_l_95))%>%
  mutate(POC_t_CO2e_ha_yr = as.numeric(POC_t_CO2e_ha_yr))%>%
  mutate(POC_se = as.numeric(POC_se))%>%
  mutate(POC_u_95 = as.numeric(POC_u_95))%>%
  mutate(POC_l_95 = as.numeric(POC_l_95))%>%
  mutate(Ditch_CH4_t_CO2e_ha_yr  = as.numeric(Ditch_CH4_t_CO2e_ha_yr ))%>%
  mutate(Ditch_CH4_se = as.numeric(Ditch_CH4_se))%>%
  mutate(Ditch_CH4_u_95 = as.numeric(Ditch_CH4_u_95))%>%
  mutate(Ditch_CH4_l_95 = as.numeric(Ditch_CH4_l_95))%>%
  mutate(TSR_t_CO2e_ha_yr  = as.numeric(TSR_t_CO2e_ha_yr ))%>%
  mutate(TSR_se = as.numeric(TSR_se))%>%
  mutate(TSR_u_95 = as.numeric(TSR_u_95))%>%
  mutate(TSR_l_95 = as.numeric(TSR_l_95))%>%
  mutate(Crop_t_CO2e_ha_yr = as.numeric(Crop_t_CO2e_ha_yr))%>%
  mutate(Crop_se = as.numeric(Crop_se))%>%
  mutate(Crop_u_95 = as.numeric(Crop_u_95))%>%
  mutate(Crop_l_95 = as.numeric(Crop_l_95))%>%
  mutate(measurement_period = as.character(measurement_period))%>%
  mutate(Days = as.numeric(Days))%>%
  mutate(notes  = as.character(notes ))%>%
  mutate(reference = as.character(reference))%>%
  mutate(Fertiliser = as.factor(Fertiliser))%>%
  mutate(PFT = as.factor(PFT))%>%
  mutate(WT_Group = as.factor(WT_Group))%>%
  mutate(Upper_WTD = as.factor(Upper_WTD))%>%
  mutate(Lower_WTD  = as.factor(Lower_WTD))%>%
  mutate(Method = as.factor(Method))%>%
  mutate(Fertiliser = str_trim(Fertiliser))

# Remove rows where there is less than a year of data to account for seasonality, also remove the tree species as these will be covered by Tier 3 methodologies
Emissions <- subset(Emissions, Days >= 290 & !(Common_name %in% c("Norway spruce", "Lodgepole pine", "Black alder")))

# Visualise how many of each emission factor I have
# Calculate the count of non-NA values for each specified column and directly rename them
count_non_na <- Emissions %>%
  summarise(
    CO2 = sum(!is.na(CO2_t_ha_yr)),
    CH4 = sum(!is.na(CH4_t_CO2e_ha_yr)),
    N2O = sum(!is.na(N2O_t_CO2e_ha_yr)),
    DOC = sum(!is.na(DOC_t_CO2e_ha_yr)),
    POC = sum(!is.na(POC_t_CO2e_ha_yr)),
    `Ditch CH4` = sum(!is.na(Ditch_CH4_t_CO2e_ha_yr)),
    TSR = sum(!is.na(TSR_t_CO2e_ha_yr)),
    Crop = sum(!is.na(Crop_t_CO2e_ha_yr))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Non_NA_Count")

# Reorder Variable levels and set custom labels to be in order of size
count_non_na$Variable <- factor(count_non_na$Variable, 
                                levels = c("CH4", "CO2", "N2O", "Crop", "DOC", "Ditch CH4", "TSR", "POC"))


# Plot the data with subscripts for CH4, CO2, N2O and no gap at zero 
ggplot(count_non_na, aes(x = Variable, y = Non_NA_Count)) +
  geom_bar(stat = "identity", fill = "#b2df8a") +
  scale_x_discrete(labels = c(
    expression(CH[4]), 
    expression(CO[2]), 
    expression(N[2] * O), 
    "Crop", "DOC", "Ditch CH4", "TSR", "POC"
  )) +
  scale_y_continuous(expand = c(0, 0)) +  # Removes the gap between x-axis and zero
  labs(y = "Count of Values") +  # No x-axis label
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15),  # Set y-axis label font size
    axis.title.x = element_blank()           # Remove x-axis title
  )

# Seperate the datasets into individual ones for CO2, CH4, N2O, etc.
DirectCO2 <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CO2_t_ha_yr))

DirectCH4 <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CH4_t_CO2e_ha_yr))

DirectN2O <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(N2O_t_CO2e_ha_yr))

DitchCH4 <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Ditch_CH4_t_CO2e_ha_yr))         

DOC <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(DOC_t_CO2e_ha_yr))

POC <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(POC_t_CO2e_ha_yr))

Crop <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Crop_t_CO2e_ha_yr))

# Now do the same for the upper confidence intervals

DirectCO2_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CO2_u_95))

DirectCH4_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CH4_u_95))

DirectN2O_upper <- Emissions %>%
    filter(!is.na(WTD_mean) & !is.na(N2O_u_95))

DitchCH4_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Ditch_CH4_u_95))         

DOC_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(DOC_u_95))

POC_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(POC_u_95))

Crop_upper <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Crop_u_95))

# Repeat for the lower confidence intervals

DirectCO2_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CO2_l_95))

DirectCH4_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(CH4_l_95))

DirectN2O_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(N2O_l_95))

DitchCH4_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Ditch_CH4_l_95))         

DOC_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(DOC_l_95))

POC_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(POC_l_95))

Crop_lower <- Emissions %>%
  filter(!is.na(WTD_mean) & !is.na(Crop_l_95))

# Initial visualisation ####
# As a first step visualise the data to see if there are any patterns or anything we want to remove

#For CO2 first
# Add an identifier column to each dataset
DirectCO2 <- DirectCO2 %>%
  rename(CO2_t_ha_yr_plot = CO2_t_ha_yr)%>%
  mutate(Source = "Mean CO2")

DirectCO2_upper <- DirectCO2_upper %>%
  rename(CO2_t_ha_yr_plot = CO2_u_95)%>%
  mutate(Source = "Upper CO2 interval")

DirectCO2_lower <- DirectCO2_lower %>%
  rename(CO2_t_ha_yr_plot = CO2_l_95)%>%
  mutate(Source = "Lower CO2 interval")

# Bind all datasets into one
CO2_combined <- bind_rows(DirectCO2, DirectCO2_upper, DirectCO2_lower)

#Visualise CO2
ggplot(CO2_combined, aes(x = WTD_mean, y = CO2_t_ha_yr_plot, color = Source)) +
  geom_point() +  # Scatter plot
  labs(x = "WTD_mean", y = "CO2 (t/ha/yr)", title = "Plot of CO2 emissions by water table depth") +
  theme_minimal() +  # Use a clean theme
  theme(axis.title.x = element_text(size = 20), # Increase x-axis title size
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20), 
    plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size, center it
    legend.text = element_text(size = 18),   # Increase legend text size
    legend.title = element_text(size = 16))   # Increase legend title size

# Visualise for methane
# Add an identifier column to each dataset
DirectCH4 <- DirectCH4 %>%
  rename(CH4_t_ha_yr_plot = CH4_t_CO2e_ha_yr)%>%
  mutate(Source = "Mean CH4")

DirectCH4_upper <- DirectCH4_upper %>%
  rename(CH4_t_ha_yr_plot = CH4_u_95)%>%
  mutate(Source = "Upper CH4 interval")

DirectCH4_lower <- DirectCH4_lower %>%
  rename(CH4_t_ha_yr_plot = CH4_l_95)%>%
  mutate(Source = "Lower CH4 interval")

# Bind all datasets into one
CH4_combined <- bind_rows(DirectCH4, DirectCH4_upper, DirectCH4_lower)

#Visualise CH4
ggplot(CH4_combined, aes(x = WTD_mean, y = CH4_t_ha_yr_plot, color = Source)) +
  geom_point() +  # Scatter plot
  labs(x = "WTD_mean", y = "CH4 (t/CO2e/ha/yr)", title = "Plot of CH4 emissions by water table depth") +
  theme_minimal() +  # Use a clean theme
  theme(axis.title.x = element_text(size = 20), # Increase x-axis title size
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size, center it
        legend.text = element_text(size = 18),   # Increase legend text size
        legend.title = element_text(size = 16))   # Increase legend title size

# Now for N20
# Add an identifier column to each dataset
DirectN2O <- DirectN2O %>%
  rename(N2O_t_ha_yr_plot = N2O_t_CO2e_ha_yr)%>%
  mutate(Source = "Mean N2O")

DirectN2O_upper <- DirectN2O_upper %>%
  rename(N2O_t_ha_yr_plot = N2O_u_95)%>%
  mutate(Source = "Upper N2O interval")

DirectN2O_lower <- DirectN2O_lower %>%
  rename(N2O_t_ha_yr_plot = N2O_l_95)%>%
  mutate(Source = "Lower N2O interval")

# Bind all datasets into one
N2O_combined <- bind_rows(DirectN2O, DirectN2O_upper, DirectN2O_lower)

#Visualise N2O
ggplot(N2O_combined, aes(x = WTD_mean, y = N2O_t_ha_yr_plot, color = Source)) +
  geom_point() +  # Scatter plot
  labs(x = "WTD_mean", y = "N2O (t/CO2e/ha/yr)", title = "Plot of N2O emissions by water table depth") +
  theme_minimal() +  # Use a clean theme
  theme(axis.title.x = element_text(size = 20), # Increase x-axis title size
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        plot.title = element_text(size = 20, hjust = 0.5),  # Increase title size, center it
        legend.text = element_text(size = 18),   # Increase legend text size
        legend.title = element_text(size = 16))   # Increase legend title size

# Quality checking dataset ####
# There are a few points in the dataset which seem erroneous even after recalcualting, I'll remove these
# Strom 2005, Evans 2022 lettuce, Grunfeld 1999, Henneberg 2015, Pangala 2013

Emissions_filtered <- Emissions %>%
  filter(!(reference %in% c("Strom 2005", "Pangala 2013", "Henneberg 2015", "Grunfeld 1999", "Huth 2018"))) %>%
  filter(`Common_name` != "Romaine lettuce")

# Including the crop ####
Emissions_filtered <- Emissions_filtered %>%
  mutate(
    Direct_CO2 = ifelse(!is.na(CO2_t_ha_yr) & !is.na(Crop_t_CO2e_ha_yr),
                        CO2_t_ha_yr + Crop_t_CO2e_ha_yr,
                        NA))%>%
  filter(`Common_name` != "Lodgepole pine")%>%
  filter(`Common_name` != "Norway spruce")%>%
  filter(`Common_name` != "Creeping bentgrass")

DirectCO2_crop <- Emissions_filtered %>%
  filter(!is.na(WTD_mean) & !is.na(Direct_CO2))

# Ordination ####
# Looking for ordination or natural groups in the data using PCA and CVA
# So I have lots of NA values, I don't want to remove them because then I'll have hardly any data points because I don't necessarily have CO2, N2O and CH4 values for every paper. In this case I'm going to impute the values by working out the mean.

# Impute missing values in columns 9 to 20 with column means
Emissions_filtered_imputed <- Emissions_filtered
Emissions_filtered_imputed[, 9:20] <- lapply(Emissions_filtered[, 9:20], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Perform PCA after imputing missing values
pca_result <- prcomp(Emissions_filtered_imputed[, 9:20], center = TRUE, scale. = TRUE)

# Create data frame for PCA results
pca_data <- as.data.frame(pca_result$x[, 1:2])

# Add Scientific_name to the PCA results
pca_data$Scientific_name <- Emissions_filtered_imputed$Scientific_name

# Plot the PCA results
ggplot(pca_data, aes(x = PC1, y = PC2, color = Scientific_name)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95, aes(group = Scientific_name), linetype = "dashed") +  # 95% confidence ellipse
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  )

# Trying to make it all green scale

# Define the number of groups
num_groups <- length(unique(pca_data$Scientific_name))

# Create a greenscale gradient from black to a very light green
greenscale_palette <- paletteer_c("ggthemes::Green-Gold", num_groups)

# Plot using the custom greenscale palette
ggplot(pca_data, aes(x = PC1, y = PC2, color = Scientific_name)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95, aes(group = Scientific_name), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) +  # Apply paletteer greenscale
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# I just want to see here if doing a Canonical analysis adds any more power than doing a PCA
# Subset data with specific columns for the analysis
selected_columns <- c(2, 9, 11, 12, 13, 15, 16, 17, 19, 20) # 2 = Scientific_name, others are variables
cva_data <- Emissions_filtered_imputed[, selected_columns]

# Perform CVA (LDA) using Scientific_name as the grouping variable
cva_result <- lda(Scientific_name ~ ., data = cva_data)

# Review the CVA results
summary(cva_result)

# Now visualise the CVA
# Predict CVA scores
cva_scores <- predict(cva_result)$x

# Convert to data frame and add group information
cva_df <- as.data.frame(cva_scores)
cva_df$Scientific_name <- cva_data$Scientific_name

# Plot CVA results
ggplot(cva_df, aes(x = LD1, y = LD2, color = Scientific_name)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Scientific_name), level = 0.95, linetype = "dashed") +
  labs(title = "Canonical Variate Analysis (CVA)", x = "Canonical Variate 1", y = "Canonical Variate 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  )

#OK this doesn't really give any further certainty
# I'll go back to PCA's for the other metrics


# Repeat the PCA using landuse
# Add Land_use to the PCA results, then all the others
pca_data$Land_use <- Emissions_filtered_imputed$Land_use
pca_data$Former_land_use <- Emissions_filtered_imputed$Former_land_use
pca_data$Days <- Emissions_filtered_imputed$Days
pca_data$Fertiliser <- Emissions_filtered_imputed$Fertiliser
pca_data$PFT <- Emissions_filtered_imputed$PFT
pca_data$WT_Group <- Emissions_filtered_imputed$WT_Group
pca_data$Upper_WTD <- Emissions_filtered_imputed$Upper_WTD
pca_data$Lower_WTD <- Emissions_filtered_imputed$Lower_WTD
pca_data$Method <- Emissions_filtered_imputed$Method


# Plot the PCA results
#Landuse
ggplot(pca_data, aes(x = PC1, y = PC2, color = Land_use)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Land_use), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

#Former land use
ggplot(pca_data, aes(x = PC1, y = PC2, color = Former_land_use)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Former_land_use), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Days
ggplot(pca_data, aes(x = PC1, y = PC2, color = Days)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95, aes(group = Days), linetype = "dashed") +
  scale_color_gradient(low = "darkgreen", high = "#b2df8a") +  # Two-color gradient from black to green
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )


# Fertiliser
ggplot(pca_data, aes(x = PC1, y = PC2, color = Fertiliser)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Fertiliser), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# PFT

ggplot(pca_data, aes(x = PC1, y = PC2, color = PFT)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = PFT), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Water table group

ggplot(pca_data, aes(x = PC1, y = PC2, color = WT_Group)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = WT_Group), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Upper water table

ggplot(pca_data, aes(x = PC1, y = PC2, color = Upper_WTD)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Upper_WTD), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Lower water table

ggplot(pca_data, aes(x = PC1, y = PC2, color = Lower_WTD)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Lower_WTD), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Method

ggplot(pca_data, aes(x = PC1, y = PC2, color = Method)) +
  geom_point(size = 3) +  # Scatter plot with points
  stat_ellipse(level = 0.95, aes(group = Method), linetype = "dashed") +  # 95% confidence ellipse
  scale_color_manual(values = greenscale_palette) + 
  labs(x = "PC1", y = "PC2", title = "PCA of Emissions Data") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 13)
  )

# Differences between groups ####
# Now lets actually start doing some statistical tests to see if there are differences.
# First we'll perform a Welch's anova, this assumes unequal variance and sample and no NA values

# Remove rows where PFT is "Lettuce" or "BogBean"
DirectCO2_clean <- subset(DirectCO2, !(PFT %in% c("Lettuce", "BogBean")))
# With CO2 and PFT
oneway.test(CO2_t_ha_yr_plot ~ PFT, data = DirectCO2_clean, var.equal = FALSE) # Not enough observations
# With CO2 and WT_group
oneway.test(CO2_t_ha_yr_plot ~ WT_Group, data = DirectCO2_clean, var.equal = FALSE) # No significant differnce in CO2 based on water table groups
# With CO2 and Scientific_name
oneway.test(CO2_t_ha_yr_plot ~ Scientific_name, data = DirectCO2_clean, var.equal = FALSE) # Not enough observations

# With CH4 and PFT
oneway.test(CH4_t_ha_yr_plot ~ PFT, data = DirectCH4, var.equal = FALSE) # Not enough observations
# With CH4 and WT_group
oneway.test(CH4_t_ha_yr_plot ~ WT_Group, data = DirectCH4, var.equal = FALSE) # No significant differences
# With CH4 and Scientific_name
oneway.test(CH4_t_ha_yr_plot ~ Scientific_name, data = DirectCH4, var.equal = FALSE) # Not enough observations

# With N2O
oneway.test(N2O_t_ha_yr_plot ~ PFT, data = DirectN2O, var.equal = FALSE) # No significant differences
# With N2O and WT_group
oneway.test(N2O_t_ha_yr_plot ~ WT_Group, data = DirectN2O, var.equal = FALSE) # No significant differences
# wITH N2O and Scientific name
oneway.test(N2O_t_ha_yr_plot ~ Scientific_name, data = DirectN2O, var.equal = FALSE) # Not enough observations
# With Feriliser
oneway.test(N2O_t_ha_yr_plot ~ Fertiliser, data = DirectN2O, var.equal = FALSE)

# With the crop value included
DirectCO2_crop <- Emissions_filtered %>%
  filter(!is.na(WTD_mean) & !is.na(Direct_CO2))

oneway.test(Direct_CO2 ~ Scientific_name, data = DirectCO2_crop, var.equal = FALSE) # Not enough observations
oneway.test(Direct_CO2 ~ PFT, data = DirectCO2_crop, var.equal = FALSE) # Not enough observations
oneway.test(Direct_CO2 ~ WT_Group, data = DirectCO2_crop, var.equal = FALSE) # Not enough observations

# Perform Games-Howell post-hoc test
gamesHowellTest(CO2_t_ha_yr_plot ~ PFT, data = DirectCO2)
gamesHowellTest(CH4_t_ha_yr_plot ~ PFT, data = DirectCH4)
gamesHowellTest(N2O_t_ha_yr_plot ~ PFT, data = DirectN2O)

gamesHowellTest(CO2_t_ha_yr_plot ~ WT_Group, data = DirectCO2)
gamesHowellTest(CH4_t_ha_yr_plot ~ WT_Group, data = DirectCH4)
gamesHowellTest(N2O_t_ha_yr_plot ~ WT_Group, data = DirectN2O)

gamesHowellTest(CO2_t_ha_yr_plot ~ Scientific_name, data = DirectCO2)
gamesHowellTest(CH4_t_ha_yr_plot ~ Scientific_name, data = DirectCH4)
gamesHowellTest(N2O_t_ha_yr_plot ~ Scientific_name, data = DirectN2O)

# Remove rows where Fertiliser is NA
DirectN2O_cleaned <- DirectN2O %>%
  filter(!is.na(Fertiliser))%>%
  filter(!(reference == "Huth 2018"))

# Perform the t-test # variances are equal as tested by levenes test
t_test_result <- t.test(N2O_t_ha_yr_plot ~ Fertiliser, data = DirectN2O_cleaned)
# Display the result
print(t_test_result)

# Create the box plot with customized aesthetics of fertiliser added or not
ggplot(data = DirectN2O_cleaned, aes(x = Fertiliser, y = N2O_t_ha_yr_plot)) +
  geom_boxplot(aes(fill = Fertiliser), color = "black") +  # Apply custom fill by Fertiliser
  scale_fill_manual(values = c("#b2df8a", "#006400")) +    # Set specific colors for each Fertiliser type
  labs(
    x = "Fertiliser applied",
    y = expression(N[2]*O~flux~(t~CO[2]*e~ha^-1~yr^-1))
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),        # Set overall text size
    axis.title = element_text(size = 10),  # Set axis title size
    axis.text = element_text(size = 12),   # Set axis text size
    legend.position = "none"               # Remove legend if not needed
  )


#Ok there is literally no significant difference between any of these
#But let's visualise them anyway

# Reshape the dataset to long format and calculate the mean values for each PFT and gas type
Emissions_long <- Emissions_filtered %>%
  select(PFT, CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr) %>%
  pivot_longer(cols = c(CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr),
               names_to = "Gas", values_to = "Emissions") %>%
  group_by(PFT, Gas) %>%
  summarise(Mean_Emissions = mean(Emissions, na.rm = TRUE))  # Calculate mean values

# Create the stacked bar plot with custom green colors and increased text size
ggplot(Emissions_long, aes(x = PFT, y = Mean_Emissions, fill = Gas)) +
  geom_bar(stat = "identity") +  # 'identity' allows us to use the actual values for the bars
  labs(x = "PFT", y = "Mean Emissions (tonnes of CO2e ha yr)", fill = "Gas Type") +
  scale_fill_manual(values = c("CO2_t_ha_yr" = "#b2df8a",    # Light green for CO2
                               "CH4_t_CO2e_ha_yr" = "#33a02c", # Medium green for CH4
                               "N2O_t_CO2e_ha_yr" = "#006400")) + # Dark green for N2O
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # X-axis text size
        axis.text.y = element_text(size = 18),                        # Y-axis text size
        axis.title.x = element_text(size = 25),                       # X-axis title size
        axis.title.y = element_text(size = 25),                       # Y-axis title size
        legend.text = element_text(size = 16),                        # Legend text size
        legend.title = element_text(size = 14),                       # Legend title size
        plot.title = element_text(size = 25))                         # Plot title size (if you add one)

# Reshape the dataset to long format and calculate the mean values for each WT_Group and gas type
Emissions_long <- Emissions_filtered %>%
  filter(!is.na(WT_Group)) %>%
  select(WT_Group, CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr) %>%  # Change PFT to WT_Group
  pivot_longer(cols = c(CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr),
               names_to = "Gas", values_to = "Emissions") %>%
  group_by(WT_Group, Gas) %>%  # Group by WT_Group instead of PFT
  summarise(Mean_Emissions = mean(Emissions, na.rm = TRUE))  # Calculate mean values

# Create the stacked bar plot with custom green colors, formatted labels, and updated legend labels
ggplot(Emissions_long, aes(x = WT_Group, y = Mean_Emissions, fill = Gas)) +
  geom_bar(stat = "identity") +  # 'identity' allows us to use the actual values for the bars
  labs(
    x = "WT Group", 
    y = expression(Mean~Emissions~(tonnes~of~CO[2]*e~ha^-1~yr^-1)),  # Correctly formatted y-axis label
    fill = "Gas Type"
  ) +
  scale_fill_manual(
    values = c(
      "CO2_t_ha_yr" = "#b2df8a",      # Light green for CO2
      "CH4_t_CO2e_ha_yr" = "#33a02c", # Medium green for CH4
      "N2O_t_CO2e_ha_yr" = "#006400"  # Dark green for N2O
    ),
    labels = c("CO2", "CH4", "N2O")   # Custom labels for the legend
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # X-axis text size
    axis.text.y = element_text(size = 18),                         # Y-axis text size
    axis.title.x = element_text(size = 25),                        # X-axis title size
    axis.title.y = element_text(size = 25),                        # Y-axis title size
    legend.text = element_text(size = 16),                         # Legend text size
    legend.title = element_text(size = 18),                        # Legend title size
    plot.title = element_text(size = 25)                           # Plot title size (if you add one)
  )


# Reshape the dataset to long format and calculate the mean values for each Upper_WTD and gas type
Emissions_long <- Emissions_filtered %>%
  filter(!is.na(Upper_WTD)) %>%  # Keep rows where Upper_WTD is not NA
  select(Upper_WTD, CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr) %>%  # Selecting relevant columns
  pivot_longer(cols = c(CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr),
               names_to = "Gas", values_to = "Emissions") %>%
  group_by(Upper_WTD, Gas) %>%  # Group by Upper_WTD instead of WT_Group
  summarise(Mean_Emissions = mean(Emissions, na.rm = TRUE), .groups = 'drop')  # Calculate mean values

# Create the stacked bar plot with custom green colors and increased text size
ggplot(Emissions_long, aes(x = Upper_WTD, y = Mean_Emissions, fill = Gas)) +  # Change WT_Group to Upper_WTD
  geom_bar(stat = "identity") +  # 'identity' allows us to use the actual values for the bars
  labs(x = "Upper WTD", y = "Mean Emissions (tonnes of CO2e ha yr)", fill = "Gas Type") +  # Update the x-axis label
  scale_fill_manual(values = c("CO2_t_ha_yr" = "#b2df8a",    # Light green for CO2
                               "CH4_t_CO2e_ha_yr" = "#33a02c", # Medium green for CH4
                               "N2O_t_CO2e_ha_yr" = "#006400")) + # Dark green for N2O
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # X-axis text size
        axis.text.y = element_text(size = 18),                        # Y-axis text size
        axis.title.x = element_text(size = 25),                       # X-axis title size
        axis.title.y = element_text(size = 25),                       # Y-axis title size
        legend.text = element_text(size = 16),                        # Legend text size
        legend.title = element_text(size = 18),                       # Legend title size
        plot.title = element_text(size = 25))                         # Plot title size (if you add one)



# Reshape the dataset to long format and calculate the mean values for each Lower_WTD and gas type
Emissions_long <- Emissions_filtered %>%
  filter(!is.na(Lower_WTD)) %>%  # Keep rows where Upper_WTD is not NA
  select(Lower_WTD, CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr) %>%  # Selecting relevant columns
  pivot_longer(cols = c(CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr),
               names_to = "Gas", values_to = "Emissions") %>%
  group_by(Lower_WTD, Gas) %>%  # Group by Upper_WTD instead of WT_Group
  summarise(Mean_Emissions = mean(Emissions, na.rm = TRUE), .groups = 'drop')  # Calculate mean values

# Create the stacked bar plot with custom green colors and increased text size
ggplot(Emissions_long, aes(x = Lower_WTD, y = Mean_Emissions, fill = Gas)) +  # Change WT_Group to Upper_WTD
  geom_bar(stat = "identity") +  # 'identity' allows us to use the actual values for the bars
  labs(x = "Lower WTD", y = "Mean Emissions (tonnes of CO2e ha yr)", fill = "Gas Type") +  # Update the x-axis label
  scale_fill_manual(values = c("CO2_t_ha_yr" = "#b2df8a",    # Light green for CO2
                               "CH4_t_CO2e_ha_yr" = "#33a02c", # Medium green for CH4
                               "N2O_t_CO2e_ha_yr" = "#006400")) + # Dark green for N2O
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # X-axis text size
        axis.text.y = element_text(size = 18),                        # Y-axis text size
        axis.title.x = element_text(size = 25),                       # X-axis title size
        axis.title.y = element_text(size = 25),                       # Y-axis title size
        legend.text = element_text(size = 16),                        # Legend text size
        legend.title = element_text(size = 18),                       # Legend title size
        plot.title = element_text(size = 25))                         # Plot title size (if you add one)


# Reshape the dataset to long format and calculate the mean values for each Scientific name and gas type
Emissions_long <- Emissions_filtered %>%
  filter(!is.na(Scientific_name)) %>%
  select(Scientific_name, CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr) %>%  # Change PFT to WT_Group
  pivot_longer(cols = c(CO2_t_ha_yr, CH4_t_CO2e_ha_yr, N2O_t_CO2e_ha_yr),
               names_to = "Gas", values_to = "Emissions") %>%
  group_by(Scientific_name, Gas) %>%  # Group by Scientific_name instead of PFT
  summarise(Mean_Emissions = mean(Emissions, na.rm = TRUE))  # Calculate mean values

# Create the stacked bar plot with custom green colors and increased text size
ggplot(Emissions_long, aes(x = Scientific_name, y = Mean_Emissions, fill = Gas)) +  # Change PFT to WT_Group
  geom_bar(stat = "identity") +  # 'identity' allows us to use the actual values for the bars
  labs(x = "Scientific name", y = "Mean Emissions (tonnes of CO2e ha yr)", fill = "Gas Type") +  # Update the x-axis label
  scale_fill_manual(values = c("CO2_t_ha_yr" = "#b2df8a",    # Light green for CO2
                               "CH4_t_CO2e_ha_yr" = "#33a02c", # Medium green for CH4
                               "N2O_t_CO2e_ha_yr" = "#006400")) + # Dark green for N2O
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # X-axis text size
        axis.text.y = element_text(size = 18),                        # Y-axis text size
        axis.title.x = element_text(size = 25),                       # X-axis title size
        axis.title.y = element_text(size = 25),                       # Y-axis title size
        legend.text = element_text(size = 16),                        # Legend text size
        legend.title = element_text(size = 18),                       # Legend title size
        plot.title = element_text(size = 25))                         # Plot title size (if you add one)

# Modelling relationships ####
# Let's see if we can start to model relationships
# Fit a multiple regression model with both continuous and categorical predictors
CO2_model <- lm(CO2_t_ha_yr ~ WTD_mean + Land_use + Former_land_use + Scientific_name + measurement_period + 
              Days + Fertiliser + PFT + WT_Group + Upper_WTD + Lower_WTD + Method, data = Emissions_filtered)
summary(CO2_model)

# Check the diagnostic plots
par(mfrow = c(1, 1))  # Multiple plots in one window
plot(CO2_model)

# This is actually a pretty good model, not too skewed in any of the diagnostic plots, I'm pretty happy with this.

# But maybe we can make it better by looking at covariates and controlling for them better?
# For continuous variables: check correlation matrix
continuous_vars <- Emissions_filtered %>%
  select(WTD_mean, Days)  # Add other continuous covariates
cor(continuous_vars, use = "complete.obs")

# For multicollinearity: calculate VIF
vif(CO2_model)  # Calculate VIF for your model

# Some of my predictors are aliased or highly collinear, I need to remove these but first I'll identify them
alias(CO2_model)

# PFT and Former landuse are causing problems let's  reomve these and then do it again.
CO2_model_2 <- lm(CO2_t_ha_yr ~ WTD_mean + Land_use + Scientific_name + 
                  Days + Fertiliser + WT_Group + Upper_WTD + Lower_WTD + Method, data = Emissions_filtered)
summary(CO2_model_2)

# For multicollinearity: calculate VIF
vif(CO2_model_2)  # Calculate VIF for your model

# Some of my predictors are aliased or highly collinear, I need to remove these but first I'll identify them
alias(CO2_model_2)

# Ok so my Piceas species are perfectly collinear with my land use type, I need to get rid of this
CO2_model_3 <- lm(CO2_t_ha_yr ~ WTD_mean + Land_use + Scientific_name + Days + 
                    Fertiliser + WT_Group + Upper_WTD + Lower_WTD + Method, 
                  data = Emissions_filtered)

# Drop the collinear level 'Picea abies'
Emissions_filtered_no_abies <- subset(Emissions_filtered, Scientific_name != "Picea abies")

# Refit the model
CO2_model_3 <- lm(CO2_t_ha_yr ~ WTD_mean + Land_use + Scientific_name + Days + 
                    Fertiliser + WT_Group + Lower_WTD + Method, 
                  data = Emissions_filtered_no_abies)

summary(CO2_model_3)

# For multicollinearity: calculate VIF
vif(CO2_model_3)  # Calculate VIF for your model
# Some VIF values are high, but not completely unreasonable especially when adjusted.

# Some of my predictors are aliased or highly collinear, I need to remove these but first I'll identify them
alias(CO2_model_3)
plot(CO2_model_3)

# Removing collinearity has actually improved the diagnostic plots.
# Now I want to get rid of non significant predictors including land use, days, fertiliser

CO2_model_4 <- lm(CO2_t_ha_yr ~ Scientific_name, 
                  data = Emissions_filtered_no_abies)

summary(CO2_model_4)

# For multicollinearity: calculate VIF
vif(CO2_model_4)  # Calculate VIF for your model
# Some VIF values are high, but not completely unreasonable especially when adjusted.

# Some of my predictors are aliased or highly collinear, I need to remove these but first I'll identify them
alias(CO2_model_4)
plot(CO2_model_4)

# Plot residuals
residuals_CO2 <- residuals(CO2_model_4)

# Step 3: Plot a histogram of the residuals
hist(residuals_CO2, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

# Now I want to repeat al of this for CH4

CH4_model <- lm(CH4_t_CO2e_ha_yr ~ WTD_mean + Land_use + Former_land_use + Scientific_name + measurement_period + Fertiliser + WT_Group + Upper_WTD + Lower_WTD + Method, data = Emissions_filtered)
summary(CH4_model)

# Check the diagnostic plots
par(mfrow = c(1, 1))  # Multiple plots in one window
plot(CO2_model)
vif(CH4_model)
alias(CH4_model)

# Remove aliased variables

# Exclude specific levels from the dataset
Emissions_filtered_2 <- Emissions_filtered %>%
  filter(!(Scientific_name %in% c("Menyanthes trifoliata", "Picea abies"))) %>%
  filter(!(measurement_period %in% c("20 days", "3 months", "59 days"))) %>%
  filter(!(Upper_WTD %in% c("D", "E")))

# Fit the model again
CH4_model_2 <- lm(CH4_t_CO2e_ha_yr ~ WTD_mean + Land_use + Former_land_use + 
                          Scientific_name + 
                          measurement_period + Fertiliser + 
                          WT_Group + Upper_WTD + Lower_WTD + Method, 
                        data = Emissions_filtered_2)

# Check summary
summary(CH4_model_2)

# Finally let's look at N2O
N2O_model <- lm(N2O_t_CO2e_ha_yr ~ WTD_mean + Land_use + Former_land_use + Scientific_name + 
                  Days + Fertiliser + PFT + WT_Group + Upper_WTD + Lower_WTD, data = Emissions_filtered)
summary(N2O_model)

# Exclude specific levels from the dataset
Emissions_filtered_4 <- Emissions_filtered %>%
  filter(!(Scientific_name %in% c("Menyanthes trifoliata", "Picea abies"))) %>%
  filter(!(PFT %in% c("Grass", "Sedge", "Moss", "Rush"))) %>%
  filter(WT_Group != "E") %>%  
  filter(Upper_WTD != "D") %>% 
  filter(!(Lower_WTD %in% c("B", "C", "E")))
  
N2O_model3 <- lm(N2O_t_CO2e_ha_yr ~ WTD_mean + Land_use + Former_land_use + Scientific_name + 
                  Days + Fertiliser + PFT + WT_Group + Upper_WTD + Lower_WTD, data = Emissions_filtered_4)
summary(N2O_model3) 


# I want to look at water table depth with CH4 and CO2

# Filter out rows with NA values in WTD_mean
Emissions_filtered_3 <- Emissions_filtered_2[!is.na(Emissions_filtered_2$WTD_mean), ]

# Fit the linear model
lm_model_CO2_WTD <- lm(CO2_t_ha_yr ~ WTD_mean, data = Emissions_filtered_3)

# Get summary statistics
lm_model_CO2_WTD_summary <- summary(lm_model_CO2_WTD)
r_squared_CO2 <- lm_model_CO2_WTD_summary$r.squared
p_value_CO2 <- lm_model_CO2_WTD_summary$coefficients[2, 4]  # p-value for WTD_mean

# Create gompertz line, this is based on the non linear relationship between emissions and water table depth (Evans 2021)
# Define the Gompertz function
gompertz_function <- function(WTD_mean) {
  -10 + 45 * exp(-2.47467 * exp(-0.03984 * (-WTD_mean)))  # Flip WTD_mean sign
}

# Create a data frame for the Gompertz line
WTD_mean_values <- seq(min(Emissions_filtered_3$WTD_mean), max(Emissions_filtered_3$WTD_mean), length.out = 100)
gompertz_values <- gompertz_function(WTD_mean_values)

# Create a data frame for plotting
gompertz_data <- data.frame(WTD_mean = WTD_mean_values, CO2_t_ha_yr = gompertz_values)

# Create the scatter plot with regression line and Gompertz curve
ggplot(data = Emissions_filtered_2, aes(x = WTD_mean, y = CO2_t_ha_yr)) +
  geom_point() +  # Add points to the scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +  # Add linear regression line
  geom_line(data = gompertz_data, aes(x = WTD_mean, y = CO2_t_ha_yr), color = "#b2df8a", size = 1) +  # Add Gompertz curve
  labs(
    x = "Mean water table depth (cm)",
    y = expression(CO[2]~emissions~(t~ha^-1~yr^-1))  # Properly formatted y-axis label
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r_squared_CO2, 3), "\n p =", round(p_value_CO2, 3)), 
           hjust = 1.1, vjust = 1.5, size = 5, color = "darkgreen", 
           fontface = "bold") +  # Add R² and p-value text
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),  # Increase x-axis title size
    axis.title.y = element_text(size = 25),  # Increase y-axis title size
    axis.text.x = element_text(size = 18),   # Increase x-axis text size
    axis.text.y = element_text(size = 18)    # Increase y-axis text size
  )



# CH4
# Fit the linear model
lm_model_CH4_WTD <- lm(CH4_t_CO2e_ha_yr ~ WTD_mean, data = Emissions_filtered_2)

# Get summary statistics
lm_model_CH4_WTD_summary <- summary(lm_model_CH4_WTD)
r_squared <- lm_model_CH4_WTD_summary$r.squared
p_value <- lm_model_CH4_WTD_summary$coefficients[2, 4]  # p-value for WTD_mean

# Create the scatter plot with regression line
ggplot(data = Emissions_filtered_2, aes(x = WTD_mean, y = CH4_t_CO2e_ha_yr)) +
  geom_point() +  # Add points to the scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +  # Add linear regression line
  labs(title = "Scatter Plot of WTD_mean vs CH4_t_CO2e_ha_yr",
       x = "Water Table Depth Mean (WTD_mean)",
       y = "CH4 Emissions (t CO2e/ha/yr)") +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r_squared, 3), "\n p =", round(p_value, 3)), 
           hjust = 1.1, vjust = 1.5, size = 5, color = "darkgreen", 
           fontface = "bold") +  # Add R² and p-value text
  theme_minimal() +
  theme(
    plot.title = element_text(size = 25, face = "bold"),  # Increase title size
    axis.title.x = element_text(size = 25),  # Increase x-axis title size
    axis.title.y = element_text(size = 25),  # Increase y-axis title size
    axis.text.x = element_text(size = 18),  # Increase x-axis text size
    axis.text.y = element_text(size = 18)   # Increase y-axis text size
  )


# Comparing to existing Tier 2 EF ####
# First read in the Tier 2 EF's

Tier_2 <- read.csv("Tier 2 EF.csv")

# I want to perform t-tests to see if there are significant differences between the mean values I have for the paludiculture crops and the existing emissions factors
# Step 1: Calculate the mean of CO2_t_ha_yr from Emissions_filtered
mean_emissions_CO2 <- mean(Emissions_filtered$CO2_t_ha_yr, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CO2_t_ha_yr[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CO2_t_ha_yr, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CO2 <- do.call(rbind, lapply(t_test_results, as.data.frame))

## Now for the CO2 UPPER
mean_emissions_CO2_upper <- mean(Emissions_filtered$CO2_u_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CO2_u_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CO2_u_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CO2_u <- do.call(rbind, lapply(t_test_results, as.data.frame))

## And CO2 LOWER

mean_emissions_CO2_lower <- mean(Emissions_filtered$CO2_l_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CO2_l_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CO2_l_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CO2_l <- do.call(rbind, lapply(t_test_results, as.data.frame))

## Now we'll repeat this for CH4

# Step 1: Calculate the mean of CH4_t_CO2e_ha_yr from Emissions_filtered
mean_emissions_CH4 <- mean(Emissions_filtered$CH4_t_CO2e_ha_yr, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CH4_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CH4_t_CO2e_ha_yr[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CH4_t_CO2e_ha_yr, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CH4 <- do.call(rbind, lapply(t_test_results, as.data.frame))

## Now for the CH4 UPPER
mean_emissions_CH4_u <- mean(Emissions_filtered$CH4_u_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CH4_u_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CH4_u_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CH4_u <- do.call(rbind, lapply(t_test_results, as.data.frame))

## And CH4 LOWER

mean_emissions_CH4_lower <- mean(Emissions_filtered$CH4_l_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CH4_l_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$CH4_l_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CH4_l <- do.call(rbind, lapply(t_test_results, as.data.frame))


## Now we'll repeat this for N2O

# Step 1: Calculate the mean of N2O_t_CO2e_ha_yr from Emissions_filtered
mean_emissions_N2O <- mean(Emissions_filtered$N2O_t_CO2e_ha_yr, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$N2O_t_CO2e_ha_yr[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$N2O_t_CO2e_ha_yr, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_N2O <- do.call(rbind, lapply(t_test_results, as.data.frame))

## Now for the N2O UPPER
mean_emissions_N2O_u <- mean(Emissions_filtered$N2O_u_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$N2O_u_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$N2O_u_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_N2O_u <- do.call(rbind, lapply(t_test_results, as.data.frame))

## And N2O LOWER

mean_emissions_N2O_lower <- mean(Emissions_filtered$N2O_l_95, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$N2O_l_95[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$N2O_l_95, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_N2O_l <- do.call(rbind, lapply(t_test_results, as.data.frame))

# I want to repeat this t-test again using the Direct CO2 including harvest values and the existing emissions factors
# Step 1: Calculate the mean of CO2_t_ha_yr from Emissions_filtered
mean_emissions_CO2_crop <- mean(Emissions_filtered$Direct_CO2, na.rm = TRUE)

# Step 2: Create an empty list to store t-test results
t_test_results <- list()

# Step 3: Loop through each value in the CO2_t_ha_yr column of Tier_2 and use Tier_2$Category as the label
for (i in 1:nrow(Tier_2)) {
  # Extract the current CO2 value and category from Tier_2
  value_tier2 <- Tier_2$CO2_t_ha_yr[i]
  category <- Tier_2$Category[i]  # The category label
  
  # Perform a one-sample t-test
  t_test <- t.test(x = Emissions_filtered$Direct_CO2, mu = value_tier2, alternative = "two.sided")
  
  # Store the result in the list, using the category as the identifier
  t_test_results[[category]] <- list(
    Category = category,
    Tier_2_value = value_tier2,
    p_value = t_test$p.value,
    t_statistic = t_test$statistic
  )
}

# Step 4: Convert the list to a data frame for easier viewing
t_test_results_df_CO2_crop <- do.call(rbind, lapply(t_test_results, as.data.frame))



# OK now I want to see if I can combine these all into a single table

# Create a named list of all the data frames
df_list <- list(
  CO2 = t_test_results_df_CO2,
  CO2_u = t_test_results_df_CO2_u,
  CO2_l = t_test_results_df_CO2_l,
  CH4 = t_test_results_df_CH4,
  CH4_u = t_test_results_df_CH4_u,
  CH4_l = t_test_results_df_CH4_l,
  N2O = t_test_results_df_N2O,
  N2O_u = t_test_results_df_N2O_u,
  N2O_l = t_test_results_df_N2O_l
)

# Function to rename the columns with a suffix
add_suffix <- function(df, suffix) {
  df %>%
    rename_with(~ paste0(., "_", suffix), c("Tier_2_value", "p_value", "t_statistic"))
}

# Apply the suffix function to each data frame
df_list_renamed <- imap(df_list, add_suffix)

# Now merge all the data frames on the 'Category' column
combined_Tier_2_t_statistics <- reduce(df_list_renamed, full_join, by = "Category")

# View the resulting combined data frame
combined_Tier_2_t_statistics

# Save the combined data frame as a CSV file
write.csv(combined_Tier_2_t_statistics, "combined_Tier_2_t_statistics_day_rmv.csv", row.names = FALSE)

# Now calculate the mean and standard error of each dataset
# Define a function to calculate mean and standard error
mean_se <- function(x) {
  n <- sum(!is.na(x))  # number of non-missing values
  mean_val <- mean(x, na.rm = TRUE)
  se_val <- sd(x, na.rm = TRUE) / sqrt(n)
  return(c(Mean = mean_val, SE = se_val))
}

# List of columns to summarize
columns_to_summarize <- c(
  "CO2_t_ha_yr", "CO2_u_95", "CO2_l_95",
  "CH4_t_CO2e_ha_yr", "CH4_u_95", "CH4_l_95",
  "N2O_t_CO2e_ha_yr", "N2O_u_95", "N2O_l_95"
)

# Apply the function to each column and store results in a data frame
summary_df <- sapply(Emissions_filtered[columns_to_summarize], mean_se)

# Convert to a data frame and transpose it for better readability
summary_df <- as.data.frame(t(summary_df))

# View the resulting data frame
summary_df

# write it
write.csv(summary_df, "Paludiculture means_2_day.csv", row.names = TRUE)

# Plotting paludiculture against the other emission factors
# Step 1: Clean up Tier_2 dataset by removing columns 11, 12, and 13
Tier_2_2 <- Tier_2 %>% select(-c(DOC_t_CO2e_ha_yr, POC_t_CO2e_ha_yr, Ditch_CH4_t_CO2e_ha_yr))

Summary_df_2 <- summary_df %>% select(-c(SE))

# Create the new row as a data frame
new_row <- data.frame(
  Category = "Paludiculture",
  CO2_t_ha_yr = Summary_df_2$Mean[1],
  CO2_u_95 = Summary_df_2$Mean[2],
  CO2_l_95 = Summary_df_2$Mean[3],
  CH4_t_CO2e_ha_yr = Summary_df_2$Mean[4],
  CH4_u_95 = Summary_df_2$Mean[5],
  CH4_l_95 = Summary_df_2$Mean[6],
  N2O_t_CO2e_ha_yr = Summary_df_2$Mean[7],
  N2O_u_95 = Summary_df_2$Mean[8],
  N2O_l_95 = Summary_df_2$Mean[9]
)

# Add the new row to Tier_2_2
Tier_2_2 <- rbind(Tier_2_2, new_row)

# View the updated dataset
print(Tier_2_2)

ggplot(Tier_2_2, aes(x = Category)) +
  # CO2 Bar and Error Bars
  geom_bar(aes(y = CO2_t_ha_yr, fill = "CO2"), stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = CO2_l_95, ymax = CO2_u_95), width = 0.2, position = position_dodge(width = 0.8), color = "#b2df8a") +
  
  # CH4 Bar and Error Bars
  geom_bar(aes(y = CH4_t_CO2e_ha_yr, fill = "CH4"), stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = CH4_l_95, ymax = CH4_u_95), width = 0.2, position = position_dodge(width = 0.8), color = "#33a02c") +
  
  # N2O Bar and Error Bars
  geom_bar(aes(y = N2O_t_CO2e_ha_yr, fill = "N2O"), stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = N2O_l_95, ymax = N2O_u_95), width = 0.2, position = position_dodge(width = 0.8), color = "#006400") +
  
  # Labels and Themes
  labs(
    x = "Category",
    y = expression(Emissions~(CO[2]~eq~ha^-1~yr^-1)),
    fill = "Gas Type"
  ) +
  scale_fill_manual(values = c("CO2" = "#b2df8a", "CH4" = "#33a02c", "N2O" = "#006400")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Combine `t_ha_yr` and `t_CO2e_ha_yr` into `t_ha_yr`, then remove `t_CO2e_ha_yr`
Tier_2_2_long <- Tier_2_2_long %>%
  mutate(t_ha_yr = ifelse(is.na(t_ha_yr), t_CO2e_ha_yr, t_ha_yr)) %>% # Fill `t_ha_yr` with `t_CO2e_ha_yr` values where `t_ha_yr` is NA
  select(-t_CO2e_ha_yr)

# Add the new row to your dataset
Tier_2_2_long <- Tier_2_2_long %>%
  add_row(
    Category = "Paludiculture_Crop",
    Gas = "CO2",
    t_ha_yr = 18.815,
    u_95 = 26.850686,
    l_95 = 10.779314
  )

# Rename specific values in the Category column
Tier_2_2_long <- Tier_2_2_long %>%
  mutate(Category = case_when(
    Category == "Paludiculture" ~ "Paludiculture no harvest",  # Rename "Paludiculture" to "Paludiculture no harvest"
    Category == "Paludiculture_Crop" ~ "Paludiculture",       # Rename "Paludiculture_Crop" to "Paludiculture"
    TRUE ~ Category  # Keep all other values unchanged
  ))

Tier_2_2_long <- Tier_2_2_long %>%
  mutate(Category = if_else(
    Category == "Paludiculture no harvest" & (Gas == "N2O" | Gas == "CH4"),
    "Paludiculture",  # Convert back to "Paludiculture" if Gas is N2O or CH4
    Category))

# Fix the order of the gases
Tier_2_2_long$Gas <- fct_relevel(Tier_2_2_long$Gas, "CO2", "CH4", "N2O")
Tier_2_2_long$Category <- fct_relevel(Tier_2_2_long$Category, 
                                      "Cropland", "Extensive Grassland", 
                                      "Intensive Grassland", "Paludiculture","Paludiculture no harvest", 
                                      "Rewetted Fen", "Near-Natural Fen")

# Plotting the data with the correct order of gases and categories
ggplot(Tier_2_2_long, aes(x = Category, y = t_ha_yr, fill = Gas)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.6) +  # Reduced gap between bars
  geom_errorbar(aes(ymin = l_95, ymax = u_95), width = 0.2, position = position_dodge(width = 0.4)) +  # Error bars
  facet_wrap(~ Gas, scales = "free_x") +  # Creates separate clusters for each gas
  scale_fill_manual(values = c("CO2" = "#b2df8a", "CH4" = "#33a02c", "N2O" = "#006400")) +
  labs(
    x = "Category",
    y = expression(Emissions~(CO[2]~eq~ha^-1~yr^-1)),
    fill = "Gas Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis text size
    axis.text.y = element_text(size = 12),  # Increase y-axis text size
    axis.title.x = element_text(size = 12),  # Increase x-axis title size
    axis.title.y = element_text(size = 12),  # Increase y-axis title size
    strip.background = element_blank(),
    strip.text = element_text(size = 12),  # Increase facet label text size
    panel.spacing = unit(1, "lines"),  # Reduces space between facets (gas types)
    legend.position = "none"  # Remove legend
  )
