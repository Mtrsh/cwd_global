library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)

##########################################################################################
# Option 1: mean of CWD
##########################################################################################


load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/combined_df_merged_southamerica.Rdata")

df_grid=combined_df_merged

df_grid=df_grid%>%
  filter(year>=2000)

# Ensure df_grid is a data.table
setDT(df_grid)

# Order the data by location and date to ensure correct accumulation
df_grid <- df_grid[order(lat, lon, date)]

#  start accumulating 'wbal'
df_grid[, deficit := wbal]

# Calculate the cumulative deficit with resets
df_grid[, c_deficit := {
  # empty vector to store cumulative deficit values
  c_def <- numeric(.N)
  # Initialize a variable to keep track of the ongoing deficit
  ongoing_deficit <- 0

  # loop to calculate cwd
  for (i in 1:.N) {
    # Accumulate the deficit
    ongoing_deficit <- ongoing_deficit + wbal[i]

    # reset the ongoing deficit to 0 with the two conditions
    if (ongoing_deficit > 0 ) {
      ongoing_deficit = 0
    }

    # assign the ongoing deficit to the vector
    c_def[i] <- ongoing_deficit
  }

  # Return the vector of ccwd values
  c_def
}, by = .(lat, lon)]

# Assign the calculated cumulative deficit to the 'deficit' column
df_grid[, deficit := c_deficit]

# Remove
df_grid[, c_deficit := NULL]

#transform into + values
df_grid[, deficit := abs(deficit)]


# mean seasonal cycle of the cumulative water deficit
mean_seasonal_cycle <- df_grid %>%
  group_by(lat, lon, month) %>%
  summarize(mean_cwd = mean(deficit, na.rm = TRUE), .groups = 'drop')


#month with the lowest average cumulative water deficit
cwd_lowest_avg_month <- mean_seasonal_cycle %>%
  group_by(lat, lon) %>%
  slice_min(mean_cwd, n = 1) %>%
  ungroup() %>%
  dplyr::select(lat, lon, wettest_month = month)


cwd_lowest_avg_month$wettest_month=as.factor(cwd_lowest_avg_month$wettest_month)


p1= ggplot(cwd_lowest_avg_month) +
  geom_tile(aes(x = lon, y = lat, fill=wettest_month)) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", fill = "Month") +
  scale_fill_viridis(discrete = T, option = "B", na.value = "grey") +
  ggtitle("CWD - Wettest month (mean) ") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"))

##########################################################################################
######### Option 2: Wettest month based on monthly mean Cumulative water balance #########
##########################################################################################

load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/combined_df_merged_southamerica.Rdata")


df_grid=combined_df_merged
df_grid=df_grid%>%
  filter(year>=2000)

# Ensure df_grid is a data.table
setDT(df_grid)

# Order the data by location and date to ensure correct accumulation
df_grid <- df_grid[order(lat, lon, date)]

# Calculate the cumulative sum of wbal for each lat, lon group
df_grid[, cumulative_wbal := cumsum(wbal), by = .(lat, lon)]

# mean seasonal cycle of the cumulative water balance
mean_seasonal_cycle <- df_grid %>%
  group_by(lat, lon, month) %>%
  summarize(mean_cwbal = mean(cumulative_wbal, na.rm = TRUE), .groups = 'drop')


# Month with the highest average cumulative water balance
cwb_mean_month <- mean_seasonal_cycle %>%
  group_by(lat, lon) %>%
  slice_max(mean_cwbal, n = 1) %>%
  ungroup() %>%
  dplyr::select(lat, lon, wettest_month = month)


cwb_mean_month$wettest_month=as.factor(cwb_mean_month$wettest_month)


p2=ggplot(cwb_mean_month) +
  geom_tile(aes(x = lon, y = lat, fill=wettest_month)) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", fill = "Month") +
  scale_fill_viridis(discrete = T, option = "B", na.value = "grey") +
  ggtitle("CWB - Wettest month (mean) ") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"))


# Then, the ouput of the cwb_mean_month allows to reset to 0 during the computation of CWD

df_grid <- df_grid |>
  left_join(cwb_mean_month, by = c( "lat", "lon")) %>%
  mutate(is_wettest_month = month == wettest_month)

setDT(df_grid)

# Order the data
df_grid <- df_grid[order(lat, lon, date)]

#  start accumulating 'wbal'
df_grid[, deficit := wbal]

# Calculate the cumulative deficit with resets
df_grid[, c_deficit := {
  # empty vector to store cumulative deficit values
  c_def <- numeric(.N)
  # Initialize a variable to keep track of the ongoing deficit
  ongoing_deficit <- 0

  # loop to calculate cwd
  for (i in 1:.N) {
    # Accumulate the deficit
    ongoing_deficit <- ongoing_deficit + wbal[i]

    # reset the ongoing deficit to 0 with these two conditions
    if (ongoing_deficit > 0 || is_wettest_month[i] ) {
      ongoing_deficit = 0
    }

    # assign the ongoing deficit to the vector
    c_def[i] <- ongoing_deficit
  }

  # Return the vector of ccwd values
  c_def
}, by = .(lat, lon)]

# Assign the calculated cumulative deficit to the 'deficit' column
df_grid[, deficit := c_deficit]

# Remove
df_grid[, c_deficit := NULL]

#transform into + values
df_grid[, deficit := abs(deficit)]

##########################################################################################
######### Option 3 (Beni): Wettest month based on monthly sum Cumulative water balance #########
##########################################################################################

load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/combined_df_merged_southamerica.Rdata")


df_grid=combined_df_merged

df_grid=df_grid%>%
  filter(year>=2000)

# Ensure df_grid is a data.table
setDT(df_grid)

# Order the data by location and date to ensure correct accumulation
df_grid <- df_grid[order(lat, lon, date)]

# Calculate the cumulative sum of wbal for each lat, lon group
df_grid[, cumulative_wbal := cumsum(wbal), by = .(lat, lon)]

# mean seasonal cycle of the cumulative water balance
mean_seasonal_cycle <- df_grid %>%
  group_by(lat, lon, month) %>%
  summarize(mean_cwbal = sum(cumulative_wbal, na.rm = TRUE), .groups = 'drop')


# Month with the highest average cumulative water balance
cwb_sum_month <- mean_seasonal_cycle %>%
  group_by(lat, lon) %>%
  slice_max(mean_cwbal, n = 1) %>%
  ungroup() %>%
  dplyr::select(lat, lon, wettest_month = month)


cwb_mean_month$wettest_month=as.factor(cwb_mean_month$wettest_month)


p3=ggplot(cwb_mean_month) +
  geom_tile(aes(x = lon, y = lat, fill=wettest_month)) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", fill = "Month") +
  scale_fill_viridis(discrete = T, option = "B", na.value = "grey") +
  ggtitle("CWB - Wettest month (sum) ") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"))


# Then, the ouput of the cwb_mean_month allows to reset to 0 during the computation of CWD

df_grid <- df_grid |>
  left_join(cwb_sum_month, by = c( "lat", "lon")) %>%
  mutate(is_wettest_month = month == wettest_month)

setDT(df_grid)

# Order the data
df_grid <- df_grid[order(lat, lon, date)]

#  start accumulating 'wbal'
df_grid[, deficit := wbal]

# Calculate the cumulative deficit with resets
df_grid[, c_deficit := {
  # empty vector to store cumulative deficit values
  c_def <- numeric(.N)
  # Initialize a variable to keep track of the ongoing deficit
  ongoing_deficit <- 0

  # loop to calculate cwd
  for (i in 1:.N) {
    # Accumulate the deficit
    ongoing_deficit <- ongoing_deficit + wbal[i]

    # reset the ongoing deficit to 0 with these two conditions
    if (ongoing_deficit > 0 || is_wettest_month[i] ) {
      ongoing_deficit = 0
    }

    # assign the ongoing deficit to the vector
    c_def[i] <- ongoing_deficit
  }

  # Return the vector of ccwd values
  c_def
}, by = .(lat, lon)]

# Assign the calculated cumulative deficit to the 'deficit' column
df_grid[, deficit := c_deficit]

# Remove
df_grid[, c_deficit := NULL]

#transform into + values
df_grid[, deficit := abs(deficit)]


# Plot the 3 maps to compare
p1+p2+p3
