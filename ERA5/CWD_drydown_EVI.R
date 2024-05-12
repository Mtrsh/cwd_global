library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(data.table)

#load the cwd dataframe
load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_deficiti_spain_2000_2023_.Rdata")

#load the dataframe computing the events
load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_events_spain_2000_2023_.Rdata")

################################################################
################ one grid cell example ################
################################################################
df_grid = df_grid%>%
  filter(lat==41.125& lon==-5.125)

df_grid_events= deficit_events_list_short%>%
  filter(lat==41.125& lon==-5.125)

# Select rows with maximum total_deficit for each year
max_deficits <- df_grid_events  %>%
  group_by(year = year(start_date)) %>%
  filter(total_deficit == max(total_deficit, na.rm = TRUE)) %>%
  ungroup()


p= ggplot() +
  geom_rect(
    data = max_deficits,
    aes(xmin = start_date, xmax = end_date, ymin = -99, ymax = 99999),
    fill = rgb(0,0,0,0.3),
    color = NA) +
  geom_line(data  =  df_grid, aes(date, deficit), color = "tomato") +
  coord_cartesian(ylim = c(0, 1500)) +
  theme_classic() +
  labs(x = "Date", y = "Cumulative water deficit (mm)")

p

# Load weekly EVI df

load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/EVI/df_evi_weekly_spain.Rdata")

# Select the corresponding grid cell
df_grid_evi = evi_weekly_df%>%
  filter(lat==41.125& lon==-5.125)

#EVI time series with dry down event
p2=ggplot() +
  geom_rect(
    data = max_deficits,
    aes(xmin = start_date, xmax = end_date, ymin = -1, ymax = 0.50),
    fill = rgb(0,0,0,0.3),
    color = NA) +
  geom_line(data  =  df_grid_evi, aes(date, wevi), color = "tomato") +
  coord_cartesian(ylim = c(0, 0.50)) +
  theme_classic() +
  labs(x = "Date", y = "EVI")

library(patchwork)

# Both time series during drydown events
p/p2


# Plot CWD event for each year

# Convert to data.table
setDT(df_grid)
setDT(df_grid_events)
setDT(max_deficits)

head(max_deficits)

# Function to extract rows from df_grid based on each event
extract_rows <- function(row) {
  start_date <- as.Date(row$start_date)
  end_date <- as.Date(row$end_date)

  df_grid %>%
    filter(
      date >= start_date &
        date <= end_date &
        lat == row$lat &
        lon == row$lon
    )
}

# Apply the function to each row of max_deficits: it will extract the rows corresponding to a drydown event
extracted_dfs <- lapply(1:nrow(max_deficits), function(i) {
  extract_rows(max_deficits[i, ])
})

#  Unlist into a single data frame
extracted_df <- bind_rows(extracted_dfs)

# Convert year to factor for plotting
extracted_df$year <- as.factor(extracted_df$year)

# Convert date to day of the year
extracted_df$day_of_year <- yday(extracted_df$date)

names(extracted_df)

library(ggplot2)
# Plot with day in the x-axis
p1=ggplot(extracted_df, aes(x = day_of_year, y = deficit, color = year)) +
  geom_line(alpha = 0.7, size = 0.5) +
  geom_line(data = filter(extracted_df, year == 2023), aes(x = day_of_year, y = deficit), color = "red", size = 1.5) + # highlight in red 2023

  labs(
    title = "Comparison of annual maximum CWD events across years",
    x = "Day of the Year",
    y = "Deficit"
  ) +

  theme_minimal() +
  theme(legend.position = "left")



# Second alternative to have the month in the x-axis

# Define breaks for each month and label them for the x-axis
month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_labels <- month.abb

num_years <- 24 # define the number of years in your time period
year_colors <- scales::hue_pal()(num_years)

year_colors <- setNames(year_colors, 2000:2022)
year_colors["2023"] <- "red" # 2023 is the year i am interested in so it should be highlighted in red

# Filter data for 2023 to position the label and get the last data point for 2023
last_point_2023 <- extracted_df %>%
  filter(year == 2023) %>%
  slice(which.max(day_of_year))

# Plot
p2=ggplot(extracted_df, aes(x = day_of_year, y = deficit, group = year, color = as.factor(year))) +
  geom_line(alpha = 0.7, size = 0.5) +
  labs(
    title = "Comparison of annual maximum CWD events across years",
    x = "Month",
    y = "Deficit"
  ) +
  geom_line(data = filter(extracted_df, year == 2023), aes(x = day_of_year, y = deficit), color = "red", size = 1.5) +
  geom_text(data = last_point_2023, aes(label = "2023", x = day_of_year, y = deficit),
            hjust = 2, vjust = 0, color = "red", size = 5) +
  theme_minimal() +
  theme(legend.position = "left") +
  scale_x_continuous(breaks = month_starts, labels = month_labels) +
  scale_color_manual(values = year_colors) +
  guides(color = guide_legend(title = "Years of CWD events"))

p1/p2

######### CWD drydowns Return Period and EVI trajectories ###########

str(extracted_df)

# Convert year to numeric
extracted_df$year <- as.numeric(as.character(extracted_df$year))


str(df_grid_evi)

#merge the drydown with evi
cwd_evi_drydowns= merge(extracted_df, df_grid_evi, by=c("lat", "lon", "date", "year"), all.x=T)

str(cwd_evi_drydowns)

cwd_evi_drydowns$year <- as.factor(cwd_evi_drydowns$year)

save(cwd_evi_drydowns, file = "/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_evi_drydowns_grid_spain.Rdata")

# Find the last point of 2023 for labeling in the EVI graph
last_point_2023 <- cwd_evi_drydowns %>%
  filter(year == 2023) %>%
  slice(which.max(day_of_year))

# add EVI seasonal cycle also to see how 2023 departs from it

load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/EVI/df_evi_monthy_spain.Rdata")

# get the grid cell

evi_monthly_df_grid=  evi_monthly_df%>%
  filter(lat==41.125& lon==-5.125)

evi_monthly_df_grid <- evi_monthly_df_grid %>%
  mutate(day_of_year = month_starts[month])

num_years <- 24
year_colors <- scales::hue_pal()(num_years)

year_colors <- setNames(year_colors, 2000:2022)
year_colors["2023"] <- "red"
year_colors["Seasonal cycle"] <- "blue"

# Plot the EVI behavior during CWD-soil dry down
p3=ggplot(cwd_evi_drydowns, aes(x = day_of_year, y = wevi, color = as.factor(year))) +
  geom_line(alpha = 0.7, size = 0.5) +
  geom_line(data = filter(cwd_evi_drydowns, year == 2023), aes(x = day_of_year, y = wevi), color = "red", size = 1.5) +
  geom_text(data = last_point_2023, aes(label = "2023", x = day_of_year, y = wevi),
            hjust = 1.5, vjust = -1, color = "red", size = 5) +
  geom_line(data = evi_monthly_df_grid, aes(x = day_of_year, y = mean_EVI, color = "Seasonal cycle"), size = 1) +
  labs(
    title = "Comparison of EVI during annual maximum CWD events across years",
    x = "Day of the Year",
    y = "EVI"
  ) +
  theme_minimal() +
  theme(legend.position = "left") +
  scale_x_continuous(breaks = month_starts, labels = month_labels) +
  scale_color_manual(values = year_colors) +
  guides(color = guide_legend(title = "Years of CWD events"))


p2/p3


###########################################################################
############### how weekly EVI responds? example for one grid cell
###########################################################################


ggplot(cwd_evi_drydowns, aes(x = deficit, y = wevi)) +
  geom_point(alpha = 0.7, color = "blue") +  # Scatter plot
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Smoothing curve
  labs(
    title = "Relationship between CWD and wEVI (2000-2023)",
    x = "CWD",
    y = "wEVI"
  ) +
  theme_minimal()

# Find CWDcrit and CWDmax

g=ggplot(cwd_evi_drydowns, aes(x = deficit, y = wevi)) +
  geom_smooth(method = "loess", se = FALSE)

# Get and filter extrema
ld <- layer_data(g)
ld <- ld[c(which.max(ld$y), which.min(ld$y)), ]

# Make an annotation
g + geom_segment(
  data = ld,
  aes(x = x, y = y, xend = x, yend = c(Inf, -Inf)),
  arrow = arrow(ends = "first")
)

max_cwd_crit=ld%>%
  select(1,2)

# Ft the LOESS model
Loess_model <- loess(wevi ~ deficit, data = cwd_evi_drydowns)

# Add the LOESS fitted values into the dataframe
cwd_evi_drydowns$loess_fitted = Loess_model$fitted

# Compute the 90th percentile of the deficit + calculate the mean of wevi from it
quantile_10_mean_evi <- cwd_evi_drydowns %>%
  filter(deficit >= quantile(deficit, 0.9)) %>%
  summarise(mean_evi = mean(wevi)) %>%
  pull(mean_evi)

# Identify the max value from the LOESS fitted values
max_fitted_evi <- max(cwd_evi_drydowns$loess_fitted)
# Find the index of the max fitted EVI (which.max returns the position  where the max value is found) + corresponding CWD value
max_cwd_fitted_evi <- cwd_evi_drydowns$deficit[which.max(cwd_evi_drydowns$loess_fitted)]

# Calculate the point where EVI drops to 90% from maximum towards the mean EVI
target_evi <- max_fitted_evi - 0.90 * (max_fitted_evi - quantile_10_mean_evi)

# Find the clossest CWD value matching target_evi
deficit_at_target_evi <- cwd_evi_drydowns$deficit[which.min(abs(cwd_evi_drydowns$loess_fitted - target_evi))]

# Output
print(paste("Maximum fitted EVI:", max_fitted_evi, "at CWD:", max_cwd_fitted_evi))
print(paste("Target EVI (90% decline):", target_evi))
print(paste("CWD for 90% decline:", deficit_at_target_evi))
print(paste("Mean EVI the uppermost 10%-quantile CWD :", quantile_10_mean_evi))

# Plot the results

# get  starting value from the 90th percentile of the deficit (CWD min 90%)
deficit_threshold <- quantile(cwd_evi_drydowns$deficit, 0.9)

# CWD max 90%
max_deficit <- max(cwd_evi_drydowns$deficit)

# Define breaks for the x-axis
breaks <- c(deficit_threshold, max_deficit, max_cwd_fitted_evi, deficit_at_target_evi)
labels <- c(
  paste("CWD min 90%: ", format(deficit_threshold, digits = 2)),
  paste("CWD max : ", format(max_deficit, digits = 2)),
  paste("CWDcritical at: ", format(max_cwd_fitted_evi, digits = 2)),
  paste("CWDthreshold at: ", format(deficit_at_target_evi, digits = 2))
)

g <- ggplot(cwd_evi_drydowns, aes(x = deficit, y = wevi)) +
  geom_rect(aes(xmin = deficit_threshold, xmax = max_deficit, ymin = -Inf, ymax = Inf),   # Highlight the area for the uppermost 10%-quantile CWD
            fill = "#fcbba1", alpha = 0.5) +
  geom_rect(aes(xmin = max_cwd_fitted_evi, xmax = deficit_at_target_evi, ymin = -Inf, ymax = Inf), # Highlight the area of max loess to EVI decline point
            fill = "#99d8c9", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = c(deficit_threshold, max_deficit, max_cwd_fitted_evi, deficit_at_target_evi),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(x = max_cwd_fitted_evi, y = max_fitted_evi), color = "blue", size = 4) +
  annotate("text", x =  max_cwd_fitted_evi, y = max_fitted_evi, label = sprintf("Max LOESS: %.2f", max_fitted_evi),
           hjust = 1, vjust = -1, color = "blue") +
  geom_point(aes(x = deficit_at_target_evi, y = target_evi), color = "red", size = 4) +
  annotate("text", x = deficit_at_target_evi, y = target_evi, label = sprintf("EVI-decline: %.2f", target_evi),
           hjust = -0.1, vjust = -1, color = "red") +
  annotate("text", x = (deficit_threshold + max_deficit) / 2,
           y = quantile_10_mean_evi, label = paste("Mean wEVI:", format(quantile_10_mean_evi, digits = 2)),
           color = "black", fontface = "bold", vjust = -8 ) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme_minimal() +
  labs(title = "LOESS - Relationship CWD vs. EVI ", x = "CWD", y = "EVI")

g


# Store the results in a df
cwd_evi_loess_grid_cell <- data.frame(
  Latitude = cwd_evi_drydowns$lat[1],  # Assuming all lat values are the same
  Longitude = cwd_evi_drydowns$lon[1],  # Assuming all lon values are the same
  Max_fitted_LOESS = max_fitted_evi,
  CWD_max_LOESS = max_cwd_fitted_evi,
  EVI_90_decline = target_evi,
  CWD_90_decline = deficit_at_target_evi,
  Mean_EVI_10p = quantile_10_mean_evi,
  Min_CWD_10p = deficit_threshold,
  Max_CWD_10p = max_deficit
)

save(cwd_evi_loess_grid_cell , file = "/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_evi_loess_grid_cell.Rdata")


############### CWP RP to wEVI  ##############################

# load the df of the deficit events, select the max deficit per grid cell
load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_drydowns_evi_spain.Rdata")


# Select rows with maximum total_deficit for each year
max_deficits <- deficit_events_list_short %>%
  group_by(year = year(start_date), lat, lon) %>%
  summarize(max_deficit = max(total_deficit, na.rm = TRUE),
            date = start_date[which.max(total_deficit)]) %>%
  ungroup()

# Identify grid cells that have data for each year in the range
valid_cells <- max_deficits %>%
  group_by(lat, lon) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years == 24) %>%
  ungroup()

# Join the list of valid cells back to the initil max deficit df
df_filtered_cwd <- valid_cells %>%
  dplyr::select(lat, lon) %>%
  left_join(max_deficits, by = c("lat", "lon"))


df <- df_filtered_cwd  |>
  mutate(
    site = paste0("lon", as.character(lon), "_lat", as.character(lat))
  ) |>
  group_by(site) |>
  nest()


# Time serie for a grid cell
df$data[[1]] |>
  ggplot(aes(year, total_deficit)) +
  geom_line()

# function to fit extreme value distribution
get_evd <- function(df, type = "Gumbel"){
  vals <- df |>
    pull(total_deficit)

  extRemes::fevd(
    x = vals,
    type = type,
    method = "MLE",
    units = "years"
  )
}

return_period <- c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# function to calculate return periods of each year in observations
get_return_periods <- function(df, mod){

  vals <- df |>
    pull(total_deficit)

  calc_return_period(
    vals,
    extract_loc(mod),
    extract_scale(mod)
  )
}

calc_return_period <- function(x, loc, scale){
  1 / (1 - exp(-exp(-(x-loc)/scale)))
}

extract_loc <- function(mod){
  loc <- mod$results$par[ "location" ]
  if (!is.null(loc)){
    return(loc)
  } else {
    return(NA)
  }
}

extract_scale <- function(mod){
  scale <- mod$results$par[ "scale" ]
  if (!is.null(scale)){
    return(scale)
  } else {
    return(NA)
  }
}



df <- df |>

  # # example for one pixel
  # ungroup() |>
  # slice(1) |>

  mutate(

    # fit Gumbel - no warnings
    gumbel = purrr::map(data, ~get_evd(., type = "Gumbel"))
  ) |>

  # get return magnitudes for pre-defined set of return intervals
  mutate(

    return_magnitudes_gumbel = purrr::map(
      gumbel,
      ~unname(c(extRemes::return.level(., return.period = return_period))))
  ) |>

  # get return periods for all observations
  mutate(
    return_periods_gumbel = purrr::map2(data, gumbel, ~get_return_periods(.x, .y))
  ) |>

  # re-organise data: drop model objects and add return periods as columns to data
  dplyr::select(-gumbel, -starts_with("return_magnitudes_")) |>
  unnest(c(data, return_periods_gumbel)) |>
  group_by(site) |>
  nest()


gg1 <- df |>
  ungroup() |>
  slice(1) |>
  unnest(cols = c(data)) |>
  ggplot(aes(year, total_deficit)) +
  geom_line() +
  labs(x = "Year", y = "CWD (mm)") +
  theme_classic()

gg2 <- df |>
  ungroup() |>
  slice(1) |>
  unnest(cols = c(data)) |>
  ggplot(aes(year)) +
  geom_line(aes(y = return_periods_gumbel, color = "Gumbel")) +
  labs(x = "Year", y = "Return period (yr)") +
  theme_classic()

gg1 / gg2


# Unnest the data
df_unnested <- df %>%
  unnest(data)

# select evi per year during drydown event
evi_drydown= cwd_evi_drydowns%>%
  filter(lat==41.125& lon==-5.125)

max_deficits_evi <- cwd_evi_drydowns%>%
  group_by(year, lat, lon) %>%
  summarize(max_evi = max(wevi, na.rm = TRUE),
            date = date[which.max(wevi)]) %>%
  ungroup()

# Merge Return Period CWD (based on max CWD during drydown event) and max_evi during these dry down event
cwd_evi_rp= merge(df_unnested, max_deficits_evi, by=c("year", "lat", "lon"))

head(cwd_evi_rp)
library(viridis)

# Plota LOESS curve
cwd_evi_rp_loess <- ggplot(cwd_evi_rp, aes(x = return_periods_gumbel, y = max_evi, group = site, color = site)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    title = "LOESS-Relationship between return period of annual max CWD and annual max wEVI during drydown events (2000-2023)",
    x = "Return Period of CWD",
    y = "Maximum EVI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if too many groups of grid cells

print(cwd_evi_rp_loess)


###########################################################################
############### how weekly EVI responds? example for all the grid cells
###########################################################################

#load the df
load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_drydowns_evi_spain.Rdata")

#remove rows where we don't have matching values between CWD and EVI
cwd_evi_drydowns_all <- na.omit(cwd_evi_drydowns_all)

str(cwd_evi_drydowns_all)

#assign an unique ID to each grid cell
unique_pixels <- cwd_evi_drydowns_all %>%
  distinct(lat, lon) %>%
  mutate(pixel_id = row_number())  # Assign sequential ID

cwd_evi_drydowns_all <- cwd_evi_drydowns_all %>%
  left_join(unique_pixels, by = c("lat", "lon"))

cwd_evi_drydowns_all <- cwd_evi_drydowns_all %>%
  mutate(pixel_id = as.factor(pixel_id))

head(cwd_evi_drydowns_all)

#
cwd_evi_drydowns_all_fig_loess <- ggplot(cwd_evi_drydowns_all, aes(x = deficit, y = wevi, color = pixel_id)) +
  geom_smooth(aes(group = pixel_id), method = "loess", span = 0.9, se = FALSE) +
  labs(
    title = "LOESS-Relationship between CWD and wEVI (2000-2023)",
    x = "CWD",
    y = "wEVI"
  ) +
  theme_minimal() +
  scale_color_viridis_d(guide = "none")

#error message: `geom_smooth()` using formula = 'y ~ x'
#Warning messages:
#1: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric, : span too small.   fewer data values than degrees of freedom.
#2: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  : pseudoinverse used at 839.49
#3: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  : neighborhood radius 53.597
#4: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  : reciprocal condition number  0
# 5: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  : There are other near singularities as well. 2542.3

hist(cwd_evi_drydowns_all$deficit,
     main = "Histogram of Deficit", xlab = "Deficit")


# Sample a subset of the data
sampled_data <- cwd_evi_drydowns_all %>%
  filter(pixel_id<=110)

sampled_data <- sampled_data %>%
  mutate(pixel_id = as.factor(pixel_id))


# Plot with sampled data
sampled_plot <- ggplot(sampled_data, aes(x = deficit, y = wevi, color = pixel_id), size=0.01) +
  geom_smooth(aes(group = pixel_id), method = "loess", se = FALSE, span=0.5) +
  labs(title = "Sampled data (110 grid cells): Relationship between CWD and wEVI (2000-2023)", x = "CWD", y = "wEVI") +
  theme_minimal()+guides(fill="none", color="none")

print(sampled_plot)



