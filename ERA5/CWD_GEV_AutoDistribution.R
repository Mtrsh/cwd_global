# packages to load

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(extRemes)
library(lubridate)
library(ggplot2)


#df of CWD to load
load("/Net/Groups/BGI/scratch/mterristi/PhD/Data/CWD/cwd_deficiti_spain_1950_2023_.Rdata")

# get the annual max CWD
max_deficits <- df_grid %>%
  group_by(year, lat, lon) %>%
  summarize(max_deficit = max(deficit, na.rm = TRUE),
            date = date[which.max(deficit)]) %>%
  ungroup()

# keep only grid cells that have deficit > 0
max_deficits <- max_deficits %>%
  filter(max_deficit > 0)

# identify grid cells that have data for each year in the range (1950-2023)
valid_cells <- max_deficits %>%
  group_by(lat, lon) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years == 74) %>%
  ungroup()

# join the list of valid cells back to the filtered cwd df
df_max_cwd <- valid_cells %>%
  dplyr::select(lat, lon) %>%
  left_join(max_deficits, by = c("lat", "lon"))



###################################################################################################
########### FUNCTION 1: function to compute return levels for predefined return periods ###########
###################################################################################################

## here we want to know which distribution has been fitted by the function.

process_grid_cell <- function(data) {

  # Get the max CWD values
  vals <- data$max_deficit  # Assuming there is no NA values, only grid cells with 74 values (time range) are kept

  # Fit the GEV model
  evd_gev <- extRemes::fevd(vals, type = "GEV", method = "MLE", units = "years")
  shape_param <-   evd_gev$results$par[ "shape" ] # extract the shape parameter to know which distribution has been fitted by the function

  # Determine distribution type based on the shape parameter
  distribution_type <- ifelse(shape_param > 0, "Frechet",
                              ifelse(shape_param < 0, "Reversed Weibull",
                                     ifelse(shape_param == 0, "Gumbel")))

  return_period <- c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  return_level <- extRemes::return.level(evd_gev, return.period = return_period)

  # Extract max deficit for 2023
  max_2023 <-  data$max_deficit[data$year == 2023]


  # Create the data frame
  return_level_df <- data.frame(
    Return_Period = return_period,
    Return_Level = unname(return_level),
    Exceeded_2023 = sapply(return_level, function(x) x < max_2023),
    Max_2023_Deficit = max_2023,
    Distribution_Type = distribution_type,
    Latitude = rep(unique(data$lat)[1]),
    Longitude = rep(unique(data$lon)[1])
  )

  return(list(return_level_df = return_level_df))
}

# Apply the function to cell in the df
results_gev <- df_max_cwd%>%
  dplyr::group_by(lat, lon) %>%
  dplyr::group_split() %>%
  purrr::map(~process_grid_cell(.x))



return_level_df_list <- results_gev  %>% map("return_level_df")

# Unlist
results_gev  <- bind_rows(return_level_df_list)

rownames(results_gev) <- NULL

head(results_gev)

#extract one row per grid cell and map the distribution related to this grid cell
results_gev=results_gev%>%
  filter(Return_Period==100)


library(ggplot2)

ggplot(results_gev) +
  geom_tile(aes(x = Longitude, y = Latitude, fill = Distribution_Type)) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", fill = "Distribution_Type") +
  scale_fill_manual(values = c("Gumbel" = "blue", "Frechet" = "turquoise", "Reversed Weibull" = "lightpink")) +
  labs(title = "GEV Distribution type by grid cell",
       fill = "Distribution Type") +
  theme_minimal()


### The Weibull seems to fit the data the best.

#####################################################################################
################## Function 2: Return period for a given extreme  ##################
#####################################################################################


# extract one grid cell
test=df_max_cwd%>%
  filter(lat==41.125& lon==-5.125)

str(test)

# nested df
df <- test |>
  mutate(
    site = paste0("lon", as.character(lon), "_lat", as.character(lat))
  ) |>
  group_by(site) |>
  nest()

# plot pixel 1 to get a hint of how extreme was 2023
df$data[[1]] |>
  ggplot(aes(year, max_deficit)) +
  geom_line()

#all the grid cell
df <- df_max_cwd %>%
  mutate(
    site = paste0("lon", as.character(lon), "_lat", as.character(lat))
  ) |>
  group_by(site) |>
  nest()


# Function to fit extreme value distribution
get_evd <- function(df, type = "GEV"){
  vals <- df |> pull(max_deficit)
  extRemes::fevd(
    x = vals,
    type = type,
    method = "MLE",
    units = "years"
  )
}

# Calculate return period for Gumbel Distribution
calc_return_period_gumbel <- function(x, loc, scale){
  1 / (1 - exp(-exp(-(x-loc)/scale)))
}

# Calculate return period for Frechet Distribution
calc_return_period_frechet <- function(x, loc, scale, shape) {
  1 / (1 -  exp(-((x - loc) / scale)^(-shape)))
}

# Calculate return period for Reversed Weibull Distribution
calc_return_period_weibull <- function(x, loc, scale, shape) {
  1 / (1 - exp(-((loc - x) / scale)^shape))
}


# Extract location parameter
extract_loc <- function(mod){
  loc <- mod$results$par["location"]
  if (!is.null(loc)) {
    return(loc)
  } else {
    return(NA)
  }
}

# Extract scale parameter
extract_scale <- function(mod){
  scale <- mod$results$par["scale"]
  if (!is.null(scale)) {
    return(scale)
  } else {
    return(NA)
  }
}

# Extract shape parameter and determine the distribution type
determine_distribution_type <- function(mod){
  shape_param <- mod$results$par["shape"]
  ifelse(shape_param > 0, "Frechet",
         ifelse(shape_param < 0, "Reversed Weibull",
                ifelse(shape_param == 0, "Gumbel")))
}

# Calculate return periods based on distribution type
get_return_periods <- function(df, mod){
  vals <- df |> pull(max_deficit)
  loc <- extract_loc(mod)
  scale <- extract_scale(mod)
  dist_type <- determine_distribution_type(mod)


  switch(dist_type,
         "Frechet" = sapply(vals, calc_return_period_frechet, loc = loc, scale = scale, shape = mod$results$par["shape"]),
         "Reversed Weibull" = sapply(vals, calc_return_period_weibull, loc = loc, scale = scale, shape = mod$results$par["shape"]),
         "Gumbel" = sapply(vals, calc_return_period_gumbel, loc = loc, scale = scale)
  )
}

df <- df |>
  mutate(gev_model = map(data, ~get_evd(., type = "GEV")),
         distribution_type = map_chr(gev_model, determine_distribution_type),
         return_magnitudes = map(gev_model, ~unname(extRemes::return.level(., return.period = return_period))),
         return_periods = map2(data, gev_model, ~get_return_periods(.x, .y))
  ) |>
  dplyr::select(-gev_model, -starts_with("return_magnitudes_")) |>
  unnest(c(data, return_periods)) |>
  group_by(site) |>
  nest()

df =unnest(df)
head(df, n=20)
df_2023 <- df %>%
  filter(year==2023)

