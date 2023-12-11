# Objective: run the ED model for multiple days in a row, with the minimum 
# production being set up to 0. This is the simplest model possible. 

# 0. Set Up --------------------------------------------------------------------------------------
# Load the packages
library(dplyr)
library(ggplot2)
library(lpSolve)
library(lubridate)

theme_set(theme_bw())

# Load the data sets for August 25th 
gen_info <- read.csv("generator_information.csv")
demand <- read.csv("demand.csv")
capacity <- read.csv("capacity_factor.csv")

# 1. Define Functions --------------------------------------------------------------------------------------
# Parse out the current demand that must be met via electricity generation 
# Args
#   hr: the current hour to look at 
#   demand_df: data frame of the electricity demands 
# Return: scalar valu of the electricity demand in MW for 1 hour 
get_current_demand <- function(hr, demand_df = demand){
  out <- demand_df[demand_df$hour == hr, "demand"]
  return(out)
}


# Parse out the current max capacity (only changes for the certain fuel types)
# Args 
#   hr: the current hour to look at 
#   capacity_df: data frame with information about the generation capacity
# Returns: time series of max generation, this will be used to set the upper 
# limits on the possible vlaues of theta
get_current_capacity <- function(hr, capacity_df = capacity){
  out <- capacity_df[capacity_df$hour == hr, "max"]
  return(out)
}


# Optimize electricity generation per generator for single hour to meet consumer demands
# and also the variable max generation capacity 
# Args 
#   current_hr: integer current hour to be looking at must exist in the capacity and demand data frame (not checks are currently written)
#   capacity: data frame of the max capacity for each generator
#   demand: data frame of the consumer electricity demand 
#   gen_info: data frame of generator information
#   gen_cost: vector of the cost per unit of electricity (this does not include the carbon price)
# Returns: electricity produced per generator (not labeled this was designed to be an internal function)
single_time_load <- function(current_hr, capacity = capacity, demand = demand, gen_info = gen_info, gen_cost = gen_cost){
  
  # getting close! okay the next step is to figure out how to get the max in there!! 
  n <- 25
  lower_bound_matrix <- diag(nrow = n)
  lower_bound_vals <- rep_len(0, length.out = n)
  lower_bound_sign <- rep_len(">=", n)
  
  # construct the upper matrix 
  upper_bound_matrix <- diag(1, nrow = n)
  upper_bound_vals <- get_current_capacity(hr = current_hr)
  upper_bound_sign <- rep_len("<=", n)
  
  sum_row <- t(rep_len(1, length.out = n))
  sum_val <- get_current_demand(hr = current_hr)
  sum_sign <- "=="
  
  f.obj <- gen_cost
  f.con <- rbind(lower_bound_matrix, upper_bound_matrix, sum_row)
  f.dir <- c(lower_bound_sign, upper_bound_sign, sum_sign)
  f.rhs <- c(lower_bound_vals, upper_bound_vals, sum_val)
  
  
  fit <- lp ("min", f.obj, f.con, f.dir, f.rhs)
  
  # okay so if unable to meet the demands then what happens? do we have some 
  # unbound but extreemly expensive electicity? is there a way to figure out whey the solver 
  # is unabel to reach a solution? 
  
  
  
  return(fit$solution)
  
}


# Solve the optimization model over a time series of demand and capacity generation data 
# Args 
#   demand: data frame of the consumer electricity demand 
#   capacity: data frame of the max capacity for each generator
#   gen: data frame of generator information
# Returns: long formatted data frame of energy generated over the course of the demand time series
solve_ED <- function(demand, capacity, gen){
  
  # Calculate the cost associated with each type of electricity generation 
  gen_cost <- (gen_info$heat_rate_mmbtu_per_mwh * gen_info$fuel_cost + gen_info$var_om_cost_per_mwh) 
  
  # For each hour of the day figure the electricity produced by each generator within 
  # san deigos grid. 
  hrs <- demand$hour
  loads <- as.data.frame(do.call(lapply(hrs, single_time_load, gen_cost = gen_cost), what = "rbind"))
  
  # Add the r_id information to the results matrix
  colnames(loads) <- gen_info$r_id
  loads$hour <- hrs
  
  # Format the results matrix into a nice data frame, that has more information about the 
  # the generator type and fuel stuff. 
  tidyr::pivot_longer(data = loads, cols = -hour, names_to = "r_id") %>% 
    dplyr::mutate(r_id = as.character(r_id)) -> 
    out
  
  gen %>%
    dplyr::select(r_id, resource, region, fuel) %>% 
    dplyr::mutate(r_id = as.character(r_id))  %>% 
    dplyr::right_join(out, by = "r_id") %>%
    dplyr::select(hour, r_id, fuel, resource, region, value) ->
    out
  
  return(out)
}


# 2. Solve for the full year --------------------------------------------------------------------
# Run the ED model for the entire year. 
hr <- demand$hour
rslts <- solve_ED(demand = demand %>% filter(hour %in% hr),
                  capacity = capacity %>% filter(hour %in% hr), 
                  gen = gen_info)

# Quick check to make sure that the total energy produced each hour 
# matches the hourly demand, this will indicate that the implementation is correct 
rslts %>% 
  group_by(hour) %>%  
  summarise(value = sum(value)) -> 
  hourly_total_production
abs(hourly_total_production$my_demand - demand$demand) %>% summary()

# 3. Modify for plotting  ------------------------------------------------------------------------
# Now modify the hour information for the rslts and demands data frame for nice plotting. 
rslts %>%
  dplyr::select(fuel, resource, region, r_id) %>%
  distinct() ->
  metadata

mapping_df <- data.frame(resource = unique(metadata$resource),
                         type = c("biomass", "hyrdo", "natural gas", "natural gas", "wind", "hyrdo", "solar"))

# Add a time information and update the hours to reflect the actual time
# zone on the east coast.
rslts %>%
  dplyr::select(hour, r_id, value)  %>%
  pivot_wider(names_from = r_id, values_from = value) %>%
  arrange(hour) ->
  wide_rslts

# Convert from hours within a year to an date time object, convert from GMT to the 
# local time zone for CA. 
date1 <- as.POSIXct("2020-01-01 00:00", tz = "GMT") + as.difftime(wide_rslts$hour, units = "hours")
date_local <- format(date1, tz = "US/Pacific")
wide_rslts$time <- ymd_hms(date_local)
wide_rslts$hour <- NA
wide_rslts$hour[8:nrow(wide_rslts)] <- 1:8753
wide_rslts$hour[1:7] <- 8754:8760

#arrange(wide_rslts, time) %>% dplyr::select(hour, time)

cols <- setdiff(names(wide_rslts), c("hour", "time"))

wide_rslts %>%
  pivot_longer(cols = all_of(cols),
                 names_to = "r_id",
               values_to = "value") %>%
  full_join(metadata, by = "r_id") %>%
  mutate(date = date(time)) %>%
  full_join(mapping_df, by = "resource") ->
  rslts_long

demand %>% 
  arrange(hour) %>% 
  mutate(hour = wide_rslts$hour, 
         time = wide_rslts$time) -> 
  new_demand

# Check to make sure that the hourly things worked well. 
rslts_long %>% 
  arrange(time) %>% 
  group_by(time) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  rslts_new_check

which((rslts_new_check$time - new_demand$time) != 0)

# Okay yes so that is correct why is the area stacked
ggplot() + 
  geom_line(data = rslts_new_check[100:400, ], aes(time, value)) + 
  geom_line(data = new_demand[100:400, ], aes(time, demand, color = "mine"), linetype = 2)

total_e <- sum(rslts$value)
total_demand <- sum(demand$demand)
stopifnot( abs(total_e - total_demand) <= 1e-4)


rslts <- rslts_long
demand <- new_demand

rslts %>%  
  group_by(hour, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  checking_types

total_demand <- sum(demand$demand)
stopifnot((sum(checking_types$value) - total_demand) < 1e-4)


# 4. Project Results  ------------------------------------------------------------------------
# Minimum table 
gen_info %>%  
  left_join(mapping_df, by = "resource") %>% 
  group_by(type) %>% 
  summarise(existing_cap_mw = min(existing_cap_mw), 
            heat_rate_mmbtu_per_mwh = min(heat_rate_mmbtu_per_mwh), 
            fuel_cost = min(fuel_cost), 
            var_om_cost_per_mwh = min(var_om_cost_per_mwh))

# Max table 
gen_info %>%  
  left_join(mapping_df, by = "resource") %>% 
  group_by(type) %>% 
  summarise(existing_cap_mw = max(existing_cap_mw), 
            heat_rate_mmbtu_per_mwh = max(heat_rate_mmbtu_per_mwh), 
            fuel_cost = max(fuel_cost), 
            var_om_cost_per_mwh = max(var_om_cost_per_mwh))

# Mean table 
gen_info %>%  
  left_join(mapping_df, by = "resource") %>% 
  group_by(type) %>% 
  summarise(existing_cap_mw = mean(existing_cap_mw), 
            heat_rate_mmbtu_per_mwh = mean(heat_rate_mmbtu_per_mwh), 
            fuel_cost = mean(fuel_cost), 
            var_om_cost_per_mwh = mean(var_om_cost_per_mwh))

capacity %>% 
  left_join(mapping_df, by = "resource") %>% 
  group_by(type) %>% 
  summarise(min_max = min(max), 
            mean_max = mean(max), 
            max_max = max(max))

rslts %>% 
  group_by(time, hour, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  rslts_by_type


# Start by making a pie chart of annual energy production
rslts %>% 
  group_by(type) %>%  
  summarise(value = sum(value)) -> 
  annual_production

total_demand <- sum(demand$demand)
annual_production$percent <- (annual_production$value/total_demand) * 100

ggplot(data = annual_production, aes(x = "", y = percent, fill = type)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(x = NULL, title = "Percent of Annual Energy Production", y = NULL) + 
  theme(legend.title = element_blank())


# Now plot 24 hour periods from the four different meterological seasons 
# Meteorological spring in the Northern Hemisphere = March, April, and May
spring_date <- "2020-04-25"

rslts_by_type %>% 
  dplyr::filter(date(time) == spring_date) %>%  
  mutate(hour = rep(x = 1:24, each = 5)) -> 
  rlsts_to_plot

demand %>% 
  dplyr::filter(date(time) == spring_date) %>% 
  mutate(hour = 1:24) -> 
  demand_to_plot

ggplot() + 
  geom_area(data = rlsts_to_plot, aes(hour, value, fill = type)) + 
  geom_line(data = demand_to_plot, aes(hour, demand, color = "Total Demand"), 
            size = 1) + 
  scale_color_manual(values = c("Total Demand" = "Black")) + 
  theme(legend.title = element_blank()) + 
  labs(y = "MW", x = "Hour", title = paste0("Spring (", spring_date, ")")) + 
  ylim(0, 4000)


# Meteorological summer includes June, July, and August 
summer_date <- "2020-07-25"

rslts_by_type %>% 
  dplyr::filter(date(time) == summer_date) %>%  
  mutate(hour = rep(x = 1:24, each = 5)) -> 
  rlsts_to_plot

demand %>% 
  dplyr::filter(date(time) == summer_date) %>% 
  mutate(hour = 1:24) -> 
  demand_to_plot

ggplot() + 
  geom_area(data = rlsts_to_plot, aes(hour, value, fill = type)) + 
  geom_line(data = demand_to_plot, aes(hour, demand, color = "Total Demand"), 
            size = 1) + 
  scale_color_manual(values = c("Total Demand" = "Black")) + 
  theme(legend.title = element_blank()) + 
  labs(y = "MW", x = "Hour", title = paste0("Summer (", summer_date, ")")) + 
  ylim(0, 4000)

# Meteorological fall includes September, October, and November
fall_date <- "2020-10-25"

rslts_by_type %>% 
  dplyr::filter(date(time) == fall_date) %>%  
  mutate(hour = rep(x = 1:24, each = 5)) -> 
  rlsts_to_plot

demand %>% 
  dplyr::filter(date(time) == fall_date) %>% 
  mutate(hour = 1:24) -> 
  demand_to_plot

ggplot() + 
  geom_area(data = rlsts_to_plot, aes(hour, value, fill = type)) + 
  geom_line(data = demand_to_plot, aes(hour, demand, color = "Total Demand"), 
            size = 1) + 
  scale_color_manual(values = c("Total Demand" = "Black")) + 
  theme(legend.title = element_blank()) + 
  labs(y = "MW", x = "Hour", title = paste0("Fall (", fall_date, ")")) + 
  ylim(0, 4000)

# Meteorological winter includes December, January, and February.
winter_date <- "2020-01-25"

rslts_by_type %>% 
  dplyr::filter(date(time) == winter_date) %>%  
  mutate(hour = rep(x = 1:24, each = 5)) -> 
  rlsts_to_plot

demand %>% 
  dplyr::filter(date(time) == winter_date) %>% 
  mutate(hour = 1:24) -> 
  demand_to_plot

ggplot() + 
  geom_area(data = rlsts_to_plot, aes(hour, value, fill = type)) + 
  geom_line(data = demand_to_plot, aes(hour, demand, color = "Total Demand"), 
            size = 1) + 
  scale_color_manual(values = c("Total Demand" = "Black")) + 
  theme(legend.title = element_blank()) + 
  labs(y = "MW", x = "Hour", title = paste0("Winter (", winter_date, ")")) + 
  ylim(0, 4000)
