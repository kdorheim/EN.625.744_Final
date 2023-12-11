# Objective: Process the data so that is is ready to be used in the ED model. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


# 1. Import Data --------------------------------------------------------------
gen_info <- read.csv("data/Generators_data.csv") 
fuels <- read.csv("data/Fuels_data.csv")
loads <- read.csv("data/Demand.csv")
gen_variable <- read.csv("data/Generators_variability.csv")

# Rename all the columns to lowercase
names(gen_info) <- tolower(names(gen_info))
names(fuels) <- tolower(names(fuels))
names(loads) <- tolower(names(loads))
names(gen_variable) <- tolower(names(gen_variable))



# 1. generator_information -----------------------------------------------------
# Construct a data frame containing information about the generators in the San 
# Diego grid including information about capacity and other variables. 

# Subset the generator information so that we are only working with the types 
# of generators that are in SanDiego. 
gen_info %>% 
  # Subset the generator information to generators that have capacity since we are 
  # not doing a capacity expansion problem
  filter(existing_cap_mw > 0) %>% 
  # Keep only the information we are going to be using in this problem, too much 
  # information is overwhelming
  dplyr::select(r_id, resource, region, fuel, existing_cap_mw,
                heat_rate_mmbtu_per_mwh, var_om_cost_per_mwh, 
                cluster, num_units, ramp_up_percentage, ramp_dn_percentage, start_cost_per_mw) %>% 
  mutate(gen_full = tolower(paste0(region, "_", resource, "_", as.character(cluster), ".0"))) %>% 
  mutate(existing_cap_mw = existing_cap_mw * num_units) %>% 
  dplyr::select(-cluster, -num_units) -> 
  gen_info

# Add the fuel cost information, note that biomass here has a cost of 0 which um no. 
# But the renewable energies will have a fuel cost of 0. 
gen_info %>% 
  left_join(y = fuels, by = "fuel") %>% 
  rename(fuel_cost = cost_per_mmbtu) %>%  
  # If the fuel type was not included in our fuels data frame we are going to 
  # assume that the fuel cost is 0. 
  mutate(fuel_cost = if_else(is.na(fuel_cost), 0, fuel_cost))  %>% 
  # Categorize the resource as being variable or fixed (will we depend on weather conditions?)
  # wind and solar, small hydroelectric dams that may have variable water availability. 
  mutate(is_variable = if_else(resource %in% c("onshore_wind_turbine",
                                               "small_hydroelectric","solar_photovoltaic", 
                                               "landbasedwind", "utilitypv_losangeles"), TRUE, FALSE))  -> 
  gen_df
# this is a data frame of all the generators contributing to the San Deigo grid, 
# it contains some basic information about the region, the full name of the generator, 
# the maximum capacity, heat production, operating cost, fuel cost, and co2 content.  
write.csv(gen_df, file = file.path("generator_information.csv"), row.names = FALSE)


# 2. Identify the dates to keep  -----------------------------------------------
# For this example let's select August 25 as our date (arbitrarily) selected as 
# my birthday.... 

# # All of the data is in GMT but the local time is US/Pacific so when we are looking 
# # at the demand curves we will want to visualize the data in terms of the local time... 
# date1 <- as.POSIXct("2020-01-01 00:00", tz = "GMT") + as.difftime(loads$hour, units = "hours")
# date_local <- format(date1, tz = "US/Pacific")
# 
# aug_25_indicies <- which(ymd(as.Date(date_local)) %in% ymd("2020-08-25"))
# hrs_to_keep <- loads$hour[aug_25_indicies]


# 3. Format the demand data set  -----------------------------------------------
# Dataframe for the electricity demand per hour on August 25. 
# loads %>% 
#   filter(hour %in% hrs_to_keep) %>% 
#   mutate(hour = 1:24) -> 
#   demand

write.csv(loads, file = file.path("demand.csv"), row.names = FALSE)

ggplot(data = loads) + 
  geom_line(aes(hour, demand))


# 4. Format the demand data set  -----------------------------------------------
full_names <- names(gen_variable)[names(gen_variable) != "hour"]

to_keep <- c("hour", gen_df$gen_full)
gen_variable <- gen_variable[, names(gen_variable) %in% to_keep]

# These variables are fossil fuels aka non variables fuel types therefore the 
# capacity factor will always be equal to 1!
missing_names <- setdiff(gen_df$gen_full, full_names)
missing_data <- data.frame(matrix(1,
                                  nrow = nrow(gen_variable),
                                  ncol = length(missing_names)))
colnames(missing_data) <- missing_names
gen_variable <- cbind(gen_variable, missing_data)

gen_variable %>% 
  # # select only the hours in August and reset the hours such that they are 
  # # hours from the start of the day for august 25. 
  # filter(hour %in% hrs_to_keep) %>% 
  # mutate(hour = 1:24) %>%
  pivot_longer(!hour, names_to = "gen_full", values_to = "cf") -> 
  capacity_factor


capacity_factor %>% 
  inner_join(gen_info, by = "gen_full") %>% 
  arrange(r_id)  %>% 
  mutate(max = existing_cap_mw * cf) -> 
  capacity_factor


write.csv(capacity_factor, file = file.path("capacity_factor.csv"), row.names = FALSE)

