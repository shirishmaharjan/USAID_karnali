# Calculate seasonal sens slope of mean temperature and total precipitation


# Load libraries  ---------------------------------------------------------

library(Kendall)
library(forecast)
library(dplyr)
library(tidyr)
library(broom)
library(dplyr)
library(magrittr)
library(vroom)
library(trend)


# Load max, min, mean and total precipitation datasets of three grids ----------------------------------------------------------

tempMean_1970_2023_monthly_mean <- read.csv("C:/GIIS/USAID Karnali/Data/monthly_max_mean_min_precp (Second)/tempMean_1970_2023_monthly_mean.csv")
precTot_1970_2023_monthly_sum <- read.csv("C:/GIIS/USAID Karnali/Data/monthly_max_mean_min_precp (Second)/precTot_1970_2023_monthly_sum.csv")


#  MUGU MEAN TEMPERATURE --------------------------------------------------------

# Mean temp Annual

MUGU_mean_temp_annual <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, MUGU) %>% 
  group_by(year) %>% 
  summarise(MUGU_mean_temp_annual = mean(MUGU))

# Mean temp Premonsoon (March - May)

MUGU_mean_temp_premonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(MUGU_mean_temp_premonsoon = mean(MUGU))

# Mean temp monsoon (June - Sept)

MUGU_mean_temp_monsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(MUGU_mean_temp_monsoon = mean(MUGU))


# Mean temp postmonsoon (Oct - Nov)

MUGU_mean_temp_postmonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(MUGU_mean_temp_postmonsoon = mean(MUGU))


# Mean temp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

MUGU_mean_temp_winter <- tempMean_1970_2023_monthly_mean %>%
  group_by(year,month, MUGU) %>%
  summarise(MUGU_mean_temp_winter = mean(MUGU)) %>% 
  filter(month %in% target)

# Remove 1970 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

MUGU_mean_temp_winter <- MUGU_mean_temp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(MUGU_mean_temp_winter) - 1)%/%3

MUGU_mean_temp_winter_final <- data.frame(
  range = aggregate(MUGU_mean_temp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(MUGU_mean_temp_winter$MUGU_mean_temp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

MUGU_mean_temp_winter_final <- MUGU_mean_temp_winter_final[1:53, ]

MUGU_mean_temp_winter_final <- MUGU_mean_temp_winter_final %>% 
  select(sum) %>% 
  mutate(MUGU_mean_temp_winter = sum/3,
         year = 1970:2022) %>% 
  select(MUGU_mean_temp_winter, year) 

MUGU_mean_temp_winter_final <- MUGU_mean_temp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual Mean Temperature 

MUGU_mean_temp_annual_slope_pvalue <- MUGU_mean_temp_annual %>%
  summarise(MUGU_mean_temp_annual_temp_slope = sens.slope(MUGU_mean_temp_annual)$estimates,
            MUGU_mean_temp_annual_temp_pvalue = mk.test(MUGU_mean_temp_annual)$p.value)

# Pre-Monsoon Mean Temperature 

MUGU_mean_temp_premonsoon_slope_pvalue <- MUGU_mean_temp_premonsoon %>%
  summarise(MUGU_mean_temp_premonsoon_temp_slope = sens.slope(MUGU_mean_temp_premonsoon)$estimates,
            MUGU_mean_temp_premonsoon_temp_pvalue = mk.test(MUGU_mean_temp_premonsoon)$p.value)

# Monsoon Mean Temperature

MUGU_mean_temp_monsoon_slope_pvalue <- MUGU_mean_temp_monsoon %>%
  summarise(MUGU_mean_temp_monsoon_temp_slope = sens.slope(MUGU_mean_temp_monsoon)$estimates,
            MUGU_mean_temp_monsoon_temp_pvalue = mk.test(MUGU_mean_temp_monsoon)$p.value)

# Post-Monsoon Mean Temperature 

MUGU_mean_temp_postmonsoon_slope_pvalue <- MUGU_mean_temp_postmonsoon %>%
  summarise(MUGU_mean_temp_postmonsoon_temp_slope = sens.slope(MUGU_mean_temp_postmonsoon)$estimates,
            MUGU_mean_temp_postmonsoon_temp_pvalue = mk.test(MUGU_mean_temp_postmonsoon)$p.value)


# Winter Mean Temperature 

MUGU_mean_temp_winter_slope_pvalue <- MUGU_mean_temp_winter_final %>%
  summarise(MUGU_mean_temp_post_temp_winter_slope = sens.slope(MUGU_mean_temp_winter)$estimates,
            MUGU_mean_temp_post_temp_winter_pvalue = mk.test(MUGU_mean_temp_winter)$p.value)




MUGU_mean_temp_slope_pvalue <- cbind(MUGU_mean_temp_annual_slope_pvalue,
                                                MUGU_mean_temp_premonsoon_slope_pvalue,
                                                MUGU_mean_temp_monsoon_slope_pvalue,
                                                MUGU_mean_temp_postmonsoon_slope_pvalue,
                                                MUGU_mean_temp_winter_slope_pvalue)

MUGU_mean_temp_slope_pvalue


# MUGU TOTAL PRECIPITATION -------------------------------------------------

# Total precp

# total precp Annual

MUGU_total_precp_annual <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, MUGU) %>% 
  group_by(year) %>% 
  summarise(MUGU_total_precp_annual = sum(MUGU))

# total precp Premonsoon (March - May)

MUGU_total_precp_premonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(MUGU_total_precp_premonsoon = sum(MUGU))

# total precp monsoon (June - Sept)

MUGU_total_precp_monsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(MUGU_total_precp_monsoon = sum(MUGU))


# total precp postmonsoon (Oct - Nov)

MUGU_total_precp_postmonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, MUGU) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(MUGU_total_precp_postmonsoon = sum(MUGU))


# total precp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

MUGU_total_precp_winter <- precTot_1970_2023_monthly_sum %>%
  group_by(year,month, MUGU) %>%
  summarise(MUGU_total_precp_winter = sum(MUGU)) %>% 
  filter(month %in% target)

# Remove 1951 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

MUGU_total_precp_winter <- MUGU_total_precp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(MUGU_total_precp_winter) - 1)%/%3

MUGU_total_precp_winter_final <- data.frame(
  range = aggregate(MUGU_total_precp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(MUGU_total_precp_winter$MUGU_total_precp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

MUGU_total_precp_winter_final <- MUGU_total_precp_winter_final[1:52, ]

MUGU_total_precp_winter_final <- MUGU_total_precp_winter_final %>% 
  select(sum) %>% 
  mutate(year = 1970:2021) %>% 
  rename(MUGU_total_precp_winter = sum) %>% 
  select(MUGU_total_precp_winter, year) 

MUGU_total_precp_winter_final <- MUGU_total_precp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual total precperature 

MUGU_total_precp_annual_slope_pvalue <- MUGU_total_precp_annual %>%
  summarise(MUGU_total_precp_annual_precp_slope = sens.slope(MUGU_total_precp_annual)$estimates,
            MUGU_total_precp_annual_precp_pvalue = mk.test(MUGU_total_precp_annual)$p.value)

# Pre-Monsoon total precperature 

MUGU_total_precp_premonsoon_slope_pvalue <- MUGU_total_precp_premonsoon %>%
  summarise(MUGU_total_precp_premonsoon_precp_slope = sens.slope(MUGU_total_precp_premonsoon)$estimates,
            MUGU_total_precp_premonsoon_precp_pvalue = mk.test(MUGU_total_precp_premonsoon)$p.value)

# Monsoon total precperature

MUGU_total_precp_monsoon_slope_pvalue <- MUGU_total_precp_monsoon %>%
  summarise(MUGU_total_precp_monsoon_precp_slope = sens.slope(MUGU_total_precp_monsoon)$estimates,
            MUGU_total_precp_monsoon_precp_pvalue = mk.test(MUGU_total_precp_monsoon)$p.value)

# Post-Monsoon total precperature 

MUGU_total_precp_postmonsoon_slope_pvalue <- MUGU_total_precp_postmonsoon %>%
  summarise(MUGU_total_precp_postmonsoon_precp_slope = sens.slope(MUGU_total_precp_postmonsoon)$estimates,
            MUGU_total_precp_postmonsoon_precp_pvalue = mk.test(MUGU_total_precp_postmonsoon)$p.value)


# Winter total precperature 

MUGU_total_precp_winter_slope_pvalue <- MUGU_total_precp_winter_final %>%
  summarise(MUGU_total_precp_winter_precp_slope = sens.slope(MUGU_total_precp_winter)$estimates,
            MUGU_total_precp_winter_precp_pvalue = mk.test(MUGU_total_precp_winter)$p.value)



MUGU_total_precp_slope_pvalue <- cbind(MUGU_total_precp_annual_slope_pvalue,
                                                MUGU_total_precp_premonsoon_slope_pvalue,
                                                MUGU_total_precp_monsoon_slope_pvalue,
                                                MUGU_total_precp_postmonsoon_slope_pvalue,
                                                MUGU_total_precp_winter_slope_pvalue)


MUGU_total_precp_slope_pvalue




#  KALIKOT MEAN TEMPERATURE --------------------------------------------------------

# Mean temp Annual

KALIKOT_mean_temp_annual <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, KALIKOT) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_mean_temp_annual = mean(KALIKOT))

# Mean temp Premonsoon (March - May)

KALIKOT_mean_temp_premonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_mean_temp_premonsoon = mean(KALIKOT))

# Mean temp monsoon (June - Sept)

KALIKOT_mean_temp_monsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_mean_temp_monsoon = mean(KALIKOT))


# Mean temp postmonsoon (Oct - Nov)

KALIKOT_mean_temp_postmonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_mean_temp_postmonsoon = mean(KALIKOT))


# Mean temp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

KALIKOT_mean_temp_winter <- tempMean_1970_2023_monthly_mean %>%
  group_by(year,month, KALIKOT) %>%
  summarise(KALIKOT_mean_temp_winter = mean(KALIKOT)) %>% 
  filter(month %in% target)

# Remove 1970 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

KALIKOT_mean_temp_winter <- KALIKOT_mean_temp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(KALIKOT_mean_temp_winter) - 1)%/%3

KALIKOT_mean_temp_winter_final <- data.frame(
  range = aggregate(KALIKOT_mean_temp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(KALIKOT_mean_temp_winter$KALIKOT_mean_temp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

KALIKOT_mean_temp_winter_final <- KALIKOT_mean_temp_winter_final[1:53, ]

KALIKOT_mean_temp_winter_final <- KALIKOT_mean_temp_winter_final %>% 
  select(sum) %>% 
  mutate(KALIKOT_mean_temp_winter = sum/3,
         year = 1970:2022) %>% 
  select(KALIKOT_mean_temp_winter, year) 

KALIKOT_mean_temp_winter_final <- KALIKOT_mean_temp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual Mean Temperature 

KALIKOT_mean_temp_annual_slope_pvalue <- KALIKOT_mean_temp_annual %>%
  summarise(KALIKOT_mean_temp_annual_temp_slope = sens.slope(KALIKOT_mean_temp_annual)$estimates,
            KALIKOT_mean_temp_annual_temp_pvalue = mk.test(KALIKOT_mean_temp_annual)$p.value)

# Pre-Monsoon Mean Temperature 

KALIKOT_mean_temp_premonsoon_slope_pvalue <- KALIKOT_mean_temp_premonsoon %>%
  summarise(KALIKOT_mean_temp_premonsoon_temp_slope = sens.slope(KALIKOT_mean_temp_premonsoon)$estimates,
            KALIKOT_mean_temp_premonsoon_temp_pvalue = mk.test(KALIKOT_mean_temp_premonsoon)$p.value)

# Monsoon Mean Temperature

KALIKOT_mean_temp_monsoon_slope_pvalue <- KALIKOT_mean_temp_monsoon %>%
  summarise(KALIKOT_mean_temp_monsoon_temp_slope = sens.slope(KALIKOT_mean_temp_monsoon)$estimates,
            KALIKOT_mean_temp_monsoon_temp_pvalue = mk.test(KALIKOT_mean_temp_monsoon)$p.value)

# Post-Monsoon Mean Temperature 

KALIKOT_mean_temp_postmonsoon_slope_pvalue <- KALIKOT_mean_temp_postmonsoon %>%
  summarise(KALIKOT_mean_temp_postmonsoon_temp_slope = sens.slope(KALIKOT_mean_temp_postmonsoon)$estimates,
            KALIKOT_mean_temp_postmonsoon_temp_pvalue = mk.test(KALIKOT_mean_temp_postmonsoon)$p.value)


# Winter Mean Temperature 

KALIKOT_mean_temp_winter_slope_pvalue <- KALIKOT_mean_temp_winter_final %>%
  summarise(KALIKOT_mean_temp_post_temp_winter_slope = sens.slope(KALIKOT_mean_temp_winter)$estimates,
            KALIKOT_mean_temp_post_temp_winter_pvalue = mk.test(KALIKOT_mean_temp_winter)$p.value)




KALIKOT_mean_temp_slope_pvalue <- cbind(KALIKOT_mean_temp_annual_slope_pvalue,
                                     KALIKOT_mean_temp_premonsoon_slope_pvalue,
                                     KALIKOT_mean_temp_monsoon_slope_pvalue,
                                     KALIKOT_mean_temp_postmonsoon_slope_pvalue,
                                     KALIKOT_mean_temp_winter_slope_pvalue)

KALIKOT_mean_temp_slope_pvalue


# KALIKOT TOTAL PRECIPITATION -------------------------------------------------

# Total precp

# total precp Annual

KALIKOT_total_precp_annual <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, KALIKOT) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_total_precp_annual = sum(KALIKOT))

# total precp Premonsoon (March - May)

KALIKOT_total_precp_premonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_total_precp_premonsoon = sum(KALIKOT))

# total precp monsoon (June - Sept)

KALIKOT_total_precp_monsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_total_precp_monsoon = sum(KALIKOT))


# total precp postmonsoon (Oct - Nov)

KALIKOT_total_precp_postmonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, KALIKOT) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(KALIKOT_total_precp_postmonsoon = sum(KALIKOT))


# total precp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

KALIKOT_total_precp_winter <- precTot_1970_2023_monthly_sum %>%
  group_by(year,month, KALIKOT) %>%
  summarise(KALIKOT_total_precp_winter = sum(KALIKOT)) %>% 
  filter(month %in% target)

# Remove 1951 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

KALIKOT_total_precp_winter <- KALIKOT_total_precp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(KALIKOT_total_precp_winter) - 1)%/%3

KALIKOT_total_precp_winter_final <- data.frame(
  range = aggregate(KALIKOT_total_precp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(KALIKOT_total_precp_winter$KALIKOT_total_precp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

KALIKOT_total_precp_winter_final <- KALIKOT_total_precp_winter_final[1:52, ]

KALIKOT_total_precp_winter_final <- KALIKOT_total_precp_winter_final %>% 
  select(sum) %>% 
  mutate(year = 1970:2021) %>% 
  rename(KALIKOT_total_precp_winter = sum) %>% 
  select(KALIKOT_total_precp_winter, year) 

KALIKOT_total_precp_winter_final <- KALIKOT_total_precp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual total precperature 

KALIKOT_total_precp_annual_slope_pvalue <- KALIKOT_total_precp_annual %>%
  summarise(KALIKOT_total_precp_annual_precp_slope = sens.slope(KALIKOT_total_precp_annual)$estimates,
            KALIKOT_total_precp_annual_precp_pvalue = mk.test(KALIKOT_total_precp_annual)$p.value)

# Pre-Monsoon total precperature 

KALIKOT_total_precp_premonsoon_slope_pvalue <- KALIKOT_total_precp_premonsoon %>%
  summarise(KALIKOT_total_precp_premonsoon_precp_slope = sens.slope(KALIKOT_total_precp_premonsoon)$estimates,
            KALIKOT_total_precp_premonsoon_precp_pvalue = mk.test(KALIKOT_total_precp_premonsoon)$p.value)

# Monsoon total precperature

KALIKOT_total_precp_monsoon_slope_pvalue <- KALIKOT_total_precp_monsoon %>%
  summarise(KALIKOT_total_precp_monsoon_precp_slope = sens.slope(KALIKOT_total_precp_monsoon)$estimates,
            KALIKOT_total_precp_monsoon_precp_pvalue = mk.test(KALIKOT_total_precp_monsoon)$p.value)

# Post-Monsoon total precperature 

KALIKOT_total_precp_postmonsoon_slope_pvalue <- KALIKOT_total_precp_postmonsoon %>%
  summarise(KALIKOT_total_precp_postmonsoon_precp_slope = sens.slope(KALIKOT_total_precp_postmonsoon)$estimates,
            KALIKOT_total_precp_postmonsoon_precp_pvalue = mk.test(KALIKOT_total_precp_postmonsoon)$p.value)


# Winter total precperature 

KALIKOT_total_precp_winter_slope_pvalue <- KALIKOT_total_precp_winter_final %>%
  summarise(KALIKOT_total_precp_winter_precp_slope = sens.slope(KALIKOT_total_precp_winter)$estimates,
            KALIKOT_total_precp_winter_precp_pvalue = mk.test(KALIKOT_total_precp_winter)$p.value)



KALIKOT_total_precp_slope_pvalue <- cbind(KALIKOT_total_precp_annual_slope_pvalue,
                                       KALIKOT_total_precp_premonsoon_slope_pvalue,
                                       KALIKOT_total_precp_monsoon_slope_pvalue,
                                       KALIKOT_total_precp_postmonsoon_slope_pvalue,
                                       KALIKOT_total_precp_winter_slope_pvalue)


KALIKOT_total_precp_slope_pvalue




#  JUMLA MEAN TEMPERATURE --------------------------------------------------------

# Mean temp Annual

JUMLA_mean_temp_annual <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, JUMLA) %>% 
  group_by(year) %>% 
  summarise(JUMLA_mean_temp_annual = mean(JUMLA))

# Mean temp Premonsoon (March - May)

JUMLA_mean_temp_premonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(JUMLA_mean_temp_premonsoon = mean(JUMLA))

# Mean temp monsoon (June - Sept)

JUMLA_mean_temp_monsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(JUMLA_mean_temp_monsoon = mean(JUMLA))


# Mean temp postmonsoon (Oct - Nov)

JUMLA_mean_temp_postmonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(JUMLA_mean_temp_postmonsoon = mean(JUMLA))


# Mean temp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

JUMLA_mean_temp_winter <- tempMean_1970_2023_monthly_mean %>%
  group_by(year,month, JUMLA) %>%
  summarise(JUMLA_mean_temp_winter = mean(JUMLA)) %>% 
  filter(month %in% target)

# Remove 1970 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

JUMLA_mean_temp_winter <- JUMLA_mean_temp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(JUMLA_mean_temp_winter) - 1)%/%3

JUMLA_mean_temp_winter_final <- data.frame(
  range = aggregate(JUMLA_mean_temp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(JUMLA_mean_temp_winter$JUMLA_mean_temp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

JUMLA_mean_temp_winter_final <- JUMLA_mean_temp_winter_final[1:53, ]

JUMLA_mean_temp_winter_final <- JUMLA_mean_temp_winter_final %>% 
  select(sum) %>% 
  mutate(JUMLA_mean_temp_winter = sum/3,
         year = 1970:2022) %>% 
  select(JUMLA_mean_temp_winter, year) 

JUMLA_mean_temp_winter_final <- JUMLA_mean_temp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual Mean Temperature 

JUMLA_mean_temp_annual_slope_pvalue <- JUMLA_mean_temp_annual %>%
  summarise(JUMLA_mean_temp_annual_temp_slope = sens.slope(JUMLA_mean_temp_annual)$estimates,
            JUMLA_mean_temp_annual_temp_pvalue = mk.test(JUMLA_mean_temp_annual)$p.value)

# Pre-Monsoon Mean Temperature 

JUMLA_mean_temp_premonsoon_slope_pvalue <- JUMLA_mean_temp_premonsoon %>%
  summarise(JUMLA_mean_temp_premonsoon_temp_slope = sens.slope(JUMLA_mean_temp_premonsoon)$estimates,
            JUMLA_mean_temp_premonsoon_temp_pvalue = mk.test(JUMLA_mean_temp_premonsoon)$p.value)

# Monsoon Mean Temperature

JUMLA_mean_temp_monsoon_slope_pvalue <- JUMLA_mean_temp_monsoon %>%
  summarise(JUMLA_mean_temp_monsoon_temp_slope = sens.slope(JUMLA_mean_temp_monsoon)$estimates,
            JUMLA_mean_temp_monsoon_temp_pvalue = mk.test(JUMLA_mean_temp_monsoon)$p.value)

# Post-Monsoon Mean Temperature 

JUMLA_mean_temp_postmonsoon_slope_pvalue <- JUMLA_mean_temp_postmonsoon %>%
  summarise(JUMLA_mean_temp_postmonsoon_temp_slope = sens.slope(JUMLA_mean_temp_postmonsoon)$estimates,
            JUMLA_mean_temp_postmonsoon_temp_pvalue = mk.test(JUMLA_mean_temp_postmonsoon)$p.value)


# Winter Mean Temperature 

JUMLA_mean_temp_winter_slope_pvalue <- JUMLA_mean_temp_winter_final %>%
  summarise(JUMLA_mean_temp_post_temp_winter_slope = sens.slope(JUMLA_mean_temp_winter)$estimates,
            JUMLA_mean_temp_post_temp_winter_pvalue = mk.test(JUMLA_mean_temp_winter)$p.value)




JUMLA_mean_temp_slope_pvalue <- cbind(JUMLA_mean_temp_annual_slope_pvalue,
                                        JUMLA_mean_temp_premonsoon_slope_pvalue,
                                        JUMLA_mean_temp_monsoon_slope_pvalue,
                                        JUMLA_mean_temp_postmonsoon_slope_pvalue,
                                        JUMLA_mean_temp_winter_slope_pvalue)

JUMLA_mean_temp_slope_pvalue


# JUMLA TOTAL PRECIPITATION -------------------------------------------------

# Total precp

# total precp Annual

JUMLA_total_precp_annual <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, JUMLA) %>% 
  group_by(year) %>% 
  summarise(JUMLA_total_precp_annual = sum(JUMLA))

# total precp Premonsoon (March - May)

JUMLA_total_precp_premonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(JUMLA_total_precp_premonsoon = sum(JUMLA))

# total precp monsoon (June - Sept)

JUMLA_total_precp_monsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(JUMLA_total_precp_monsoon = sum(JUMLA))


# total precp postmonsoon (Oct - Nov)

JUMLA_total_precp_postmonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, JUMLA) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(JUMLA_total_precp_postmonsoon = sum(JUMLA))


# total precp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

JUMLA_total_precp_winter <- precTot_1970_2023_monthly_sum %>%
  group_by(year,month, JUMLA) %>%
  summarise(JUMLA_total_precp_winter = sum(JUMLA)) %>% 
  filter(month %in% target)

# Remove 1951 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

JUMLA_total_precp_winter <- JUMLA_total_precp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(JUMLA_total_precp_winter) - 1)%/%3

JUMLA_total_precp_winter_final <- data.frame(
  range = aggregate(JUMLA_total_precp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(JUMLA_total_precp_winter$JUMLA_total_precp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

JUMLA_total_precp_winter_final <- JUMLA_total_precp_winter_final[1:52, ]

JUMLA_total_precp_winter_final <- JUMLA_total_precp_winter_final %>% 
  select(sum) %>% 
  mutate(year = 1970:2021) %>% 
  rename(JUMLA_total_precp_winter = sum) %>% 
  select(JUMLA_total_precp_winter, year) 

JUMLA_total_precp_winter_final <- JUMLA_total_precp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual total precperature 

JUMLA_total_precp_annual_slope_pvalue <- JUMLA_total_precp_annual %>%
  summarise(JUMLA_total_precp_annual_precp_slope = sens.slope(JUMLA_total_precp_annual)$estimates,
            JUMLA_total_precp_annual_precp_pvalue = mk.test(JUMLA_total_precp_annual)$p.value)

# Pre-Monsoon total precperature 

JUMLA_total_precp_premonsoon_slope_pvalue <- JUMLA_total_precp_premonsoon %>%
  summarise(JUMLA_total_precp_premonsoon_precp_slope = sens.slope(JUMLA_total_precp_premonsoon)$estimates,
            JUMLA_total_precp_premonsoon_precp_pvalue = mk.test(JUMLA_total_precp_premonsoon)$p.value)

# Monsoon total precperature

JUMLA_total_precp_monsoon_slope_pvalue <- JUMLA_total_precp_monsoon %>%
  summarise(JUMLA_total_precp_monsoon_precp_slope = sens.slope(JUMLA_total_precp_monsoon)$estimates,
            JUMLA_total_precp_monsoon_precp_pvalue = mk.test(JUMLA_total_precp_monsoon)$p.value)

# Post-Monsoon total precperature 

JUMLA_total_precp_postmonsoon_slope_pvalue <- JUMLA_total_precp_postmonsoon %>%
  summarise(JUMLA_total_precp_postmonsoon_precp_slope = sens.slope(JUMLA_total_precp_postmonsoon)$estimates,
            JUMLA_total_precp_postmonsoon_precp_pvalue = mk.test(JUMLA_total_precp_postmonsoon)$p.value)


# Winter total precperature 

JUMLA_total_precp_winter_slope_pvalue <- JUMLA_total_precp_winter_final %>%
  summarise(JUMLA_total_precp_winter_precp_slope = sens.slope(JUMLA_total_precp_winter)$estimates,
            JUMLA_total_precp_winter_precp_pvalue = mk.test(JUMLA_total_precp_winter)$p.value)



JUMLA_total_precp_slope_pvalue <- cbind(JUMLA_total_precp_annual_slope_pvalue,
                                          JUMLA_total_precp_premonsoon_slope_pvalue,
                                          JUMLA_total_precp_monsoon_slope_pvalue,
                                          JUMLA_total_precp_postmonsoon_slope_pvalue,
                                          JUMLA_total_precp_winter_slope_pvalue)


JUMLA_total_precp_slope_pvalue





#  HUMLA MEAN TEMPERATURE --------------------------------------------------------

# Mean temp Annual

HUMLA_mean_temp_annual <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, HUMLA) %>% 
  group_by(year) %>% 
  summarise(HUMLA_mean_temp_annual = mean(HUMLA))

# Mean temp Premonsoon (March - May)

HUMLA_mean_temp_premonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(HUMLA_mean_temp_premonsoon = mean(HUMLA))

# Mean temp monsoon (June - Sept)

HUMLA_mean_temp_monsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(HUMLA_mean_temp_monsoon = mean(HUMLA))


# Mean temp postmonsoon (Oct - Nov)

HUMLA_mean_temp_postmonsoon <- tempMean_1970_2023_monthly_mean %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(HUMLA_mean_temp_postmonsoon = mean(HUMLA))


# Mean temp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

HUMLA_mean_temp_winter <- tempMean_1970_2023_monthly_mean %>%
  group_by(year,month, HUMLA) %>%
  summarise(HUMLA_mean_temp_winter = mean(HUMLA)) %>% 
  filter(month %in% target)

# Remove 1970 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

HUMLA_mean_temp_winter <- HUMLA_mean_temp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(HUMLA_mean_temp_winter) - 1)%/%3

HUMLA_mean_temp_winter_final <- data.frame(
  range = aggregate(HUMLA_mean_temp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(HUMLA_mean_temp_winter$HUMLA_mean_temp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

HUMLA_mean_temp_winter_final <- HUMLA_mean_temp_winter_final[1:53, ]

HUMLA_mean_temp_winter_final <- HUMLA_mean_temp_winter_final %>% 
  select(sum) %>% 
  mutate(HUMLA_mean_temp_winter = sum/3,
         year = 1970:2022) %>% 
  select(HUMLA_mean_temp_winter, year) 

HUMLA_mean_temp_winter_final <- HUMLA_mean_temp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual Mean Temperature 

HUMLA_mean_temp_annual_slope_pvalue <- HUMLA_mean_temp_annual %>%
  summarise(HUMLA_mean_temp_annual_temp_slope = sens.slope(HUMLA_mean_temp_annual)$estimates,
            HUMLA_mean_temp_annual_temp_pvalue = mk.test(HUMLA_mean_temp_annual)$p.value)

# Pre-Monsoon Mean Temperature 

HUMLA_mean_temp_premonsoon_slope_pvalue <- HUMLA_mean_temp_premonsoon %>%
  summarise(HUMLA_mean_temp_premonsoon_temp_slope = sens.slope(HUMLA_mean_temp_premonsoon)$estimates,
            HUMLA_mean_temp_premonsoon_temp_pvalue = mk.test(HUMLA_mean_temp_premonsoon)$p.value)

# Monsoon Mean Temperature

HUMLA_mean_temp_monsoon_slope_pvalue <- HUMLA_mean_temp_monsoon %>%
  summarise(HUMLA_mean_temp_monsoon_temp_slope = sens.slope(HUMLA_mean_temp_monsoon)$estimates,
            HUMLA_mean_temp_monsoon_temp_pvalue = mk.test(HUMLA_mean_temp_monsoon)$p.value)

# Post-Monsoon Mean Temperature 

HUMLA_mean_temp_postmonsoon_slope_pvalue <- HUMLA_mean_temp_postmonsoon %>%
  summarise(HUMLA_mean_temp_postmonsoon_temp_slope = sens.slope(HUMLA_mean_temp_postmonsoon)$estimates,
            HUMLA_mean_temp_postmonsoon_temp_pvalue = mk.test(HUMLA_mean_temp_postmonsoon)$p.value)


# Winter Mean Temperature 

HUMLA_mean_temp_winter_slope_pvalue <- HUMLA_mean_temp_winter_final %>%
  summarise(HUMLA_mean_temp_post_temp_winter_slope = sens.slope(HUMLA_mean_temp_winter)$estimates,
            HUMLA_mean_temp_post_temp_winter_pvalue = mk.test(HUMLA_mean_temp_winter)$p.value)




HUMLA_mean_temp_slope_pvalue <- cbind(HUMLA_mean_temp_annual_slope_pvalue,
                                      HUMLA_mean_temp_premonsoon_slope_pvalue,
                                      HUMLA_mean_temp_monsoon_slope_pvalue,
                                      HUMLA_mean_temp_postmonsoon_slope_pvalue,
                                      HUMLA_mean_temp_winter_slope_pvalue)

HUMLA_mean_temp_slope_pvalue


# HUMLA TOTAL PRECIPITATION -------------------------------------------------

# Total precp

# total precp Annual

HUMLA_total_precp_annual <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, HUMLA) %>% 
  group_by(year) %>% 
  summarise(HUMLA_total_precp_annual = sum(HUMLA))

# total precp Premonsoon (March - May)

HUMLA_total_precp_premonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 3 & month <= 5) %>% 
  group_by(year) %>% 
  summarise(HUMLA_total_precp_premonsoon = sum(HUMLA))

# total precp monsoon (June - Sept)

HUMLA_total_precp_monsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 6 & month <= 9) %>% 
  group_by(year) %>% 
  summarise(HUMLA_total_precp_monsoon = sum(HUMLA))


# total precp postmonsoon (Oct - Nov)

HUMLA_total_precp_postmonsoon <- precTot_1970_2023_monthly_sum %>% 
  select(year, month, HUMLA) %>% 
  filter(month >= 10 & month <= 11) %>% 
  group_by(year) %>% 
  summarise(HUMLA_total_precp_postmonsoon = sum(HUMLA))


# total precp winter (Dec - Feb)
# Ref : https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

# select December, jan and feb only 

target <- c(12,1,2) 

HUMLA_total_precp_winter <- precTot_1970_2023_monthly_sum %>%
  group_by(year,month, HUMLA) %>%
  summarise(HUMLA_total_precp_winter = sum(HUMLA)) %>% 
  filter(month %in% target)

# Remove 1951 Jan and Feb 1 and 2 rows and chose from 3rd row only. 

HUMLA_total_precp_winter <- HUMLA_total_precp_winter[3:219,]

# https://stackoverflow.com/questions/59505473/sum-every-3-rows 
# Sum every 3 rows 1982 : 12 1983 : 1,2 and so on

grp = (1:nrow(HUMLA_total_precp_winter) - 1)%/%3

HUMLA_total_precp_winter_final <- data.frame(
  range = aggregate(HUMLA_total_precp_winter$month, list(grp),
                    function(x) paste(range(x), collapse=" -- "))$x,
  sum = aggregate(HUMLA_total_precp_winter$HUMLA_total_precp_winter, list(grp), sum)$x,
  stringsAsFactors = FALSE)

HUMLA_total_precp_winter_final <- HUMLA_total_precp_winter_final[1:52, ]

HUMLA_total_precp_winter_final <- HUMLA_total_precp_winter_final %>% 
  select(sum) %>% 
  mutate(year = 1970:2021) %>% 
  rename(HUMLA_total_precp_winter = sum) %>% 
  select(HUMLA_total_precp_winter, year) 

HUMLA_total_precp_winter_final <- HUMLA_total_precp_winter_final %>% 
  select(year, everything())



# # Calculate slope an p value  -------------------------------------------

# Annual total precperature 

HUMLA_total_precp_annual_slope_pvalue <- HUMLA_total_precp_annual %>%
  summarise(HUMLA_total_precp_annual_precp_slope = sens.slope(HUMLA_total_precp_annual)$estimates,
            HUMLA_total_precp_annual_precp_pvalue = mk.test(HUMLA_total_precp_annual)$p.value)

# Pre-Monsoon total precperature 

HUMLA_total_precp_premonsoon_slope_pvalue <- HUMLA_total_precp_premonsoon %>%
  summarise(HUMLA_total_precp_premonsoon_precp_slope = sens.slope(HUMLA_total_precp_premonsoon)$estimates,
            HUMLA_total_precp_premonsoon_precp_pvalue = mk.test(HUMLA_total_precp_premonsoon)$p.value)

# Monsoon total precperature

HUMLA_total_precp_monsoon_slope_pvalue <- HUMLA_total_precp_monsoon %>%
  summarise(HUMLA_total_precp_monsoon_precp_slope = sens.slope(HUMLA_total_precp_monsoon)$estimates,
            HUMLA_total_precp_monsoon_precp_pvalue = mk.test(HUMLA_total_precp_monsoon)$p.value)

# Post-Monsoon total precperature 

HUMLA_total_precp_postmonsoon_slope_pvalue <- HUMLA_total_precp_postmonsoon %>%
  summarise(HUMLA_total_precp_postmonsoon_precp_slope = sens.slope(HUMLA_total_precp_postmonsoon)$estimates,
            HUMLA_total_precp_postmonsoon_precp_pvalue = mk.test(HUMLA_total_precp_postmonsoon)$p.value)


# Winter total precperature 

HUMLA_total_precp_winter_slope_pvalue <- HUMLA_total_precp_winter_final %>%
  summarise(HUMLA_total_precp_winter_precp_slope = sens.slope(HUMLA_total_precp_winter)$estimates,
            HUMLA_total_precp_winter_precp_pvalue = mk.test(HUMLA_total_precp_winter)$p.value)



HUMLA_total_precp_slope_pvalue <- cbind(HUMLA_total_precp_annual_slope_pvalue,
                                        HUMLA_total_precp_premonsoon_slope_pvalue,
                                        HUMLA_total_precp_monsoon_slope_pvalue,
                                        HUMLA_total_precp_postmonsoon_slope_pvalue,
                                        HUMLA_total_precp_winter_slope_pvalue)


HUMLA_total_precp_slope_pvalue


