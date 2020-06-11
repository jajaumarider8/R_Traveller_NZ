### Script Info ###################################################################################
# Analysis - Traveller Arrivals in NZ
# Author: Shota Shirai 
# Date created: 10 Feb 2020
# Date last update: 11 Jun 2020
# Input: 
# Output:
#
# Source data: StatsNZ https://www.stats.govt.nz/information-releases/international-travel-november-2019
#
# This script analyses the number of overseas and NZ passenger arrivals over time.
###################################################################################################

# * Note #### 
# 1. This code follows "Projected-oriented workflow" described on https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
#    Please store the data file in the directory named "data" under project directory.
# 2. This code uses a package "pacman" (require R - v3.50 or higher) to load other packages. 
#    If pacman is not available, please go to "Load Library" section and uncomment 'library()' lines.

# * Parameter Settings ####

# * Load Library   ################################################################################
if (!require("pacman")) install.packages("pacman", type = "binary")
library(pacman)
pacman::p_load(readr, tidyr, plotly, ggplot2, dplyr, zoo, DT, data.table, here, maps)

# ******************* if pacman is not avilable plese use the codes below *************************

# requiredPackages = c("readr", "plotly", "ggplot2", "dplyr", "zoo", "DT", "data.table","here")
# for(p in requiredPackages){
#   if(!require(p,character.only = TRUE)) install.packages(p, dependencies =TRUE)
#   library(p,character.only = TRUE)
#   message(!require(p,character.only = TRUE))
# }
# 
# library(readr)
# library(tidyr)
# library(plotly)
# library(ggplot2)
# library(dplyr)
# library(zoo)
# library(DT)
# library(data.table)
# library(here)

# * Load Data  ####################################################################################
# Data used in this analysis is loaded from four different .csv files
monthly_overseas_visitor_arrivals <- read_csv(here("data", "monthly-overseas-visitor-arrivals,-november-2009–19.csv"))
monthly_resident_traveller_arrivals <- read_csv(here("data","monthly-new-zealand-resident-traveller-arrivals,-november-2009–19.csv"))
overseas_arrivals_by_country <- read_csv(here("data","overseas_visitors_by_country.csv"))
coordinates_info <- read_csv(here("data","countries.csv"))

# * Analysis - Trend ############################################################################## 

# ** Overseas Visitors ####
# pre-processing - define date format / change column name
monthly_overseas_visitor_arrivals$year <- format(monthly_overseas_visitor_arrivals$DateTime, "%Y")
monthly_overseas_visitor_arrivals$month <- format(monthly_overseas_visitor_arrivals$DateTime, "%m")
colnames(monthly_overseas_visitor_arrivals)[2] <- "Seasonally_adjusted"

# *** Annual Trend - overseas visitors ############################################################
summary_overseas_visitor_annual <- monthly_overseas_visitor_arrivals %>%
  group_by(year) %>%
  dplyr::summarise(max_value = max(Actual) # maximum number of the visitors 
                   , min_value = min(Actual) # minimum number of the visitors 
                   , trend = mean(Seasonally_adjusted) # mean value of seasonally adjusted data 
  )

diff_trend_overseas = c(NA, diff(summary_overseas_visitor_annual$trend)) # change from the last year
diff_trend_overseas[diff_trend_overseas < 0 ] <- NA # only pick up increase of the number of visitor

summary_overseas_visitor_annual <- cbind(summary_overseas_visitor_annual, diff_trend_overseas)

# *** Monthly Trend - overseas visitors ###########################################################
summary_overseas_visitor_monthly <- monthly_overseas_visitor_arrivals %>%
  mutate(yaer = format(DateTime,"%Y")) %>% # add year
  mutate(month = format(DateTime,"%m")) %>% # add month
  mutate(month = as.numeric(month)) %>% # change the format of month
  filter(year >= 2010 & year <= 2018) %>% # obtain data between 2010 and 2018
  group_by(year) %>%
  mutate(p = Actual / sum(Actual) *100) %>% # Calculate the percentage of the visitors by month
  ungroup()

# Calculate monthly averaged overseas visitors over 10 years 
average_overseas_visitor_monthly <- summary_overseas_visitor_monthly %>%
  group_by(month) %>%
  dplyr::summarise(p_ave = mean(p),
            year = 'Average, 2010 - 2018'
            )

# output
summary_overseas_visitor_monthly <- bind_rows(summary_overseas_visitor_monthly, average_overseas_visitor_monthly)

# ** NZ Resident Travellers  ######################################################################
# pre-processing - define date format / change column name
monthly_resident_traveller_arrivals$year <- format(monthly_resident_traveller_arrivals$DateTime, "%Y")
monthly_resident_traveller_arrivals$month <- format(monthly_resident_traveller_arrivals$DateTime, "%m")
colnames(monthly_resident_traveller_arrivals)[2] <- "Seasonally_adjusted"

# *** Annual Trend - NZ Resident Travellers #######################################################

summary_NZres_traveller_annual <- monthly_resident_traveller_arrivals %>%
  group_by(year) %>%
  dplyr::summarise(max_value = max(Actual) # maximum number of the visitors 
                   , min_value = min(Actual) # minimum number of the visitors 
                   , trend = mean(Seasonally_adjusted) # mean value of seasonally adjusted data 
  )

# obtain change of the visitors from the last year
diff_trend_NZres = c(NA, diff(summary_NZres_traveller_annual$trend))
diff_trend_NZres[diff_trend_NZres < 0 ] <- NA

# output
summary_NZres_traveller_annual <- cbind(summary_NZres_traveller_annual, diff_trend_NZres)

# *** Monthly Trend - NZ Resident Traveller #######################################################
summary_NZres_traveller_monthly <- monthly_resident_traveller_arrivals %>%
  mutate(yaer = format(DateTime,"%Y")) %>% # add year
  mutate(month = format(DateTime,"%m")) %>% # add month
  mutate(month = as.numeric(month)) %>%  # change the format of month
  filter(year >= 2010 & year <= 2018) %>% # obtain data between 2010 and 2018
  group_by(year) %>%
  mutate(p = Actual / sum(Actual) *100) %>% # Calculate the percentage of the NZ resident by month
  ungroup()

# Calculate monthly averaged NZ resident traveller over 10 years 
average_NZres_traveller_monthly <- summary_NZres_traveller_monthly %>%
  group_by(month) %>%
  summarise(p_ave = mean(p),
            year = "Average, 2010 - 2018") 

# output
summary_NZres_traveller_monthly <- bind_rows(summary_NZres_traveller_monthly, average_NZres_traveller_monthly)

# * Analysis - The number of overseas traveller by country ########################################

# pre-processing - define date format / change column name
colnames(overseas_arrivals_by_country)[1] <- "country"
overseas_arrivals_by_country <- overseas_arrivals_by_country[
  !(overseas_arrivals_by_country$country %in% c("Oceania", "Asia", "Europe" , "Americas" 
                                             , "Africa and the Middle East", "Not stated", "Total(2)"
                                              )
  ) 
  & !is.na(overseas_arrivals_by_country$country), ]

overseas_arrivals_by_country$long <- NA
overseas_arrivals_by_country$lat <- NA

# Define the coordinates
overseas_arrivals_by_country$country[overseas_arrivals_by_country$country == "China, People's Republic of"] <- "China"
overseas_arrivals_by_country$country[overseas_arrivals_by_country$country == "Hong Kong (SAR)"] <- "Hong Kong"
overseas_arrivals_by_country$country[overseas_arrivals_by_country$country == "Korea, Republic of"] <- "South Korea"
overseas_arrivals_by_country$country[overseas_arrivals_by_country$country == "Viet Nam"] <- "Vietnam"
overseas_arrivals_by_country$country[overseas_arrivals_by_country$country == "United States of America"] <- "United States"

for (i in overseas_arrivals_by_country$country) {
  overseas_arrivals_by_country$long[overseas_arrivals_by_country$country == i] <- coordinates_info$longitude[coordinates_info$name == i]
  overseas_arrivals_by_country$lat[overseas_arrivals_by_country$country == i] <- coordinates_info$latitude[coordinates_info$name == i]
}
overseas_arrivals_by_country$change_val <- overseas_arrivals_by_country$YearEnd_November_2019 - overseas_arrivals_by_country$YearEnd_November_2018

## check increse or decrease
overseas_arrivals_by_country$increase <- ifelse(overseas_arrivals_by_country$change_val > 0, TRUE, FALSE)
overseas_arrivals_by_country$change <- ifelse(overseas_arrivals_by_country$change_val > 0, "Increase", "Decrease")

# * Plots #########################################################################################
# pre-define axis (y2-axis: right-y axis)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Increase from the previous year",
  showline = FALSE,
  showgrid = FALSE
)
# ** Annual Trend - Overseas visitors #############################################################
p_annual_overseas <- plot_ly(summary_overseas_visitor_annual, x = ~year, y = ~trend
                            , type = "scatter", mode = "line"
                            , name = "Annual average of seaonally adjusted arrivals"
                            , line = list(color = "blue")) %>%
  add_trace(y = ~diff_trend_overseas, name = "Increase from the previous year"
            , type = "scatter", mode = "lines+markers"
            , line = list(color = "red") , marker = list(color = "red")
            , yaxis = "y2") %>%
  layout(title = ""
         , xaxis = list(title = "Year")
         , yaxis = list(title = "Number of arrivals (trend)", tickfont = list(color = "blue"))
         , yaxis2 = ay
         , legend = list(x = 0, y = 0.9, direction = 'h')
         , margin = list(l = 50, r = 50, b= 50, t = 150, pad = 4) 
  )

# show a figure
p_annual_overseas

# ** Monthly Trend - Overseas visitors ############################################################
p_monthly_overseas <- plot_ly(summary_overseas_visitor_monthly, x = ~month, y = ~p
                              , type = 'scatter', mode = "line"
                              , color = ~year, colors = "grey85"
) %>%
  add_trace(x = ~month, y = ~p_ave, line = list(width = 2, color = "red")) %>%
  layout(showlegend = FALSE
         , xaxis = list(title = "Month", dtick = 1, tick0 = 1,  tickmode = "linear")
         , yaxis = list(title = "Persentage of passengers (%)")
  )

p_monthly_overseas

# ** Annual Trend - NZ residence travellers #######################################################

p_annual_NZres <- plot_ly(summary_NZres_traveller_annual, x = ~year, y = ~trend
                         , type = "scatter", mode = "line"
                         , name = "Annual average of seaonally adjusted arrivals"
                         , line = list(color = "blue")
                         ) %>%
  add_trace(y = ~diff_trend_NZres, name = "Increase from the previous year"
            , type = "scatter", mode = "lines+markers"
            , line = list(color = "red") , marker = list(color = "red")
            , yaxis = "y2"
            ) %>%
  layout(title = ""
         , xaxis = list(title = "Year")
         , yaxis = list(title = "Number of arrivals (trend)", tickfont = list(color = "blue"))
         , yaxis2 = ay
         , legend = list(x = 0, y = 0.9, direction = 'h')
         , margin = list(l = 50, r = 50, b= 50, t = 150, pad = 4) 
  )
# show a figure
p_annual_NZres

# ** Monthly Trend - NZ residence travellers ######################################################
p_monthly_NZres <- plot_ly(summary_NZres_traveller_monthly, x = ~month, y = ~p
                           , type = 'scatter', mode = "line"
                           , color = ~year, colors = "grey85"
) %>%
  add_trace(x = ~month, y = ~p_ave, line = list(width = 2, color = "red")) %>%
  layout(showlegend = FALSE
         , xaxis = list(title = "Month", dtick = 1, tick0 = 1,  tickmode = "linear")
         , yaxis = list(title = "Persentage of passengers (%)")
  )

# show a figure
p_monthly_NZres

# ** World map - The number of overseas arrivals by country #######################################

## Create map - pacific centered map
shift_value1 <- -16.5
shift_value2 <- 343.5

map_world_df <- map_data('world', wrap=c(shift_value1, shift_value2)) %>%
  dplyr::filter(region != "Antarctica")

## shift coordinates in "overseas_arrivals_by_country"
overseas_arrivals_by_country$long[overseas_arrivals_by_country$long < shift_value1] <- overseas_arrivals_by_country$long[overseas_arrivals_by_country$long < shift_value1] + 360
overseas_arrivals_by_country$long[overseas_arrivals_by_country$long > shift_value2] <- overseas_arrivals_by_country$long[overseas_arrivals_by_country$long > shift_value2] - 360

## country shapes
country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group)
                                ,colour = "grey90", fill = "#FFFFFF"
                                ,size = 0.15
)

# ## Check increase/decrease of overseas travellers by country
# overseas_arrivals_increase <- overseas_arrivals_by_country[overseas_arrivals_by_country$increase == T,]
# overseas_arrivals_decrease <- overseas_arrivals_by_country[overseas_arrivals_by_country$increase != T,]

## plot the number of overseas travellers by country on the world map
map <- ggplot() + 
  country_shapes +
  geom_point(aes(x = long, y = lat, size = YearEnd_November_2019, colour = factor(change))
             , data = overseas_arrivals_by_country
             , alpha = .5) +
  scale_color_manual(values=c("red", 'blue')) +
  scale_size_continuous(range = c(1, 20) 
                        ,breaks = c(10000, 50000, 100000, 500000, 1000000)
                        ,labels = c("10k", "50k", "100k", "500k", "1 million")) +
  # labs(colours = 'Change from 2018', size = 'Arraivals') + 
  labs(x = element_blank(),
       y = element_blank(),
       colour = 'Change from 2018', size = 'Arraivals',
       caption = "Source of the original data: Stats NZ (https://www.stats.govt.nz/information-releases/international-travel-november-2019)") + 
  guides(colour = guide_legend(order = 1),
         size = guide_legend(order = 2)) +
  theme(
     axis.text.x = element_blank()
    ,axis.text.y = element_blank()
    ,axis.ticks = element_blank()
  ) 
map



