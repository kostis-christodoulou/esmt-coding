# Make sure we have the relevant packages loaded

library(tidyverse)
library(mosaic)
library(here)
library(lubridate)
library(skimr)
library(ggpubr) # to display regression equation

# The data come from https://www.kaggle.com/datasets/timmate/avocado-prices-2020 


# summary of weekly Hass avocado sales for the contiguous US. https://hassavocadoboard.com/ 
# The underlying data are from The Hass Avocado Board (free registration required). 
# Hass Avocados are the most popular variety of avocados sold in the US and the 
# Haas Avocado Board (HAB) provides crucial data on them to growers and marketers.
# 
# The HAB makes this information available to anyone who may be interested 
# (free registration required). An important note to remember is that when they use the 
# term ‘units’, it often means the weight in US pounds. The HAB does not provide 
# (at least publicly) actual piece-count sales to retailers.

# PLU codes
# 4046: small/medium Hass Avocados (~3-5 oz)
# 4225: large Hass Avocados (~8-10 oz)
# 4770: extra large Hass Avocados (~10-15 oz)

#read the CSV file
avocado <- read_csv(here::here("data", "avocado-prices.csv")) |> 
  # Remove "Total U.S."
  filter(geography != "Total U.S.") 

# examine what the data frame looks like
skim(avocado)

# Task 1 ------------------------------------------------------------------


# Get the total volume of avocados sold on each `date` and in each `geography`
# this is the weekly sales volume of avocados for the week beginning in that date
avocado |> 
  group_by(???? ) |> 
  summarise(????) |> 
  arrange(????)


# Task 2 ------------------------------------------------------------------


# Get the total volume sold on each `date`, grouped by `type`, conventional or organic
# For each date, add a new variable 'percent' to calculate the % split between conventional and organic
avocado |> 
  group_by(????) |> 
  summarise(?????) |> 
  mutate(????)



  

# Task 3- Histogram of prices ---------------------------------------------

# Price of conventional vs. organic avocados
# In this exercise, you'll create  histograms to compare the prices of conventional and organic avocados.
avocado |> 
  ggplot()+
  aes(?????)+
  geom_histogram()+
  theme_bw()+
  facet_wrap(~type, ncol=1)+
  theme(legend.position = "none")
  


# Task 4- Table of price vs volume ------------------------------------
# 

avocado |>
  # Group the data by 'date' and 'type' of avocado
  group_by(date, type) |>
  
  # Summarize the grouped data to calculate total volume and mean price
  summarise(total_volume = sum(total_volume),  # Total volume of avocados sold
            mean_price = mean(average_price))


# Task 5- scatter plot price vs volume ------------------------------------

  
# Scatter plot of avg_price vs. total_volume - 
# for the different `type` conventional vs organic
# Is there price elasticity of demand?
# Get the total volume of avocados and average_price for each `date` and `type`

# Load the avocado dataset and start the data manipulation pipeline
avocado |>
  # Group the data by 'date' and 'type' of avocado
  group_by(date, type) |>
  
  # Summarize the grouped data to calculate total volume and mean price
  summarise(total_volume = sum(total_volume),  # Total volume of avocados sold
            mean_price = mean(average_price)) |>  # Average price of avocados sold
  
  # Initialize a ggplot object for visualization
  ggplot() +
  
  # Set aesthetic mappings for the plot
  # Map total_volume to x-axis, mean_price to y-axis, and colour by type
  aes(x = ???, 
      y =  ???, 
      colour = ???) +  
  
  # Add points to the plot with some transparency
  geom_point(alpha = 0.5) +
  
  # Customize x-axis to show numbers and y-axis to show dollar amounts
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::dollar) +
  
  # Add a linear model trend line without confidence intervals
  geom_smooth(method = "lm", se = FALSE) +
  
  # Create separate panels for each type of avocado, allowing for independent scaling
  # what happens if you remove scales="free"
  facet_wrap(~type, scales = "free") +
  
  # Apply a clean black-and-white theme to the plot
  theme_bw() +
  
  # Remove the legend from the plot for clarity
  theme(legend.position = "none") +
  
  # End of the pipeline
  NULL

# Task 6- Which geography generates most sales ---------------------------

  
# Which geography creates most sales  
# Does it matter how you group_by()
  

avocado |> 
  
  # Group the data by 'geography' and 'type' of avocado
  group_by(geography, type) |> 
  
  # Summarize the grouped data to calculate the total volume of avocados sold
  summarise(total_volume = sum(total_volume)) |> 
  
  # Calculate the percentage of total volume for each group relative to the overall total
  mutate(percent = total_volume / sum(total_volume)) |> 
  
  # Arrange the results in descending order based on total volume
  arrange(desc(total_volume))

# what does the 0.963 for West and 0.98 for South Central mean?
  
avocado |> 
  group_by(type, geography) |> 
  summarise(total_volume = sum(total_volume)) |> 
  mutate(percent = total_volume / sum(total_volume)) |> 
  arrange(desc(total_volume)) |> 
  
  # calc cmulative percent of sales
  mutate(cumulative_percent = cumsum(percent))  

# what does the 0.108 for West and 0.106 for South Central mean?  



# Task 7 - Cumulative change of price by type ----------------------------


#cumulative change of price for each avocado type
avocado |>
  group_by( type, date) |>
  summarise(mean_price = mean(average_price)) |> 
  mutate(cum_change = mean_price/first(mean_price)) |> 
  ggplot()+
  aes(x=date, y = cum_change, colour=type)+
  geom_line()+
  theme_bw() + 
  
  # Create separate panels for each type of avocado, allowing for independent scaling
  facet_wrap(~type) + 
  scale_y_continuous(labels=scales::percent)+
  NULL


# Challenge 1 - Value ($) in top geographies over time ---------------------------

  
# Find geographies  sales in terms of volume. Save results in a new dataframe
# called top_geographies



# Plot evolution of total revenue, in $ by type of avocado for top 10 geographies
top_geographies |> 
  slice_max(order_by = perc, n = 10) |>     
  
  

# Challenge 2 - Price elasticity of demand ---------------------------------------------------------


# Price elasticity of demand  
# Load the avocado dataset and start the data manipulation pipeline
