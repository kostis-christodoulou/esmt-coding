library(httr)
library(janitor)
library(tidyverse) # the usual suspects
library(treemapify) # to make waffle chart
library(readxl) # to read excel files
library(gganimate) # for animations
library(gifksi)
library(see)


# Look at historical total CO2 emissions
# source of data = https://globalcarbonbudgetdata.org/latest-data.html from the Global Carbon Budget
# rstats cannot directly read an excel file off the web, so we get it locally on a temp_file

url1 <- "https://globalcarbonbudgetdata.org/downloads/latest-data/Global_Carbon_Budget_2023v1.1.xlsx"
GET(url1, write_disk(temp_excel_file <- tempfile(fileext = ".xlsx")))

#    Read the "Fossil Emissions by Category" sheet into a data frame, skipping the first 8 rows
global_emissions <- read_excel(temp_excel_file,
                               sheet = "Fossil Emissions by Category",
                               skip = 8) |> 
  janitor::clean_names()

# All values in million tonnes of carbon per year (MtC/yr), except the per capita emissions which are in 
# tonnes of carbon per person per year (tC/person/yr). 
# For values in million tonnes of CO2 per year, multiply the values below by 3.664

global_emissions_long <- global_emissions %>%
  select(1, 3:8) %>% # columnns to select and reshape into long format
  pivot_longer(cols = 2:7,
               values_to = "carbon_emissions",
               names_to = "source") |> 
  
  # For values in million tonnes of CO2 per year, multiply the values below by 3.664
  mutate(co2_emissions = 3.664* carbon_emissions )

# arrange commodities by total emissions, and not the default alphabetic
levels <- global_emissions_long |> 
  dplyr::group_by(source) |> 
  dplyr::summarise(
    total = sum(co2_emissions, na.rm=TRUE)
  ) |> 
  arrange(total) |> 
  pull(source)

# Reorder the 'source' factor levels based on total emissions
global_emissions_long <- global_emissions_long |> 
  mutate(source = factor(source, levels = levels)) 

# Define a colour palette for the plots
my_colours <- c('#b2182b','#ef8a62','#c7eae5','#e0e0e0','#d8b365','#8c510a')



# time series plot of global emissions ------------------------------------


global_emissions_long |> 
  
  ggplot()+
  aes(x=year, y= co2_emissions, fill = source)+
  
  # Stacked bar chart
  geom_bar(stat="identity", 
           
           #so all bars add up to 100% for every year
           position = "stack",
           
           # set width = 1 to remove all space between bars
           width = 1)+
  
  scale_x_continuous(expand = c(0, 0),
                     breaks=seq(1850,2050,20)) + 
  scale_y_continuous(expand = c(0, 0),
                     
                     # format y-axis as %
                     labels = scales::number)+
  scale_fill_manual(values = my_colours)+
  
  theme_bw()+
  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        # Ensure the title is top-left aligned
        plot.title.position = "plot") + 
  
  labs(title = "Global CO2 emissions",
       subtitle = "million tonnes of CO2 per year (MtC/yr)",
       x=NULL, y = NULL,
       fill = "Source",
       colour = NULL,
       caption= "Source: https://globalcarbonbudgetdata.org/index.html")+
  NULL

# Create a percentage plot of global emissions by source-------------------

global_emissions_long |> 
  
  ggplot()+
  aes(x=year, y= co2_emissions, fill = source)+
  geom_bar(stat="identity", 
           
           #so all bars add up to 100% for every year
           position = "fill",
           
           # set width = 1 to remove all space between bars
           width = 1)+
  
  scale_x_continuous(expand = c(0, 0),
                     breaks=seq(1850,2050,20)) + 
  scale_y_continuous(expand = c(0, 0),
                     
                     # format y-axis as %
                     labels = scales::percent)+
  scale_fill_manual(values = my_colours)+
  
  theme_bw()+
  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        # Ensure the title is top-left aligned
        plot.title.position = "plot") + 
  
  labs(title = "Global CO2 emissions",
       subtitle = "million tonnes of CO2 per year (MtC/yr)",
       x=NULL, y = NULL,
       fill = "Source",
       colour = NULL,
       caption= "Source: https://globalcarbonbudgetdata.org/index.html")+
  NULL





# Animation ---------------------------------------------------------------


my_colours <- c('#b2182b','#ef8a62','#c7eae5','#e0e0e0','#d8b365','#8c510a')

animation <- global_emissions_long |> 
  mutate(year = as.integer(year)) |> 
  
  ggplot()+
  aes(x=year, y= co2_emissions, fill = source)+
  geom_bar(stat="identity", 
           
           #so all bars add up to 100% for every year
           position = "stack",
           
           # set width = 1 to remove all space between bars
           width = 1)+
  
  scale_x_continuous(expand = c(0, 0),
                     breaks=seq(1850,2050,20)) + 
  scale_y_continuous(expand = c(0, 0),
                     
                     # format y-axis as %
                     labels = scales::number)+
  scale_fill_manual(values = my_colours)+
  
  theme_bw()+
  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        # Ensure the title is top-left aligned
        plot.title.position = "plot") + 
  
  labs(title = "{frame_time} Global CO2 emissions",
       subtitle = "million tonnes of CO2 per year (MtC02)",
       x=NULL, y = NULL,
       fill = "Source",
       colour = NULL,
       caption= "Source: https://globalcarbonbudgetdata.org/index.html")+
  
  transition_time(year) +
  ease_aes('sine-in-out')+
  shadow_mark()+ # keeps past data displayed
  NULL


animate(animation, height=600, width=900, renderer=gifski_renderer())


#  Carbon Majors  --------



# We're exploring historical emissions data from Carbon Majors https://carbonmajors.org/  
# They have complied a database of emissions data going back to 1854. 
# 
# Carbon Majors is a database of historical production data from 122 of the world’s largest oil, gas, coal, 
# and cement producers. This data is used to quantify the direct operational emissions and emissions from the 
# combustion of marketed products that can be attributed to these entities. These entities include:
# 
# 75 Investor-owned Companies, 36 State-owned Companies, 11 Nation States, 82 Oil Producing Entities, 
# 81 Gas Entities, 49 Coal Entities, 6 Cement Entities
# 
# The data spans back to 1854 and contains over 1.42 trillion tonnes of CO2e covering 72% of 
# global fossil fuel and cement emissions since the start of the Industrial Revolution in 1751.
# 
# They share data with low, medium and high levels of granularity.  This dataset is the 'medium' granularity 
# that contains year, entity, entity type, commodity, commodity production, commodity unit, and total emissions.
# 
# Are there any trends or changes that surprised you?

# Read data off Carbon Majors website
emissions <- readr::read_csv('https://carbonmajors.org/evoke/391/get_cm_file?type=Basic&file=emissions_medium_granularity.csv')



emissions <- emissions |> 
  
  # write a regex that turns everything from " *** Coal", to "Coal"
  mutate(new_commodity = str_replace_all(commodity, ".*(Coal)$",  "Coal"))

# arrange commodities by total emissions, and not the default aphabetic
levels <- emissions |> 
  group_by(new_commodity) |> 
  summarise(
    total = sum(total_emissions_MtCO2e)
  ) |> 
  arrange(total) |> 
  pull(new_commodity)

# We only have 4 categories now
my_colours <- c('#c7eae5','#e0e0e0','#d8b365','#8c510a')


# time series plot
emissions |> 
  mutate(new_commodity = factor(new_commodity, levels = levels)) |> 
  group_by(year, new_commodity) |> 
  summarise(
    total = sum(total_emissions_MtCO2e)
  ) |> 
  mutate(percent = total/sum(total)) |> 
  
  ggplot()+
  aes(x=year, y= total, fill = new_commodity)+
  
  scale_fill_manual(values = my_colours)+
  
  
  geom_bar(stat="identity", 
           
           #so all bars add up to 100% for every year
           position = "fill",
           
           # set width = 1 to remove all space between bars
           width = 1)+
  
  # make sure graphs starts at (0,0)
  scale_x_continuous(expand = c(0, 0),
                     breaks=seq(1860,2050,20)) + 
  scale_y_continuous(expand = c(0, 0),
                     
                     # fomat y-axis as %
                     labels = scales::percent)+
  theme_bw()+
  
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  
  labs(title = "CO2 emissions from 122 of the world’s largest coal, oil, gas, and cement producers",
       x=NULL, y = NULL,
       fill = "Commodity",
       colour = NULL,
       caption= "Source: https://carbonmajors.org/")
NULL

# Waffle chart

emissions |> 
  group_by(parent_entity, new_commodity ) |> 
  summarise(
    total = sum(total_emissions_MtCO2e)
  ) |> 
  ungroup() |> 
  mutate(percent = total/sum(total)) |> 
  
  
  # arrange commodities by total  
  mutate(new_commodity = factor(new_commodity, levels = levels)) |> 
  
  ggplot()+
  aes(area = total, 
      fill=new_commodity, 
      
      label = paste(parent_entity, scales::percent(percent, accuracy = 0.1), sep = "\n")) +
  scale_fill_manual(values = my_colours)+
  geom_treemap() +
  geom_treemap_text(colour = "grey54",
                    place = "centre",
                    size = 10)  +
  labs(title = "CO2 emissions (1850-today) from 122 of the world’s largest coal, oil, gas, and cement producers",
       x=NULL, y = NULL,
       fill = "Commodity",
       colour = NULL,
       caption= "Source: https://carbonmajors.org/")


