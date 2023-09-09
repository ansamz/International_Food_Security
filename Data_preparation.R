library(tidyverse)   #for dplyr
library(magrittr)   #for %>%
library(ggplot2)  #for graphs
library(countrycode) #for addding continents
library(tidyr)
library(gridExtra)
library(rnaturalearth)
library(sf)
library(osmdata)
library(maptiles) ## for get_tiles() / used instead of OpenStreetMap
library(tidyterra)

#TODO check preparation file

### load data sets
# 1) dataset for temperature changes in countries 1961 - 2003 in C°
    # source: https://www.kaggle.com/datasets/sevgisarac/temperature-change
temperature_raw <- read.csv("temperatures.csv", 
                        stringsAsFactors=FALSE, 
                        fileEncoding="latin1")
# 2) dataset for wheat production in countries from 1996 - 2020
    # https://www.kaggle.com/datasets/rajkumarpandey02/wheat-production-statistics
#wheat_raw<- read.csv("wheat.csv")
# 3) dataset for food security in 1980 - 2013
    # https://data.nal.usda.gov/dataset/international-food-security/resource/45fda032-a799-480b-870b-e085e2378f1a#{}
food_raw <- read.csv("food_security.csv")

#################################################################################################################
################################## data preparation #############################################################
##################################################################################################################

######################### preparation of datatset temperature ###################################################
# clean column names (years) with Regex and check changes
colnames_temp_clean <- sub("^Y", "", colnames(temperature_raw))
temperature_clean <- data.frame(temperature_raw)
colnames(temperature_clean) <- colnames_temp_clean
colnames(temperature_clean)

# put all the years in one column (wide to long)
temperature_long <- 
  pivot_longer(temperature_clean,
               cols = -c(Area.Code, Area, Months.Code, Months, Element.Code, Element, Unit), 
               names_to = "Year", 
               values_to = "Temperature")
head(temperature_long)
colnames(temperature_long)
str(temperature_long)

# cast "Year" from chr to int
# temperature_long$Year <- as.integer(temperature_long$Year)

# filter: 1980 - 2013 (preparation for merge with food)
temperature_long_2013 <- temperature_long %>%
  filter(Year >= 1980 & Year <= 2013)
head(temperature_long_2013)

#take only the annual data for temperature (preparation for merging)
temperature_year_only <- temperature_long_2013 %>% 
  filter(Months == "Meteorological year")

head(temperature_year_only)

##################################### merging  #################################
dim(food_raw)
dim(temperature_long_2013)

# merge the two dataframes
merged_1 <- merge(food_raw, temperature_year_only, 
                     by.x = c("Country", "Year"), 
                     by.y = c("Area","Year"))

# filter for the temperatur change (cut of standard deviation)
merged_2 <- merged_1 %>% 
  filter(Element == "Temperature change")

# drop columns:  "Months.Code"  "Months"  "Element.Code" "Element" "Unit.y"
merged_3 <- subset(merged_2,select = -c(Months.Code, Months, Element.Code, 
                                              Element, Unit.y, Area.Code))
unique(merged_3$Commodity)
unique(merged_3$Item)

# Values to match
values_to_match <- c("Area Harvested", "Production Quantity", 
                     "Food Supply", "Import Quantity", "Export Quantity", 
                     "Total Population - Both Sexes", 
                     "WB GDP (constant 2005 US$) / FAO population",
                     "Gross Domestic Product, constant prices",
                     "Food Availability per capita"
                     )

# Select subset of dataframe
merged_3 <- merged_3[merged_3$Item %in% values_to_match, ]


# combine three columns as preperation for long format
combined.Commodity <- paste(merged_3$Commodity, 
                            merged_3$Item, 
                            merged_3$Unit.x, sep = ", ")

merged_4 <- cbind(merged_3, combined.Commodity)
merged_4 <- subset(merged_4,select = -c(Commodity, Item, Unit.x))

merged_4_wide <- pivot_wider(merged_4, 
                            names_from = "combined.Commodity", 
                            values_from = "Amount")
colnames(merged_4_wide)

# add import and export Grains and Cereals to Root crops to have them in one column
merged_4_wide$Total.Grains.Cereals.Root.Export.Quantity.1000.MT = 
  merged_4_wide$`Root Crops (R&T), Export Quantity, Grain Equiv. 1000 MT` +
  merged_4_wide$`Total Grains/Cereals, Export Quantity, 1000 MT`

merged_4_wide$Total.Grains.Cereals.Root.Import.Quantity.1000.MT = 
  merged_4_wide$`Total Grains/Cereals, Import Quantity, 1000 MT` +
  merged_4_wide$`Root Crops (R&T), Import Quantity, Grain Equiv. 1000 MT`

merged_4_wide <- subset(merged_4_wide,select = -c(
  `Total Grains/Cereals, Production Quantity, 1000 MT`,
  `Root Crops (R&T), Production Quantity, Grain Equiv. 1000 MT`,
  `Total Grains/Cereals, Food Availability per capita, kg/cap/yr`,
  `Root Crops (R&T), Food Availability per capita, kg/cap/yr`,
  `Total Grains/Cereals, Area Harvested, 1000 Ha`,                         
  `Root Crops (R&T), Area Harvested, 1000 Ha`,
  `Root Crops (R&T), Export Quantity, Grain Equiv. 1000 MT`,                                     
  `Total Grains/Cereals, Export Quantity, 1000 MT`,
  `Total Grains/Cereals, Import Quantity, 1000 MT`,                             
  `Root Crops (R&T), Import Quantity, Grain Equiv. 1000 MT`
))

colnames(merged_4_wide)

merged_4_wide <- merged_4_wide %>% 
  rename(
    "Total.Grains.Cereals.Root.Production.Quantity.1000.MT" = 
      "Total Grains/Cereals and Root Crops (R&T), Production Quantity, Grain Equiv. 1000 MT",
    "Total.Grains.Cereals.Root.Food.Supply.1000.MT" = 
      "Total Grains/Cereals and Root Crops (R&T), Food Supply, Grain Equiv. 1000 MT",
    "Total.Grains.Cereals.Root.Area.Harvested.1000.Ha" = 
      "Total Grains/Cereals and Root Crops (R&T), Area Harvested, 1000 Ha",
    "Population.Million" = "Population, Total Population - Both Sexes, Million",
    "GDP.US.dollars.per.person" = "Economic Data, WB GDP (constant 2005 US$) / FAO population, $/Person",
    "Gross.Domestic.Product.constant.prices.Percent.change" =
      "Economic Data, Gross Domestic Product, constant prices, Percent change",
    "Food.Supply.Grain.Equiv.1000.MT.yr" = 
      "Other, Food Supply, Grain Equiv. 1000 MT / yr",
    "Food.Supply.Grain.Equiv.kg.cap.yr" = "Other, Food Supply, Grain Equiv. kg/cap/yr",
    "Grains.Cereals.Root.Food.Availability.per.capita.kg.cap.yr" =
      "Total Grains/Cereals and Root Crops (R&T), Food Availability per capita, kg/cap/yr"
    )

merged_5_final <- merged_4_wide
merged_5_final$Continent <- countrycode(sourcevar = merged_5_final$Country,
                                       origin = "country.name",
                                       destination = "continent")
colnames(merged_5_final)

merged_5_final$Year <- factor(merged_5_final$Year)

write.csv(merged_5_final, file = "merged_5_final")
########################################################################################################
#################################### descriptives ######################################################
########################################################################################################



########################################################################################################
#################################### solo temperature ##################################################
########################################################################################################


########################################################################################################
####################################### solo temp ######################################################
########################################################################################################

### graph 1: temp change over years in one country
random_country <- sample(unique(merged_5_final$Country), 1)
selected_country <- merged_5_final[merged_5_final$Country == random_country, ]

ggplot(selected_country, aes(x = as.numeric(Year), y = Temperature))+
  geom_line() +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Year")+
  ylab("Temperature Change in °C") +
  ggtitle (paste("Temperature Change 1980 - 2013 in", unique(selected_country$Country)), 
           subtitle = "Baseline Period: 1951 - 1980") +
  theme_minimal()

##############################################
# reshaping the data to another format. 
# using the gather function from the tidyr package to convert the 
# Import and Export columns into a single column representing the values, 
# and another column indicating whether it's import or export. 
# because we want separate axes for import and export

selected_country2 <- merged_5_final[merged_5_final$Country == "Burkina Faso", ]
colnames(merged_5_final)
str(selected_country2)

ggplot(selected_country, aes(x = Year, y = Temperature))+
  geom_line() +
  geom_smooth(method = lm, se = FALSE)

color_palette <- colorRampPalette(c("red", "blue"))
# Creating two lattice plots
import_plot <- barchart(
  Year ~ Total.Grains.Cereals.Root.Import.Quantity.1000.MT,
  data = selected_country2,
  horizontal = TRUE,
  origin = 0,
  scales = list(y = list(alternating = 1)),
  main = "Import",
  col = color_palette(length(selected_country2$Year))
) 

export_plot <- barchart(
  Year ~ Total.Grains.Cereals.Root.Export.Quantity.1000.MT,
  data = selected_country2,
  horizontal = TRUE,
  origin = 0,
  scales = list(y = list(alternating = 1)),
  main = "Export",
  col = color_palette(length(selected_country2$Year))
)

grid.arrange(import_plot, export_plot, ncol = 2)
#####################

library(ggplot2)
library(dplyr)
library(forcats)

# Calculate import and export percentages
selected_country2 <- selected_country2 %>% replace(is.na(.), 0)

# Calculate import and export percentages
selected_country2 <- selected_country2 %>%
  mutate(Import_Percentage = (Total.Grains.Cereals.Root.Import.Quantity.1000.MT / (Total.Grains.Cereals.Root.Import.Quantity.1000.MT + Total.Grains.Cereals.Root.Export.Quantity.1000.MT)) * 100,
         Export_Percentage = (Total.Grains.Cereals.Root.Export.Quantity.1000.MT / (Total.Grains.Cereals.Root.Import.Quantity.1000.MT + Total.Grains.Cereals.Root.Export.Quantity.1000.MT)) * 100)

# Combine data for plotting
combined_data <- selected_country2 %>%
  select(Year, Import_Percentage, Export_Percentage) %>%
  pivot_longer(cols = c(Import_Percentage, Export_Percentage),
               names_to = "Type",
               values_to = "Percentage") %>%
  arrange(Year, desc(Type)) %>% 
  replace(is.na(.), 0)

# fct_rev() function from the forcats package to reverse the order of the 
# levels of the Type variable in the combined_data dataframe.
combined_data$Type <- fct_rev(combined_data$Type)

combined_data2 <- combined_data[combined_data$Type == "Export_Percentage", ]
# Create the line chart data
line_data <- combined_data2 %>%
  select(Year, Percentage)

# Create stacked bar plot
plot <- ggplot(combined_data, aes(x = Year, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Import and Export Percentages",
       x = "Year",
       y = "Percentage",
       fill = "Type") +
  scale_fill_manual(values = c("Import_Percentage" = "#bdc9e1", 
                               "Export_Percentage" = "#02818a")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

print(plot)

str(combined_data)
#####
plot <- ggplot(combined_data, aes(x = as.numeric(Year), y = Percentage, color = Type)) +
  geom_line() +
  labs(title = "Import and Export Percentages",
       x = "Year",
       y = "Percentage",
       fill = "Type") +
  #scale_fill_manual(values = c("Import_Percentage" = "#bdc9e1", 
                              # "Export_Percentage" = "#02818a")) +
  #scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

print(plot)


########################################################################################################
##################################### combined analysis ################################################
########################################################################################################

write.csv(merged_5_final, 'merged_5_final.csv')
colnames(merged_5_final)
str(merged_5_final)
unique(merged_5_final$Continent)

world <- ne_countries(scale = "medium", returnclass = "sf")
head(world)

?st_read
st_drivers()
#options("SHAPE_RESTORE_SHX" = "YES")
#world <- st_read("C:/Users/Ansam/Documents/HSLU/Semester 1 Feb 2023/R-Bootcamp/group_project/world-administrative-boundaries/world-administrative-boundaries.shp")

data <- subset(merged_5_final, Year == 1991)
world_data <- merge(world, data, by.x = "name", by.y = "Country")
colnames(world_data)
dim(world_data)

#missing values
colSums(is.na(world_data))

ggplot() +
  geom_sf(data = world_data, aes(
    fill = `Food.Supply.Grain.Equiv.kg.cap.yr`)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "gray50",
                      name = "Grain Production") +
  theme_void()

# Combine the world map with your plot
combined_plot <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # World map layer
  geom_sf(data = world_data, aes(
    fill = `Food.Supply.Grain.Equiv.kg.cap.yr`)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "gray50",
                      name = "Grain Production") +
  theme_void()

# Show the combined plot
print(combined_plot)

###################3
# Line plots
read.csv(file = "merged_5_final.csv")
d.merged <- read.csv("merged_5_final.csv")
unique(d.merged$Country)

country1_data <- d.merged[d.merged$Country == 'Morocco', ]
country2_data <- d.merged[d.merged$Country == 'Mali', ]

ggplot() +
  geom_line(data = country1_data, 
            aes_string(x = 'Year', 
                       y = 'Temperature',
                       color = 'Country'
            )) +
  geom_line(data = country2_data, 
            aes_string(x = 'Year', 
                       y = 'Temperature',
                       color = 'Country'
            )) +
  labs(x = "Year",
       y = 'Temperature') #+
#scale_fill_manual(values = c("blue", "red")) +
#theme_minimal()