library(tidyverse)    #for dplyr
library(magrittr)     #for %>%
library(ggplot2)      #for graphs
library(countrycode)  #for addding continents
library(tidyr)
library(gridExtra)
library(rnaturalearth)
library(sf)
library(osmdata)
library(maptiles) ## for get_tiles() / used instead of OpenStreetMap
library(tidyterra)
library(lattice)
library(forcats)


### load data sets
# 1) dataset for temperature changes in countries 1961 - 1961–2019 in C°
    # source: https://www.kaggle.com/datasets/sevgisarac/temperature-change
temperature_raw <- read.csv("temperatures.csv", 
                        stringsAsFactors=FALSE, 
                        fileEncoding="latin1")
# 2) dataset for wheat production in countries from 1996 - 2020
    # https://www.kaggle.com/datasets/rajkumarpandey02/wheat-production-statistics
# We ended up not using this dataset and concentrate on the other two
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
# this has proven to be problematic for data visualization so it was commented
# especially converting year to integer it gave it numbers instead of the actual year
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

# filter for the temperature change (cut of standard deviation)
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
    "Temperature.Change" = "Temperature",
    "Total.Grains.Cereals.Root.Production.Quantity.1000.MT" = 
      "Total Grains/Cereals and Root Crops (R&T), Production Quantity, Grain Equiv. 1000 MT",
    "Total.Grains.Cereals.Root.Food.Supply.1000.MT" = 
      "Total Grains/Cereals and Root Crops (R&T), Food Supply, Grain Equiv. 1000 MT",
    "Total.Grains.Cereals.Root.Area.Harvested.1000.Ha" = 
      "Total Grains/Cereals and Root Crops (R&T), Area Harvested, 1000 Ha",
    "Population.Million" = "Population, Total Population - Both Sexes, Million",
    "GDP.US.dollars.per.person" = "Economic Data, WB GDP (constant 2005 US$) / FAO population, $/Person",
    "GDP.constant.prices.Percent.change" =
      "Economic Data, Gross Domestic Product, constant prices, Percent change",
    "Food.Supply.Grain.Equiv.1000.MT.yr" = 
      "Other, Food Supply, Grain Equiv. 1000 MT / yr",
    "Food.Supply.Grain.Equiv.kg.cap.yr" = "Other, Food Supply, Grain Equiv. kg/cap/yr",
    "Grains.Cereals.Root.Food.Availability.per.capita.kg.cap.yr" =
      "Total Grains/Cereals and Root Crops (R&T), Food Availability per capita, kg/cap/yr"
  )

# add continents
merged_5_final <- merged_4_wide
merged_5_final$Continent <- countrycode(sourcevar = merged_5_final$Country,
                                       origin = "country.name",
                                       destination = "continent")
colnames(merged_5_final)

merged_5_final$Year <- factor(merged_5_final$Year)

#### check if there are a lot of missing values for temperature in every country
missing_values <- merged_5_final %>% group_by(Country) %>% summarise(Missing_Values = sum(is.na(Temperature.Change)))
missing_values %>% filter(Missing_Values > 0)

# delete countries having more than 50% missing values, i.e. >= 16 NAs
countries_to_delete <- c("Burundi", "Eritrea", "Rwanda", "Sudan", "Yemen")
merged_5_final <- merged_5_final %>% filter(!Country %in% countries_to_delete)

# Remove rows where year is equal to 2013 because this year has a lot of missing values
merged_5_final <- merged_5_final %>%
  filter(Year != 2013)

write.csv(merged_5_final, file = "merged_5_final.csv")