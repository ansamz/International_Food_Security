# Data Importing
#################
library(ggplot2)   
library(tidyr)     
library(forecast)  
library(tseries)   
library(lubridate) 
library(corrplot)
library(tidyverse)
library(car)
library(randomForest)
library(lattice)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(patchwork)
library(corrr)
library(pastecs)


#Read data
###############
merged_5_final = read.csv("merged_5_final.csv")

#basic statistics
################
stat.desc(merged_5_final)

dim(merged_5_final)
colnames(merged_5_final)
data2 <- drop_na(merged_5_final)
dim(data2)
sum(is.na(data2))

########################################################################################################
#################################### solo descriptive ###########################################################
########################################################################################################

###################### graph 1: temp change over years ###################################################
# for one random country
random_country <- sample(unique(merged_5_final$Country), 1)
selected_country <- merged_5_final[merged_5_final$Country == random_country, ]

ggplot(selected_country, aes(x = Year, y = Temperature))+
  geom_line(col = "grey50") +
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  xlab("Year")+
  ylab("Temperature Change in °C") +
  ggtitle (paste("Temperature Change 1980 - 2013 in", unique(selected_country$Country)), 
           subtitle = "Baseline Period: 1951 - 1980") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#for specific country (optional, e.g. Rwanda)
selected_country_fix <- merged_5_final[merged_5_final$Country == "Cameroon", ]

ggplot(selected_country_fix, aes(x = Year, y = Temperature))+
  geom_line(col = "grey50") +
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  xlab("Year")+
  ylab("Temperature Change in °C") +
  ggtitle (paste("Temperature Change 1980 - 2013 in", unique(selected_country_fix$Country)), 
           subtitle = "Baseline Period: 1951 - 1980") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# addition: subplots for all countries
ggplot(merged_5_final, aes(x = Year, y = Temperature))+
  geom_line(col = "grey50") +
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  xlab("Year")+
  ylab("Temperature Change in °C") +
  facet_wrap(~ Country) +
  ggtitle (paste("Temperature Change 1980 - 2013 in", unique(merged_5_final$Country)), 
         subtitle = "Baseline Period: 1951 - 1980") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


#############  graph 2 export and import quantities  #################################
# reshaping the data to another format. 
# using the gather function from the tidyr package to convert the 
# Import and Export columns into a single column representing the values, 
# and another column indicating whether it's import or export. 
# because we want separate axes for import and export

color_palette <- colorRampPalette(c("red", "blue"))
max.import <- max(selected_country$Total.Grains.Cereals.Root.Import.Quantity.1000.MT)
max.export <- max(selected_country$Total.Grains.Cereals.Root.Export.Quantity.1000.MT)
x.limit <- max(max.import, max.export)

# Creating two lattice plots
import_plot <- barchart(
  as.character(Year) ~ Total.Grains.Cereals.Root.Import.Quantity.1000.MT,
  data = selected_country,
  horizontal = TRUE,
  origin = 0,
  scales = list(y = list(alternating = 1)),
  xlab = "Import quanitity in million tonnes",
  ylab = "Year",
  xlim = c(0,x.limit),
  col = color_palette(length(selected_country$Year)))

export_plot <- barchart(
  as.character(Year) ~ Total.Grains.Cereals.Root.Export.Quantity.1000.MT,
  data = selected_country,
  horizontal = TRUE,
  origin = 0,
  scales = list(y = list(alternating = 1)),
  xlab = "Export quanitity in million tonnes",
  ylab = list("Year"),
  xlim = c(0,x.limit),
  col = color_palette(length(selected_country$Year)))

grid.arrange(import_plot, export_plot, ncol = 2)
combined_title <- paste("Import and Export of Grains, Cereals, and Root in", unique(selected_country$Country))
grid.text(combined_title, x = 0.5, y = 1, just = "center")

#####################  graph 3 import exportpercentage of total quanity ########################################################

####### preparation
# Calculate import and export percentages
selected_country2 <- selected_country %>% replace(is.na(.), 0)

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

######### Create stacked bar plot
ggplot(combined_data, aes(x = Year, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Import and Export Percentages of total quantity",
       x = "Year",
       y = "Percentage",
       fill = "Type") +
  scale_fill_manual(values = c("Import_Percentage" = "#bdc9e1", 
                               "Export_Percentage" = "#1c9099")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme(legend.position = "bottom", 
        panel.grid.major.y = element_line(color = "grey95"),
        #panel.grid.minor.y = element_line(color = "grey85"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

str(combined_data)

######### Create stacked bar plot
ggplot(combined_data2, aes(x = Year, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Export Share of Total Quantity",
       x = "Year",
       y = "Percentage")+
  scale_fill_manual(values = c("Export_Percentage" = "#1c9099")) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  theme(legend.position = "bottom", 
        panel.grid.major.y = element_line(color = "grey95"),
        #panel.grid.minor.y = element_line(color = "grey85"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#####
ggplot(combined_data, aes(x = as.numeric(Year), y = Percentage, color = Type)) +
  geom_line() +
  labs(title = "Import and Export Percentages",
       x = "Year",
       y = "Percentage",
       fill = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),   
        plot.title = element_text(hjust = 0.5))

colnames(merged_5_final)
str(merged_5_final)
unique(merged_5_final$Continent)

# ne_countries to get geographic information
world <- ne_countries(scale = "medium", returnclass = "sf")
head(world)

st_drivers()

data.sub.year <- subset(merged_5_final, Year == 1991)
world_data <- merge(world, data.sub.year, by.x = "name", by.y = "Country")
colnames(world_data)
dim(world_data)

#Check missing values
colSums(is.na(world_data))


#################### graph 4 Food Supply world map
ggplot() +
  geom_sf(data = world_data, aes(
    fill = `Food.Supply.Grain.Equiv.kg.cap.yr`)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "gray50",
                      name = "Grain Production") +
  ggtitle("Food Supply for Grain") +
  theme_void()

# Combine the world map with plot
ggplot() +
  geom_sf(data = world, fill = "gray97", color = "gray50") +  # World map layer
  geom_sf(data = world_data, aes(
    fill = `Food.Supply.Grain.Equiv.kg.cap.yr`)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "gray40",
                      name = "kg per capita") +
  ggtitle("Food Supply for Grain Production") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

########################################################################################################
##################################### combined descriptice ################################################
########################################################################################################

###################
# Line plots
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
       y = 'Temperature') +
theme_minimal()


########################################################################################################
##################################### visual analysis ################################################
########################################################################################################

##############################################################
#################SCATTER####################

#  scatter plot to visualize the relationship between the export value against the GDP
# in one country
ggplot(country1_data, aes(x = GDP.US.dollars.per.person, y = Total.Grains.Cereals.Root.Export.Quantity.1000.MT, size = Population.Million)) +
  geom_point(aes(color = Total.Grains.Cereals.Root.Export.Quantity.1000.MT), alpha = 0.7) + 
  labs(
    title = "Export Value vs. GDP per Person",
    x = "GDP (US dollars per person)",
    y = "Export Value (1000 MT)",
    size = "Population (Million)",
    color = "Export 1000 MT"
  ) +
  scale_size_continuous(range = c(3, 12)) +  # Adjust the size range
  scale_color_gradient(low = "blue", high = "red") +  # Add temperature-based color gradient
  theme_minimal()

#all the countries 
ggplot(data2, aes(x = GDP.US.dollars.per.person, y = Total.Grains.Cereals.Root.Export.Quantity.1000.MT)) +
  geom_point() + 
  labs(
    title = "Export Value vs. GDP per Person",
    x = "GDP (US dollars per person)",
    y = "Export Value (1000 MT)"
  )

#Visual Analysis
##############

# Time series of temperature change varies over time.
ggplot(data2, aes(x=Year, y=`Temperature`, color=Country)) +
  geom_line() +
  labs(x="Year", y="Temperature Change", 
       title="Time Series Plot of Temperature Change")

# Temprature change for each country
ggplot(data2, aes(x=Year, y=`Temperature`)) +
  geom_line() +
  facet_wrap(~Country) +
  labs(x="Year", y="Temperature Change", 
       title="Yearly Plot of Temperature Change for each Country")

# Temprature change for each continent
ggplot(data2, aes(x=Year, y=`Temperature`)) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE, color="red") + # Add correlation line
  facet_wrap(~Continent) +
  labs(x="Year", y="Temperature Change", 
       title="Yearly Plot of Temperature Change for each Country")

########################################################################
# Correlation Plot: This will show the correlation between 
# Temperature change and other numeric features
# get the numeric data
numeric_data <- data2 %>%
  select_if(is.numeric)

correlation_matrix <- cor(numeric_data)

# create a correlation dataframe
correlation_data <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_data) <- c("Variable1", "Variable2", "Correlation")

ggplot(correlation_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "#9ecae1", high = "#095185", limits = c(-1, 1)) +
  geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +  # Remove axis labels
  labs(title = "Correlation Plot")


#######################################################################
# Correlation Heatmap
# per country
# countries in regional approximation - we can make a list of countries

# select countries in regional approximation
# uncomment which one you want to use
# data.per.region <- data2[data2$Country %in% c("Algeria", "Morocco", "Mauritania", "Niger" ), ]
data.per.region <- data2[data2$Continent == "Africa", ]

# Select only numeric columns from your data
numeric_data <- data.per.region %>%
  select_if(is.numeric)

correlation_matrix <- cor(numeric_data)

# create a correlation dataframe
correlation_data <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_data) <- c("Variable1", "Variable2", "Correlation")

ggplot(correlation_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "#9ecae1", high = "#095185", limits = c(-1, 1)) +
  geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +  # Remove axis labels
  labs(title = "Correlation Plot")

# Box Plot for each Continent: This will show the distribution of 
# temperature change for each continent.
# note: we have little data from the americas
ggplot(merged_5_final, aes(x = Continent, y = Temperature)) +
  geom_boxplot(fill = "#9ecae1", color = "#095185", alpha = 0.7) +  # Customize box appearance
  labs(x = "Continent", y = "Temperature Change", 
       title = "Box Plot of Temperature Change for each Continent") +
  theme_minimal() +  # Minimalistic theme
  theme(plot.background = element_rect(fill = "white"))  # Set plot background to white

# Scatter Plot of Temperature change vs 
# Total.Grains.Cereals.Root.Production.Quantity.1000.MT: This will 
# show the relationship between Temperature change and Total Grains/ 
# Cereals/ Root Production Quantity.
ggplot(merged_5_final, aes(x=`Temperature`, y=`Total.Grains.Cereals.Root.Production.Quantity.1000.MT`)) +
  geom_point(
    color="#3182bd",
    fill="#9ecae1",
    shape=19,
    alpha=0.3,
    size=2
  ) +
  labs(x="Temperature Change", 
       y="Total Grains/ Cereals/ Root Production Quantity", 
       title="Scatter Plot of Temperature change vs Total Grains/ Cereals/ Root Production Quantity") +
  theme_minimal() +  # Use a minimalistic theme
  theme(plot.background = element_rect(fill = "white"))

# Bar Plot by Continent
ggplot(merged_5_final, aes(x = Continent, y = `Temperature`, fill = Continent)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill = "white")) +
  labs(title = "Mean Temperature Change per Continent")

# Box Plot of Temperature change per year
ggplot(merged_5_final, aes(x = Year, y = `Temperature`, group = Year)) +
  geom_boxplot(fill = "#095185") +  
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  labs(title = "Temperature Distribution by Year")



########################################################################################################
##################################### Forecasting model ################################################
########################################################################################################

# ARIMA model for forecasting. Before that, ensure your data is stationary 
# using the Augmented Dickey-Fuller (ADF) test.
# we use data2 here because there are a lot of missing values 
adf.test(data2$`Temperature`, alternative = "stationary")
#p-value = 0.1 -> less than 0.05 then we can do ARIMA

# Forecasting Model (ARIMA)
ts_data <- ts(data2$`Temperature`, start = c(1980), frequency = 1)
arima_model <- auto.arima(ts_data)
forecast_values <- forecast(arima_model, h = 50)

# Print forecasted values
print(forecast_values)
plot(forecast_values)

# Not enough data for ARIMA forecasting, will not add it to the app


########################################################################################################
##################################### statisical analysis ################################################
########################################################################################################

# correlation test and t-test to check if the temperature is 
# connected to the rest of the features.
# correlation test
cor.test(data2$`Temperature`, 
         data2$`Total.Grains.Cereals.Root.Production.Quantity.1000.MT`)

#T.test
t.test(data2$`Temperature`, 
       data2$`Total.Grains.Cereals.Root.Production.Quantity.1000.MT`)

# ANOVA
anova_result <- aov(`Temperature` ~ Continent, data = data2)
print(anova_result)

# Regression Analysis
lm_model <- lm(`Temperature` ~ `Total.Grains.Cereals.Root.Production.Quantity.1000.MT` + 
                 `Total.Grains.Cereals.Root.Food.Supply.1000.MT` + `Total.Grains.Cereals.Root.Area.Harvested.1000.Ha`, 
               data = data2)
summary(lm_model)

# Collinearity Check with Variance Inflation Factor 
vif(lm_model)

##########
# results:
# Total.Grains.Cereals.Root.Production.Quantity.1000.MT         Total.Grains.Cereals.Root.Food.Supply.1000.MT 
# 238.28729                                                     191.23529 
# Total.Grains.Cereals.Root.Area.Harvested.1000.Ha 
# 16.84047 
# VIF is significantly greater than 1 (commonly a threshold of 5 or 10 is used), 
# it indicates potential multicollinearity.

##############
# explanation of results
# means that two or more independent variables in the model are highly correlated, which can lead to several issues:
#   Inflated Standard Errors: High multicollinearity results in inflated standard errors for the regression coefficients. This means that the estimated coefficients become less precise and less reliable, leading to wider confidence intervals.
# Difficulty in Interpretation: When independent variables are highly correlated, it can become challenging to interpret the individual impact of each variable on the dependent variable because their effects are confounded.
# Unstable Coefficients: Small changes in the data can lead to significant changes in the estimated coefficients, making the model unstable.

# steps to do for the future: collect more data, or remove those variables

########################################################################################################
##################################### Random Forest Prediction ################################################
########################################################################################################
# prediction using Random Forest
# Split data into training and testing sets
set.seed(42) # For reproducibility and the answer for everything
sample_index <- sample(1:nrow(data2), 0.8 * nrow(data2)) # 80% for training
train_data <- data2[sample_index, ]
test_data <- data2[-sample_index, ]

# Train a Random Forest model
rf_model <- randomForest(Total.Grains.Cereals.Root.Production.Quantity.1000.MT ~ .,
                         data = train_data,
                         ntree = 500) # Number of trees in the forest

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Calculate R-squared
r_squared <- 1 - sum((test_data$Total.Grains.Cereals.Root.Production.Quantity.1000.MT - predictions)^2) / 
  sum((test_data$Total.Grains.Cereals.Root.Production.Quantity.1000.MT - mean(test_data$Total.Grains.Cereals.Root.Production.Quantity.1000.MT))^2)

print(paste("R-squared:", round(r_squared, 4)))

# Create a data frame for visualization
prediction_data <- data.frame(Actual = test_data$Total.Grains.Cereals.Root.Production.Quantity.1000.MT,
                              Predicted = predictions)

# Scatter plot of actual vs. predicted values
ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#1f78b4", shape = 8) +
  geom_abline(intercept = 0, slope = 1, color = "#fc8d62", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values")

# More visualization for predictions
ggplot(prediction_data, aes(x = seq_along(Actual), y = Actual, color = "Actual")) +
  geom_point(alpha = 0.3, size = 3) +
  geom_point(aes(y = Predicted, color = "Predicted"), alpha = 0.3, size = 3) +
  scale_color_manual(values = c("Actual" = "#66c2a5", "Predicted" = "#fc8d62")) +
  labs(x = "Sample", y = "Total Grains Cereals Root Production Quantity", color = "Legend") +
  theme_minimal()

