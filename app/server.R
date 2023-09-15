library(shiny)
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


# Load your data from the CSV file
data <- read.csv("data/merged_5_final.csv")

# dict
data_map_features_names <- c(
                  "Temperature.Change" = "Temperature Change",                                  
                  "Total.Grains.Cereals.Root.Production.Quantity.1000.MT" = "Production Quantity (1000 MT) Grains Cereals and Roots",
                  "Total.Grains.Cereals.Root.Food.Supply.1000.MT" = "Food Supply (1000 MT) Grains Cereals and Root",
                  "Total.Grains.Cereals.Root.Area.Harvested.1000.Ha" = "Area Harvested (1000 hectares) Grains Cereals and Root",
                  "Population.Million" = "Population in Millions",
                  "GDP.US.dollars.per.person" = "GDP US$ per Million",
                  "Gross.Domestic.Product.constant.prices.Percent.change" = "Domestic Product price change in percentage",
                  "Food.Supply.Grain.Equiv.1000.MT.yr" = "Food supply (1000 MT per year)",
                  "Food.Supply.Grain.Equiv.kg.cap.yr" = "Food supply kg/capita/year",               
                  "Grains.Cereals.Root.Food.Availability.per.capita.kg.cap.yr" = "Food availability (kg/capita/year) Grains Cereals and Roots",
                  "Total.Grains.Cereals.Root.Export.Quantity.1000.MT" = "Export quantity Grains (Total), Cereals and Root",
                  "Total.Grains.Cereals.Root.Import.Quantity.1000.MT" = "Import quantity Grains (Total), Cereals and Root"
                )

column_names <- c( "X"="X", "Country"="Country", "Year"="Year", "Temperature Change"="Temperature.Change", 
                   "Total Grains, Cereals and Root Production Quantity in 1000MT"="Total.Grains.Cereals.Root.Production.Quantity.1000.MT",
                   "Total Grains, Cereals and Root Food Supply in 1000MT"="Total.Grains.Cereals.Root.Food.Supply.1000.MT", 
                   "Total Grains, Cereals and Root Area Harvested in 1000Ha"="Total.Grains.Cereals.Root.Area.Harvested.1000.Ha", 
                   "Population in Millions"="Population.Million", 
                   "GDP in US dollars per person"="GDP.US.dollars.per.person", 
                   "Gross Domestic Product constant prices percent change"="Gross.Domestic.Product.constant.prices.Percent.change", 
                   "Food Supply Grain in 1000MT per year"="Food.Supply.Grain.Equiv.1000.MT.yr", 
                   "Food Supply Grain kg/capita/year"="Food.Supply.Grain.Equiv.kg.cap.yr", 
                   "Grains, Cereals and Root Food Availability kg/capita/year"="Grains.Cereals.Root.Food.Availability.per.capita.kg.cap.yr", 
                   "Total Grains, Cereals and Root Export Quantity in 1000MT"="Total.Grains.Cereals.Root.Export.Quantity.1000.MT", 
                   "Total Grains, Cereals and Root Import Quantity in 1000MT"="Total.Grains.Cereals.Root.Import.Quantity.1000.MT", 
                   "Continent"="Continent")

function(input, output) {
  ################################## tab 1 #############################################
  ### tab 1, graph 1 world map of chosen variable
  output$world_map <- renderPlot({
    selected_year_data <- data[data$Year == input$year, ]
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_data <- merge(world, selected_year_data, by.x = "name", 
                        by.y = "Country")
    
    ggplot() +
      geom_sf(data = world_data, aes_string(
        fill = column_names[input$map_variable]), color = "black") + # Add borders to the polygons
      geom_sf(data = world, fill = NA, color = "black") + # Country borders
      scale_fill_gradient(low = "blue", high = "red", na.value = "gray50",
                          name = "") +
      ggtitle(input$map_variable, subtitle = input$year) +
      theme_void() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
### tab 1, graph 2 temperature change against chosen variable
  output$relationship <- renderPlot({
    ggplot(data, aes_string(x="Temperature.Change", y=column_names[input$map_variable])) +
      geom_point(
        color="#3182bd",
        fill="#9ecae1",
        shape=19,
        alpha=0.3,
        size=2
      ) +
      ggtitle(paste("Temperature Change vs.", column_names[input$map_variable]), 
              subtitle = "over all countries and years")+
      labs(x="Temperature Change", 
           y=data_map_features_names[column_names[input$map_variable]]) + 
      theme(plot.background = element_rect(fill = "white"),
            panel.border = element_blank())+ 
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })

######################### tab 2 #####################################
### tab 2, country comparisons
  output$country_comparison <- renderPlot({
    country1_data <- data[data$Country == input$country1, ]
    country2_data <- data[data$Country == input$country2, ]
    
    ggplot() +
      geom_line(data = country1_data, 
                aes_string(x = 'Year', 
                           y = column_names[input$map_variable2],
                           color = 'Country'
                )) +
      geom_line(data = country2_data, 
                aes_string(x = 'Year', 
                           y = column_names[input$map_variable2],
                           color = 'Country'
                )) +
      labs(x = "Year",
           y = data_map_features_names[column_names[input$map_variable2]]) +
      ggtitle(paste(data_map_features_names[column_names[input$map_variable2]], "in", input$country1, "and", input$country2)) + 
      theme(plot.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.border = element_blank()) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })

########################### tab 3 ######################################################
### tab 3, graph upper left, import quantity
  output$import_plot <- renderPlot({
    country_data <- data[data$Country == input$country, ]
    
    color_palette <- colorRampPalette(c("red", "blue"))
    max.import <- max(country_data$Total.Grains.Cereals.Root.Import.Quantity.1000.MT, na.rm = TRUE)
    max.export <- max(country_data$Total.Grains.Cereals.Root.Export.Quantity.1000.MT, na.rm = TRUE)
    x.limit <- max(max.import, max.export)
    
    import_plot <- barchart(
      as.character(Year) ~ Total.Grains.Cereals.Root.Import.Quantity.1000.MT,
      data = country_data,
      horizontal = TRUE,
      origin = 0,
      scales = list(y = list(alternating = 1)),
      xlab = "Import quantity in million tonnes",
      ylab = "Year",
      main = list("Import Quantity 1980 - 2013"),
      col = color_palette(length(country_data$Year)),
      xlim = c(0, x.limit)
    )
  
    print(import_plot)
  })

### grapgh upper right, export
  output$export_plot <- renderPlot({
    country_data <- data[data$Country == input$country, ]
    
    color_palette <- colorRampPalette(c("red", "blue"))
    max.import <- max(country_data$Total.Grains.Cereals.Root.Import.Quantity.1000.MT, na.rm = TRUE)
    max.export <- max(country_data$Total.Grains.Cereals.Root.Export.Quantity.1000.MT, na.rm = TRUE)
    x.limit <- max(max.import, max.export)
    
    export_plot <- barchart(
      as.character(Year) ~ Total.Grains.Cereals.Root.Export.Quantity.1000.MT,
      data = country_data,
      horizontal = TRUE,
      origin = 0,
      scales = list(y = list(alternating = 1)),
      xlab = "Export quantity in million tonnes",
      ylab = list("Year"),
      main = list("Export Quantity 1980 - 2013"),
      col = color_palette(length(country_data$Year)),
      xlim = c(0, x.limit)
    )
    print(export_plot)
  })

### graph left lower, Temperature change
  output$third_plot <- renderPlot({
    country_data <- data[data$Country == input$country, ]
    
    ggplot(country_data, aes(x = Year, y = Temperature.Change))+
      geom_line(col = "grey50") +
      geom_point()+
      geom_smooth(method = lm, se = FALSE) +
      xlab("Year")+
      ylab("Temperature Change in °C") +
      ggtitle ("Temperature Change 1980 - 2013", 
               subtitle = "Baseline Period: 1951 - 1980") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })

### graph right lower  
  output$fourth_plot <- renderPlot({
    country_data <- data[data$Country == input$country, ]
    
    ggplot(country_data, aes(x = GDP.US.dollars.per.person, y = Total.Grains.Cereals.Root.Export.Quantity.1000.MT, size = Population.Million)) +
      geom_point(aes(color = Total.Grains.Cereals.Root.Export.Quantity.1000.MT), alpha = 0.7) +
      labs(
        x = "GDP (US dollars per person)",
        y = "Export Value (1000 MT)",
        size = "Population (Million)",
        color = "Export 1000 MT"
      ) +
      ggtitle ("Export Value vs. GDP per Capita 1980 - 2013", subtitle = "") +
      scale_size_continuous(range = c(3, 12)) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
 
################################## tab 4 ########################### 
  output$correlation_world <- renderPlot({
    # Use match to get the indices of the matching keys in column_names
    indices <- match(input$selected_items, names(column_names))
    # Create a new vector with the corresponding values from column_names
    selected_columns <- column_names[indices]
    
    subdata <- data %>% select(selected_columns)
    data2 <- drop_na(subdata)
    numeric_data <- data2 %>%
      select_if(is.numeric)
 # adding if condition in case there are NAs   
    if (nrow(data2) >= 10) {
      correlation_matrix <- cor(numeric_data)
      
      # Create a correlation dataframe
      correlation_data <- as.data.frame(as.table(correlation_matrix))
      colnames(correlation_data) <- c("Variable1", "Variable2", "Correlation")
      
      correlation_plot <- ggplot(correlation_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#de2d26", high = "#43a2ca",mid = "#f0f0f0", limits = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
        ggtitle("Correlation Plot Across All Countries") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"))
      print(correlation_plot)
    } else {
      correlation_plot <- "Sorry, not enough data to create a correlation plot."
      plot.new()
      text(0.5, 0.5, correlation_plot)
    }
  })
  
  output$correlation_country <- renderPlot({
    data2 <- drop_na(data)
  
    indices <- match(input$selected_items, names(column_names))
    use_selected_items <- column_names[indices]
    selected_columns <- c(use_selected_items, "Country")
    subdata <- data2 %>% select(selected_columns)
    country_data_corr <- subdata[subdata$Country == input$country_pick, ]
    numeric_data_country <- country_data_corr %>%
      select_if(is.numeric)
    
    if (nrow(country_data_corr) >= 5) {
      correlation_matrix2 <- cor(numeric_data_country)
      # create a correlation dataframe
      correlation_data2 <- as.data.frame(as.table(correlation_matrix2))
      colnames(correlation_data2) <- c("Variable1", "Variable2", "Correlation")
      
      correlation_plot2 <- ggplot(correlation_data2, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#de2d26", mid = "#f0f0f0", high = "#43a2ca", limits = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
        labs(title = paste("Correlation Plot in", input$country_pick)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold")) 
      print(correlation_plot2)
    } else {
      correlation_plot2 <- "Sorry, not enough data to create a correlation plot."
      plot.new()
      text(0.5, 0.5, correlation_plot2)
    }
  
  })
 
################################# tab 5 prediction ############################# 
  output$reandom_forest <- renderPlot({
    data2 <- drop_na(data)
    sample_index <- sample(1:nrow(data2), 0.8 * nrow(data2)) # 80% for training
    train_data <- data2[sample_index, ]
    test_data <- data2[-sample_index, ]
    
    # Create a dynamic formula based on user input
    formula <- as.formula(paste(column_names[input$map_variable3], "~ ."))
    
    # Train a Random Forest model
    rf_model <- randomForest(formula,
                             data = train_data,
                             ntree = 500)
    
    predictions <- predict(rf_model, newdata = test_data)
    r_squared <- 1 - sum((test_data[, column_names[input$map_variable3]] - predictions)^2) / 
      sum((test_data[, column_names[input$map_variable3]] - mean(test_data[, column_names[input$map_variable3]]))^2)
    
    # Create a data frame for visualization
    prediction_data <- data.frame(Actual = test_data[, column_names[input$map_variable3]],
                                  Predicted = predictions)
    
    # Scatter plot of actual vs. predicted values
    ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
      geom_point(color = "#1f78b4", shape = 8) +
      geom_abline(intercept = 0, slope = 1, color = "#fc8d62", linetype = "dashed") +
      labs(x = "Actual Values",
           y = "Predicted Values")+
      ggtitle("Actual vs. Predicted Values for all countries", subtitle = paste("R-Squared = ", round(r_squared, 2)))+
      theme_light() + 
      theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  output$reandom_forest2 <- renderPlot({
    country_pred <- data[data$Country == input$country3, ]
    
    data_clean <- drop_na(country_pred)
    
    # Check if data_clean is empty
    if (nrow(data_clean) < 2) {
      # If data_clean is empty, display a message and stop execution
      plot.new()
      text(0.5, 0.5, paste("Sorry, not enough data to perform prediction for", input$country3))
    } else {
      sample_index <- sample(1:nrow(data_clean), 0.8 * nrow(data_clean)) # 80% for training
      train_data <- data_clean[sample_index, ]
      test_data <- data_clean[-sample_index, ]
      
      # Create a dynamic formula based on user input
      formula <- as.formula(paste(column_names[input$map_variable3], "~ ."))
      
      # Train a Random Forest model
      rf_model <- randomForest(formula,
                               data = train_data,
                               ntree = 500)
      
      predictions <- predict(rf_model, newdata = test_data)
      r_squared <- 1 - sum((test_data[, column_names[input$map_variable3]] - predictions)^2) / 
        sum((test_data[, column_names[input$map_variable3]] - mean(test_data[, column_names[input$map_variable3]]))^2)
      
      # Create a data frame for visualization
      prediction_data <- data.frame(Actual = test_data[, column_names[input$map_variable3]],
                                    Predicted = predictions)
      
      # Scatter plot of actual vs. predicted values
      ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
        geom_point(color = "#1f78b4", shape = 8) +
        geom_abline(intercept = 0, slope = 1, color = "#fc8d62", linetype = "dashed") +
        labs(title = paste("Actual vs. Predicted Values in", input$country3),
             subtitle = paste("R-Squared = ", round(r_squared, 2)),
             x = "Actual Values",
             y = "Predicted Values")+
        theme_light() + 
        theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
              plot.subtitle = element_text(hjust = 0.5))
    }
  })
}
