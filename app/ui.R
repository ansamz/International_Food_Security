library(shiny)

# Load your data from the CSV file
data <- read.csv("data/merged_5_final.csv")
formatted_years <- format(data$Year, format = "####")

column_names_edited <- c("X", "Country", "Year", "Temperature", 
                  "Total Grains, Cereals and Root Production Quantity in 1000MT",
                  "Total Grains, Cereals and Root Food Supply in 1000MT", 
                  "Total Grains, Cereals and Root Area Harvested in 1000Ha", 
                  "Population in Millions", "GDP in US dollars per person", 
                  "Gross Domestic Product constant prices percent change", 
                  "Food Supply Grain in 1000MT per year", 
                  "Food Supply Grain kg/capita/year", 
                  "Grains, Cereals and Root Food Availability kg/capita/year", 
                  "Total Grains, Cereals and Root Export Quantity in 1000MT", 
                  "Total Grains, Cereals and Root Import Quantity in 1000MT", 
                  "Continent")
column_names <- c("X", "Country", "Year", "Temperature", 
                  "Total.Grains.Cereals.Root.Production.Quantity.1000.MT",
                  "Total.Grains.Cereals.Root.Food.Supply.1000.MT", 
                  "Total.Grains.Cereals.Root.Area.Harvested.1000.Ha", 
                  "Population.Million", "GDP.US.dollars.per.person", 
                  "Gross.Domestic.Product.constant.prices.Percent.change", 
                  "Food.Supply.Grain.Equiv.1000.MT.yr", "Food.Supply.Grain.Equiv.kg.cap.yr", 
                  "Grains.Cereals.Root.Food.Availability.per.capita.kg.cap.yr", 
                  "Total.Grains.Cereals.Root.Export.Quantity.1000.MT", 
                  "Total.Grains.Cereals.Root.Import.Quantity.1000.MT", "Continent")

fluidPage(
  titlePanel("Food Security And Temperature Change"),
  navbarPage("",
             tabPanel("World Map",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Select a Year", min(data$Year), max(data$Year), value = min(data$Year), step = 1, sep = ""),
                          selectInput("map_variable", "Select a Variable", choices = column_names_edited[4:14],
                                      selected = "Total Grains, Cereals and Root Production Quantity in 1000MT"), width=3
                        ),
                        mainPanel(
                          fluidRow(
                            column(12, plotOutput("world_map"))
                          ),
                          fluidRow(
                            column(12, plotOutput("relationship"))
                          )
                        )
                      )
             ),
             tabPanel("Country Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country1", "Select Country 1", choices = unique(data$Country)),
                          selectInput("country2", "Select Country 2", choices = unique(data$Country), selected = "Egypt"),
                          selectInput("map_variable2", "Select Map Variable", choices = column_names_edited[4:14]), width=3
                        ),
                        mainPanel(
                          plotOutput("country_comparison")
                        )
                      )
             ),
             tabPanel("Country Detailed View",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country", "Select Country", choices = unique(data$Country), selected = "Indonesia"), width=3
                        ),
                        mainPanel(
                          fluidRow(
                            column(6, plotOutput("import_plot")),
                            column(6, plotOutput("export_plot"))
                          ),
                          fluidRow(
                            column(6, plotOutput("third_plot")),
                            column(6, plotOutput("fourth_plot"))
                        )
                        )
                      )
             ),
             tabPanel("Statistical Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country_pick", "Select Country", choices = unique(data$Country), selected = "Pakistan"), 
                          selectInput("selected_items", "Select Variables", choices = column_names_edited[4:14], 
                                      multiple = TRUE, selected = c("Temperature",                                  
                                                                    "Total Grains, Cereals and Root Production Quantity in 1000MT",
                                                                    "Total Grains, Cereals and Root Food Supply in 1000MT",
                                                                    "Total Grains, Cereals and Root Area Harvested in 1000Ha" ,
                                                                    "Population in Millions", "GDP in US dollars per person")), width=3
                  
                        ),
                        mainPanel(
                          fluidRow(
                            column(12, plotOutput("correlation_world"))
                            ),
                            fluidRow(
                              column(12, plotOutput("correlation_country"))
                          )
                        )
                      )
             ),
             tabPanel("Random Forest Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country3", "Select Country", choices = unique(data$Country)),
                          selectInput("map_variable3", "Select Variable to Predict", choices = column_names_edited[5:14]), width=3
                        ),
                        mainPanel(
                          fluidRow(
                            column(12, plotOutput("reandom_forest"))
                          ),
                          fluidRow(
                            column(12, plotOutput("reandom_forest2"))
                          )
                        )
                      )
             )
  )
)