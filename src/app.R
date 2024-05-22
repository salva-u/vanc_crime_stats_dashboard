options(shiny.port = 8050, shiny.autoreload = TRUE)
library(shiny)
library(ggplot2)
library(thematic)
library(readr)
library(dplyr)
library(leaflet)
source("Functions.R")

# Load and preprocess data
cleaned_data <- preprocess()

## ------------------UI ------------------------------
# Load libraries and data

## UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("
      .irs-bar { background-color: #00FFFF !important; }
      .irs-bar-edge { background-color: #00FFFF !important; }
      .irs-line { background-color: #00FFFF !important; }
      .irs-single, .irs-handle-single { background-color: #00FFFF !important; border-color: #00FFFF !important; }
      .irs-handle-single:hover { background-color: #00FFFF !important; }
      .irs-grid-pol { background-color: #00FFFF !important; }
    "))
  ),
  titlePanel(
    "Vancouver Police Department Crimes Navigation Tool"
  ),
  
  sidebarLayout(
    # Filters at the top
    fluidRow(
      column(3,
             # Slider for year
             sliderInput("year", "Select Year", min = 2018, max = 2024, value = c(2018, 2024))),
      column(3,
             # Slider for month
             sliderInput("month", "Select Month", min = 1, max = 12, value = c(1, 12))),
      column(3,
             # Slider for day
             sliderInput("day", "Select Day", min = 1, max = 31, value = c(1, 31))),
      column(3,
             # Select input for neighbourhood
             selectInput("neighbourhood", "Select Neighbourhood", 
                         choices = unique(cleaned_data$NEIGHBOURHOOD), 
                         selected = "West Point Grey"))
    ), # Empty sidebar
    mainPanel(
      div()
    ) ),
    fluidRow(
      column(12,
             div(
        h3("Crime Locations colored by Type of Crime"),
        leafletOutput("map"),
        style = "border: 2px solid white; padding: 10px; margin: 10px; width: 100%;"
      )
      )
    ),
    fluidRow(
      column(6,
             div(
               h3("Crime Count by Time of Day"),
               plotOutput("crime_hour_plot"),
               style = "border: 2px solid white; padding: 10px; margin: 10px;"
             )),
    column(6,
           div(
             h3("Blocks with Highest Crime Count in Neighbourhood"),
             plotOutput("heatmap"),
             style = "border: 2px solid white; padding: 10px; margin: 10px; "
           ))
    )
 
)

# ------SERVER -------------------------------
server <- function(input, output, session) {
  thematic_shiny()
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    subset(cleaned_data, 
           YEAR >= input$year[1] & YEAR <= input$year[2] & 
             MONTH >= input$month[1] & MONTH <= input$month[2] & 
             DAY >= input$day[1] & DAY <= input$day[2] & 
             NEIGHBOURHOOD == input$neighbourhood)
  })
  
  # Output filtered data
  output$filtered_data <- renderTable({
    filtered_data()
  })
  
  output$map <- renderLeaflet({
    # Define a custom palette based on the types of crime
    pal <- colorFactor(
      c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50"),
      domain = filtered_data()$TYPE
    )
    
    leaflet() |> 
      addProviderTiles("CartoDB.DarkMatter") |> 
      addCircleMarkers(
        data = filtered_data(),
        # Use X and Y columns as points
        lng = ~X,
        lat = ~Y,
        color = ~pal(TYPE),
        opacity = 0.65,
        radius = 4,
        label = paste(
          "Hundred Block:", 
          filtered_data()$HUNDRED_BLOCK, "<br>"
        ) |>  lapply(htmltools::HTML)
      ) |> 
      addLegend(
        data = filtered_data(),
        pal = pal,
        values = ~TYPE,
        title = "Type of Crime",
        opacity = 0.9,
        position = "bottomright"
      )  |> 
      setView(lng = -123.10607700995347, lat = 49.252132479531134, zoom = 12)
  })
  
  output$crime_hour_plot <- renderPlot({
    # Count the number of crimes by hour of the day
    crime_counts <- table(filtered_data()$HOUR)
    
    # Convert the hour labels to numeric
    hour_labels <- as.numeric(names(crime_counts))
    
    # Plot the count of crimes by hour
    plot(hour_labels, crime_counts, type = "l", 
         xlab = "Hour of Day", ylab = "Number of Crimes")
  })
  

  
  # Output the heatmap plot
  output$heatmap <- renderPlot({
    # Get the top 3 crimes in the neighborhood along with their counts
    top_crimes <- filtered_data() |> 
      group_by(TYPE, HUNDRED_BLOCK) |> 
      summarise(count = n()) 
    
    max_count <- max(top_crimes$count)
    top_crimes <- top_crimes |> filter(count == max_count)
    
    # Create the heatmap
    heatmap_plot <- ggplot(top_crimes, aes(x = TYPE, y = HUNDRED_BLOCK, fill = count)) +
      geom_tile() +
      scale_fill_viridis_c() +  # Change the color scale as desired
      labs(x = "Type of Crime", y = "Hundred Block") +
      theme(axis.text.x = element_text( hjust = 1, size=10),
            axis.text.y = element_text(vjust=1, size=10)) 
   
     return(heatmap_plot)
    
  })
  
}

shinyApp(ui, server)
