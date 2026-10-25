# Homework Question 3
# MacLeish Weather Explorer - Shiny App


library(shiny)
library(ggplot2)
library(macleish)

# Load the 2015 weather data
whately <- macleish::whately_2015
orchard <- macleish::orchard_2015

# Weather measurements shared by both stations
measurements <- c(
  "Temperature (C)" = "temperature",
  "Wind Speed (m/s)" = "wind_speed",
  "Relative Humidity (%)" = "rel_humidity",
  "Atmospheric Pressure (mb)" = "pressure",
  "Rainfall (mm)" = "rainfall"
)


# 1. User Interface

ui <- fluidPage(
  
  titlePanel("MacLeish Weather Explorer"),
  
  p("Explore weather observations from two MacLeish
    Field Station weather stations during 2015."),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Widget 1: Select weather station
      selectInput(
        "station",
        "Select Weather Station:",
        choices = c(
          "Whately 2015" = "whately",
          "Orchard 2015" = "orchard"
        ),
        selected = "whately"
      ),
      
      # Widget 2: Select date range
      dateRangeInput(
        "dates",
        "Select Date Range:",
        start = "2015-01-01",
        end = "2015-12-31",
        min = "2015-01-01",
        max = "2015-12-31",
        format = "yyyy-mm-dd"
      ),
      
      # Widget 3: Select weather measurement
      selectInput(
        "measurement",
        "Select Weather Measurement:",
        choices = measurements,
        selected = "temperature"
      ),
      
      # Widget 4: Change line color
      radioButtons(
        "linecolor",
        "Line Color:",
        choices = c(
          "Blue" = "steelblue",
          "Red" = "red",
          "Green" = "darkgreen"
        ),
        selected = "steelblue",
        inline = TRUE
      ),
      
      # Widget 5: Show data points
      checkboxInput(
        "points",
        "Show Individual Data Points",
        value = FALSE
      ),
      
      # Widget 6: Show or hide gridlines
      checkboxInput(
        "grid",
        "Show Gridlines",
        value = TRUE
      ),
      
      helpText(
        "Tip: Select Orchard 2015 and dates in
         late November to investigate unusual
         temperature readings."
      )
      
    ),
    
    mainPanel(
      
      h3("Interactive Weather Time Series"),
      
      plotOutput(
        "weatherplot",
        height = "450px"
      ),
      
      h3("Selected Data Summary"),
      
      tableOutput("summarytable"),
      
      h3("Observations"),
      
      verbatimTextOutput("observation")
      
    )
  )
)


# 2. Server

server <- function(input, output, session) {
  
  # Select station and date range
  filtered_weather <- reactive({
    
    req(input$station, input$dates)
    
    if (input$station == "whately") {
      data <- whately
    } else {
      data <- orchard
    }
    
    # Convert timestamps to local dates
    data$day <- as.Date(
      data$when,
      tz = "America/New_York"
    )
    
    # Filter the selected date range
    data <- data[
      !is.na(data$day) &
        data$day >= input$dates[1] &
        data$day <= input$dates[2],
    ]
    
    data
  })
  

# 3. Interactive time series plot
  
  output$weatherplot <- renderPlot({
    
    data <- filtered_weather()
    
    validate(
      need(
        nrow(data) > 0,
        "No observations in this date range."
      )
    )
    
    # Choose measurement
    data$value <- data[[input$measurement]]
    
    # Create time series plot
    p <- ggplot(
      data,
      aes(x = when, y = value)
    ) +
      geom_line(
        color = input$linecolor,
        linewidth = 0.5,
        na.rm = TRUE
      ) +
      labs(
        title = paste(
          ifelse(
            input$station == "whately",
            "Whately",
            "Orchard"
          ),
          "Weather Station"
        ),
        subtitle = paste(
          input$dates[1],
          "to",
          input$dates[2]
        ),
        x = "Date",
        y = names(measurements)[
          match(input$measurement, measurements)
        ]
      )
    
    # Add points if selected
    if (input$points) {
      p <- p +
        geom_point(
          color = input$linecolor,
          size = 0.5,
          alpha = 0.5,
          na.rm = TRUE
        )
    }
    
    # Show or hide grid
    if (input$grid) {
      p <- p + theme_minimal()
    } else {
      p <- p + theme_classic()
    }
    
    p
  })
  

# 4. Summary table
  
  output$summarytable <- renderTable({
    
    data <- filtered_weather()
    values <- data[[input$measurement]]
    
    data.frame(
      Station = ifelse(
        input$station == "whately",
        "Whately",
        "Orchard"
      ),
      Observations = nrow(data),
      Minimum = if (all(is.na(values))) {
        NA_real_
      } else {
        round(min(values, na.rm = TRUE), 2)
      },
      Maximum = if (all(is.na(values))) {
        NA_real_
      } else {
        round(max(values, na.rm = TRUE), 2)
      },
      Average = if (all(is.na(values))) {
        NA_real_
      } else {
        round(mean(values, na.rm = TRUE), 2)
      },
      Missing_Values = sum(is.na(values))
    )
    
  })
  

# 5. Irregularity observations
  
  output$observation <- renderText({
    
    if (
      input$station == "orchard" &&
      input$measurement == "temperature" &&
      input$dates[1] <= as.Date("2015-11-30") &&
      input$dates[2] >= as.Date("2015-11-20")
    ) {
      
      paste(
        "Observation: The Orchard weather station",
        "has unusual temperature readings in",
        "late November 2015. According to the",
        "MacLeish documentation, a loose wire",
        "caused erroneous temperature values."
      )
      
    } else {
      
      paste(
        "Explore different stations, date ranges,",
        "and measurements to investigate",
        "weather patterns and irregularities."
      )
      
    }
    
  })
  
}


# 6. Run the Shiny application

shinyApp(ui = ui, server = server)
