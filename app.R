# Homework Question 2
# Penguin Measurements Explorer - Shiny App
# Dataset: palmerpenguins

library(shiny)
library(ggplot2)
library(palmerpenguins)

# Load the penguins dataset
penguin_data <- palmerpenguins::penguins

# Available measurements
measurements <- c(
  "Bill Length (mm)" = "bill_length_mm",
  "Bill Depth (mm)" = "bill_depth_mm",
  "Flipper Length (mm)" = "flipper_length_mm",
  "Body Mass (g)" = "body_mass_g"
)


# 1. User Interface

ui <- fluidPage(
  
  titlePanel("Penguin Measurements Explorer"),
  
  p("Explore penguin measurements by species and sex."),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Widget 1: Select species
      selectInput(
        "species",
        "Select Penguin Species:",
        choices = c(
          "All Species",
          levels(penguin_data$species)
        ),
        selected = "All Species"
      ),
      
      # Widget 2: Select sex
      radioButtons(
        "sex",
        "Select Penguin Sex:",
        choices = c(
          "All",
          "Male",
          "Female"
        ),
        selected = "All"
      ),
      
      # Widget 3: X-axis measurement
      selectInput(
        "xvar",
        "Select X-Axis Measurement:",
        choices = measurements,
        selected = "flipper_length_mm"
      ),
      
      # Widget 4: Y-axis measurement
      selectInput(
        "yvar",
        "Select Y-Axis Measurement:",
        choices = measurements,
        selected = "body_mass_g"
      ),
      
      # Widget 5: Trend line
      checkboxInput(
        "trend",
        "Show Trend Line",
        value = FALSE
      ),
      
      # Widget 6: Point size
      sliderInput(
        "point_size",
        "Scatterplot Point Size:",
        min = 1,
        max = 6,
        value = 3,
        step = 0.5
      )
      
    ),
    
    mainPanel(
      
      h3("Penguin Measurements Scatterplot"),
      
      plotOutput(
        "penguinplot",
        height = "420px"
      ),
      
      h3("Summary Statistics"),
      
      tableOutput("summarytable"),
      
      h3("Penguin Measurements Data"),
      
      tableOutput("penguintable")
      
    )
  )
)


# 2. Server

server <- function(input, output, session) {
  
  # Filter the dataset reactively
  filtered_penguins <- reactive({
    
    data <- penguin_data
    
    # Filter by species
    if (input$species != "All Species") {
      data <- data[
        !is.na(data$species) &
          as.character(data$species) == input$species,
      ]
    }
    
    # Filter by sex
    if (input$sex != "All") {
      data <- data[
        !is.na(data$sex) &
          as.character(data$sex) == tolower(input$sex),
      ]
    }
    
    # Remove missing measurements for selected axes
    data <- data[
      !is.na(data[[input$xvar]]) &
        !is.na(data[[input$yvar]]),
    ]
    
    return(data)
  })
  

# 3. Scatterplot
  
  output$penguinplot <- renderPlot({
    
    data <- filtered_penguins()
    
    validate(
      need(
        nrow(data) > 0,
        "No penguins match the selected filters."
      )
    )
    
    p <- ggplot(
      data,
      aes(
        x = .data[[input$xvar]],
        y = .data[[input$yvar]],
        color = species
      )
    ) +
      geom_point(
        size = input$point_size,
        alpha = 0.75
      ) +
      labs(
        title = "Penguin Measurements",
        subtitle = paste(
          "Species:", input$species,
          "| Sex:", input$sex
        ),
        x = names(measurements)[
          match(input$xvar, measurements)
        ],
        y = names(measurements)[
          match(input$yvar, measurements)
        ],
        color = "Species"
      ) +
      theme_minimal()
    
    # Add optional trend line
    if (
      input$trend &&
      nrow(data) >= 2 &&
      length(unique(data[[input$xvar]])) >= 2
    ) {
      p <- p +
        geom_smooth(
          aes(group = 1),
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          color = "black"
        )
    }
    
    p
  })
  

# 4. Summary Statistics
  
  output$summarytable <- renderTable({
    
    data <- filtered_penguins()
    
    data.frame(
      Number_of_Penguins = nrow(data),
      Average_Bill_Length = round(
        mean(data$bill_length_mm, na.rm = TRUE), 2
      ),
      Average_Flipper_Length = round(
        mean(data$flipper_length_mm, na.rm = TRUE), 2
      ),
      Average_Body_Mass = round(
        mean(data$body_mass_g, na.rm = TRUE), 2
      )
    )
    
  })
  
  
# 5. Penguin Measurements Table
  
  output$penguintable <- renderTable({
    
    data <- filtered_penguins()
    
    data.frame(
      Species = data$species,
      Sex = data$sex,
      Bill_Length = data$bill_length_mm,
      Bill_Depth = data$bill_depth_mm,
      Flipper_Length = data$flipper_length_mm,
      Body_Mass = data$body_mass_g
    )
    
  })
  
}


# 6. Run Shiny App

shinyApp(ui = ui, server = server)
