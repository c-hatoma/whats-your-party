library(shiny)

ui <- fluidPage(
  
    titlePanel("Tell us about yourself and we'll guess your political affiliation"),
    mainPanel(
        tabsetPanel(
            tabPanel("tab title", 
                     selectInput(InputId = "sex",
                                 label = "Sex",
                                 choices = c("Male", "Female", "Neither/Prefer Not to Answer")),
                     numericInput(inputId = "age",
                                  label = "Age",
                                  min = 18,
                                  max = 88,
                                  step = 1),
                     selectInput(inputId = "race",
                                 label = "Race",
                                 choices = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Asian,non-Hispanic", "Other")),
                     sliderInput(inputId = "income",
                                 label = "Family Income",
                                 min = 0,
                                 max = 1000000,
                                 value = 30000,
                                 animate = TRUE,
                                 sep = ""),
                     selectInput(inputId = "education",
                                 label = "Education Level",
                                 choices = c("College graduate", "Postgraduate", "Some college", "High school or less")),
                    radioButton(inputId = "married",
                                label = "Are you married?",
                                choices = c("Yes", "No")),
                    radioButton(inputId = "community",
                                label = "How would you describe your community?",
                                choices = c("Urban", "Suburban", "Rural")),
                    
                    # submitButton("Submit"),
                    
                    textOutput(outputId = "prediction")
    
))))

server <- function(input, output, session) {
  
    output$prediction <- renderText({
        
        test
        
    })
    
}

shinyApp(ui, server)