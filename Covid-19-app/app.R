library(shiny)
library(tidyverse)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trends in COVID19 Cases"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "states",
                        label = "State",
                        choices = sort(covid19$state),
                        selected = c("California", "Minnesota", "Massachusetts"),
                        multiple = TRUE),
            submitButton(text = "Create my plot!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("covidPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    options(scipen = 10)
    output$covidPlot <- renderPlot(
        covid19 %>% 
            group_by(state) %>% 
            filter(cases > 20,
                   state %in% input$states) %>% 
            mutate(days_since_20 = date - min(date)) %>% 
            ggplot(aes(x = days_since_20, y = cases, linetype = state, color = state)) +
            geom_line(size = 1.2) +
            scale_y_log10(labels = scales::comma) +
            scale_x_continuous() +
            labs(x = "Days since 20 cases",
                 y = "Total Cases (Log 10)",
                 linetype = "",
                 color = "")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
