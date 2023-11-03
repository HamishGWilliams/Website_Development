library(shiny)
getwd()
# Define UI ----
ui <- fluidPage(
             
             titlePanel("Title"),
             
             sidebarLayout(
               sidebarPanel(                 
                 img(src = "rstudio.png")), 
               mainPanel(imageOutput("outputImage"))
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # Render image
  output$outputImage <- renderImage({
    # Return a list containing the src attribute
    list(src = "./www/rstudio.png",
         alt = "Sample Image",
         width = 200, height = 100)
  }, deleteFile = FALSE)
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
