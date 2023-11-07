# packages ----
library(shiny)
library(ggplot2)
library(shinyjs)

# Questions ----
questions <- list(
  list(
    question_text = "When you move the mean values apart, the p-value...",
    
    answer_choices = c("","Decreases", "Stays the same", "Increases", "dOeS a DaNce"),
    
    correct_answer = "Increases")
  )

# UI ----
ui = fluidPage(

    # Application title
    titlePanel("T.Test Interactable Example"),
    
    # Add this block to include custom CSS for increasing font size
    tags$head(
      tags$style(
        HTML("
        #t_test_output {
          font-size: 12px;  # Adjust the font size as needed
        }
        #codeOutput {
          font-size: 12px; # Adjust the font size as needed
        }
      ")
      )
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          h2("Universal Options"),
          
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            checkboxInput("show_ci", "Show 95% Confidence Intervals", value = TRUE),
            
            h2("Group 1"),
            
            # Distribution 1
            sliderInput("mean1",
                        "mean value of distribution 1:",
                        min = -2,
                        max = 2,
                        value = 0,
                        step = 0.1),
            sliderInput("sd1",
                        "SD value of distribution 1:",
                        min = 1,
                        max = 5,
                        value = 1,
                        step = 0.1),
            selectInput("color_group1", 
                        "Color for Group 1:", 
                        choices = c("Blue" = "#1f78b4",   # Colorbrewer blue
                                    "Green" = "#33a02c",  # Colorbrewer green
                                    "Red" = "#e31a1c",    # Colorbrewer red
                                    "Purple" = "#6a3d9a", # Colorbrewer purple
                                    "Orange" = "#ff7f00"),# Colorbrewer orange
                        selected = "#1f78b4"),         # Default: blue
            
            h2("Group 2"),
            
            # Distribution 2
            sliderInput("mean2",
                        "mean value of distribution 2:",
                        min = -2,
                        max = 2,
                        value = 1,
                        step = 0.1),      
          sliderInput("sd2",
                      "SD value of distribution 2:",
                      min = 1,
                      max = 5,
                      value = 1,
                      step = 0.1),
            
            selectInput("color_group2", 
                        "Color for Group 2:", 
                        choices = c("Blue" = "#1f78b4",   # Colorbrewer blue
                                    "Green" = "#33a02c",  # Colorbrewer green
                                    "Red" = "#e31a1c",    # Colorbrewer red
                                    "Purple" = "#6a3d9a", # Colorbrewer purple
                                    "Orange" = "#ff7f00"),# Colorbrewer orange
                        selected = "#e31a1c"), # Default: red
          
          br(),
          
          h2("Answer Checker"),
          h3("Results"),
          textOutput("result_text")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",
                      height = "300px"),
           h2("Histogram Code input"),
           verbatimTextOutput("codeOutput"),
           h2("T.Test Results"),
           verbatimTextOutput("t_test_output"),
           
           h2("How to use this ShinyAPP"),
           p("This ShinyAPP (which is a package in r) allows a user to generate 
             interactable HTML based 'websites' which can be shared with others
             as a learning tool, and serves as another example of how useful
             coding can be!"),
           p("Using this interactable session will show you the visual links between 
             distributions of data and the corresponding T.Test results from 
            comparing these two distributions."),
           p("You can adjust the mean (average) and standard deviation (sd) 
             values in the left hand column of each group in the dataset, 
             which will impact the shape of each group independently."),
           p("You can also change the 'breaks' of the histogram plot in the 
             'Global Options' section, which changes how many individual 
             bars will represent the histogram. In other words, it changes 
             how precisely you histogram is visualised"),
           strong("Try moving the mean values around!"),
           
           h2("Question 1"),
           h3("When you move the mean values apart, the p-value..."),
           selectInput("student_answer1", "Select an answer:", choices = questions[[1]]$answer_choices),
           useShinyjs(),  # Initialize shinyjs
           actionButton("showHintBtn", "Show Hint"),
           div(id = "hintBox", style = "display: none;",
               h4("Hint: Move the means further apart and watch the P-VALUE!")),
           actionButton("check_button1", "Check Answer")
        )
    )
)

# Server Logic ----
server <- function(input, output) {

  # Reactive expression to generate normal distributions based on the slider input
  generate_distributions <- reactive({
    n <- 100
    mean_group1 <- input$mean1
    mean_group2 <- input$mean2
    sd_group1 <- input$sd1
    sd_group2 <- input$sd2
    
    # Generate example datasets
    set.seed(123)
    group1 <- rnorm(n, mean = mean_group1, sd = sd_group1)
    group2 <- rnorm(n, mean = mean_group2, sd = sd_group2)
    
    # Combine the datasets for plotting
    combined_data <- data.frame(
      Value = c(group1, group2),
      Group = rep(c("Group 1", "Group 2"), each = n)
    )
    
    return(list(combined_data = combined_data, 
                mean_group1 = mean_group1, 
                mean_group2 = mean_group2))
  })
  
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- generate_distributions()$combined_data$Value
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # Calculate 95% confidence intervals for each group
        ci_group1 <- quantile(generate_distributions()$combined_data$Value[generate_distributions()$combined_data$Group == "Group 1"], c(0.025, 0.975))
        ci_group2 <- quantile(generate_distributions()$combined_data$Value[generate_distributions()$combined_data$Group == "Group 2"], c(0.025, 0.975))

        # draw the histogram with the specified number of bins
        p <- ggplot(generate_distributions()$combined_data, aes(x = Value, fill = Group)) +
          geom_histogram(position = "identity", alpha = 0.7, bins = input$bins, color = "white") +
          geom_vline(xintercept = input$mean1, linetype = "dashed", color = input$color_group1, linewidth = 1) +
          geom_vline(xintercept = input$mean2, linetype = "dashed", color = input$color_group2, linewidth = 1) +
          scale_fill_manual(values = c(input$color_group1, input$color_group2)) # Set fill colors manually
          
        if (input$show_ci) {
          p <- p +
            geom_vline(xintercept = ci_group1, linetype = "dashed", color = input$color_group1, alpha = 0.5) +
            geom_vline(xintercept = ci_group2, linetype = "dashed", color = input$color_group2, alpha = 0.5)
        }
        
        p +
          labs(title = "Comparison of Two Normal Distributions",
               x = "Value", y = "Frequency") +
          theme_bw()
    })
    
    # Reactive expression to perform t-test based on the generated datasets
    t_test_results <- reactive({
      t_test_result <- t.test(
        generate_distributions()$combined_data$Value ~ generate_distributions()$combined_data$Group
      )
      
      return(t_test_result)
    })
    
    # Render t-test output
    output$t_test_output <- renderPrint({
      cat("Independent Samples t-test Results:\n")
      cat("==================================\n")
      cat("p-value:", t_test_results()$p.value, "\n")
      cat("Test Statistic:", t_test_results()$statistic, "\n")
      cat("Degrees of Freedom:", t_test_results()$parameter, "\n")
      cat("==================================\n")
      
      # Interpret the results
      if (t_test_results()$p.value < 0.05) {
        cat("The means of the two groups are significantly different.\n")
      } else {
        cat("There is no significant difference in the means of the two groups.\n")
      }
    })
    
    # Histogram Code Text ----
    output$codeOutput <- renderPrint({
      code <- sprintf(
        'hist(data = data,\n     breaks = %d,\n     col = c("%s", "%s"),\n     main = "%s",\n     xlab = "%s",\n     ylab = "%s")\n',
        input$bins,
        input$color_group1,
        input$color_group2,
        "Comparison of Two Normal Distributions",
        "Value",
        "Frequency"
      )
      cat(code)
      return(invisible())
    })
    
    # Checking Answers ----
    checkAnswer <- function(answer, correct_answer = NULL) {
      if (tolower(answer) %in% c(tolower(correct_answer))) {
        result <- "Correct!"
      } else {
        result <- "Incorrect. Try again."
      }
      output$result_text <- renderText(result)
      output$correct_answers_text <- renderText(paste("Correct Answers: ", correct_answers()))
    }
    
    # "Check Answer" Buttons ----
    observeEvent(input$check_button1, {
      checkAnswer(input$student_answer1, questions[[1]]$correct_answer)
    })
    
    # Hint Box ----
    observeEvent(input$showHintBtn, {
      shinyjs::toggle("hintBox")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
