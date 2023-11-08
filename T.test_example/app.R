# packages ----
library(shiny)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library(kableExtra)
library(thematic)

# Questions ----
questions <- list(
  list(
    question_text = "When you move the mean values apart, the p-value...",
    
    answer_choices = c("",
                       "Decreases", 
                       "Stays the same", 
                       "Increases", 
                       "dOeS a DaNce"),
    
    correct_answer = "Decreases"),
  list(
    question_text = "What does the 'Test Statistic' (or 'T value') represent?",
    
    answer_choices = c("",
                       "The shape of the histograms", 
                       "It is a different value to measure significance", 
                       "The mean value of each histogram", 
                       "The difference in the mean values of the distributions"),
    correct_answer = "It is a different value to measure significance"),
  list(
    question_text = "How does changing the 'Standard Deviation' (SD) 
    of the ditributions change their shape?",
    
    answer_choices = c("",
                       "They stay the same", 
                       "They affect the 'flatness/sharpness' of the distributions" , 
                       "They change the means of the distributions", 
                       "They change the distribution from normally distributed 
                       binomally distributed"),
    correct_answer = "They affect the 'flatness/sharpness' of the distributions")
  )

# Correct Answers number ----
target_correct_answers <- length(questions)

# UI ----
ui = fluidPage(
  
    # Change theme of page ----
    theme = bslib::bs_theme(bootswatch = "darkly"),

    # Application title
    titlePanel("T.Test Interactable ShinyAPP", windowTitle = "My Shiny App"),
    
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
        .sticky-title {
         position: fixed;
          top: 0;
          width: 100%;
          z-index: 1000;
        }
        .content {
          margin-left: 400px; /* Adjust the margin as needed */;
          margin-right: 100px;
        }
      "))
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 3,
        tabsetPanel(
          tabPanel("Global Options",
                   h4("Universal Options"),
                   sliderInput("bins",
                               "Number of bins:",
                               min = 1,
                               max = 30,
                               value = 15),
                   checkboxInput("show_ci", "Show 95% Confidence Intervals", value = FALSE),
                   
                   h5("Change Plot view"),
                   sliderInput("x_limit_min", "X-axis Minimum:", min = -10, max = 10, value = -5),
                   sliderInput("x_limit_max", "X-axis Maximum:", min = -10, max = 10, value = 5),
                   sliderInput("y_limit_max", "Y-axis Maximum:", min = 0, max = 50, value = 30)
          ),
          tabPanel("Distribution Options",
                   sliderInput("mean1",
                               "mean value of distribution 1:",
                               min = -3,
                               max = 3,
                               value = 0,
                               step = 0.1),
                   sliderInput("sd1",
                               "SD value of distribution 1:",
                               min = 1,
                               max = 3,
                               value = 1,
                               step = 0.1),
                   selectInput("color_group1", 
                               "Color for Group 1:", 
                               choices = c("Blue" = "#1f78b4",
                                           "Green" = "#33a02c",
                                           "Red" = "#e31a1c",
                                           "Purple" = "#6a3d9a",
                                           "Orange" = "#ff7f00"),
                               selected = "#1f78b4"),
                   
                   sliderInput("mean2",
                               "mean value of distribution 2:",
                               min = -3,
                               max = 3,
                               value = 1,
                               step = 0.1),      
                   sliderInput("sd2",
                               "SD value of distribution 2:",
                               min = 1,
                               max = 2,
                               value = 1,
                               step = 0.1),
                   selectInput("color_group2", 
                               "Color for Group 2:", 
                               choices = c("Blue" = "#1f78b4",
                                           "Green" = "#33a02c",
                                           "Red" = "#e31a1c",
                                           "Purple" = "#6a3d9a",
                                           "Orange" = "#ff7f00"),
                               selected = "#e31a1c")
          ),
          tabPanel("Answer Checker",
                   h2("Answer Checker"),
                   h3("Results"),
                   textOutput("result_text"),
                   br(),
                   h3("Correct Answers"),
                   textOutput("correct_answers_text"), 
                   br(),
                   h3("Final Result"),
                   textOutput("final_result_text"),
                   br(),
                   actionButton("reset_button", "Reset Answers")
          )
        )
      ),

        # Show a plot of the generated distribution
        mainPanel(
          
          class = "content",
          
           plotOutput("distPlot",
                      height = "500px"),
           
           # T-test results output ----
           h2("T.Test Results"),
           verbatimTextOutput("t_test_output"),
           
           # histogram code output ----
           h2("Histogram Code input"),
           verbatimTextOutput("codeOutput"),

           
           h2("How to use this ShinyAPP"),
           p("This ShinyAPP (which is a package in r) allows a user to generate 
             interactable HTML based 'websites' which can be shared with others
             as a learning tool, and serves as another example of how useful
             coding can be!"),
           p("Using this interactable session will show you the visual links between 
             distributions of data and the corresponding T.Test results from 
            comparing these two distributions."),
           strong("Click the Sidebar Tab options to move between global options which 
             affect the plotting options; distribution options which alter the 
             distributions of the groups; and an answer checker for the 
             short quiz at the end!"),
           br(),
           br(),
           p("You can adjust the mean (average) and standard deviation (sd) 
             values in the left hand column of each group in the dataset, 
             which will impact the shape of each group independently."),
           p("You can also change the 'breaks' of the histogram plot in the 
             'Global Options' section, which changes how many individual 
             bars will represent the histogram. In other words, it changes 
             how precisely you histogram is visualised"),
           strong("Try moving the mean values around!"),
           
           h1("Quiz"),
           
           h2("Question 1"),
           h3("When you move the mean values apart, the p-value..."),
           selectInput("student_answer1", "Select an answer:", choices = questions[[1]]$answer_choices),
           useShinyjs(),  # Initialize shinyjs
           actionButton("showHintBtn1", "Show Hint"),
           div(id = "hintBox1", style = "display: none;",
               h4("Hint: Move the means further apart and watch the P-VALUE!")),
           actionButton("check_button1", "Check Answer"),
           
           br(),
          
           h2("Question 2"),
           h3("What does the 'Test Statistic' (or 'T value') represent?"),
           selectInput("student_answer2", "Select an answer:", choices = questions[[2]]$answer_choices),
           useShinyjs(),  # Initialize shinyjs
           actionButton("showHintBtn2", "Show Hint"),
           div(id = "hintBox2", style = "display: none;",
               h4("Hint: When the distributions move apart, the t value increases, 
                  what could this mean?")),
           actionButton("check_button2", "Check Answer"),
          
          br(),
          
          h2("Question 3"),
          h3("How does changing the 'Standard Deviation' (SD) of the ditributions change their shape?"),
          selectInput("student_answer3", "Select an answer:", choices = questions[[3]]$answer_choices),
          useShinyjs(),  # Initialize shinyjs
          actionButton("showHintBtn3", "Show Hint"),
          div(id = "hintBox3", style = "display: none;",
              h4("Hint: Move the SD sliders in the 'Distribution options' tab 
                 and watch how the ditributions change")),
          actionButton("check_button3", "Check Answer"),
           
           # Added lines at end for scrolling
           
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br()
           
        )
    )
)

# Server Logic ----
server <- function(input, output, session) {
  
  # Correct answers reactive input ----
  correct_answers <- reactiveVal(0)
  
  # Final Answers Reactive input ----
  final_result_text <- reactiveVal("")

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
  
  # Set to theme of page ----
  thematic::thematic_shiny()
  
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
          scale_fill_manual(values = c(input$color_group1, input$color_group2))  # Set fill colors manually
          
        if (input$show_ci) {
          p <- p +
            geom_vline(xintercept = ci_group1, linetype = "dashed", color = input$color_group1, alpha = 0.5) +
            geom_vline(xintercept = ci_group2, linetype = "dashed", color = input$color_group2, alpha = 0.5)
        }
        
        p +
          labs(title = "Comparison of Two Normal Distributions",
               x = "Value", y = "Frequency") +
          theme_bw()
        
        p + ylim(0, input$y_limit_max)
        
        p + coord_cartesian(xlim = c(input$x_limit_min, input$x_limit_max)) 
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
      cat("p-value:", t_test_results()$p.value, "# Significance of the difference", "\n")
      cat("Test Statistic:", t_test_results()$statistic, "# Difference between group 1 and group 2", "\n")
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
        'hist(data = data, # select which "data" object to use\n     
        breaks = %d, # Number of "breaks" \n     
        col = c("%s", "%s"), # HTML colour IDs\n     
        main = "%s", # Changes the name of the plot title\n     
        xlab = "%s", # Changes the X-axis name\n     
        ylab = "%s") # Changes the Y-axis name\n',
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
        correct_answers(correct_answers() + 1)
        result <- "Correct!"
      } else {
        result <- "Incorrect. Try again."
      }
      
      output$result_text <- renderText(result)
      output$correct_answers_text <- renderText(paste("Correct Answers: ", correct_answers()))
      
      if (correct_answers() == target_correct_answers) {
        final_result_text("Congratulations! You got all the answers correct!")
      } else {
        final_result_text("")
      }
    }
    
    # Final result text output ----
    output$final_result_text <- renderText(final_result_text())
    
    # "Check Answer" Buttons ----
    observeEvent(input$check_button1, {
      checkAnswer(input$student_answer1, questions[[1]]$correct_answer)
    })
    
    observeEvent(input$check_button2, {
      checkAnswer(input$student_answer2, questions[[2]]$correct_answer)
    })
    
    observeEvent(input$check_button3, {
      checkAnswer(input$student_answer3, questions[[3]]$correct_answer)
    })
    
    # "Reset Answers" Button ----
    observeEvent(input$reset_button, {
      correct_answers(0)
      final_result_text("")
    })
    
    # Hint Boxes ----
    observeEvent(input$showHintBtn1, {
      shinyjs::toggle("hintBox1")
    })
    observeEvent(input$showHintBtn2, {
      shinyjs::toggle("hintBox2")
    })
    observeEvent(input$showHintBtn3, {
      shinyjs::toggle("hintBox3")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
