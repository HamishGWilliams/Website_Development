# Install and load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(shinyjs)) install.packages("shinyjs")

library(shiny)
library(shinyjs)

# Define the answer object
answer_object <- "Option B"

answer_choices <- c("Option A", "Option B", "Option C")

# Define questions, answer choices, and correct answers
questions <- list(
  list(
    question_text = "What is the capital of France?",
    answer_choices = c("Paris", "Berlin", "London"),
    correct_answer = "Paris"
  ),
  list(
    question_text = "What is the largest planet in our solar system?",
    answer_choices = c("Mars", "Jupiter", "Venus"),
    correct_answer = "Jupiter"
  )
)

# Set the target number of correct answers
target_correct_answers <- 4

# Define the UI
ui <- fluidPage(
  titlePanel("Workshop 1"),
  
  tags$head(
    tags$style(HTML("
      .sticky {
        position: -webkit-sticky;
        position: sticky;
        top: 0;
        padding: 10px;
        background-color: #f1f1f1;
        border: 1px solid #d4d4d4;
      }
    "))
  ),
  
  fluidRow(
    column(
      width = 3,
      id = "sidebar",
      class = "sticky",
      h2("Answer Checker"),
      id = "sidebarPanel",  
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
    ),
    column(
      width = 9,
      h2("Question 1"),
      selectInput("student_answer1", "Select an answer:", choices = questions[[1]]$answer_choices),
      actionButton("check_button1", "Check Answer"),
      br(),
      h2("Question 2"),
      textInput("text_answer2", "Enter your answer:"),
      actionButton("check_button2", "Check Answer"),
      br(),
      h2("Question 3"),
      selectInput("student_answer3", "Select an answer:", choices = questions[[1]]$answer_choices),
      actionButton("check_button3", "Check Answer"),
      br(),
      h2("Question 4"),
      selectInput("student_answer4", "Select an answer:", choices = questions[[2]]$answer_choices),
      actionButton("check_button4", "Check Answer"),
      br()
    )
  )
)

# Define the server
server <- function(input, output, session) {
  correct_answers <- reactiveVal(0)
  final_result_text <- reactiveVal("")
  
  observeEvent(input$reset_button, {
    correct_answers(0)
    final_result_text("")
  })
  
  checkAnswer <- function(answer, correct_answer = NULL) {
    if (tolower(answer) %in% c(tolower(answer_object), 
                               "your correct answer goes here", 
                               tolower(correct_answer))) {
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
  
  observeEvent(input$check_button1, {
    checkAnswer(input$student_answer1, questions[[1]]$correct_answer)
  })
  
  observeEvent(input$check_button2, {
    checkAnswer(input$text_answer2)
  })
  
  observeEvent(input$check_button3, {
    checkAnswer(input$student_answer3, questions[[1]]$correct_answer)
  })
  
  observeEvent(input$check_button4, {
    checkAnswer(input$student_answer4, questions[[2]]$correct_answer)
  })
  
  output$final_result_text <- renderText(final_result_text())
}

# Run the app
shinyApp(ui, server)
