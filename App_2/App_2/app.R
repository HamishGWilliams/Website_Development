# Install and load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(shinyjs)) install.packages("shinyjs")

library(shiny)
library(shinyjs)

# Data ----
penguins <- read.table("./data/penguins.txt", header = T, sep = "\t")
adelie <- subset(penguins, species == "Adelie")
Chinstrap <- subset(penguins, species == "Chinstrap")
Gentoo <- subset(penguins, species == "Gentoo")


# Define questions, answer choices, and correct answers
questions <- list(
  list(
    question_text = "How would you describe the distribution of the three 
    species data combined?",
    
    answer_choices = c("Symmetrical, they are normally distrbuted", 
                       "Non-Symmetrical, their central tendency is towards the left,
                       and the distribution has a tail to the right (skewed)"),
    
    correct_answer = "Non-Symmetrical, their central tendency is towards the left,
                       and the distribution has a tail to the right (skewed)"
  ),
  list(
    question_text = "Are the distributions of the three
    species seperately similar to normal distributions?",
    
    answer_choices = c("Yes, they look normally distributed when separated", 
                       "No, there is no change to the central tendency of the 
                       distribution of any of the species", 
                       "Adelie Penguins now have a normal disrtibution, but
                       the other species remain non-normally distributed"),
    
    correct_answer = "Yes, they look normally distributed when separated"
  )
)

# Set the target number of correct answers
target_correct_answers <- 2

# Define the UI
ui <- fluidPage(
  titlePanel("Workshop 1: Probabilities"),
  
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
      id = "sidebarPanel",  
      
      # Sidebar options here:
        # Learning Pbjectives
      h2("Learning Objectives"),
      
      tags$ul(
        tags$li("Computation and Understading of",
                strong("quantiles"),
                "and",
                strong("range intervals")),
        tags$li(strong("Assessing Normalilty"),
                "visualy using Density and Q-Q plots"),
        tags$li("Familiarity with approximate inference using descriptive statistics"),
        tags$li("Distinguishing probability distribution of a sample vs.
                probability distribution of estimator (and SD vs. SE of the mean")
      ),
  
        # Answer Checker
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
    ),
  
      column(
      width = 9,
      
      # Main Panel Here:
      
      h1("Instructions"),
      br(),
      p("Here we look at probability distributions, using a dataset on penguins 
        that was madeavailable by Dr. Kristen Gorman and the Palmer Station, 
        Antarctica Long Term EcologicalResearch Network. 
        There are data on three species of penguins, collected from threeislands 
        in the Palmer Archipelago. We will work with this dataset for several 
        differentexercises during the course.We would like you to use RStudio. 
        If you have not used this before, please go to theinstructions on 
        Getting Started with R and RStudio."),
      
      h2("Getting Started"),
      br(),
      h3("Set Working Directory"),
      p("If you haven’t already created a folder for the course on your 
        computer or your universitydrive, do so now. You might create a folder 
        for the course (e.g., BI3010) with subfolderswithin that for each of 
        the workshops (e.g., workshop1)."),
      br(),
      strong("To set the working directory to the folder you want to use for 
             saving your work, usethe drop down menu in RStudio: session > set 
             working directory > choose directory"),
      br(),
      br(),
      code("getwd() #to check which directory RStudio is using", 
           style = "font-size:16px;"),
      br(),
      h3("Donload the Data file"),
      p(strong("Download the datafile from MyAberdeen to your working directory."),
        "You will find thefile in the module for Topic One, folder for Workshop 
        1, document labelled Data File. Usingthe three dots to the right of the 
        content area that has the file name, download the file andsave it to 
        your working directory"),
      h3("Load file into RStudio"),
      p("In order to load the file into RStudio, you need to execute the 
        following line of code"),
      code('penguins<-read.table("penguins.txt",header=TRUE)', 
           style = "font-size:16px;"),
      br(),
      br(),
      p('If you get an error, some common problems are as follows: you have
        misspelled one of thewords or used a capital letter instead of lower 
        case; your file is missing a column heading;the file is not the correct 
        format (e.g, use a TAB delimited file); the quotation marks are inthe in 
        wrong location; you have missed off the final bracket “)”.'),
      p("To confirm that the file has been read properly, double click the 
        file name in the upper rightpanel or read how many observations and 
        variables are there. In this case, the file has 333observations of 9 
        variables."),
      h2("Proability Distribution of the data"),
      p("Let’s visualise all the data by plotting a histogram for male and 
        female heights combined.Then, plot histograms for each species separately."),
      code("hist(penguins$body_mass_g) # be sure that you include the file name 
           and variable name spelled correctly", style = "font-size:16px;"),
      
      plotOutput("histogram1", height = "400px", width = "800px"),
      
      # Centered slider input
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        sliderInput("bins1", "Number of Bins:", min = 1, max = 30, value = 10)),
      
      p("You can change the number of breaks in a histogram by using the 
        'breaks' option when generating the plot:"),
      code("hist(penguins$body_mass_g, breaks = 24",
           style = "font-size:16px;"),
      br(),
      br(),
      p("In this example, you can use the 'Breaks' slider in the side panel to
        visualise this in action"),
      
      # Question 1
      h2("Question 1"),
      selectInput("student_answer1", "Select an answer:", choices = questions[[1]]$answer_choices),
      actionButton("check_button1", "Check Answer"),
      br(),
      
      # Question 2
      h2("Question 2"),
      selectInput("student_answer2", "Select an answer:", choices = questions[[2]]$answer_choices),
      useShinyjs(),  # Initialize shinyjs
      actionButton("showHintBtn", "Show Hint"),
      div(id = "hintBox", style = "display: none;",
          h4("Hint: This is a helpful hint!"),
          p("Additional information goes here.")),
      actionButton("check_button2", "Check Answer"),
      br(),

      fluidRow(
        column(4,  # Adjust the width of the first column as needed
               plotOutput("histogram2", height = 300)
        ),
        
        column(4,  # Adjust the width of the second column as needed
               plotOutput("histogram3", height = 300)
        ),
        
        column(4,  # Adjust the width of the third column as needed
               plotOutput("histogram4", height = 300)
        )
      ),
      
      tags$div(
        sliderInput("bins2", "Number of Bins:", min = 1, max = 35, value = 10)),
    )
  ),
    
    tags$head(
      tags$style(
        HTML(
          "
        .selectize-input {
          height: 100px; width: 500px /* Adjust the height as needed */
        }
        "
    )
  )
)
)

# Server ----
server <- function(input, output, session) {
  correct_answers <- reactiveVal(0)
  final_result_text <- reactiveVal("")
  
  observeEvent(input$reset_button, {
    correct_answers(0)
    final_result_text("")
  })
  
  # Show/hide the hint box
  observeEvent(input$showHintBtn, {
    shinyjs::toggle("hintBox")
  })
  
  checkAnswer <- function(answer, correct_answer = NULL) {
    if (tolower(answer) %in% c(tolower(answer_object),
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
    checkAnswer(input$student_answer2, questions[[2]]$correct_answer)
  })
  
  output$final_result_text <- renderText(final_result_text())

  # Render histogram plot of penguins
  output$histogram1 <- renderPlot({
    hist(penguins$body_mass_g, main = "Histogram of Body Mass",
         xlab = "Body Mass (g)", col = "grey", border = "black", breaks = input$bins1)
  })
  
  # Render histogram plot of Adelie penguins
  output$histogram2 <- renderPlot({
    hist(adelie$body_mass_g, main = "Histogram of Adelie penguin Body Mass",
         xlab = "Body Mass (g)", col = "grey", border = "black", breaks = input$bins2)
  })
  
  # Render histogram plot of Gentoo penguins
  output$histogram3 <- renderPlot({
    hist(Gentoo$body_mass_g, main = "Histogram of Gentoo penguin Body Mass",
         xlab = "Body Mass (g)", col = "grey", border = "black", breaks = input$bins2)
  })
  # Render histogram plot of Chinstrap penguins
  output$histogram4 <- renderPlot({
    hist(Chinstrap$body_mass_g, main = "Histogram of Adelie penguin Body Mass",
         xlab = "Body Mass (g)", col = "grey", border = "black", breaks = input$bins2)
  })
  
}

# Run the app
shinyApp(ui, server)
