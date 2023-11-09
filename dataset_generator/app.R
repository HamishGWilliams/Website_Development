# Purpose of App ----
# Making example data sets seems to be a tool and asset that would be useful
# for generating my own shinyapps and also for developing example datasets
# for use on statistical courses. 

# So I'm aiming to make this shiny app produce example datasets with 
# tailored correlation, variability, and additional variables for use in
# linear modelling, and potentially further beyond.

# To do this, I need 2 things to begin with:
  # 1. A "response" variable
    # - normally distributed
    # - can choose the mean and sd of the distribution
    # - use the rnorm() function to generate a normally distributed response.
  # 2. A "Explanatory" variable:
    # - Will be a length of n (can choose length)
    # - can be 'dependent'/'correlate' with the response
      # Can do this my muliplying the explanatory by a proportion of the response
      # i.e. explanatory = n * (Proportion [0:1] * response + variability [-n:n])

  # the explanatory variable will need to be the same length as the response
  # it will also need to randomly select a variability value [-n:n]

# Packages ----
library(shiny)

# Making an example dataset test: ----

explanatory <- rnorm(20, mean = 5, sd = 1)
explanatory <- seq(from = 0.5, to = 10, by = 0.5)
boxplot(explanatory)
response <- seq(from = 0.5, to = 10, by = 0.5)
response2 <- response*(0.5*explanatory) #+(runif(1, min = -2, max = 2)))
df <- data.frame(response2, explanatory)
plot(df$response2~df$explanatory)

# I *kinda* had the right idea, but was applying it wrong,
# So I generated the code with chatGPT:

# ChatGPT's code ----

# Set a seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Explanatory variable (independent variable)
explanatory_variable <- rnorm(n, mean = 0, sd = 1)

# Adjust the standard deviation to control the correlation
# Higher standard deviation leads to lower correlation
# Lower standard deviation leads to higher correlation
response_variable <- explanatory_variable + rnorm(n, mean = 0, sd = 2)

# Create a data frame
example_data <- data.frame(Explanatory = explanatory_variable, Response = response_variable)

# Scatter plot to visualize the correlation 
{
plot(example_data$Explanatory, example_data$Response, main = "Scatter Plot", 
     xlab = "Explanatory Variable", ylab = "Response Variable")

# Fit a linear model
linear_model <- lm(Response ~ Explanatory, data = example_data)

# Add the regression line to the plot
abline(linear_model, col = "white")
}

# ChatGPT adding in a categorical variable ----
{
# Set a seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Explanatory variable (independent variable)
explanatory_variable <- rnorm(n, mean = 0, sd = 1)

# Categorical variable
category <- factor(rep(c("Category A", "Category B"), each = n/2))

# Adjust the standard deviation to control the correlation
# Higher standard deviation leads to lower correlation
# Lower standard deviation leads to higher correlation
response_variable <- explanatory_variable + rnorm(n, mean = 0, sd = 0.5)

# Interaction term
interaction_term <- ifelse(category == "Category A", 0.5, -0.5)

# Adjust the response variable based on the interaction term
response_variable <- response_variable + interaction_term

# Create a data frame
example_data <- data.frame(
  Explanatory = explanatory_variable,
  Category = category,
  Response = response_variable
)

# Scatter plot to visualize the interaction
plot(example_data$Explanatory, example_data$Response, 
     col = as.numeric(example_data$Category),
     main = "Scatter Plot with Interaction",
     xlab = "Explanatory Variable", ylab = "Response Variable",
     pch = 19)

# Fit a linear model with interaction
linear_model <- lm(Response ~ Explanatory * Category, data = example_data)
anova <- anova(linear_model)
summary <- summary(linear_model)

# Add the regression lines to the plot
abline(coef(linear_model)[1:2], col = "red")
abline(coef(linear_model)[1:2] + coef(linear_model)[3], col = "blue")

# Add legend
legend("topright", legend = levels(example_data$Category), col = c("red", "blue"), pch = 19)
}


#  UI ----
ui <- fluidPage(

    # Application title
    titlePanel("Linear Model Interactable ShinyAPP"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of observations):",
                        min = 1,
                        max = 100,
                        value = 50,
                        step = 1),
            sliderInput("sd",
                        "Variability:",
                        min = 0,
                        max = 5,
                        value = 2.5,
                        step = 0.1),
            
            sliderInput("pch",
                        "Shape of datapoints:",
                        min = 1,
                        max = 19,
                        value = 1,
                        step = 1),
            
            sliderInput("size",
                        "Size of datapoints:",
                        min = 0.5,
                        max = 2,
                        value = 1,
                        step = 0.1),
            
            selectInput("colour", 
                        "Colour:", 
                        choices = c("Blue" = "#1f78b4",
                                    "Green" = "#33a02c",
                                    "Red" = "#e31a1c",
                                    "Purple" = "#6a3d9a",
                                    "Orange" = "#ff7f00",
                                    "Yellow" = "#ebdc00"),
                        selected = "#1f78b4")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot"),

           # lm outputs ----
           h2("Linear Model Statistcal outputs"),
           verbatimTextOutput("lm_output"),
                      
           # histogram code output ----
           h2("Histogram Code input"),
           verbatimTextOutput("codeOutput")

        )
    )
)

# Server ----
server <- function(input, output) {

  # Reactive expression for inputs ----
  data <- reactive({
    # Generate example datasets
    set.seed(123)
    explanatory_variable <- rnorm(input$n, mean = 0, sd = 1)
    response_variable <- explanatory_variable + rnorm(input$n, mean = 0, sd = input$sd)
    
    # Combine the datasets for plotting
    example_data <- data.frame(Explanatory = explanatory_variable, Response = response_variable)
    linear_model <- lm(Response ~ Explanatory, data = example_data)
    anova <- anova(linear_model)
    summary <- summary(linear_model)
    
    return(list(example_data = example_data,
                linear_model = linear_model,
                anova = anova,
                summary = summary))
  })
  
  # Render Plot ----
  output$Plot <- renderPlot({
    # draw the histogram with the specified number of bins
    plot(data()$example_data$Explanatory, data()$example_data$Response, main = "Scatter Plot", 
         xlab = "Explanatory Variable", ylab = "Response Variable",
         col = input$colour,
         pch = input$pch,
         cex = input$size)
    
    abline(coef(data()$linear_model)[1:2], col = input$colour, lwd = 2)
  })
  
  # Scatterplot Code ----
  output$codeOutput <- renderPrint({
    code <- sprintf(
      'plot(data = data, # select which "data" object to use\n
        %s, # The first argument - the Y-axis variable\n
        %s, # The second argument - the X-axis variable\n
        main = "%s", # Changes the name of the plot title\n     
        xlab = "%s", # Changes the X-axis name\n     
        ylab = "%s", # Changes the Y-axis name\n
        col = "%s", # HTML colour IDs\n    
        pch = %s, # Changes the shape of the datapoints \n
        cex = %s) # Changes the size of the datapoints',
      
      "Response_Variable",
      "Explanatory_Variable",
      "Scatter Plot",
      "Explanatory Variable",
      "Response Variable",
      input$colour,
      input$pch,
      input$size)
    
    cat(code)
    return(invisible())
  })
  
  # Render lm output ----
  output$lm_output <- renderPrint({
    cat("Linear Model Statistics:\n")
    cat("==================================\n")
    cat("p-value:", data()$anova$'Pr(>F)'[1], "# How well the explanatory explains the response", "\n")
    cat("R-Squared:", data()$summary$r.squared, "# How much of the variability the model explains", "\n")
    cat("Degrees of Freedom:", data()$summary$df[2], "\n")
    cat("==================================\n")
    
    # Interpret the results
    if (data()$anova$'Pr(>F)'[1] < 0.05) {
      cat("The model significantly explains the data.\n")
    } else {
      cat("There model does NOT explain the data.\n")
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
