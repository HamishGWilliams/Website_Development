# packages ----
library(shiny)
library(ggplot2)

# data ----

# Set a seed for reproducibility
set.seed(123)

# Generate two example datasets
n <- 100  # Number of observations in each group

# Example 1: Normal distribution with mean 0 and standard deviation 1
group1 <- rnorm(n, mean = 0, sd = 1)

# Example 2: Normal distribution with mean 1 (slightly shifted) and standard deviation 1
group2 <- rnorm(n, mean = 1, sd = 1)

# Perform independent samples t-test
t_test_result <- t.test(group1, group2)

# Display the results
cat("Independent Samples t-test Results:\n")
cat("==================================\n")
cat("p-value:", t_test_result$p.value, "\n")
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")
cat("==================================\n")

# Interpret the results
if (t_test_result$p.value < 0.05) {
  cat("The means of the two groups are significantly different.\n")
} else {
  cat("There is no significant difference in the means of the two groups.\n")
}

# Plot the distributions
{
par(mfrow = c(1, 2))
hist(group1, col = "skyblue", main = "Group 1 (Mean = 0)")
hist(group2, col = "lightcoral", main = "Group 2 (Mean = 1)")
par(mfrow = c(1,1))
}

# Combine the datasets for plotting
combined_data <- data.frame(
  Value = c(group1, group2),
  Group = rep(c("Group 1", "Group 2"), each = n)
)

# Calculate means
mean_group1 <- mean(group1)
mean_group2 <- mean(group2)

ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 20, color = "white") +
  geom_vline(xintercept = mean_group1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_group2, linetype = "dashed", color = "blue", linewidth = 1) +
  labs(title = "Comparison of Two Normal Distributions",
       x = "Value", y = "Frequency") +
  theme_minimal()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("T.Test Interactable Example"),

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

            
            selectInput("color_group2", 
                        "Color for Group 2:", 
                        choices = c("Blue" = "#1f78b4",   # Colorbrewer blue
                                    "Green" = "#33a02c",  # Colorbrewer green
                                    "Red" = "#e31a1c",    # Colorbrewer red
                                    "Purple" = "#6a3d9a", # Colorbrewer purple
                                    "Orange" = "#ff7f00"),# Colorbrewer orange
                        selected = "#e31a1c")          # Default: red
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",
                      height = "500px"),
           verbatimTextOutput("t_test_output")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Reactive expression to generate normal distributions based on the slider input
  generate_distributions <- reactive({
    n <- 100
    mean_group1 <- input$mean1
    mean_group2 <- input$mean2
    
    # Generate example datasets
    set.seed(123)
    group1 <- rnorm(n, mean = mean_group1, sd = 1)
    group2 <- rnorm(n, mean = mean_group2, sd = 1)
    
    # Combine the datasets for plotting
    combined_data <- data.frame(
      Value = c(group1, group2),
      Group = rep(c("Group 1", "Group 2"), each = n)
    )
    
    return(list(combined_data = combined_data, mean_group1 = mean_group1, mean_group2 = mean_group2))
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
