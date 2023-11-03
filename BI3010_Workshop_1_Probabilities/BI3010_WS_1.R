# Packages ----
library(shiny)
  # install.packages("shinyjs")
library(shinyjs)

# Data ----
penguins <- read.table("./data/penguins.txt", header = T, sep = "\t")

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Workshop 1: Probabilities"),
  
  # Include JavaScript to handle scrolling
  tags$head(
    tags$script('
      $(document).ready(function(){
        var sidebar = $("#sidebarPanel");
        var sidebarTop = sidebar.position().top;
        var scrollThreshold = 50;  // Adjust the scroll threshold as needed
        var scrollSpeed = 0.5;     // Adjust the scroll speed as needed

        $(window).scroll(function(){
          var windowTop = $(window).scrollTop();
          if (windowTop > sidebarTop + scrollThreshold) {
            sidebar.css("top", (windowTop - sidebarTop - scrollThreshold) * scrollSpeed + "px");
          } else {
            sidebar.css("top", "0px");
          }
        });
      });
    ')
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      id = "sidebarPanel",  # Added an ID for targeting in JavaScript
      
      style = "width: 500px",  # Adjust the height as needed
      
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
        br(),
      
      

      
    ), 
    
    mainPanel(
      
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
      
      plotOutput("histogram"),
      
      # Centered slider input
      tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        sliderInput("bins", "Number of Bins:", min = 1, max = 30, value = 10)),

      p("You can change the number of breaks in a histogram by using the 
        'breaks' option when generating the plot:"),
      code("hist(penguins$body_mass_g, breaks = 24",
           style = "font-size:16px;"),
      br(),
      br(),
      p("In this example, you can use the 'Breaks' slider in the side panel to
        visualise this in action"),

        h2("Page"),
        p("Page number 17 of 19"),
        p("This preview may have altered the layout of this file. You can still download the original file."),
        
        h3("Assessing Normality"),
        p("As discussed in the lecture, one can assess the normality of a distribution of data in several ways. There are statistical tests that compare the sample distribution to the normal distribution and produce a probability value. As all statistical calculations, these tests are sensitive to sample size so in this course, we encourage you to rely on visual assessments, for example by using QQ plots."),
        p("Working with the Adelie dataset, produce another histogram to remind yourself of the shape of the distribution and then produce a QQ plot. Optional activities included at the end of the workshop are to overlay an empirical probability distribution and a normal distribution over the Adelie sample histograms."),
        code("hist(Adelie$body_mass_g)"),
        code("qqnorm(Adelie$body_mass_g, plot.it=TRUE)"),
        code("qqline(Adelie$body_mass_g, distribution=qnorm, probs=c(0.25,0.75))"),
        
        h3("Standard Error"),
        p("The data represent one finite random sample from a population, out of a near-infinite number of possible random samples. The uncertainty about the mean estimated from that sample can itself be estimated by the Standard Error (SE) or “standard error of the mean” (SEM), using the formula given in the lecture, which assumes a very large number of hypothetical repeats of the experiment."),
        p("Oddly, base R does not have a standard error function. So instead we have to calculate it using the command for standard deviation and the square root of the sample size."),
        code("se_Adelie<-sd(Adelie$body_mass_g) / sqrt(length(Adelie$body_mass_g))"),
        
        h3("Confidence Intervals"),
        p("How good is the estimate of the mean body mass for the Adelie penguins? Calculate the 95% confidence interval. You can remind yourself of the calculation by referring back to the lecture material. You need the mean, standard deviation, and sample size to do the calculation. You also need to determine the t statistic (i.e., critical value) for the 95% confidence limits. For the upper limit, the associated probability is 0.975 and for the lower limit, the associated probability is 0.025."),
        code("mean<-mean(Adelie$body_mass_g)"),
        code("t<-qt(0.975,length(Adelie$body_mass_g))"),
        code("se<-sd(Adelie$body_mass_g)/sqrt(length(Adelie$body_mass_g))"),
        code("upr95Adelie<-mean+t*selwr95"),
        code("lwr95Adelie<-mean-t*se"),
        
        h3("Question Set Four"),
        p("9. Based on the Q-Q plot, how well do the Adelie body mass data fit a normal distribution?"),
        code("qqnorm(Adelie$body_mass_g, plot.it=TRUE)"),
        code("qqline(Adelie$body_mass_g, distribution=qnorm,probs=c(0.25,0.75))"),
        p("The majority of the sample points in the central part of the distribution fall on or near the line that represents the normal distribution. Towards the two tails, the sample points fall slightly away from expectation. If you do the optional plot later in the workshop, you can see the way that the histogram differs from the normal overlay."),
        code("hist(Adelie$body_mass_g,xlim=c(2000,7000),breaks=24)"),
        code("hist_data<-hist(Adelie$body_mass_g,xlim=c(2000,7000),breaks=24)"),
        code("x_values<-seq(min(Adelie$body_mass_g),max(Adelie$body_mass_g),length=100)"),
        code("y_values<-dnorm(x_values,mean=mean(Adelie$body_mass_g),sd=sd(Adelie$body_mass_g))"),
        code("y_values<-y_values*diff(hist_data$mids[1:2])*length(Adelie$body_mass_g)"),
        code("lines(x_values,y_values,lwd=2)"),
        
        p("10. What is the SE of the mean for the Adelie penguins body mass?"),
        code("# The standard error for the Adelie body mass is 37.96g."),
        code("sd(Adelie$body_mass_g) / sqrt(length(Adelie$body_mass_g))"),
        
        p("11. How does the SE compare with the SD of the sample? In which circumstances would you choose to report one or the other in a scientific report?"),
        code("# The SE is smaller than the SD (37.96 compared to 458.62). It depends on the purpose. The standard deviation is useful for describing the variation in the data whereas the standard error expresses the precision of your estimate of the mean so it is useful if you are comparing groups."),
        
        p("12. How would you interpret the SE in words?"),
        code("# The standard error of the mean quantifies how precisely you know the population mean. The SE does not directly quantify variability among values in a population."),
        
        p("13. What is 95% confidence interval for the Adelie body mass? Write out in words the appropriate interpretation of the interval."),
        code("mean<-mean(Adelie$body_mass_g)"),
        code("df<-length(Adelie$body_mass_g)"),
        code("t<-qt(0.975,length(Adelie$body_mass_g))"),
        code("se<-sd(Adelie$body_mass_g)/sqrt(length(Adelie$body_mass_g))"),
        code("upr95Adelie<-mean+t*selwr95"),
        code("lwr95Adelie<-mean-t*se"),
        p("# The answer should be [3631,3781]. This means that if you were to repeatedly sample the Adelie penguin population on these islands, the mean body mass would fall between 3631 and 3781 g in 95% of the samples."),
        
        h3("Optional Work"),
        p("Add a normal curve to a histogram"),
        p("Sometimes it is useful to overlay a normal curve onto a histogram, for example, to see how well the sample data conform to the assumptions of a normal distribution. The normal curve is created from the properties of the data in the histogram (the mean and standard deviation) and assumes that the data are normally distributed."),
        p("This example uses the full penguin dataset for the body_mass_g variable:"),
        code("hist(penguins$body_mass_g,xlim=c(2000,7000),breaks=24)"),
        code("hist_data<-hist(penguins$body_mass_g,xlim=c(2000,7000),breaks=24)"),
        code("x_values<-seq(min(penguins$body_mass_g),max(penguins$body_mass_g),length=100)"),
        code("y_values<-dnorm(x_values,mean=mean(penguins$body_mass_g),sd=sd(penguins$body_mass_g))"),
        code("y_values<-y_values*diff(hist_data$mids[1:2])*length(penguins$body_mass_g)"),
        code("lines(x_values,y_values,lwd=2)"),
        
        p("This example illustrates that the underlying data for the 3 species combined does not follow a normal distribution. The reason is that these data are not all from the same population but rather from three separate ones that have been pooled together."),
        
        p("Add reference lines to a histogram"),
        p("To visualize the various intervals (e.g. 95%, 68%, mean+/-SD, mean+/-2SD or mean+/-3SD), you may add reference lines on the histograms. Let’s do that for the Chinstrap species, adding a reference line for the mean, median, 0.05 quantile and 0.95 quantile. Be sure that the Chinstrap frequency distribution is visible in the lower right frame in RStudio so that the reference lines are added to the correct plot."),
        code("hist(Chinstrap$body_mass_g,xlim=c(2000,7000),breaks=24)"),
        code('abline(v=mean(Chinstrap$body_mass_g),col="blue",lwd=2) #this adds a reference line for the mean value onto the frequency distribution for the Chinstrap body mass; lwd refers to the width of the line'),
        code('abline(v=median(Chinstrap$body_mass_g),col="red",lwd=2)'),
        code('abline(v=quantile(Chinstrap$body_mass_g,0.05),col="black",lwd=2)'),
        code('abline(v=quantile(Chinstrap$body_mass_g,0.95),col="black",lwd=2)'),
        
        p("Add a smoother or empirical density curve"),
        code('hist(Adelie$body_mass_g,freq=FALSE) #freq=FALSE changes the plot from a frequency histogram to a density histogram'),
        code('lines(density(Adelie$body_mass_g)) #this adds the empirical density curve'),
        code('hist(penguins$body_mass_g,freq=FALSE)'),
        code('lines(density(penguins$body_mass_g))')
      


      
    )
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # Render histogram plot
  output$histogram <- renderPlot({
    hist(penguins$body_mass_g, main = "Histogram of Body Mass",
         xlab = "Body Mass (g)", col = "grey", border = "black", breaks = input$bins)
  })
  

}

# Run the app ----
shinyApp(ui = ui, server = server)
