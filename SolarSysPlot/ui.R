
library(shiny)
shinyUI(fluidPage(
    titlePanel("Create your Solar System Plot!"),
    sidebarLayout(
        sidebarPanel(   
            checkboxGroupInput(inputId = "planets", 
                               label = "Choose the Planets you whish to plot", 
                               choices = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto"), selected = "Earth"),
            sliderInput(inputId = "slider", label = "How many Earth Days do you whish to plot?", min = 1, max = 90736, value = 365, step = 100),
            radioButtons(inputId = "transformation", 
                         choiceNames = c("Keep them straight",
                                         "Throw some logs!",
                                         "Throw some expos!",
                                         "Go Crazy!"),
                         label = "What to do with the lines?", 
                         choiceValues =  c("str", "log", "exp", "crazy")),
            submitButton(text = "Plot!"), 
            
            
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Plotting",
                         textOutput("planets.out"),
                         plotOutput("planets.plot"),
                         h4("How much time is this?"),
                         h5(textOutput("text")),
                         h4("What is this that I am seeing?"),
                         h5("You are seeing a graphic representation of year-cyles of the plantes from our Solar System. Yes yes, Pluto is 'not a planet', but can we give the guy a break? Each planet is a sequence of numbers going from 0 to 1 and back to 0. If there was a planet with a 5 earth-days cycle, its sequence would be [0, 0.5, 1, 0.5, 0]. Pluto has the longest sequence, with 907365 numbers! If you selected to see logs, you are seeing the natural log of those numbers. The same goes from exponents. If you went crazy, you are seeing the log those numbers multiplied by the number squared.")
                ),
                tabPanel("About this",
                         h4("How to use this?"),
                         h5("I tried to keep this very user-friendly. All you have to do is select the planets you whish to plot, how many earth-days units of time you whish to plot and, finally, what transformation you whish to perform on the data. All changes will take effect once you press the 'Submit' button. If you select lots of planets or a long length of time, it may take a few seconds."),
                         h4("How did you do this?"),
                         h5("This was done using R language, particularly using the Shiny and GGplot2 packages for RStudio."),
                         h4("How can I talk to you?"),
                         h5("Just send me an e-mail! You can find me at arthurlunabcf@outlook.com"))
                
            )))
))
