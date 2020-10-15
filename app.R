# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)
library(shinyMatrix)
library(shinyjs)
library(ggplot2)
library(tidyr)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Convergence of Discrete-Time Markov Chains"
APP_DESCP  <<- paste(
  "This app explores discrete time Markov Chains and their long run behavior",
  "by considering multiple sample problems."
)
# End App Meta Data------------------------------------------------------------

# Function for showing the check or X based on a condition
# Input: boolean for whether the condition is true or false
# Output: appropriate image
correctnessPic <- function(condition){
  if(condition){
    img(src = "check.PNG", alt = "Correct Answer", width = 30)
  }
  else{
    img(src = "cross.PNG", alt = "InCorrect Answer", width = 30)
  }
}

# Define UI for App
ui <- list(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      title = "MC Convergence", 
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Convergence of Discrete-Time Markov Chains"), 
          p("In this app you will explore how a discrete time Markov Chain 
            approaches long run behavior by exploring several scenarios and 
            utilizing a simple built-in matrix calculator."),
          h2("Instructions"),
          tags$ol(
            tags$li("Use the given contexts to construct a transition matrix for 
                    the Markov Chain."),
            tags$li("Use the transition matrix you created and the transition 
                    probability calculator to answer the questions posed for each
                    context and see how quickly these probabilities converge to 
                    long run behavior.")
          ),
          ##### Go Button
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          )
        ),

        #### Set up an Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore Discrete Time Markov Chains and Their Long Run Behavior"),
          p("In this section, you will explore the behavior of Discrete Time 
            Markov chains over time and their behavior in the long run."),
          p(
            "Below you are given a calculator to help you understand the long run
            behavior. To use the calculator, you should first choose a size for 
            your probability matrix then fill in the transition probabilities. 
            Note that since this is a probability matrix, the rows must sum to 1. 
            The Number of Steps allows you to choose how many steps in the future
            to consider the matrix, i.e. the calculator with take your probability
            to the nth power."
          ),
          p(
            "Use the calculator to answer the problems for each scenario. To 
            generate new numbers for the problem, click the new problem button.
            Notice the rate at which the chain approaches long run behavior in each 
            problem."
          ),
          
          fluidRow(
            # Title
            titlePanel("Transition Probability Calculator"),
            sidebarLayout(
              sidebarPanel(
                # Input for number of rows in the matrix
                selectInput("nrows", 
                            "Number of States in the Matrix:", 
                            c(2,3,4,5), 2),
                
                # Input to select current state of the matrix conditional on the 
                # matrix size
                conditionalPanel(
                  condition = "input.nrows == 2", 
                  matrixInput("ogMat2", matrix(diag(2), 
                                               nrow=2, 
                                               dimnames = list(0:1, 0:1)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric")
                  ),
                conditionalPanel(
                  condition = "input.nrows == 3",
                  matrixInput("ogMat3", 
                              matrix(diag(3), nrow=3, dimnames = list(0:2, 0:2)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric")
                ),
                conditionalPanel(
                  condition = "input.nrows == 4",
                  matrixInput("ogMat4", 
                              matrix(diag(4), nrow=4, dimnames = list(0:3, 0:3)), 
                              rows = list(names = TRUE),
                              cols = list( names = TRUE), 
                              class = "numeric" )
                ),
                conditionalPanel(
                  condition = "input.nrows == 5",
                  matrixInput("ogMat5", 
                              matrix(diag(5), nrow=5, dimnames = list(0:4, 0:4)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric" )
                ),
                conditionalPanel(
                  condition = "input.nrows > 5 | input.nrows < 2",
                  "This number of rows is not supported."
                ),
                
                # Input for number of steps to take
                numericInput("steps", 
                             "Number of steps", 
                             min = 1, 
                             max = 10000, 
                             value = 1, 
                             step = 1),
                actionButton("subMat", "Calculate Matrix")
              ),
              
              # Outputs: plot of states visited and the matrix to the n-steps power
              mainPanel(
                conditionalPanel(
                  condition = "!input.subMat",
                  p("To see the matrix x steps out, click the Calculate Matrix 
                    button.")
                ),
                textOutput("matlabel"),
                tableOutput("mat")
              )
            )
            ),
          tabsetPanel(
            tabPanel(
               title = "Candy",
          h2("Candy or Cookies"),
          textOutput("candyProb"),
          br(),
          fluidRow(
            column(
              width = 5,
              fluidRow(
                column(width = 10, 
              numericInput(
                inputId = "child1Prob", 
                label = "Probability that the next child chooses candy", 
                value = NA,
                min = 0,
                max = 1)
              ),
              column(width = 2, br(), uiOutput("correctnessChild1"))),
              fluidRow(
                column(
                  width = 10, 
                  numericInput(
                    inputId = "child5Prob", 
                    label = "Probability that the fifth child chooses candy", 
                    value = NA,
                    min = 0,
                    max = 1)
                ),
                column(width = 2, br(), uiOutput("correctnessChild5"))),
              fluidRow(
                column(width = 10, 
              numericInput(inputId = "child10Prob", 
                           label = "Probability that the tenth child chooses candy", 
                           value = NA,
                           min = 0,
                           max = 1)),
              column(width = 2, br(), uiOutput("correctnessChild10"))),
              fluidRow(
                column(
                  width = 10, 
                  numericInput(
                    inputId = "child20Prob", 
                    label = "Probability that the last child chooses candy", 
                    value = NA,
                    min = 0,
                    max = 1)),
                column(width = 2, br(), uiOutput("correctnessChild20"))),
              
          actionButton("checkCandy", "Check Answer"),
          actionButton("newCandy", "New Problem")),
          column(width = 7,
                 checkboxInput("showCandyPlots", "Show sample candy plots"),
                 conditionalPanel( 
                   condition = "input.showCandyPlots",
                   plotOutput("candyDaily", height = '250px'), 
                   htmlOutput("candyDailyAlt"),
                   plotOutput("candyCumulativeProb", height = '300px'),
                   htmlOutput("candyCumulativeAlt"),
                   actionButton("newCandyPlotSamples", "New Sample"))
                 )
          
          )),
          tabPanel(
            title = "Traffic Lights",
            h2("Traffic Lights"),
            textOutput("lightProb"),
            br(),
            fluidRow(
              column(width = 4,
                     numericInput(inputId = "TLG1", 
                                  label = "Probability of Next Light Green", 
                                  value = NA,
                                  min = 0,
                                  max = 1)),
              column(width = 1, br(), uiOutput("correctnessLG1")),
              column(width = 4,
                     numericInput(inputId = "TLR1", 
                                  label = "Probability of Next Light Red", 
                                  value = NA,
                                  min = 0,
                                  max = 1)),
              column(width = 1, br(), uiOutput("correctnessLR1"))),
            fluidRow(  
              column(
                width = 4,
                numericInput(inputId = "TLG5", 
                             label = "Probability of Fifth Light from Now Green", 
                             value = NA,
                             min = 0,
                             max = 1)
                     ),
              column(width = 1, br(), uiOutput("correctnessLG5")),
              column(
                width = 4,
                numericInput(inputId = "TLR5", 
                             label = "Probability of Fifth Light from Now Red", 
                             value = NA,
                             min = 0,
                             max = 1)),
              column(width = 1, br(), uiOutput("correctnessLR5"))
            ),
            fluidRow(  
              column(width = 4,
                     numericInput(inputId = "TLG9", 
                                  label = "Probability of Last Light Green", 
                                  value = NA,
                                  min = 0,
                                  max = 1)
              ),
              column(width = 1, br(), uiOutput("correctnessLG9")),
              column(width = 4,
                     numericInput(inputId = "TLR9", 
                                  label = "Probability of Last Light Red", 
                                  value = NA,
                                  min = 0,
                                  max = 1)),
              column(width = 1, br(), uiOutput("correctnessLR9"))
            ),
            actionButton("checkGameLights", "Check Answer"),
            actionButton("newLights", "New Problem"),
            checkboxInput("showLightPlots", "Show sample light plots"),
            conditionalPanel( 
              condition = "input.showLightPlots",
              plotOutput("lightDaily", height = '250px'),
              htmlOutput("lightDailyAlt"),
              plotOutput("lightCumulativeProb", height = '300px'),
              htmlOutput("lightCumulativeAlt"),
              actionButton("newLightPlotSamples", "New Sample"))
          ),
          tabPanel(
            title = "Weather",
            h2("Rain or No Rain"),
            textOutput("weatherProb"),
            p("Today, it rained. Calculate the probability that it will rain 
              each of the following days."),
            fluidRow(
              column(width = 5,
                         fluidRow(
                           column(
                             width = 10, 
                             numericInput(inputId = "tomorrowProb1", 
                                          label = "Probability of Rain Tomorrow", 
                                          value = NA,
                                          min = 0,
                                          max = 1)
                           ),
                           column(width = 2, br(), uiOutput("correctnessW1"))),
                         fluidRow(
                           column(
                             width = 10, 
                                  numericInput(
                                    inputId = "weekProb1", 
                                    label = "Probability of Rain One Week from Now", 
                                    value = NA,
                                    min = 0,
                                    max = 1)),
                           column(width = 2, br(), uiOutput("correctnessW2"))),
                     fluidRow(
                       column(
                         width = 10, 
                         numericInput(
                           inputId = "monthProb1", 
                           label = "Probability of Rain One Month from Now", 
                           value = NA,
                           min = 0,
                           max = 1)),
                       column(width = 2, br(), uiOutput("correctnessW3"))),
                     fluidRow(
                       column(
                         width = 10, 
                         numericInput(
                           inputId = "yearProb1", 
                           label = "Probability of Rain One Year from Now", 
                           value = NA,
                           min = 0,
                           max = 1)),
                       
                       column(width = 2, br(), uiOutput("correctnessW4"))),
                     
                     actionButton("checkGame", "Check Answer"),
                     actionButton("newWeather", "New Problem")),
              column(
                width = 6,
                checkboxInput("showWeatherPlots", "Show sample weather plots"),
                conditionalPanel( 
                  condition = "input.showWeatherPlots",
                  plotOutput("weatherDaily", height = '250px'), 
                  htmlOutput("weatherDailyAlt"),
                  plotOutput("weatherCumulativeProb", height = '300px'),
                  htmlOutput("weatherCumulativeAlt"),
                  actionButton("newWeatherPlotSamples", "New Sample"))
              )
            )
            ))),


        #### Set up the References Page
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent", 
            "Attali, D. (2020), shinyjs: Easily Improve the User Experience of 
            Your Shiny Apps in Seconds, R package. Available from  
            https://CRAN.R-project.org/package=shinyjs"
          ), 
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R package. Available 
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent", 
            "Neudecker, A. (2019), shinyMatrix: Shiny Matrix Input Field, R 
            package, R package. Available from 
            https://CRAN.R-project.org/package=shinyMatrix"
          ), 
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom 
            Inputs Widgets for Shiny, R package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. and Lionel, H. (2020), tidyr: Tidy Messy Data, R package.
            Available from https://CRAN.R-project.org/package=tidyr"
            ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          ),
        )
      )
    )
  )
)

# Define server logic

server<-function(input, output, session) {
  # Code for instructions (i) button
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "In this app, you will explore the long run behavior of discrete
      time Markov Chains using several example problems for illustration. Solve
      each problem to see how quickly the chain approaches long run behavior
      in the examples.",
      type = "info"
    )
  })
  
  # Handles verification that the entered matrix is a probability matrix for 2x2 
  ogMat2 <- reactive({
   validate(
      need(min(input$ogMat2)>=0 && sum(input$ogMat2[1,])==1 && 
             sum(input$ogMat2[2,])==1, 
           "Rows must be non-negative and sum to 1")
      )
    input$ogMat2
  })
  
  # Handles verification that the entered matrix is a probability matrix for 3x3 
  ogMat3 <- reactive({
    validate(
      need(min(input$ogMat3)>=0 && sum(input$ogMat3[1,])==1 && 
             sum(input$ogMat3[2,])==1 && sum(input$ogMat3[3,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat3
  })
  
  # Handles verification that the entered matrix is a probability matrix for 4x4 
  ogMat4 <- reactive({
    validate(
      need(min(input$ogMat4)>=0 && sum(input$ogMat4[1,])==1 && 
             sum(input$ogMat4[2,])==1 && sum(input$ogMat4[3,])==1 && 
             sum(input$ogMat4[4,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat4
  })
  
  # Handles verification that the entered matrix is a probability matrix for 5x5
  ogMat5 <- reactive({
    validate(
      need(min(input$ogMat5)>=0 && sum(input$ogMat5[1,])==1 && 
             sum(input$ogMat5[2,])==1 && sum(input$ogMat5[3,])==1 && 
             sum(input$ogMat5[4,])==1&& sum(input$ogMat5[5,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat5
  })
  
  # Gets the current probability matrix for the calculator based on number of rows
  getCurrentMatrix <- reactive({
    if(as.numeric(input$nrows)==2){
        ogMat2()
    }
    else if(as.numeric(input$nrows)==3){
      ogMat3()
    }
    else if(as.numeric(input$nrows)==4){
      ogMat4()
    }
    else{
      ogMat5()
    }
  })
  
  # Updates the matrix when the Calculate Matrix button is pressed
  updateMatrix<-eventReactive(input$subMat, {
    if(input$nrows == 2){
      expr=as.data.frame(matrixcalc::matrix.power(ogMat2(),
                                                  as.integer(input$steps)), 
                         row.names=0:1,
                         optional=TRUE)} 
    else if (input$nrows==3){
      as.data.frame(matrixcalc::matrix.power(ogMat3(),as.integer(input$steps)), 
                    row.names=0:2, 
                    optional=TRUE)}
    else if(input$nrows == 4){
      as.data.frame(matrixcalc::matrix.power(ogMat4(), as.integer(input$steps)), 
                    row.names=0:3, 
                    optional=TRUE)} 
    else{
      as.data.frame(matrixcalc::matrix.power(ogMat5(),as.integer(input$steps)), 
                    row.names=0:4, 
                    optional=TRUE)}})
  
  # Outputted matrix (the one to the nth power)
  output$mat<-renderTable({
    updateMatrix()}, rownames=TRUE, striped=FALSE
  )

  # PROBLEMS ----
  # All of the variables that will be tracked in the various scenarios
  game <- reactiveValues(score = 0, context = "", probw1 = 0, probw2=0, 
                         probSLRG = 0, probSLGY = 0, probSLGR = 0, probSLGG = 0,
                         probSLYY = 0, probSLYR = 0, probSLYG = 0,
                         probSLRY = 0, probSLRR = 0, probCandy = 0, 
                         probCookie = 0, inLab = "", correctMat = diag(2), 
                         correctMatSL = diag(3), correctMatC = diag(2),
                         showFeedback = F)
  
  # WEATHER
  # Sets the variables for the weather scenario
  weatherVars <- reactive({
    game$probW1 <- sample(c(.5, .6, .7, .8, .9), size = 1, replace = T)
    game$probW2 <- sample(c(.5, .4, .3, .2, .1), size = 1, replace = T)
    game$correctMat <- matrix(c(game$probW1, 1-game$probW1, 
                                game$probW2, 1-game$probW2),
                              nrow=2, byrow = TRUE)
  })
  
  # Used to get answers to the questions
  # Takes matrix correctMat to power pow then reads element [start, col]
  gameAns <- function(correctMat, start, pow, col = 1){
    matrixcalc::matrix.power(correctMat, pow)[start, col]
  }
  
  # Picture to go with weather question 1 (x or check)
  output$correctnessW1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$tomorrowProb1)) && 
                     round(input$tomorrowProb1,2) == 
                     round(gameAns(game$correctMat, 1, 1),2))})
  
  # Picture to go with weather question 2 (x or check)
  output$correctnessW2 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$weekProb1)) && 
                     round(input$weekProb1, 2) == 
                     round(gameAns(game$correctMat, 1, 7), 2))})
  
  # Picture to go with weather question 3 (x or check)
  output$correctnessW3 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$monthProb1)) && 
                     round(input$monthProb1, 2) == 
                     round(gameAns(game$correctMat, 1, 30), 2))})
  
  # Picture to go with weather question 4 (x or check)
  output$correctnessW4 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$yearProb1)) && 
                     round(input$yearProb1, 2) == 
                     round(gameAns(game$correctMat, 1, 365), 2))})

  # Show feedback after Check Game but reset to no feedback if any input is changed
  observeEvent(input$checkGame, {game$showFeedback <- T})
  observeEvent(input$tomorrowProb1, {game$showFeedback <- F})
  observeEvent(input$weekProb1, {game$showFeedback <- F})
  observeEvent(input$monthProb1, {game$showFeedback <- F})
  observeEvent(input$yearProb1, {game$showFeedback <- F})
  
  # Mostly here to make sure the weather vars get set at the beginning
  observeEvent(input$tabs, {click("newWeather")})
  
  # Statement for the weather problem
  output$weatherProb <- renderText({
  paste("Every morning, Ben wakes up at 8am and looks out his window to check 
  the weather. If it rains today tomorrow it will rain with probability", 
  game$probW1, "If it doesn't rain today, then it will rain tomorrow with 
  probability", game$probW2,".")})
  
  # When user clicks the New Problem button for the weather scenario
  observeEvent(input$newWeather, {
    click("newWeatherPlotSamples") # Updates plot
    game$showFeedback <- F # Hides feedback
    game$probW1 <- sample(.1*(2:9), size = 1, replace = T) # Updates probabilities
    # Decides on the probability of no rain after rain; 
    # make sure it is less than rain to rain
    if(game$probW1 ==.2){options <- (.01*(1:10))}
    else{options <- .1*(1:(ceiling(game$probW1*5)))}
    game$probW2 <- sample(options, size = 1, replace = T)
    
    # Updates correct probability matrix based on above
    game$correctMat <- matrix(c(game$probW1, 1-game$probW1, 
                                game$probW2, 1-game$probW2), 
                              nrow=2, byrow=TRUE)})
  
  # Do walk of 365 days for weather problem
  weatherSteps <- eventReactive(input$newWeatherPlotSamples, {
    curState <- 0
    index <- 0:365
    states <- c(0)
    rainSum <- c(1)
    
    # For each day, sample either rain or not based on the day before
    for(x in 1:365){
      curState <- sample(c(0,1), 1, replace=TRUE, prob=game$correctMat[curState+1,])
      states <- c(states, curState)
      rainSum <-c(rainSum, rainSum[x]+1-curState)
    }
    
    # Return a data frame with the samples
    data.frame(day = index, 
               state = states, 
               Rain = rainSum/(index+1), 
               None = 1 - rainSum/(index+1))
  })
  
  # Create the daily weather plot for first week
  output$weatherDaily <- renderPlot({
      plot<-ggplot2::ggplot(aes(x = day, y = state), data= weatherSteps()[1:31,]) +
        geom_point(size = 2) +
        geom_path() +
        xlab("Day Number") + 
        ylab('State') +
        scale_y_continuous(breaks = 0:1, labels = c("Rain", "No Rain")) +
        ggtitle("States by Day Over the First Month")+
        theme(axis.text = element_text(size=18),
              plot.title = element_text(size=18, face="bold"),
              axis.title = element_text(size=18),
              panel.background = element_rect(fill = "white", color="black"),
              legend.position=c(.89,1.07),
              legend.text = element_text(size=14),
              legend.title = element_text(size = 16)
        )
      plot
    
  })
  
  # Alt-text
  output$weatherDailyAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('weatherDaily').setAttribute('aria-label',
            `This plot shows the weather for one month of samples drawn from
             the chain in the problem.`)})"
      )))
  })
  
  # Create the plot of cumulative probability for rain and not rain
  output$weatherCumulativeProb <- renderPlot({
    data <- pivot_longer(weatherSteps(), 
                         cols = c("Rain", "None"), 
                         names_to = "Rain", 
                         values_to = "Proportion")
    plot<-ggplot2::ggplot(aes(x = day, y =Proportion, color = Rain), data= data) +
      geom_hline(aes(yintercept = 1 - gameAns(game$correctMat, 1, 365)), 
                 color = boastPalette[1], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(game$correctMat, 1, 365)), 
                 color = boastPalette[2], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      scale_colour_manual(values = boastUtils::boastPalette) +
      geom_path(lwd = 1) +
      xlab("Day Number") + 
      ylab('Cumulative portion of rainy days') +
      ggtitle("Portion of Days of Rain Over Time")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.text = element_text(size=14),
            legend.title = element_text(size = 16)
      )
    plot
  })
  
  # Alt-text
  output$weatherCumulativeAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('weatherCumulativeProb').setAttribute('aria-label',
            `This plot shows the cumulative proportion of days of each weather type
            for one simulated year. Over time, these proportions get closer to the 
             long run proportions.`)})"
      )))
  })
  
  # STOP LIGHTS
  # Resets the probabilities of Green, Red, or Yellow lights
  # The double letter pair at the end of each name stands for the color to color
  # Ex. YG means the probability of transitioning from a yellow to a green light
  createProbs <- reactive({
    game$probSLYY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLGG <- round(runif(n = 1, min = .5, max = .8), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                             game$probSLYG, game$probSLYY, game$probSLYR,
                             game$probSLRG, game$probSLRY, game$probSLRR), 
                             nrow=3, byrow = T)
  })
  
  # Update lights values for a new problem
  observeEvent(input$newLights, {
    click("newLightPlotSamples")
    game$showFeedback <- F
    game$probSLYY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLGG <- round(runif(n = 1, min = .5, max = .8), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                                  game$probSLYG, game$probSLYY, game$probSLYR,
                                  game$probSLRG, game$probSLRY, game$probSLRR), 
                                nrow=3, byrow = T)})

  # Text for the traffic lights problem
  output$lightProb <- renderText({
    createProbs()
  paste0("Sarah is driving down a road with 10 traffic lights.
    These lights are timed such that if the current light is green, the probability 
    of the next light being green is ", game$probSLGG, " while the probability of 
    it being red is ", game$probSLGR, " with the remaining probability on yellow. 
    If the current light is yellow, then the probability of the next light being 
        green is ", game$probSLYG, " and the probability of the next light being 
        red is ", game$probSLYR, ". Lastly, if the current light is red, then the 
        probability of the next light being green is ", game$probSLRG, " while 
        the probability of it being red is ", game$probSLRR, ". Suppose the first 
        light is green; calculate the below probabilities for some of the 
         following lights." 
    )
  })
  
  # Check or X for probability that the first light is green
  output$correctnessLG1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG1)) && 
                     round(input$TLG1, 2) == 
                     round(gameAns(game$correctMatSL, 1, 1), 2))})
  
  # Check or X for probability that the first light is red
  output$correctnessLR1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR1)) && 
                     round(input$TLR1, 2) == 
                     round(gameAns(game$correctMatSL, 1, 1, col = 3), 2))})
  
  # Check or X for probability that the fifth light is green
  output$correctnessLG5 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG5)) && 
                     round(input$TLG5, 2) == 
                     round(gameAns(game$correctMatSL, 1, 5), 2))})

  # Check or X for probability that the fifth light is red
  output$correctnessLR5 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR5)) && 
                     round(input$TLR5, 2) == 
                     round(gameAns(game$correctMatSL, 1, 5, col = 3), 2))})
  
  # Check or X for probability that the last light is green
  output$correctnessLG9 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG9)) && 
                     round(input$TLG9, 2) == 
                     round(gameAns(game$correctMatSL, 1, 9), 2))})
  
  # Check or X for probability that the last light is red
  output$correctnessLR9 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR9)) && 
                     round(input$TLR9, 2) == 
                     round(gameAns(game$correctMatSL, 1, 9, col = 3), 2))})
  
  # Controls feedback display
  # Show feedback if the button is pressed; hide feedback if the answers change
  observeEvent(input$checkGameLights, {game$showFeedback <- T})
  observeEvent(input$TLG1, {game$showFeedback <- F})
  observeEvent(input$TLR1, {game$showFeedback <- F})
  observeEvent(input$TLG5, {game$showFeedback <- F})
  observeEvent(input$TLR5, {game$showFeedback <- F})
  observeEvent(input$TLG9, {game$showFeedback <- F})
  observeEvent(input$TLR9, {game$showFeedback <- F})
  
  observeEvent(input$tabs, {click("newLights")})
  
  # Does a walk of length 10 using the probability matrix for lights problem
  lightSteps <- eventReactive(input$newLightPlotSamples, {
    curState <- 0
    index <- 0:10
    states <- c(0)
    Green <- 1
    Yellow <- 0
    Red <- 0
    greens <- c(1)
    reds <- c(0)
    yellows <- c(0)
    
    # For each light, sample the outcome based on the previous light
    for(x in 1:10){
      curState <- sample(c(0,1,2), 1, 
                         replace=TRUE, 
                         prob=game$correctMatSL[curState+1,])
      states <- c(states, curState)
      if(curState == 0){
        Green <- Green + 1
      }
      else if(curState == 1){
        Yellow <- Yellow + 1
      }
      else{
        Red <- Red + 1
      }
      reds <-c(reds, Red)
      greens <- c(greens, Green)
      yellows <- c(yellows, Yellow)
    }
    
    # Return data frame of results
    data.frame(day = index, 
               state = states, 
               Green = greens, 
               Yellow = yellows, 
               Red = reds)
  })

  # Create plot for all 10 lights by state
  output$lightDaily <- renderPlot({
    plot<-ggplot2::ggplot(aes(x = day, y = state), data= lightSteps()) +
      geom_point(size = 2) +
      geom_path() +
      xlab("Light Number") + 
      ylab('State') +
      scale_y_continuous(breaks = 0:2, labels = c("Green", "Yellow", "Red")) +
      ggtitle("States by Light Number")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.position=c(.89,1.07),
            legend.text = element_text(size=14),
            legend.title = element_text(size = 16)
      )
    plot
  })
  
  # Alt-text
  output$lightDailyAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('lightDaily').setAttribute('aria-label',
            `This plot shows the color of each light for one simulation of 
             the process.`)})"
      )))
  })
  
  # Create plot of cumulative proportions spent in each state
  output$lightCumulativeProb<- renderPlot({
    data <- pivot_longer(lightSteps(), cols = c("Red", "Yellow", "Green"), 
                         names_to = "Color", values_to = "Proportion")
    data$Proportion <- data$Proportion / (data$day + 1)
    plot<-ggplot2::ggplot(aes(x = day, y = Proportion, color = Color), 
                          data= data) +
      scale_colour_manual(values = c("#009E73", "red", "#E69F00")) +
      geom_hline(aes(yintercept = gameAns(game$correctMatSL, 1, 100, col = 1)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "#009E73", 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(game$correctMatSL, 1, 100, col = 2)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "#E69F00", 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(game$correctMatSL, 1, 100, col = 3)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "red", 
                 show.legend = TRUE) +
      geom_path(lwd = 1) +
      geom_point(aes(shape = Color), size = 3) +
      xlab("Light number") + 
      ylab('Cumulative proportions') +
      ggtitle("Portion of Light Colors Over Time")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.text = element_text(size=14),
            legend.title = element_text(size = 16)
      )
    plot
  })
  
  # Alt-text
  output$lightCumulativeAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('lightCumulativeProb').setAttribute('aria-label',
            `This plot shows the cumulative proportion of lights of each color 
            for one simulation. Over time, these proportions get closer to the 
             long run proportions.`)})"
      )))
  })
  
  # CANDY
  # Assign probabilities to the candy/cookies problem
  createCandy <- reactive({
    game$probCandy <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$probCookie <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$correctMatC <- matrix(c(game$probCandy, 1-game$probCandy, 
                                 1 - game$probCookie, game$probCookie),
                              nrow=2, byrow = TRUE)
  })
  
  # Set up text for the candy problem
  output$candyProb <- renderText({
  createCandy()
  paste0("Mrs. Gamble's kindergarten class has 20 students. 
    One day, she brings in a jar of cookies and a bag of candy and gives each 
    student the choice between either candy or a cookie. Each student then 
    sequentially chooses a treat. Without any influence from the other students, 
    each student is equally likely to choose either option, but being 
    impressionable young children, the choice of each student is impacted by the 
    choice of the previous student. As such, if the previous student chose candy, 
    then the next student will choose candy with probability ", game$probCandy, 
    ", and if the previous student chose a cookie then the next student will 
    choose a cookie with probability ", game$probCookie, ". Assuming that the 
    first student chooses a cookie, calculate the following probabilities.")})

  # Output check or X for first question (child 1)
  output$correctnessChild1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$child1Prob)) && 
                     round(input$child1Prob,2) == game$correctMatC[2,2])})
  
  # Output check or X for second question (child 5)
  output$correctnessChild5 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$child5Prob)) && 
                     round(input$child5Prob, 2) == 
                     round(.5*gameAns(game$correctMatC, 1, 5) + 
                             .5*gameAns(game$correctMatC, 2, 5), 2))})
  
  # Output check or X for third question (child 10)
  output$correctnessChild10 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$child10Prob)) && 
                     round(input$child10Prob, 2) == 
                     round(.5*gameAns(game$correctMatC, 1, 10) + 
                             .5*gameAns(game$correctMatC, 2, 10), 2))})
  
  # Output check or X for fourth question (child 20)
  output$correctnessChild20 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$child20Prob)) && 
                     round(input$child20Prob, 2) == 
                     round(.5*gameAns(game$correctMatC, 1, 20) + 
                             .5*gameAns(game$correctMatC, 2, 20), 2))})
  
  # Control when feedback is displayed (only directly after pressing the button)
  observeEvent(input$checkCandy, {game$showFeedback <- T})
  observeEvent(input$child1Prob, {game$showFeedback <- F})
  observeEvent(input$child5Prob, {game$showFeedback <- F})
  observeEvent(input$child10Prob, {game$showFeedback <- F})
  observeEvent(input$child20Prob, {game$showFeedback <- F})
  
  # When tabs switch, make sure feedback goes away and candy problem is updated
  observeEvent(input$tabs, {
    game$showFeedback <- F
    click("newCandy") # Mostly to make sure that the initial problem renders
  })
  
  # When a new problem is requested, update numbers and plots
  observeEvent(input$newCandy, {
    click("newCandyPlotSamples")
    game$showFeedback <- F
    game$probCandy <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$probCookie <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$correctMatC <- matrix(c(game$probCandy, 1-game$probCandy, 
                                 1 - game$probCookie, game$probCookie),
                               nrow=2, byrow = TRUE)
    })
  
  # Assigns all students a cookie or candy choice based on current probabilities
  candySteps <- eventReactive(input$newCandyPlotSamples, {
    curState <- 0
    index <- 1:20
    states <- c(1)
    cookieSum <- c(1)
    # Loop through all remaining students
    for(x in 2:20){
      curState <- sample(c(0,1), 1, replace=TRUE, 
                         prob=game$correctMatC[curState+1,])
      states <- c(states, curState)
      cookieSum <-c(cookieSum, cookieSum[x-1] + curState)
    }
    
    # Return data frame of data generated by the simulation
    data.frame(day = index, 
               state = states,
               Cookie = cookieSum/(index), 
               Candy = 1 - cookieSum/(index))
  })
  
  # Plots the choices of each of the children
  output$candyDaily <- renderPlot({
    plot<-ggplot2::ggplot(aes(x = day, y = state), data= candySteps()) +
      geom_point(size = 2) +
      geom_path() +
      xlab("Student") +
      ylab('State') +
      scale_y_continuous(breaks = 0:1, labels = c("Candy", "Cookie")) +
      ggtitle("Children's Choices")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.position=c(.89,1.07),
            legend.text = element_text(size=14),
            legend.title = element_text(size = 16)
      )
    plot
  })
  
  # Alt-text
  output$candyDailyAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('candyDaily').setAttribute('aria-label',
            `This plot shows the choice of each student for one simulation of 
             the process.`)})"
      )))
  })
  
  # Plots the cumulative proportion of candy and cookie chosen
  output$candyCumulativeProb <- renderPlot({
    data <- pivot_longer(candySteps(), 
                         cols = c("Candy", "Cookie"), 
                         names_to = "Choice", 
                         values_to = "Proportion")
    plot<-ggplot2::ggplot(aes(x = day, y =Proportion, color = Choice), 
                          data= data) +
      geom_hline(aes(yintercept = gameAns(game$correctMatC, 1, 365)), 
                 color = boastPalette[1], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = 1 - gameAns(game$correctMatC, 1, 365)), 
                 color = boastPalette[2], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      scale_colour_manual(values = boastUtils::boastPalette) +
      geom_path(lwd = 1) +
      xlab("Student") +
      ylab('Cumulative portion by choice type') +
      ggtitle("Portion Student Choices") +
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.text = element_text(size=14),
            legend.title = element_text(size = 16)
      )
    plot
  })
  
  # Alt-text
  output$candyCumulativeAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('candyCumulativeProb').setAttribute('aria-label',
            `This plot shows the cumulative proportion of each student's choice 
            for one simulation. Over time, these proportions get closer to the 
             long run proportions.`)})"
      )))
  })
}



# Create Shiny App using BOAST App template
boastUtils::boastApp(ui = ui, server = server)
