# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)
library(shinyMatrix)
library(matrixcalc)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Long Run Behavior"
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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      title = "Long Run Probabilities", 
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
          h1("Long Run Probabilities for Discrete Time Markov Chains"), 
          p("In this app you will explore how a discrete time Markov Chain 
            approaches long run behavior by exploring several scenarios and 
            utilizing a simple built-in matrix calculator."),
          h2("Instructions"),
          tags$ol(
            tags$li("Go to the exploration tab to try out several examples of
                    Markov Chain behavior at different points in the future.")
            #,tags$li("Challenge yourself."),
            #tags$li("Play the game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
          ##### Create two lines of space
          br(),
          br()
          
        ),
        #### Set up the Prerequisites Page CURRENTLY NOT IMPLEMENTED!!!!!
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("The definition of discrete time Markov chains"),
            tags$li("Representing discrete time Markov chains using probability 
                    matrices"),
            tags$li("Theory of long run behavior and behavior n steps from now 
                    (some of which should probably be in explore...)")
            
          )
          
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore Discrete Time Markov Chains and Their Long Run Distributions"),
          p("In this section, you will explore the behavior of Discrete Time 
            Markov chains over time and their behavior in the long run."),
          
          #h2("Calculating Markov Chain Future Behavior"),
          p(
            "Below you are given a calculator to help you understand the long run
            behavior. To use the calculator, you should first choose a size for 
            your probability matrix then fill in the transition probabilities. 
            Note that since this is a probability matrix, the rows must sum to 1. 
            You also can choose an initial state. You can then view either a bar
            graph of a simulated run of n steps or view the matrix n steps out, i.e.
            to the nth power."
          ),
          p(
            "Use the calculator to answer the problems for each scenario. To 
            generate new numbers for the problem, click the new problem button.
            Notice how quickly the chain approaches long run behavior in each problem."
          ),
          
          fluidRow(
            # Title
            titlePanel("Exploring Discrete Time Markov Chains"),
            #tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}"),
            
            
            sidebarLayout(
              sidebarPanel(
                # Input for number of rows in the matrix
                selectInput("nrows", "Matrix Size:", c(2,3,4,5), 2),
                
                # Input to select current state of the matrix conditional on the matrix size
                conditionalPanel(
                  condition = "input.nrows == 2", 
                  matrixInput("ogMat2", matrix(diag(2), 
                                               nrow=2, 
                                               dimnames = list(0:1, 0:1)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric"),
                  selectInput("curState2", 
                              "Current State of the Matrix", 
                              choices = 0:1, 
                              selected = 0)),
                conditionalPanel(
                  condition = "input.nrows == 3",
                  matrixInput("ogMat3", 
                              matrix(diag(3), nrow=3, dimnames = list(0:2, 0:2)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric"),
                  selectInput("curState3", 
                              "Current State of the Matrix", 
                              choices = 0:2, 
                              selected = 0)
                ),
                conditionalPanel(
                  condition = "input.nrows == 4",
                  matrixInput("ogMat4", 
                              matrix(diag(4), nrow=4, dimnames = list(0:3, 0:3)), 
                              rows = list(names = TRUE),
                              cols = list( names = TRUE), 
                              class = "numeric" ),
                  selectInput("curState4", 
                              "Current State of the Matrix", 
                              choices = 0:3, 
                              selected = 0)
                ),
                conditionalPanel(
                  condition = "input.nrows == 5",
                  matrixInput("ogMat5", 
                              matrix(diag(5), nrow=5, dimnames = list(0:4, 0:4)), 
                              rows = list(names = TRUE), 
                              cols = list( names = TRUE), 
                              class = "numeric" ),
                  selectInput("curState5", 
                              "Current State of the Matrix", 
                              choices = 0:4, 
                              selected = 0)
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
                actionButton("subMat", "Calculate Matrix"),
                actionButton("runSteps", "Plot Steps")
              ),
              
              # Outputs: plot of states visited and the matrix to the n-steps power
              mainPanel(
                conditionalPanel(
                  condition = "!input.subMat",
                  p("To see the matrix x steps out, click the Calculate Matrix button.")
                ),
                conditionalPanel(
                  condition = "!input.runSteps",
                  p("To see a graph of the simulation, click the Plot Steps button.")
                ),
                textOutput("matlabel"),
                tableOutput("mat"),
                plotOutput("plot", height="250px"),
                textOutput("proportions"),
                textOutput("longRun")
                #textOutput("matlabel")
                #tableOutput("outmattemp") #Is a table that shows the number of elements for each component of the bar graph for testing purposes
                
              )
            )
            ),
          tabsetPanel(
            tabPanel(
               title = "Weather",
          h2("Predict the Weather"),
          textOutput("weatherProb"),
          p("Today, it rained. Predict whether it will rain each of the following days."),
          fluidRow(
            # column(
              # width = 5, 
              # selectInput(inputId = "tomorrow1", 
              #             label = "Tomorrow", 
              #             choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
              # selectInput(inputId = "week1", 
              #             label = "One Week from Now", 
              #             choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
              # selectInput(inputId = "month1", 
              #             label = "One Month from Now", 
              #             choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
              # selectInput(inputId = "year1", 
              #             label = "One Year from Now", 
              #             choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n"))
            # ),
            column(
              width = 5,
              fluidRow(
                column(width = 10, 
              numericInput(inputId = "tomorrowProb1", 
                           label = "Probability of Rain Tomorrow", 
                           value = NA,
                           min = 0,
                           max = 1)
              ),
              column(width = 2, br(), uiOutput("correctnessW1"))),
              fluidRow(
                column(width = 10, 
              numericInput(inputId = "weekProb1", 
                           label = "Probability of Rain One Week from Now", 
                           value = NA,
                           min = 0,
                           max = 1)),
              column(width = 2, br(), uiOutput("correctnessW2"))),
              fluidRow(
                column(width = 10, 
              numericInput(inputId = "monthProb1", 
                           label = "Probability of Rain One Month from Now", 
                           value = NA,
                           min = 0,
                           max = 1)),
                       column(width = 2, br(), uiOutput("correctnessW3"))),
              fluidRow(
                column(width = 10, 
              numericInput(inputId = "yearProb1", 
                           label = "Probability of Rain One Year from Now", 
                           value = NA,
                           min = 0,
                           max = 1)),
              
              column(width = 2, br(), uiOutput("correctnessW4"))),
              
            ),
            column(
              width = 2,
              #textOutput("score")
            )
          ),
          actionButton("checkGame", "Check Answer"),
          actionButton("newWeather", "New Problem")),
          tabPanel(
            title = "Traffic Lights",
            textOutput("lightProb"),
            br(),
            # p("There are seven streetlights on a busy street you drive down each 
            #   day that are timed so that the probability that a light is red is 
            #   0.3 if the previous light was red when you arrived at it and 0.9 
            #   if the previous light was not red.  Also, the chance that the first 
            #   light you come to is red is 0.1. "),
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
              column(width = 4,
                     numericInput(inputId = "TLG5", 
                                  label = "Probability of Fifth Light from Now Green", 
                                  value = NA,
                                  min = 0,
                                  max = 1)
                     ),
              column(width = 1, br(), uiOutput("correctnessLG5")),
              column(width = 4,
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
            
          ))),
        #### Set up a Challenge Page
        tabItem(
          tabName = "Challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("Not Currently Implemented")
        ),
        #### Set up a Game Page
        tabItem(
          tabName = "Game",
          withMathJax(),
          # h2("Predict the Weather"),
          # p("Note: this is just a draft and the numbers are currently all made up."),
          # p("If it rains today tomorrow it will rain with probability .6. If it doesn't 
          #   rain today, then it will rain tomorrow with probability .3."),
          # p("Today, it rained. Predict the weather for each time period and give 
          #   the probability that your prediction will come true."),
          # fluidRow(
          #   column(
          #     width = 5, 
          #     selectInput(inputId = "tomorrow1", 
          #                 label = "Tomorrow", 
          #                 choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
          #     selectInput(inputId = "week1", 
          #                 label = "One Week from Now", 
          #                 choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
          #     selectInput(inputId = "month1", 
          #                 label = "One Month from Now", 
          #                 choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n")),
          #     selectInput(inputId = "year1", 
          #                 label = "One Year from Now", 
          #                 choices = c("Select Answer" = "", "Rain" = "y", "No Rain" = "n"))
          #   ),
          #   column(
          #     width = 5,
          #     numericInput(inputId = "tomorrowProb1", 
          #                 label = "Probability of Rain Tomorrow", 
          #                 value = NA,
          #                 min = 0,
          #                 max = 1
          #                 ),
          #     numericInput(inputId = "week1", 
          #                 label = "Probability of Rain One Week from Now", 
          #                 value = NA,
          #                 min = 0,
          #                 max = 1),
          #     numericInput(inputId = "month1", 
          #                 label = "Probability of Rain One Month from Now", 
          #                 value = NA,
          #                 min = 0,
          #                 max = 1),
          #     numericInput(inputId = "year1", 
          #                 label = "Probability of Rain One Year from Now", 
          #                 value = NA,
          #                 min = 0,
          #                 max = 1)
          #     
          #   ),
          #   column(
          #     width = 2,
          #     textOutput("score")
          #   )
          #   ),
          # actionButton("checkGame", "Check Answer")
          ),
        #### Set up the References Page-REQUIRED
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
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
            "Novomestky, F. (2012), matrixcalc: Collection
  of functions for matrix calculations, R package. Available from
            https://CRAN.R-project.org/package=matrixcalc"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom 
            Inputs Widgets for Shiny, R package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          )
        )
      )
    )
  )
)

# Define server logic
totals<-matrix(rep(0,25), nrow=5)

server<-function(input, output, session) {
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
  
  ogMat2 <- reactive({
   validate(
      need(min(input$ogMat2)>=0 && sum(input$ogMat2[1,])==1 && sum(input$ogMat2[2,])==1, 
           "Rows must be non-negative and sum to 1")
      )
    input$ogMat2
  })
  
  ogMat3 <- reactive({
    validate(
      need(min(input$ogMat3)>=0 && sum(input$ogMat3[1,])==1 && 
             sum(input$ogMat3[2,])==1 && sum(input$ogMat3[3,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat3
  })
  
  ogMat4 <- reactive({
    validate(
      need(min(input$ogMat4)>=0 && sum(input$ogMat4[1,])==1 && 
             sum(input$ogMat4[2,])==1 && sum(input$ogMat4[3,])==1 && 
             sum(input$ogMat4[4,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat4
  })
  
  ogMat5 <- reactive({
    validate(
      need(min(input$ogMat5)>=0 && sum(input$ogMat5[1,])==1 && 
             sum(input$ogMat5[2,])==1 && sum(input$ogMat5[3,])==1 && 
             sum(input$ogMat5[4,])==1&& sum(input$ogMat5[5,])==1, 
           "Rows must sum to 1")
    )
    input$ogMat5
  })
  
  matSteps <- function(start, steps, size, mat){
    curState <- start
    for(x in 1:steps){
      curState <- sample(0:(size-1), 1, replace=TRUE, prob=mat[curState+1,])
      # if(as.numeric(input$nrows)==2){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat2()[curState+1,])}
      # else if(as.numeric(input$nrows)==3){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat3()[curState+1,])}
      # else if(as.numeric(input$nrows)==4){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat4()[curState+1,])} 
      # else if(as.numeric(input$nrows)==5){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat5()[curState+1,])}
      totals[as.numeric(input$nrows)-1, curState+1]<-totals[as.numeric(input$nrows)-1, curState+1]+1
    }
    totals[as.numeric(input$nrows)-1,1:as.numeric(input$nrows)]
  }
  
  runSteps<-eventReactive(input$runSteps,{
    # curState<-as.numeric(input$curState2) 
    # for(x in 1:input$steps){
      if(as.numeric(input$nrows)==2){
        matSteps(start = as.numeric(input$curState2), 
                 steps = input$steps, 
                 size = as.numeric(input$nrows),
                 mat = ogMat2())
      }
    else if(as.numeric(input$nrows)==3){
      matSteps(start = as.numeric(input$curState3), 
               steps = input$steps, 
               size = as.numeric(input$nrows), 
               mat = ogMat3())
    }
    else if(as.numeric(input$nrows)==4){
      matSteps(start = as.numeric(input$curState4), 
               steps = input$steps, 
               size = as.numeric(input$nrows), 
               mat = ogMat4())
    }
    else{
      matSteps(start = as.numeric(input$curState5), 
               steps = input$steps, 
               size = as.numeric(input$nrows), 
               mat = ogMat5())
    }
        # curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat2()[curState+1,])}
      # else if(as.numeric(input$nrows)==3){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat3()[curState+1,])}
      # else if(as.numeric(input$nrows)==4){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat4()[curState+1,])} 
      # else if(as.numeric(input$nrows)==5){
      #   curState<-sample(0:(as.numeric(input$nrows)-1), 1, replace=TRUE, prob=ogMat5()[curState+1,])}
    #   totals[as.numeric(input$nrows)-1, curState+1]<-totals[as.numeric(input$nrows)-1, curState+1]+1
    # }
    # totals[as.numeric(input$nrows)-1,1:as.numeric(input$nrows)]
  })
  
  
  updateMatrix<-eventReactive(input$subMat, {if(input$nrows == 2){
    expr=as.data.frame(matrixcalc::matrix.power(ogMat2(),as.integer(input$steps)), 
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
  
  props<-eventReactive(input$runSteps,{
    #rowsum<-sum(totals[as.numeric(input$nrows), 1:2])
    rowsum<-sum(runSteps())
    if(input$nrows==2){
      paste("Actual: ", 
            round(runSteps()[1]/rowsum,2),
            "   ", 
            round(runSteps()[2]/rowsum,2),
            sep ="\t\t\t")}
    else if(input$nrows==3){
      paste("Actual: ", 
            round(runSteps()[1]/rowsum,2),
            " ",
            round(runSteps()[2]/rowsum,2),
            " ",
            round(runSteps()[3]/rowsum,2))}
    else if(input$nrows==4){
      paste("Actual: ", 
            round(runSteps()[1]/rowsum,2),
            " ",
            round(runSteps()[2]/rowsum,2),
            " ", 
            round(runSteps()[3]/rowsum,2),
            " ",
            round(runSteps()[4]/rowsum,2))}
    else{" "}
  })
  longRuns<-eventReactive(input$runSteps,{
    if(as.numeric(input$nrows)==2){
      mat<-matrixcalc::matrix.power(ogMat2(),20)[1,1:2]
      paste("Long Run:", round(mat[1],2), round(mat[2],2))}
    else if(as.numeric(input$nrows)==3){
      mat<-matrixcalc::matrix.power(ogMat3(),20)[1,1:3]
      paste("Long Run:", round(mat[1],2), round(mat[2],2), round(mat[3],2))}  
    else if(as.numeric(input$nrows)==4){
      mat<-matrixcalc::matrix.power(ogMat4(),20)[1,1:4]
      paste("Long Run:", round(mat[1],2), round(mat[2],2), 
            round(mat[3],2), round(mat[4],2))} 
    else{
      mat<-matrixcalc::matrix.power(mat<-ogMat5(),20)[1,1:5]}
  })
  makePlot<-eventReactive(input$runSteps,{
    barplot(runSteps(), 
            main="Distrubution of states visited", 
            names.arg=0:(as.numeric(input$nrows)-1))})
  
  output$plot<-renderPlot({makePlot()})
  output$matlabel<-renderText({
    if(input$subMat){paste("Probability Matrix ", 
                           as.integer(input$steps), 
                           " Steps Out")}})
  output$mat<-renderTable({
    updateMatrix()}, rownames=TRUE, striped=FALSE
  )
  output$proportions<-renderText({
    props()
  })
  output$longRun<-renderText({
    longRuns()})
  #output$outmattemp<-renderTable({as.data.frame(runSteps())}) # Table for testing purposes
  
  
  # PROBLEMS ----
  game <- reactiveValues(score = 0, context = "", probw1 = 0, probw2=0, probSLRG = 0,
  probSLGY = 0, probSLGR = 0, probSLGG = 0,
  probSLYY = 0, probSLYR = 0, probSLYG = 0,
  probSLRY = 0, probSLRR = 0, inLab = "", correctMat = diag(2), correctMatSL = diag(3), 
  showFeedback = F)
  output$score <- renderText({"Score: 0"})
  
  # WEATHER
  weatherVars <- reactive({
    game$probW1 <- sample(c(.5, .6, .7, .8, .9), size = 1, replace = T)
    game$probW2 <- sample(c(.5, .4, .3, .2, .1), size = 1, replace = T)
    game$correctMat <- matrix(c(game$probW1, 1-game$probW1, game$probW2, 1-game$probW2),
                              nrow=2)
  })
  
  gameAns <- function(correctMat, start, pow, col = 1){
    matrixcalc::matrix.power(correctMat, pow)[start, col]
    # c(correctMat[start, 1],
    #   matrixcalc::matrix.power(correctMat,7)[start, 1],
    #   matrixcalc::matrix.power(correctMat,30)[start, 1],
    #   matrixcalc::matrix.power(correctMat,365)[start, 1]  
    # )
  }
  output$correctnessW1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$tomorrowProb1)) && 
                     round(input$tomorrowProb1,2) == round(gameAns(game$correctMat, 1, 1),2))})
  
  output$correctnessW2 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$weekProb1)) && 
                     round(input$weekProb1, 2) == round(gameAns(game$correctMat, 1, 7), 2))})
  
  output$correctnessW3 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$monthProb1)) && 
                     round(input$monthProb1, 2) == round(gameAns(game$correctMat, 1, 30), 2))})
  
  output$correctnessW4 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$yearProb1)) && 
                     round(input$yearProb1, 2) == round(gameAns(game$correctMat, 1, 365), 2))})

  observeEvent(input$checkGame, {game$showFeedback <- T
  })
  
  observeEvent(input$tomorrowProb1, {game$showFeedback <- F})
  observeEvent(input$weekProb1, {game$showFeedback <- F})
  observeEvent(input$monthProb1, {game$showFeedback <- F})
  observeEvent(input$yearProb1, {game$showFeedback <- F})
  
  output$weatherProb <- renderText({
  weatherVars() # This may be temporary once I put in a "new probabilities" button
  paste("Every morning, Ben wakes up at 8am and looks out his window to check 
  the weather. If it rains today tomorrow it will rain with probability", 
  game$probW1, "If it doesn't rain today, then it will rain tomorrow with 
  probability", game$probW2,".")})
  
  observeEvent(input$newWeather, {
    game$showFeedback <- F
    game$probW1 <- sample(c(.5, .6, .7, .8, .9), size = 1, replace = T)
    game$probW2 <- sample(c(.5, .4, .3, .2, .1), size = 1, replace = T)
    game$correctMat <- matrix(c(game$probW1, 1-game$probW1, game$probW2, 1-game$probW2), 
                              nrow=2)})
  
  # STOP LIGHTS
  # Resets the probabilities of Green, Red, or Yellow lights. (based loosely on 
  # traffic light stats that I looked up; y=3-6 seconds, cycle = 1-3 min)
  createProbs <- reactive({
    game$probSLYY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLGG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                             game$probSLYG, game$probSLYY, game$probSLYR,
                             game$probSLRG, game$probSLRY, game$probSLRR), 
                             nrow=3, byrow = T)
  })
  
  observeEvent(input$newLights, {
    game$showFeedback <- F
    game$probSLYY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min=.02, max=.08),2)
    game$probSLGG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                                  game$probSLYG, game$probSLYY, game$probSLYR,
                                  game$probSLRG, game$probSLRY, game$probSLRR), 
                                nrow=3, byrow = T)})

  
  output$lightProb <- renderText({
    createProbs()
  paste0("A road you are travelling on features 10 traffic lights.
    These lights are timed such that if your current light is green, the probability 
    of the next light being green is ", game$probSLGG, " while the probability of 
    it being red is ", game$probSLGR, " with the remaining probability on yellow. 
    If the current light is yellow, then the probability of the next light being 
        green is ", game$probSLYG, " and the probability of the next light being 
        red is ", game$probSLYR, ". Lastly, if the current light is red, then the probability 
        of the next light being green is ", game$probSLRG, " while the probability of 
        it being red is ", game$probSLRR, ". Suppose the first light is green, predict
        the color of each of the following lights." 
    )})
  
  output$correctnessLG1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG1)) && 
                     round(input$TLG1, 2) == round(gameAns(game$correctMatSL, 1, 1), 2))})
  
  output$correctnessLR1 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR1)) && 
                     round(input$TLR1, 2) == round(gameAns(game$correctMatSL, 1, 1, col = 3), 2))})
  
  output$correctnessLG5 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG5)) && 
                     round(input$TLG5, 2) == round(gameAns(game$correctMatSL, 1, 5), 2))})

  
  output$correctnessLR5 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR5)) && 
                     round(input$TLR5, 2) == round(gameAns(game$correctMatSL, 1, 5, col = 3), 2))})
  
  output$correctnessLG9 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLG9)) && 
                     round(input$TLG9, 2) == round(gameAns(game$correctMatSL, 1, 9), 2))})
  
  output$correctnessLR9 <- renderUI(if(game$showFeedback){
    correctnessPic(!(is.na(input$TLR9)) && 
                     round(input$TLR9, 2) == round(gameAns(game$correctMatSL, 1, 9, col = 3), 2))})
  
  observeEvent(input$checkGameLights, {game$showFeedback <- T
  })
  observeEvent(input$TLG1, {game$showFeedback <- F})
  observeEvent(input$TLR1, {game$showFeedback <- F})
  observeEvent(input$TLG5, {game$showFeedback <- F})
  observeEvent(input$TLR5, {game$showFeedback <- F})
  observeEvent(input$TLG9, {game$showFeedback <- F})
  observeEvent(input$TLR9, {game$showFeedback <- F})
  
}



# Create Shiny App using BOAST App template
boastUtils::boastApp(ui = ui, server = server)
