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
library(matrixcalc)
library(DT)


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
  if (condition) {
    renderIcon(icon = "correct", html = TRUE)
  }
  else{
    renderIcon(icon = "incorrect", html = TRUE)
  }
}

# Define UI for App
ui <- list(
  useShinyjs(),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css",
  # href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  # ),
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      title = "MC Convergence", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Markov_Chain_Convergence")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
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
                    long run behavior."),
            tags$li('Click the "Show plots for sample paths" checkbox to see a 
                    sample path that the chain could take for the problem.')
          ),
          br(), 
          ##### Go Button
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "Prerequisites!",
              size = "large",
              icon = icon("book")
            )
          ),
          br(), 
          br(), 
          h2("Acknowledgements"), 
          p("This app was developed and coded by Leah Hunt and updated in 2021
             by Shravani Samala"),
            br(), 
            br(), 
            br(), 
            div(class = "updated", 
                "Last Update: 7/13/2020 by SJS.")
        ), 
        
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"), 
          p("Review the definition of Markov Covergence and how to use the different
            Markov Matrices to solve problems."), 
          br(), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = h3("Markov Chain Covergence"), 
            p("Definition: A random process is Markov if the probability of being in each 
            state might depend on the previous step, but no further information
            would be provided by looking at where things stood at earlier steps"), 
            p("For \\(i = 1,…,k\\) a finite state discrete time Markov chain would 
            then have the property that \\(P\\{X_{n} = \\text{state i}\\} | X_{0}, X_{1},...,
            X_{n-2},X_{n-1} = P\\{X_{n} = \\text{state i} |X_{n-1}\\}\\)."), 
            p("If this probability structure stays the same from step-to-step then
            the Markov chain is time homogeneous and its behavior will be independent
            of n and depend only on the probabilities \\(P_{i,j} = P\\{X_{n} = \\text{state j}
            |X_{n-1} = \\text{state i}\\}\\) and can be displayed in a \\(k\\times{k}\\) matrix
            \\(P = P_{i,j}\\) called the", strong("Transition Matrix"), "of the Markov chain.")
          ), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = h3("Example Problem"), 
            p("Random varibale \\(x_{n}\\) tells you what the person ate
            on the \\(n^{th}\\) day and \\(x_{n}\\) can be one of k = four states
            (eggs, cereal, waffles, or pancakes) for \\(n = 1, 2, 3, ...\\)"), 
            p("Probability for the various choices of what to eat for breakfast
            might be affected by what they had yesterday (e.g., a person may be
            reluctant to eat eggs two days in a row), but after taking yesterday’s
            meal into account, the person’s decision would not be affected by what
            they ate two or three days before."), 
            br(),
            
            fluidRow(
              column(
                offset = 1, 
                width = 1, 
                br(), 
                br(), 
                br(), 
                br(), 
                
                p("\\(P=\\)"), 
              ), 
              column(
                width = 10, 
                tags$table(
                  rules = "all",
                  border = "1pt",
                  align = "left",
                  #width = "500px",
                  targets = "_all",
                  tags$caption("Transition Matrix for Breakfast"),
                  tags$thead(
                    tags$tr(
                      tags$th("Breakfast"),
                      tags$th("Eggs", style = "text-align: center;"),
                      tags$th("Cereal", style = "text-align: center;"), 
                      tags$th("Waffles", style = "text-align: center;"), 
                      tags$th("Pancakes", style = "text-align: center;"),
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$th("Eggs", scope = "row"),
                      tags$td("0.05"),
                      tags$td("0.45"), 
                      tags$td("0.25"),
                      tags$td("0.25"), 
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Cereal", scope = "row"),
                      tags$td("0.45"),
                      tags$td("0.05"), 
                      tags$td("0.25"),
                      tags$td("0.25"), 
                      align = "center"
                    ), 
                    tags$tr(
                      tags$th("Waffles", scope = "row"),
                      tags$td("0.40"),
                      tags$td("0.40"), 
                      tags$td("0.05"),
                      tags$td("0.15"), 
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Pancakes", scope = "row"),
                      tags$td("0.40"),
                      tags$td("0.40"), 
                      tags$td("0.15"),
                      tags$td("0.05"), 
                      align = "center", 
                      width = "200%"
                    )
                  )
                ) 
              )
            ), 
            br(), 
            p("The columns signify Breakfast foods eaten today and the row headers
            signify the Breakfast foods eaten yesterday.", align = "center"),
            br(), 
            tags$ul(
              tags$li("Probability that person eats eggs today if they had waffles 
                      yesterday is 0.40"
              ), 
              tags$li("Probability that person eats eggs on Wednesday given that
              they had eggs two days prior on Monday is: \\(0.05(0.05) + 0.45(0.45)
              + 0.25(0.4) + 0.25(0.40) = 0.405\\)"
              ),
              tags$ul(
                tags$li("Sum over the probabilities for what the person eats on
                        Tuesday times the probability they go to eggs from that on
                        Wednesday")
              )
            ),
            br(), 
            p("The two step transition probabilities is given by: "),
            fluidRow(
              column(
                offset = 1, 
                width = 1, 
                br(), 
                br(), 
                br(), 
                br(), 
                
                p("\\(P^2=\\)"), 
              ), 
              column(
                width = 10, 
                tags$table(
                  rules = "all",
                  border = "1pt",
                  align = "left",
                  #width = "500px",
                  targets = "_all",
                  tags$caption("Transition Matrix for Breakfast"),
                  tags$thead(
                    tags$tr(
                      tags$th("Breakfast"),
                      tags$th("Eggs", style = "text-align: center;"),
                      tags$th("Cereal", style = "text-align: center;"), 
                      tags$th("Waffles", style = "text-align: center;"), 
                      tags$th("Pancakes", style = "text-align: center;"),
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$th("Eggs", scope = "row"),
                      tags$td("0.405"),
                      tags$td("0.245"), 
                      tags$td("0.175"),
                      tags$td("0.175"), 
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Cereal", scope = "row"),
                      tags$td("0.245"),
                      tags$td("0.405"), 
                      tags$td("0.175"),
                      tags$td("0.175"), 
                      align = "center"
                    ), 
                    tags$tr(
                      tags$th("Waffles", scope = "row"),
                      tags$td("0.280"),
                      tags$td("0.280"), 
                      tags$td("0.225"),
                      tags$td("0.215"), 
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Pancakes", scope = "row"),
                      tags$td("0.280"),
                      tags$td("0.280"), 
                      tags$td("0.215"),
                      tags$td("0.225"), 
                      align = "center", 
                      width = "200%"
                    )
                  )
                ) 
              )
            ), 
            br(), 
            p("Similarly, we see that the (i,j)th element of Pn (the nth power of
              the transition matrix P) gives the probability of moving from state
              i to state j in n steps."), 
            fluidRow(
              column(
                offset = 1, 
                width = 1, 
                br(), 
                br(), 
                br(), 
                br(), 
                
                p("\\(P^{365}=\\)"), 
              ), 
              column(
                width = 10, 
                tags$table(
                  rules = "all",
                  border = "1pt",
                  align = "left",
                  #width = "500px",
                  targets = "_all",
                  tags$caption("Transition Matrix for Breakfast"),
                  tags$thead(
                    tags$tr(
                      tags$th("Breakfast"),
                      tags$th("Eggs", style = "text-align: center;"),
                      tags$th("Cereal", style = "text-align: center;"), 
                      tags$th("Waffles", style = "text-align: center;"), 
                      tags$th("Pancakes", style = "text-align: center;"),
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$th("Eggs", scope = "row"),
                      tags$td("0.308"),
                      tags$td("0.308"), 
                      tags$td("0.192"),
                      tags$td("0.192"), 
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Cereal", scope = "row"),
                      tags$td("0.308"),
                      tags$td("0.308"), 
                      tags$td("0.192"),
                      tags$td("0.192"), 
                      align = "center"
                    ), 
                    tags$tr(
                      tags$th("Waffles", scope = "row"),
                      tags$td("0.308"),
                      tags$td("0.308"), 
                      tags$td("0.192"),
                      tags$td("0.192"),  
                      align = "center"
                    ),
                    tags$tr(
                      tags$th("Pancakes", scope = "row"),
                      tags$td("0.308"),
                      tags$td("0.308"), 
                      tags$td("0.192"),
                      tags$td("0.192"), 
                      align = "center", 
                      width = "200%"
                    )
                  )
                ) 
              )
            ), 
            br(), 
            p("After so many days, the distribution no longer depends on what you
              ate so long ago "),

          ), 
          box(
            width = 12, 
            title = h3("Definitions"),
            collapsible = TRUE,
            collapsed = FALSE,
            tags$ul(
              tags$li(
                "Limiting Distribution: As the number of steps goes to infinity,
                the chance of being in a state i converges to a value, \\(π_i\\)," 
              ), 
              tags$li(
                "Irreducible: when a Markov Chain has a nozero probability of
                getting from any state to any other state in a finite number of moves" 
              ), 
              tags$li(
                "Aperiodic: when a Markov Chain has a nonzero probability of
                getting from some state back to itself in n state eps and in m
                steps where the greatest common divisor of n and m is 1" 
              )
            )
          ), 
        
          br(), 
          ##### Go Button
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go2",
              label = "Explore!",
              size = "large",
              icon = icon("book")
            )
          ),
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
            to consider the matrix, i.e. the calculator will take your 
            probability matrix to the nth power."
          ),
          p(
            "Use the calculator to answer the problems for each scenario. To 
            generate new numbers for the problem, click the new problem button.
            Notice the rate at which the chain approaches long run behavior in  
            each problem."
          ),
          br(), 
          fluidRow(
            column(
              width = 12, 
              titlePanel("Transition Probability Calculator"),
              sidebarLayout(
                sidebarPanel(
                  # Input for number of rows in the matrix
                  selectInput(
                    inputId = "nrows", 
                    label = "Number of States in the Matrix:", 
                    choices = c(2,3,4,5), 
                    selected = 2),
                  
                  # Input to select current state of the matrix conditional on the 
                  # matrix size
                  conditionalPanel(
                    condition = "input.nrows == 2", 
                    matrixInput(
                      inputId = "ogMat2", 
                      value  = matrix(diag(2), nrow = 2, dimnames = list(0:1, 0:1)), 
                      rows = list(names = TRUE), 
                      cols = list( names = TRUE), 
                      class = "numeric")
                    ),
                  conditionalPanel(
                    condition = "input.nrows == 3",
                    matrixInput(
                      inputId = "ogMat3", 
                      value = matrix(diag(3), nrow = 3, dimnames = list(0:2, 0:2)), 
                      rows = list(names = TRUE), 
                      cols = list( names = TRUE), 
                      class = "numeric")
                  ),
                  conditionalPanel(
                    condition = "input.nrows == 4",
                    matrixInput(
                      inputId = "ogMat4", 
                      value = matrix(diag(4), nrow = 4, dimnames = list(0:3, 0:3)), 
                      rows = list(names = TRUE),
                      cols = list( names = TRUE), 
                      class = "numeric" )
                  ),
                  conditionalPanel(
                    condition = "input.nrows == 5",
                    matrixInput(
                      inputId = "ogMat5", 
                      value = matrix(diag(5), nrow = 5, dimnames = list(0:4, 0:4)), 
                      rows = list(names = TRUE), 
                      cols = list( names = TRUE), 
                      class = "numeric" )
                  ),
                  conditionalPanel(
                    condition = "input.nrows > 5 | input.nrows < 2",
                    "This number of rows is not supported."
                  ),
                  
                  # Input for number of steps to take
                  numericInput(
                    inputId = "steps", 
                     label = "Number of steps", 
                     min = 1, 
                     max = 10000, 
                     value = 1, 
                     step = 1),
                  bsButton(
                    inputId = "subMat", 
                    label = "Calculate Matrix")
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
            )
          ),
          br(), 
          tabsetPanel(
            id = "problems",
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
                          max = 1,
                          step = .01)
                        ),
                        column(width = 2, 
                               br(), 
                               uiOutput("correctnessChild1"))),
                    fluidRow(
                      column(
                        width = 10, 
                        numericInput(
                          inputId = "child5Prob", 
                          label = "Probability that the fifth child chooses candy", 
                          value = NA,
                          min = 0,
                          max = 1,
                          step = .01)
                      ),
                      column(width = 2, 
                             br(), 
                             uiOutput("correctnessChild5"))),
                    fluidRow(
                      column(
                        width = 10, 
                          numericInput(
                            inputId = "child10Prob", 
                             label = "Probability that the tenth child chooses 
                                      candy", 
                             value = NA,
                             min = 0,
                             max = 1,
                             step = .01)),
                      column(
                        width = 2, 
                        br(), 
                        uiOutput("correctnessChild10"))),
                    fluidRow(
                      column(
                        width = 10, 
                        numericInput(
                          inputId = "child20Prob", 
                          label = "Probability that the last child chooses candy", 
                          value = NA,
                          min = 0,
                          max = 1,
                          step = .01)),
                      column(
                        width = 2,
                        br(), 
                        uiOutput("correctnessChild20"))),
                    
                actionButton(
                  inputId = "checkCandy", 
                  label = "Check Answer"),
                actionButton(
                  inputId = "newCandy", 
                  label = "New Problem")),
                column(
                  width = 7,
                    checkboxInput(
                      inputId = "showCandyPlots", 
                      label = "Show plots for candy sample paths"),
                    conditionalPanel( 
                     condition = "input.showCandyPlots",
                     plotOutput("candyDaily", height = '250px'), 
                     htmlOutput("candyDailyAlt"),
                     plotOutput("candyCumulativeProb", height = '300px'),
                     htmlOutput("candyCumulativeAlt"),
                     actionButton(
                       inputId = "newCandyPlotSamples", 
                       label = "New Sample Path"))
                    )
                  )
                ),
          tabPanel(
            title = "Traffic Lights",
            h2("Traffic Lights"),
            textOutput("lightProb"),
            br(),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = "TLG1", 
                  label = "Probability of Next Light Green", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)),
              column(
                width = 1, 
                br(), 
                uiOutput("correctnessLG1")),
              column(
                width = 4,
                numericInput(
                  inputId = "TLR1", 
                  label = "Probability of Next Light Red", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)),
              column(
                width = 1,
                br(),
                uiOutput("correctnessLR1"))),
            fluidRow(  
              column(
                width = 4,
                numericInput(
                  inputId = "TLG5", 
                  label = "Probability of Fifth Light from Now Green", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)
              ),
              column(
                width = 1,
                br(), 
                uiOutput("correctnessLG5")),
              column(
                width = 4,
                numericInput(
                  inputId = "TLR5", 
                  label = "Probability of Fifth Light from Now Red", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)),
              column(
                width = 1,
                br(), 
                uiOutput("correctnessLR5"))
            ),
            fluidRow(  
              column(
                width = 4,
                numericInput(
                  inputId = "TLG9", 
                  label = "Probability of Last Light Green", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)
              ),
              column(
                width = 1,
                br(),
                uiOutput("correctnessLG9")),
              column(
                width = 4,
                numericInput(
                  inputId = "TLR9", 
                  label = "Probability of Last Light Red", 
                  value = NA,
                  min = 0,
                  max = 1,
                  step = .01)),
              column(
                width = 1,
                br(), 
                uiOutput("correctnessLR9"))
            ),
            actionButton(
              inputId = "checkGameLights", 
              label = "Check Answer"),
            actionButton(
              inputId = "newLights", 
              label = "New Problem"),
            checkboxInput(
              inputId = "showLightPlots", 
              label = "Show plots for light sample paths"),
            conditionalPanel( 
              condition = "input.showLightPlots",
              plotOutput("lightDaily", height = '250px'),
              htmlOutput("lightDailyAlt"),
              plotOutput("lightCumulativeProb", height = '300px'),
              htmlOutput("lightCumulativeAlt"),
              actionButton(
                inputId = "newLightPlotSamples", 
                label = "New Sample Path"))
          ),
          
          tabPanel(
            title = "Weather",
            h2("Rain or No Rain"),
            textOutput("weatherProb"),
            p("Today, it rained. Calculate the probability that it will rain 
              each of the following days."),
            fluidRow(
              column(
                width = 5,
                  fluidRow(
                    column(
                      width = 10, 
                      numericInput(
                        inputId = "tomorrowProb1", 
                         label = "Probability of Rain Tomorrow", 
                         value = NA,
                         min = 0,
                         max = 1,
                         step = .01)
                    ),
                    column(
                      width = 2, 
                      br(), 
                      uiOutput("correctnessW1"))),
                  fluidRow(
                    column(
                      width = 10, 
                        numericInput(
                          inputId = "weekProb1", 
                          label = "Probability of Rain One Week from Now", 
                          value = NA,
                          min = 0,
                          max = 1,
                          step = .01)),
                    column(width = 2,
                           br(), 
                           uiOutput("correctnessW2"))),
                  fluidRow(
                    column(
                      width = 10, 
                      numericInput(
                        inputId = "monthProb1", 
                        label = "Probability of Rain One Month from Now", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01)),
                    column(width = 2, 
                           br(), 
                           uiOutput("correctnessW3"))),
                  fluidRow(
                    column(
                      width = 10, 
                      numericInput(
                       inputId = "yearProb1", 
                       label = "Probability of Rain One Year from Now", 
                       value = NA,
                       min = 0,
                       max = 1,
                       step = .01)),
                    column(
                      width = 2,
                      br(), 
                      uiOutput("correctnessW4"))),
                  
                    bsButton(
                      inputId = "checkGame",
                      label = "Check Answer"),
                    bsButton(
                      inputId = "newWeather", 
                      label = "New Problem")),
                  column(
                    width = 6,
                    checkboxInput(
                      inputId = "showWeatherPlots", 
                      label = "Show plots for weather sample paths"),
                    conditionalPanel( 
                      condition = "input.showWeatherPlots",
                      plotOutput("weatherDaily", height = '250px'), 
                      htmlOutput("weatherDailyAlt"),
                      plotOutput("weatherCumulativeProb", height = '300px'),
                      htmlOutput("weatherCumulativeAlt"),
                      bsButton(
                        inputId  = "newWeatherPlotSamples", 
                        label = "New Sample Path"))
                  )
                )
              )
            )
          ),

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
            "Novomestky, F. (2012), matrixcalc: Collection of functions for 
            matrix calculations., R package. Available from
            https://CRAN.R-project.org/package=matrixcalc"
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
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)


# Define server logic

server <- function(input, output, session) {
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
  
  # Go button on Overview page
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = { 
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "Prerequisites" 
      ) 
    })
  
  observeEvent(
    eventExpr = input$go2, 
    handlerExpr = { 
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "Explore" 
      ) 
    })
  
  # Handles verification that the entered matrix is a probability matrix for 2x2 
  ogMat2 <- reactive({
   validate(
      need(min(input$ogMat2) >= 0 && sum(input$ogMat2[1,]) == 1 && 
             sum(input$ogMat2[2,]) == 1, 
           "Rows must be non-negative and sum to 1")
      )
    input$ogMat2
  })
  
  # Handles verification that the entered matrix is a probability matrix for 3x3 
  ogMat3 <- reactive({
    validate(
      need(min(input$ogMat3) >= 0 && sum(input$ogMat3[1,]) == 1 && 
             sum(input$ogMat3[2,]) == 1 && sum(input$ogMat3[3,]) == 1, 
           "Rows must sum to 1")
    )
    input$ogMat3
  })
  
  # Handles verification that the entered matrix is a probability matrix for 4x4 
  ogMat4 <- reactive({
    validate(
      need(min(input$ogMat4) >= 0 && sum(input$ogMat4[1,]) == 1 && 
             sum(input$ogMat4[2,]) == 1 && sum(input$ogMat4[3,]) == 1 && 
             sum(input$ogMat4[4,]) == 1, 
           "Rows must sum to 1")
    )
    input$ogMat4
  })
  
  # Handles verification that the entered matrix is a probability matrix for 5x5
  ogMat5 <- reactive({
    validate(
      need(min(input$ogMat5) >= 0 && sum(input$ogMat5[1,]) == 1 && 
             sum(input$ogMat5[2,]) == 1 && sum(input$ogMat5[3,]) == 1 && 
             sum(input$ogMat5[4,]) == 1 && sum(input$ogMat5[5,]) == 1, 
           "Rows must sum to 1")
    )
    input$ogMat5
  })
  
  # Gets the current probability matrix for the calculator based on number of rows
  getCurrentMatrix <- reactive({
    if (as.numeric(input$nrows) == 2) {
        ogMat2()
    }
    else if (as.numeric(input$nrows) == 3) {
      ogMat3()
    }
    else if (as.numeric(input$nrows) == 4) {
      ogMat4()
    }
    else{
      ogMat5()
    }
  })
  
  # Updates the matrix when the Calculate Matrix button is pressed
  updateMatrix <- eventReactive(input$subMat, {
    if (input$nrows == 2) {
      expr = as.data.frame(matrixcalc::matrix.power(ogMat2(),
                                                  as.integer(input$steps)), 
                         row.names = 0:1,
                         optional = TRUE)} 
    else if (input$nrows == 3) {
      as.data.frame(matrixcalc::matrix.power(ogMat3(),as.integer(input$steps)), 
                    row.names = 0:2, 
                    optional = TRUE)}
    else if (input$nrows == 4) {
      as.data.frame(matrixcalc::matrix.power(ogMat4(), as.integer(input$steps)), 
                    row.names = 0:3, 
                    optional = TRUE)} 
    else {
      as.data.frame(matrixcalc::matrix.power(ogMat5(),as.integer(input$steps)), 
                    row.names = 0:4, 
                    optional = TRUE)}})
  
  # Outputted matrix (the one to the nth power)
  output$mat <- renderTable({
    updateMatrix()}, rownames = TRUE, striped = FALSE
  )

  # PROBLEMS ----
  # All of the variables that will be tracked in the various scenarios
  game <- reactiveValues(score = 0, context = "", probw1 = 0, probw2 = 0, 
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
    game$correctMat <- matrix(c(game$probW1, 1 - game$probW1, 
                                game$probW2, 1 - game$probW2),
                              nrow = 2, byrow = TRUE)
  })
  
  # Used to get answers to the questions
  # Takes matrix correctMat to power pow then reads element [start, col]
  gameAns <- function(correctMat, start, pow, col = 1){
    matrixcalc::matrix.power(correctMat, pow)[start, col]
  }
  
  # Picture to go with weather question 1 (x or check)
  output$correctnessW1 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$tomorrowProb1)) && 
                     round(input$tomorrowProb1,2) == 
                     round(gameAns(correctMat = game$correctMat, 
                                   start = 1, 
                                   pow = 1),2))}) 
  
  # Picture to go with weather question 2 (x or check)
  output$correctnessW2 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$weekProb1)) && 
                     round(input$weekProb1, 2) == 
                     round(gameAns(correctMat = game$correctMat, 
                                   start = 1, 
                                   pow = 7), 2))})
  
  # Picture to go with weather question 3 (x or check)
  output$correctnessW3 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$monthProb1)) && 
                     round(input$monthProb1, 2) == 
                     round(gameAns(correctMat = game$correctMat, 
                                   start  = 1, 
                                   pow = 30), 2))})
  
  # Picture to go with weather question 4 (x or check)
  output$correctnessW4 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$yearProb1)) && 
                     round(input$yearProb1, 2) == 
                     round(gameAns(correctMat = game$correctMat, 
                                   start = 1, 
                                   pow = 365), 2))})

  # Show feedback after Check Game but reset to no feedback if any input is changed
  observeEvent(input$checkGame, {game$showFeedback <- T})
  observeEvent(input$tomorrowProb1, {game$showFeedback <- F})
  observeEvent(input$weekProb1, {game$showFeedback <- F})
  observeEvent(input$monthProb1, {game$showFeedback <- F})
  observeEvent(input$yearProb1, {game$showFeedback <- F})
  
  # Mostly here to make sure the weather vars get set at the beginning
  observeEvent(input$pages, {click("newWeather")})
  
  # Statement for the weather problem
  output$weatherProb <- renderText({
  paste("Every morning, Ben wakes up at 8am and looks out his window to check 
  the weather. If it rains today, it will rain tomorrow with probability", 
  game$probW1, "If it doesn't rain today, then it will rain tomorrow with probability", game$probW2,".")})
  
  # When user clicks the New Problem button for the weather scenario
  observeEvent(input$newWeather, {
    click("newWeatherPlotSamples") # Updates plot
    game$showFeedback <- F # Hides feedback
    game$probW1 <- sample(.1*(2:9), size = 1, replace = T) # Updates probabilities
    # Decides on the probability of no rain after rain; 
    # make sure it is less than rain to rain
    if (game$probW1 == .2) {options <- (.01*(1:10))}
    else{options <- .1*(1:(ceiling(game$probW1*5)))}
    game$probW2 <- sample(options, size = 1, replace = T)
    
    # Updates correct probability matrix based on above
    game$correctMat <- matrix(c(game$probW1, 1 - game$probW1, 
                                game$probW2, 1 - game$probW2), 
                              nrow = 2, byrow = TRUE)
    
    # Clear inputs when generating a new problem
    updateNumericInput(
      session = session,
      inputId = "tomorrowProb1",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "weekProb1",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "monthProb1",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "yearProb1",
      value = NA
    )
    })
  
  # Do walk of 365 days for weather problem
  weatherSteps <- eventReactive(input$newWeatherPlotSamples, {
    curState <- 0
    index <- 0:365
    states <- c(0)
    rainSum <- c(1)
    
    # For each day, sample either rain or not based on the day before
    for (x in 1:365) {
      curState <- sample(c(0,1), 1, replace = TRUE, prob = game$correctMat[curState + 1,])
      states <- c(states, curState)
      rainSum <- c(rainSum, rainSum[x] + 1 - curState)
    }
    
    # Return a data frame with the samples
    data.frame(day = index, 
               state = states, 
               Rain = rainSum/(index + 1), 
               None = 1 - rainSum/(index + 1))
  })
  
  # Create the daily weather plot for first week
  output$weatherDaily <- renderPlot({
      plot <- ggplot2::ggplot(aes(x = day, y = state), data = weatherSteps()[1:31,]) +
        geom_point(size = 2) +
        geom_path() +
        xlab("Day Number") + 
        ylab('State') +
        scale_y_continuous(breaks = 0:1, labels = c("Rain", "No Rain")) +
        ggtitle("States by Day Over the First Month") +
        theme(axis.text = element_text(size = 18),
              plot.title = element_text(size = 18, face = "bold"),
              axis.title = element_text(size = 18),
              panel.background = element_rect(fill = "white", color = "black"),
              legend.position = c(.89,1.07),
              legend.text = element_text(size = 14),
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
    plot <- ggplot2::ggplot(aes(x = day, y = Proportion, color = Rain), data = data) +
      geom_hline(aes(yintercept = 1 - gameAns(correctMat = game$correctMat, 
                                              start = 1, 
                                              pow = 365)), 
                 color = boastPalette[1], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(correctMat = game$correctMat, 
                                          start = 1, 
                                          pow = 365)), 
                 color = boastPalette[2], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      scale_colour_manual(values = boastUtils::boastPalette) +
      geom_path(lwd = 1) +
      xlab("Day Number") + 
      ylab('Cumulative portion of rainy days') +
      ggtitle("Portion of Days of Rain Over Time") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black"),
            legend.text = element_text(size = 14),
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
    game$probSLYY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLGG <- round(runif(n = 1, min = .5, max = .8), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                             game$probSLYG, game$probSLYY, game$probSLYR,
                             game$probSLRG, game$probSLRY, game$probSLRR), 
                             nrow = 3, byrow = T)
  })
  
  # Update lights values for a new problem
  observeEvent(input$newLights, {
    click("newLightPlotSamples")
    game$showFeedback <- F
    game$probSLYY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLYG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLYR <- 1 - game$probSLYY - game$probSLYG
    game$probSLRY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLRG <- round(runif(n = 1, min = .2, max = .7), 2)
    game$probSLRR <- 1 - game$probSLRY - game$probSLRG
    game$probSLGY <- round(runif(n = 1, min = .02, max = .08),2)
    game$probSLGG <- round(runif(n = 1, min = .5, max = .8), 2)
    game$probSLGR <- 1 - game$probSLGY - game$probSLGG
    game$correctMatSL <- matrix(c(game$probSLGG, game$probSLGY, game$probSLGR, 
                                  game$probSLYG, game$probSLYY, game$probSLYR,
                                  game$probSLRG, game$probSLRY, game$probSLRR), 
                                nrow = 3, byrow = T)
    # Clear inputs when generating a new problem
    updateNumericInput(
      session = session,
      inputId = "TLG1",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "TLR1",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "TLG5",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "TLR5",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "TLG9",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "TLR9",
      value = NA
    )
    })

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
  output$correctnessLG1 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLG1)) && 
                     round(input$TLG1, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 1), 2))})
  
  # Check or X for probability that the first light is red
  output$correctnessLR1 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLR1)) && 
                     round(input$TLR1, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 1, 
                                   col = 3), 2))})
  
  # Check or X for probability that the fifth light is green
  output$correctnessLG5 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLG5)) && 
                     round(input$TLG5, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 5), 2))})

  # Check or X for probability that the fifth light is red
  output$correctnessLR5 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLR5)) && 
                     round(input$TLR5, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 5, 
                                   col = 3), 2))})
  
  # Check or X for probability that the last light is green
  output$correctnessLG9 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLG9)) && 
                     round(input$TLG9, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 9), 2))})
  
  # Check or X for probability that the last light is red
  output$correctnessLR9 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$TLR9)) && 
                     round(input$TLR9, 2) == 
                     round(gameAns(correctMat = game$correctMatSL, 
                                   start = 1, 
                                   pow = 9, 
                                   col = 3), 2))})
  
  # Controls feedback display
  # Show feedback if the button is pressed; hide feedback if the answers change
  observeEvent(input$checkGameLights, {game$showFeedback <- T})
  observeEvent(input$TLG1, {game$showFeedback <- F})
  observeEvent(input$TLR1, {game$showFeedback <- F})
  observeEvent(input$TLG5, {game$showFeedback <- F})
  observeEvent(input$TLR5, {game$showFeedback <- F})
  observeEvent(input$TLG9, {game$showFeedback <- F})
  observeEvent(input$TLR9, {game$showFeedback <- F})
  
  observeEvent(input$pages, {click("newLights")})
  
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
    for (x in 1:10) {
      curState <- sample(c(0,1,2), 1, 
                         replace = TRUE, 
                         prob = game$correctMatSL[curState + 1,])
      states <- c(states, curState)
      if (curState == 0) {
        Green <- Green + 1
      }
      else if (curState == 1) {
        Yellow <- Yellow + 1
      }
      else {
        Red <- Red + 1
      }
      reds <- c(reds, Red)
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
    plot <- ggplot2::ggplot(aes(x = day, y = state), data = lightSteps()) +
      geom_point(size = 2) +
      geom_path() +
      xlab("Light Number") + 
      ylab('State') +
      scale_y_continuous(breaks = 0:2, labels = c("Green", "Yellow", "Red")) +
      ggtitle("States by Light Number") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black"),
            legend.position = c(.89,1.07),
            legend.text = element_text(size = 14),
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
  output$lightCumulativeProb <- renderPlot({
    data <- pivot_longer(lightSteps(), cols = c("Red", "Yellow", "Green"), 
                         names_to = "Color", values_to = "Proportion")
    data$Proportion <- data$Proportion / (data$day + 1)
    plot <- ggplot2::ggplot(aes(x = day, y = Proportion, color = Color), 
                          data = data) +
      scale_colour_manual(values = c("#009E73", "red", "#E69F00")) +
      geom_hline(aes(yintercept = gameAns(correctMat = game$correctMatSL, 
                                          start = 1, 
                                          pow = 100, 
                                          col = 1)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "#009E73", 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(correctMat = game$correctMatSL, 
                                          start = 1, 
                                          pow = 100, 
                                          col = 2)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "#E69F00", 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = gameAns(correctMat = game$correctMatSL, 
                                          start = 1, 
                                          pow = 100, 
                                          col = 3)), 
                 lwd = 1, 
                 linetype = "dashed", 
                 color = "red", 
                 show.legend = TRUE) +
      geom_path(lwd = 1) +
      geom_point(aes(shape = Color), size = 3) +
      xlab("Light number") + 
      ylab('Cumulative proportions') +
      ggtitle("Portion of Light Colors Over Time") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black"),
            legend.text = element_text(size = 14),
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
    game$correctMatC <- matrix(c(game$probCandy, 1 - game$probCandy, 
                                 1 - game$probCookie, game$probCookie),
                              nrow = 2, byrow = TRUE)
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
  output$correctnessChild1 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$child1Prob)) && 
                     round(input$child1Prob,2) == round(game$correctMatC[2,1],2))})
  
  # Output check or X for second question (child 5)
  output$correctnessChild5 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$child5Prob)) && 
                     round(input$child5Prob, 2) == 
                     round(gameAns(correctMat = game$correctMatC, 
                                        start = 2, 
                                        pow = 5), 2))})
  
  # Output check or X for third question (child 10)
  output$correctnessChild10 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$child10Prob)) && 
                     round(input$child10Prob, 2) == 
                     round(gameAns(correctMat = game$correctMatC, 
                                        start = 2, 
                                        pow = 10), 2))})
  
  # Output check or X for fourth question (child 20)
  output$correctnessChild20 <- renderUI(if (game$showFeedback) {
    correctnessPic(!(is.na(input$child20Prob)) && 
                     round(input$child20Prob, 2) == 
                     round(gameAns(correctMat = game$correctMatC, 
                                        start = 2, 
                                        pow = 20), 2))})
  
  # Control when feedback is displayed (only directly after pressing the button)
  observeEvent(input$checkCandy, {game$showFeedback <- T})
  observeEvent(input$child1Prob, {game$showFeedback <- F})
  observeEvent(input$child5Prob, {game$showFeedback <- F})
  observeEvent(input$child10Prob, {game$showFeedback <- F})
  observeEvent(input$child20Prob, {game$showFeedback <- F})
  observeEvent(input$problems, {game$showFeedback <- F})

  
  # When tabs switch, make sure feedback goes away and candy problem is updated
  observeEvent(input$pages, {
    game$showFeedback <- F
    click("newCandy") # Mostly to make sure that the initial problem renders
  })
  
  # When a new problem is requested, update numbers and plots
  observeEvent(input$newCandy, {
    click("newCandyPlotSamples")
    game$showFeedback <- F
    game$probCandy <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$probCookie <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
    game$correctMatC <- matrix(c(game$probCandy, 1 - game$probCandy, 
                                 1 - game$probCookie, game$probCookie),
                               nrow = 2, byrow = TRUE)
    
    # Clear inputs when generating a new problem
    updateNumericInput(
      session = session,
      inputId = "child1Prob",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "child5Prob",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "child10Prob",
      value = NA
    )
    updateNumericInput(
      session = session,
      inputId = "child20Prob",
      value = NA
    )
    })
  
  # Assigns all students a cookie or candy choice based on current probabilities
  candySteps <- eventReactive(input$newCandyPlotSamples, {
    curState <- 0
    index <- 1:20
    states <- c(1)
    cookieSum <- c(1)
    # Loop through all remaining students
    for (x in 2:20) {
      curState <- sample(c(0,1), 1, replace = TRUE, 
                         prob = game$correctMatC[curState + 1,])
      states <- c(states, curState)
      cookieSum <- c(cookieSum, cookieSum[x - 1] + curState)
    }
    
    # Return data frame of data generated by the simulation
    data.frame(day = index, 
               state = states,
               Cookie = cookieSum/(index), 
               Candy = 1 - cookieSum/(index))
  })
  
  # Plots the choices of each of the children
  output$candyDaily <- renderPlot({
    plot <- ggplot2::ggplot(aes(x = day, y = state), data = candySteps()) +
      geom_point(size = 2) +
      geom_path() +
      xlab("Student") +
      ylab('State') +
      scale_y_continuous(breaks = 0:1, labels = c("Candy", "Cookie")) +
      ggtitle("Children's Choices") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black"),
            legend.position = c(.89,1.07),
            legend.text = element_text(size = 14),
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
    plot <- ggplot2::ggplot(aes(x = day, y = Proportion, color = Choice), 
                          data = data) +
      geom_hline(aes(yintercept = gameAns(correctMat = game$correctMatC, 
                                          start = 1, 
                                          pow = 365)), 
                 color = boastPalette[1], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept = 1 - gameAns(correctMat = game$correctMatC, 
                                              start = 1, 
                                              pow = 365)), 
                 color = boastPalette[2], 
                 linetype = "dashed", 
                 lwd = 1, 
                 show.legend = TRUE) +
      scale_colour_manual(values = boastUtils::boastPalette) +
      geom_path(lwd = 1) +
      xlab("Student") +
      ylab('Cumulative portion by choice type') +
      ggtitle("Portion Student Choices") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black"),
            legend.text = element_text(size = 14),
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
