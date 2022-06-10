# Load Packages ----
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

## Global functions ----
calcAnswer <- function(pMatrix, power, element, digits = 2){
  raisedMat <- matrixcalc::matrix.power(x = pMatrix, k = power)
  targetValue <- raisedMat[element[1], element[2]]
  return(round(targetValue, digits = digits))
}

gameAns <- function(correctMat, start, pow, col = 1){
  matrixcalc::matrix.power(correctMat, pow)[start, col]
}


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

# Define UI for App ----
ui <- list(
  useShinyjs(),
  dashboardPage(
    skin = "blue",
    ## Header ----
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
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
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
          div(
            style = "text-align: center;",
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
          p("This app was developed and coded by Leah Hunt in 2020 and updated
            in 2021 by Shravani Samala",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated",  "Last Update: 6/7/2022 by NJH.")
          )
        ), 
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"), 
          p("Review information about the convergence of discrete time Markov
            Chains and how to use the transition matrix to solve problems."), 
          br(), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = strong("Markov Chain Covergence"), 
            p("Definition: A random process is Markov if the probability of
              being in each of \\(k\\) possible states might depend on the
              previous step, but no further information would be provided by
              looking at where things stood at earlier steps."), 
            p("For \\(i = 1,\\ldots,k\\) a finite state discrete time Markov chain
              would then have the property that 
              \\(P\\{X_{n} = \\text{state i}\\} | X_{0}, X_{1},\\ldots,
              X_{n-2},X_{n-1}\\} = P\\{X_{n} = \\text{state i} | X_{n-1}\\}\\)."), 
            p("If this probability structure stays the same from step-to-step
              then the Markov chain is time homogeneous and its behavior will be
              independent of n and depend only on the probabilities \\(P_{i,j}
              = P\\{X_{n} = \\text{state j}|X_{n-1} = \\text{state i}\\}\\) and
              can be displayed in a \\(k\\times{k}\\) matrix \\(P = \\{P_{i,j}\\}
              \\) is called the", strong("Transition Matrix"),"of the Markov
              chain.")
          ), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = TRUE,
            title = strong("Example Problem"), 
            p("Random variable \\(X_{n}\\) tells you what the person ate on the
              \\(n^{th}\\) day and \\(X_{n}\\) can be one of \\(k=\\) four states
              (eggs, cereal, waffles, or pancakes) for \\(n = 1, 2, 3, ...\\)"), 
            p("Probability for the various choices of what to eat for breakfast
              might be affected by what they had yesterday (e.g., a person may
              be reluctant to eat eggs two days in a row), but after taking 
              yesterday’s meal into account, the person’s decision would not be
              affected by what they ate two or three days before."), 
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
                      yesterday is 0.40."
              ), 
              tags$li("Probability that person eats eggs on Wednesday given that
              they had eggs two days prior on Monday is: \\(0.05(0.05) + 0.45(0.45)
              + 0.25(0.4) + 0.25(0.40) = 0.405\\)."
              ),
              tags$ul(
                tags$li("Sum over the probabilities for what the person eats on
                        Tuesday times the probability they go to eggs from that
                        on Wednesday.")
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
            p("Similarly, we see that the (i,j)th element of \\(P^n\\) (the
              \\(n^{th}\\) power of the transition matrix \\(P\\) gives the
              probability of moving from state i to state j in n steps."), 
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
            title = strong("Definitions"),
            collapsible = TRUE,
            collapsed = TRUE,
            tags$ul(
              tags$li(
                strong("Limiting Distribution:"), "as the number of steps goes
                to infinity, the chance of being in a state i converges to a
                value, \\(π_i\\)." 
              ), 
              tags$li(
                strong("Irreducible:"), "when a Markov Chain has a nozero
                probability of getting from any state to any other state in a
                finite number of moves." 
              ), 
              tags$li(
                strong("Aperiodic:"), "when a Markov Chain has a nonzero
                probability of getting from some state back to itself in n state
                steps and in m steps where the greatest common divisor of n and
                m is 1." 
              )
            )
          ), 
          br(), 
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go2",
              label = "Explore!",
              size = "large",
              icon = icon("bolt")
            )
          ),
        ), 
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore Discrete Time Markov Chains and Their Long Run Behavior"),
          p("In this section, you will explore the behavior of Discrete Time 
            Markov chains over time and their behavior in the long run."),
          p(
            "Below you are given a calculator to help you understand the long run
            behavior. To use the calculator, you should first choose a size for 
            your probability matrix then fill in the transition probabilities. 
            Note that since this is a transition probability matrix, the rows
            must sum to 1. The Number of steps (\\(n\\)) allows you to choose how
            many steps in the future to consider the matrix, i.e. the calculator
            will take your probability matrix to the \\(n^{th}\\) power."
          ),
          p(
            "Use the calculator to answer the problems for each scenario. To 
            generate new numbers for the problem, click the 'New problem' button.
            Notice the rate at which the chain approaches long run behavior in  
            each problem."
          ),
          br(),
          #### Calculator set up ----
          fluidRow(
            column(
              width = 4,
              wellPanel(
                # Input for number of rows in the matrix
                selectInput(
                  inputId = "nStates", 
                  label = "Number of states in the matrix", 
                  choices = c(2,3,4,5), 
                  selected = 2
                ),
                matrixInput(
                  inputId = "userMatrix",
                  label = "Enter the probabilities",
                  value = matrix(diag(2), nrow = 2, dimnames = list(0:1, 0:1)),
                  rows = list(names = TRUE, editableNames = TRUE),
                  cols = list(names = TRUE, editableNames = TRUE),
                  class = "numeric"
                ),
                # Input for number of steps to take
                numericInput(
                  inputId = "steps", 
                  label = "Number of steps to take", 
                  min = 1, 
                  max = 10000, 
                  value = 1, 
                  step = 1
                ),
                bsButton(
                  inputId = "subMat", 
                  label = "Calculate matrix",
                  size = "large",
                  icon = icon("calculator"))
              )
            ),
            column(
              width = 8,
              offset = 0,
              DTOutput(outputId = "calcMatrix", width = "50%"),
            )
          ),
          br(),
          #### Tabs of Situations ----
          ## Note the problem text contain randomly generated values.
          tabsetPanel(
            id = "problems",
            type = "tabs",
            tabPanel(
              title = "Candy",
              br(),
              ##### Candy ----
              h3("Candy or Cookies"),
              uiOutput("candyProb"), 
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 0,
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "child1Prob", 
                        label = "Probability that the next child chooses candy", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4, 
                      br(), 
                      uiOutput("correctnessChild1")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "child5Prob", 
                        label = "Probability that the fifth child chooses candy", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4, 
                      br(), 
                      uiOutput("correctnessChild5")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "child10Prob", 
                        label = "Probability that the tenth child chooses candy", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01)
                    ),
                    column(
                      width = 4, 
                      br(), 
                      uiOutput("correctnessChild10")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "child20Prob", 
                        label = "Probability that the last child chooses candy", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4,
                      br(), 
                      uiOutput("correctnessChild20")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 4,
                      bsButton(
                        inputId = "checkCandy", 
                        label = "Check answer",
                        size = "large"
                      )
                    ),
                    column(
                      width = 8,
                      bsButton(
                        inputId = "newCandy", 
                        label = "New problem",
                        size = "large",
                        icon = icon("retweet")
                      )
                    )
                  )
                ),
                column(
                  width = 7,
                  checkboxInput(
                    inputId = "showCandyPlots", 
                    label = "Show plots for candy sample paths"
                  ),
                  conditionalPanel( 
                    condition = "input.showCandyPlots",
                    plotOutput(outputId = "candyClass", height = "250px"),
                    plotOutput("candyCumulative", height = "300px"),
                    bsButton(
                      inputId = "newCandyPlotSamples", 
                      label = "New Sample Path",
                      size = "large",
                      icon = icon("retweet")
                    )
                  )
                )
              )
            ),
            ##### Traffic ----
            tabPanel(
              title = "Traffic Lights",
              br(),
              h2("Traffic Lights"),
              textOutput("lightProb"),
              br(),
              fluidRow(
                column(
                  width = 3,
                  numericInput(
                    inputId = "TLG1", 
                    label = "Probability of Next Light Green", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1, 
                  br(), 
                  uiOutput("correctnessLG1")
                ),
                column(
                  width = 3,
                  offset = 2,
                  numericInput(
                    inputId = "TLR1", 
                    label = "Probability of Next Light Red", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1,
                  br(),
                  uiOutput("correctnessLR1")
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  numericInput(
                    inputId = "TLG5", 
                    label = "Probability of Fifth Light from Now Green", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1,
                  br(), 
                  uiOutput("correctnessLG5")
                ),
                column(
                  width = 3,
                  offset = 2,
                  numericInput(
                    inputId = "TLR5", 
                    label = "Probability of Fifth Light from Now Red", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1,
                  br(), 
                  uiOutput("correctnessLR5")
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  numericInput(
                    inputId = "TLG9", 
                    label = "Probability of Last Light Green", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1,
                  br(),
                  uiOutput("correctnessLG9")
                ),
                column(
                  width = 3,
                  offset = 2,
                  numericInput(
                    inputId = "TLR9", 
                    label = "Probability of Last Light Red", 
                    value = NA,
                    min = 0,
                    max = 1,
                    step = .01
                  )
                ),
                column(
                  width = 1,
                  br(), 
                  uiOutput("correctnessLR9")
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "checkTraffic", 
                    label = "Check Answer",
                    size = "large"
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "newTraffic", 
                    label = "New Problem",
                    size = "large",
                    icon = icon("retweet")
                  )
                )
              ),
              checkboxInput(
                inputId = "showTrafficPLots", 
                label = "Show plots for light sample paths"
              ),
              ##### Improvement here ----
              ## Update the alt text to new approaches
              conditionalPanel( 
                condition = "input.showTrafficPLots",
                plotOutput("lightDaily", height = '250px'),
                htmlOutput("lightDailyAlt"),
                plotOutput("lightCumulativeProb", height = '300px'),
                htmlOutput("lightCumulativeAlt"),
                bsButton(
                  inputId = "newLightPlotSamples", 
                  label = "New Sample Path",
                  size = "large",
                  icon = icon("retweet")
                )
              )
            ),
            ##### Weather ----
            tabPanel(
              title = "Weather",
              br(),
              h2("Rain or No Rain"),
              uiOutput("weatherProb"),
              fluidRow(
                column(
                  width = 4,
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "tomorrowProb1", 
                        label = "Probability of rain tomorrow", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4, 
                      br(), 
                      uiOutput("correctnessW1")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "weekProb1", 
                        label = "Probability of rain one week from today", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4,
                      br(), 
                      uiOutput("correctnessW2")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "monthProb1", 
                        label = "Probability of rain one month from today", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4, 
                      br(), 
                      uiOutput("correctnessW3")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 8, 
                      numericInput(
                        inputId = "yearProb1", 
                        label = "Probability of rain one year from today", 
                        value = NA,
                        min = 0,
                        max = 1,
                        step = .01
                      )
                    ),
                    column(
                      width = 4,
                      br(), 
                      uiOutput("correctnessW4")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      bsButton(
                        inputId = "checkWeather",
                        label = "Check Answer",
                        size = "large"
                      )
                    ),
                    column(
                      width = 6,
                      bsButton(
                        inputId = "newWeather", 
                        label = "New Problem",
                        size = "large",
                        icon = icon("retweet")
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  checkboxInput(
                    inputId = "showWeatherPlots", 
                    label = "Show plots for weather sample paths"
                  ),
                  conditionalPanel( 
                    condition = "input.showWeatherPlots",
                    plotOutput(outputId = "weatherMonth", height = "250px"),
                    plotOutput(outputId = "weatherYear", height = "300px"),
                    bsButton(
                      inputId  = "newWeatherPlotSamples", 
                      label = "New Sample Path",
                      size = "large",
                      icon = icon("retweet")
                    )
                  )
                )
              )
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          #### Update after rest of improvements ----
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


# Define server logic ----
server <- function(input, output, session) {
  ## Set user specific elements ----
  ### power matrix
  calculatedMatrix <- reactiveVal(NULL)

  ### All of the variables that will be tracked in the various scenarios
  game <- reactiveValues(
    score = 0, context = "", probw1 = 0, probw2 = 0, 
    probSLRG = 0, probSLGY = 0, probSLGR = 0, probSLGG = 0,
    probSLYY = 0, probSLYR = 0, probSLYG = 0,
    probSLRY = 0, probSLRR = 0, probCandy = 0, 
    probCookie = 0, inLab = "", correctMat = diag(2), 
    correctMatSL = diag(3), correctMatC = diag(2),
    showFeedback = FALSE
  )
  
  ## info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "In this app, you will explore the long run behavior of discrete
        time Markov Chains using several example problems for illustration. Solve
        each problem to see how quickly the chain approaches long run behavior
        in the examples.",
        type = "info"
      )
    }
  )
  
  ## Overview page's go button ----
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = { 
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "prerequisites" 
      ) 
    })
  
  ## Prereq page's go button ----
  observeEvent(
    eventExpr = input$go2, 
    handlerExpr = { 
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "explore" 
      ) 
    })
  
  ## Probability Matrix Calculator ----
  ### Dynamically update the probability matrix size ----
  observeEvent(
    eventExpr = input$nStates,
    handlerExpr = {
      matrixSize <- as.numeric(input$nStates)
      updateMatrixInput(
        session = session,
        inputId = "userMatrix",
        value = matrix(
          data = diag(matrixSize),
          nrow = matrixSize,
          dimnames = list(0:(matrixSize - 1), 0:(matrixSize - 1))
        )
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  ### Calculate result matrix ----
  observeEvent(
    eventExpr = input$subMat,
    handlerExpr = {
      if (any(input$userMatrix < 0)) {
        ### Check if there are any negative values
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Negative Probabilities",
          "There is at least one negative probability value detected in your matrix.
          Please double check and fix any entries."
        )
      } else if (any(rowSums(input$userMatrix) != 1)) {
        ### Check rows add to 1
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Rows Don't Add to One",
          "There is at least one row that does not add to one. Please double
          check and fix any entries."
        )
      } else if (input$steps < 0) {
        ### Check if number of steps is negative
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Negative Number of Steps",
          "You've asked for a negative number of steps. Please double
          check and put in a whole number of steps."
        )
      } else {
        ### Check if the number of steps is a whole number and change input if not
        if (!is.integer(input$steps)) {
          updateNumericInput(
            session = session,
            inputId = "steps",
            value = as.integer(input$steps)
          )
        }
        
        ### Calculate the new matrix
        calculatedMatrix(
          round(
            x = matrixcalc::matrix.power(
              x = input$userMatrix,
              k = as.integer(input$steps)
            ),
            digits = 4
          )
        )
        
        ### Display the new matrix
        output$calcMatrix <- renderDT(
          expr = {
            validate(
              need(
                expr = !is.null(calculatedMatrix()),
                message = "Click the Calculate matrix button to see the matrix."
              )
            )
            calculatedMatrix()
          },
          caption = paste(
            "Your matrix", isolate(as.integer(input$steps)), "step(s) out"
          ),
          style = "bootstrap4",
          options = list(
            responsive = TRUE,
            scrollX = FALSE,
            ordering = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE
          )
        )
      } 
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  ### Hide calculated matrix if inputs get changed ----
  observeEvent(
    eventExpr = c(input$nStates, input$userMatrix),
    handlerExpr = {
      calculatedMatrix(NULL)
    }
  )
  
  ## Candy Scenario ----
  ### Generate initial values ----
  candyValues <- reactiveValues(
    probCandy = 5, # Prob(Candy|Cookie)
    probCookie = 5, # Prob(Cookie|Cookie)
    probMatrix = diag(1),
    validAnswers = FALSE
  )
  
  observeEvent(
    eventExpr = input$newCandy,
    handlerExpr = {
      candyValues$probCandy <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
      candyValues$probCookie <- sample(c(.6, .7, .8, .9), size = 1, replace = T)
      candyValues$probMatrix <- matrix(
        data = c(candyValues$probCandy, 1 - candyValues$probCandy,
                 1 - candyValues$probCookie, candyValues$probCookie),
        nrow = 2,
        byrow = TRUE
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  ### Display scenario text ----
  # Set up text for the candy problem
  output$candyProb <- renderUI(
    expr = {
      div(
        p(paste0("Mrs. Gamble's kindergarten class has 20 students. One day, she
             brings in a jar of cookies and a bag of candy and gives each student
             the choice between either candy or a cookie. Each student then 
             sequentially chooses a treat. Without any influence from the other
             students, each student is equally likely to choose either option,
             but being impressionable young children, the choice of each student
             is impacted by the choice of the previous student. As such, if the
             previous student chose candy, then the next student will choose candy
             with probability ", candyValues$probCandy, ", and if the previous
             student chose a cookie then the next student will choose a cookie
             with probability ", candyValues$probCookie, ".")),
        p("Assuming that the first student chooses a cookie, calculate the
        following probabilities.")
      )
    }
  )
  
  ### Check answers ----
  observeEvent(
    eventExpr = input$checkCandy,
    handlerExpr = {
      if (any(is.na(input$child1Prob), is.na(input$child5Prob),
              is.na(input$child10Prob), is.na(input$child20Prob))) {
        sendSweetAlert(
          session = session,
          title = "Missing Values",
          type = "warning",
          text = "You have one or more answers missing. Please answer all questions
          before checking all answers."
        )
        candyValues$validAnswers <- FALSE
      } else if (any(input$child1Prob < 0, input$child5Prob < 0,
                     input$child10Prob < 0, input$child20Prob < 0)) {
        sendSweetAlert(
          session = session,
          title = "Negative Probabilities",
          type = "error",
          text = "At least one of your answers is a negative value. Please check
          your answers and fix accordingly."
        )
        candyValues$validAnswers <- FALSE
      } else if (any(input$child1Prob > 1, input$child5Prob > 1,
                     input$child10Prob > 1, input$child20Prob > 1)) {
        sendSweetAlert(
          session = session,
          title = "Probabilities Greater Than 1",
          type = "error",
          text = "At least one of your answers a values greater than 1. Please check
          your answers and fix accordingly."
        )
        candyValues$validAnswers <- FALSE
      } else {
        candyValues$validAnswers <- TRUE
        output$correctnessChild1 <- renderIcon(
          icon = ifelse(
            test = round(input$child1Prob, digits = 2) == calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 1,
              element = c(2,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessChild5 <- renderIcon(
          icon = ifelse(
            test = round(input$child5Prob, digits = 2) == calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 5,
              element = c(2,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessChild10 <- renderIcon(
          icon = ifelse(
            test = round(input$child10Prob, digits = 2) == calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 10,
              element = c(2,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessChild20 <- renderIcon(
          icon = ifelse(
            test = round(input$child20Prob, digits = 2) == calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 20,
              element = c(2,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  ### Hide answers when inputs change ----
  observeEvent(
    eventExpr = c(input$child1Prob, input$child5Prob,
                  input$child10Prob, input$child20Prob),
    handlerExpr = {
      if (candyValues$validAnswers) {
        output$correctnessChild1 <- renderIcon()
        output$correctnessChild5 <- renderIcon()
        output$correctnessChild10 <- renderIcon()
        output$correctnessChild20 <- renderIcon()
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  ### Reset answers on new problem ----
  observeEvent(
    eventExpr = input$newCandy, 
    handlerExpr = {
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
    }
  )
  
  ### Create data for plots ----
  candyData <- eventReactive(
    eventExpr = c(input$newCandyPlotSamples, candyValues$probCandy),
    valueExpr = {
      intData <- data.frame(
        child = 1:20,
        state = c(2, rep(0, 19))
      )
      for (i in 2:20) {
        intData$state[i] <- sample(
          x = c(1,2),
          size = 1,
          replace = TRUE,
          prob = candyValues$probMatrix[intData$state[i - 1], ]
        )
      }
      intData$cCookies <- cumsum(intData$state == 2)
      intData$cCookieProp <- intData$cCookies / intData$child
      intData$cCandyProp <- 1 - intData$cCookieProp
      
      intData
    }
  )
  
  ### Display plots ----
  output$candyClass <- renderPlot(
    expr = {
      ggplot(
        data = candyData(),
        mapping = aes(x = child, y = state)
      ) +
        geom_point(size = 2) +
        geom_path() +
        theme_bw() +
        xlab("Child") +
        ylab("State") +
        scale_y_continuous(breaks = 1:2, labels = c("Candy", "Cookie")) +
        ggtitle("Student's Choices in Order") +
        theme(
          text = element_text(size = 18)
        ) +
        scale_x_continuous(
          limits = c(1, 20),
          breaks = c(1, 5, 10, 15, 20),
          expand = expansion(mult = 0, add = 0.5)
        )
    },
    alt = "This plot shows the choice of each student for one simulation of 
             the process."
  )
  
  output$candyCumulative <- renderPlot(
    expr = {
      ggplot(
        data = candyData(),
        mapping = aes(x = child)
      ) +
        geom_path(
          mapping = aes(y = cCookieProp, color = "Cookie"),
          size = 1
        ) +
        geom_path(
          mapping = aes(y = cCandyProp, color = "Candy"),
          size = 1
        ) +
        geom_hline(
          mapping = aes(
            color = "Candy",
            yintercept = calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 500,
              element = c(2, 1),
              digits =  2
            )
          ),
          linetype = "dashed",
          size = 1,
        ) +
        geom_hline(
          mapping = aes(
            color = "Cookie",
            yintercept = calcAnswer(
              pMatrix = candyValues$probMatrix,
              power = 500,
              element = c(2, 2),
              digits =  2
            )
          ),
          linetype = "dashed",
          size = 1,
        ) +
        theme_bw() +
        xlab("Child") +
        ylab("Cumulative proportion") +
        ggtitle("Cumulative Proportion of Choices") +
        scale_color_manual(
          name = "Choice",
          values = boastPalette
        ) +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        scale_x_continuous(
          limits = c(1, 20),
          breaks = c(1, 5, 10, 15, 20),
          expand = expansion(mult = 0, add = 0.5)
        )

    },
    alt = "This plot shows the cumulative proportion of each student's choice
            for one simulation. Over time, these proportions get closer to the
             long run proportions."
  )
  
  ## Traffic Scenario ----

  
  ## Weather Scenario ----
  ### Generate initial values ----
  weatherValues <- reactiveValues(
    probRain = 5, # Prob(Rain|Rain)
    probNoRain = 5, # Prob(Rain|No rain)
    probMatrix = diag(1),
    validAnswers = FALSE
  )

  observeEvent(
    eventExpr = input$newWeather,
    handlerExpr = {
      weatherValues$probRain <- sample(c(.5, .6, .7, .8, .9), size = 1, replace = T)
      weatherValues$probNoRain <- sample(c(.5, .4, .3, .2, .1), size = 1, replace = T)
      weatherValues$probMatrix <- matrix(
        data = c(weatherValues$probRain, 1 - weatherValues$probRain,
                 weatherValues$probNoRain, 1 - weatherValues$probNoRain),
        nrow = 2,
        byrow = TRUE
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  ### Display scenario text ----
  # Set up text for the candy problem
  output$weatherProb <- renderUI(
    expr = {
      div(
        p(paste0("Every morning, Ben wakes up at 8am and looks out his window to
             check the weather. If it rains today, it will rain tomorrow with
             probability ", weatherValues$probRain, ". If it doesn't rain today,
             then it will rain tomorrow with probability ",
             weatherValues$probNoRain, ". Treat one month as 30 days and one
             year as 365 days.")),
        p("Today, it rained. Calculate the probability that it will rain 
              each of the following days.")
      )
    }
  )

  ### Check answers ----
  observeEvent(
    eventExpr = input$checkWeather,
    handlerExpr = {
      if (any(is.na(input$tomorrowProb1), is.na(input$weekProb1),
              is.na(input$monthProb1), is.na(input$yearProb1))) {
        sendSweetAlert(
          session = session,
          title = "Missing Values",
          type = "warning",
          text = "You have one or more answers missing. Please answer all questions
          before checking all answers."
        )
        weatherValues$validAnswers <- FALSE
      } else if (any(input$tomorrowProb1 < 0, input$weekProb1 < 0,
                     input$monthProb1 < 0, input$yearProb1 < 0)) {
        sendSweetAlert(
          session = session,
          title = "Negative Probabilities",
          type = "error",
          text = "At least one of your answers is a negative value. Please check
          your answers and fix accordingly."
        )
        weatherValues$validAnswers <- FALSE
      } else if (any(input$tomorrowProb > 1, input$weekProb1 > 1,
                     input$monthProb1 > 1, input$yearProb1 > 1)) {
        sendSweetAlert(
          session = session,
          title = "Probabilities Greater Than 1",
          type = "error",
          text = "At least one of your answers a values greater than 1. Please check
          your answers and fix accordingly."
        )
        weatherValues$validAnswers <- FALSE
      } else {
        weatherValues$validAnswers <- TRUE
        output$correctnessW1 <- renderIcon(
          icon = ifelse(
            test = round(input$tomorrowProb1, digits = 2) == calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 1,
              element = c(1,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessW2 <- renderIcon(
          icon = ifelse(
            test = round(input$weekProb1, digits = 2) == calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 7,
              element = c(1,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessW3 <- renderIcon(
          icon = ifelse(
            test = round(input$monthProb1, digits = 2) == calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 30,
              element = c(1,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        output$correctnessW4 <- renderIcon(
          icon = ifelse(
            test = round(input$yearProb1, digits = 2) == calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 365,
              element = c(1,1),
              digits = 2
            ),
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ### Hide answers when inputs change ----
  observeEvent(
    eventExpr = c(input$tomorrowProb1, input$weekProb1,
                  input$monthProb1, input$yearProb1),
    handlerExpr = {
      if (weatherValues$validAnswers) {
        output$correctnessW1 <- renderIcon()
        output$correctnessW2 <- renderIcon()
        output$correctnessW3 <- renderIcon()
        output$correctnessW4 <- renderIcon()
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ### Reset answers on new problem ----
  observeEvent(
    eventExpr = input$newWeather,
    handlerExpr = {
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
    }
  )

  ### Create data for plots ----
  weatherData <- eventReactive(
    eventExpr = c(input$newWeatherPlotSamples, weatherValues$probRain),
    valueExpr = {
      intData <- data.frame(
        day = 0:365,
        state = c(1, rep(0, 365))
      )
      for (i in 2:366) {
        intData$state[i] <- sample(
          x = c(1,2),
          size = 1,
          replace = TRUE,
          prob = weatherValues$probMatrix[intData$state[i - 1], ]
        )
      }
      intData$cRain <- cumsum(intData$state == 1)
      intData$cRainProp <- intData$cRain / (intData$day + 1)
      intData$cNoRainProp <- 1 - intData$cRainProp

      intData
    }
  )

  ### Display plots ----
  output$weatherMonth <- renderPlot(
    expr = {
      ggplot(
        data = weatherData()[1:31, ],
        mapping = aes(x = day, y = state)
      ) +
        geom_point(size = 2) +
        geom_path() +
        theme_bw() +
        xlab("Days in the future") +
        ylab("State") +
        scale_y_continuous(breaks = 1:2, labels = c("Rain", "No Rain")) +
        ggtitle("States by Day (First Month)") +
        theme(
          text = element_text(size = 18)
        ) +
        scale_x_continuous(
          limits = c(0, 30),
          breaks = c(0, 1, 5, 10, 15, 20, 25, 30),
          expand = expansion(mult = 0, add = 0.5)
        )
    },
    alt = "This plot shows the weather for one month of samples drawn from
           the chain in the problem."
  )

  output$weatherYear <- renderPlot(
    expr = {
      ggplot(
        data = weatherData(),
        mapping = aes(x = day)
      ) +
        geom_path(
          mapping = aes(y = cRainProp, color = "Rain"),
          size = 1
        ) +
        geom_path(
          mapping = aes(y = cNoRainProp, color = "No Rain"),
          size = 1
        ) +
        geom_hline(
          mapping = aes(
            color = "Rain",
            yintercept = calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 365,
              element = c(1, 1),
              digits =  2
            )
          ),
          linetype = "dashed",
          size = 1,
        ) +
        geom_hline(
          mapping = aes(
            color = "No Rain",
            yintercept = 1 - calcAnswer(
              pMatrix = weatherValues$probMatrix,
              power = 365,
              element = c(1, 1),
              digits =  2
            )
          ),
          linetype = "dashed",
          size = 1,
        ) +
        theme_bw() +
        xlab("Days in the future") +
        ylab("Cumulative proportion") +
        ggtitle("Cumulative Proportion By Type of Day") +
        scale_color_manual(
          name = "Choice",
          values = boastPalette
        ) +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        scale_x_continuous(
          limits = c(0, 365),
          breaks = c(0, 50, 100, 150, 200, 250, 300, 350),
          expand = expansion(mult = 0, add = 0.5)
        )
    },
    alt = "This plot shows the cumulative proportion of days of each weather type
            for one simulated year. Over time, these proportions get closer to the 
             long run proportions."
  )

# OLD CODE BELOW ----
  ### Traffic lights context ----
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
  observeEvent(input$newTraffic, {
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
  observeEvent(input$checkTraffic, {game$showFeedback <- T})
  observeEvent(input$TLG1, {game$showFeedback <- F})
  observeEvent(input$TLR1, {game$showFeedback <- F})
  observeEvent(input$TLG5, {game$showFeedback <- F})
  observeEvent(input$TLR5, {game$showFeedback <- F})
  observeEvent(input$TLG9, {game$showFeedback <- F})
  observeEvent(input$TLR9, {game$showFeedback <- F})
  
  observeEvent(input$pages, {click("newTraffic")})
  
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
  #### Plots ----
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
    data <- tidyr::pivot_longer(lightSteps(), cols = c("Red", "Yellow", "Green"), 
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
            legend.title = element_text(size = 16),
            legend.position = "bottom"
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
  
}
  

# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)
