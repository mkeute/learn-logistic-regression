require(shiny)
require(shinythemes)

shinyUI(
  fluidPage(theme = shinytheme("spacelab"),

    titlePanel("Logistic Regression"),
      navbarPage("Learn the essentials",
         tabPanel(icon("home"),
                          
            fluidRow(column(
               textOutput("dmy"),
               br(),
               withMathJax(
                 includeMarkdown("intro_text.Rmd")
               ),
               br(),
               width=8)),
    
            hr(),
            fluidRow(column(DT::dataTableOutput("RawData"),
                                width = 12))
                          
         ),
                 
  
        tabPanel("Manipulate Data",
          sidebarLayout( 
            sidebarPanel(
              sliderInput("Cov1",p("Modify the correlation between CGI and t-cell count to see the effect"),
                                                   value=0,
                                                   min=-.8,
                                                   max=.8,
                                                   step=0.01),
              sliderInput("Cov2",p("Modify the correlation between CGI and Blood Pressure  to see the effect"),
                          value=0,
                          min=-.8,
                          max=.8,
                          step=0.01),
              sliderInput("proportion_sex",p("Modify the sex difference in CGI to see the effect"),
                          value=0.5,
                          min=0,
                          max=1,
                          step=0.01),
              sliderInput("proportion_smoker",p("Modify the smoker/non-smoker difference in CGI to see the effect"),
                          value=0.5,
                          min=0,
                          max=1,
                          step=0.01)
              
          ),
          mainPanel(
            column(br(),              
               plotOutput("scatter1"),
               br(),width=3
             ),
            column(br(),
               plotOutput("scatter2"),
               br(),width=3
             ),
            column(br(),
               plotOutput("scatter3"),br(),width=3
             ),
            column(br(),
             plotOutput("scatter4"),br(),width=3
            )
          )#close mainPanel
          )#close sidebarLayout
        ), #close tabPanel
        tabPanel("Linear Regression Recap",
                 
                 fluidRow(column(
                   
                   br(),
                   withMathJax(
                   includeMarkdown("linreg_text.Rmd")
                   ),
                   br(),
                   verbatimTextOutput("console_message_linreg"),
                   
                   br(),
                   plotOutput("scatter5"),br(),
                   width=8),
                 ) #close fluidRow
        ), #close tabPanel
        tabPanel("Logistic Regression",
                 
                 fluidRow(column(
                   
                   br(),
                   withMathJax(
                     includeMarkdown("logreg_text.Rmd")
                   ),
                   br(),
                   verbatimTextOutput("console_message_logreg"),
                   
                   br(),
                   plotOutput("scatter6"),br(),
                   
                   width=8),
                 ) #close fluidRow
        ), #close tabPanel
        tabPanel("Several Predictors",

                 fluidRow(column(

                   br(),
                   withMathJax(
                     includeMarkdown("multivar_text.Rmd")
                   ),
                   br(),
                   verbatimTextOutput("console_message_multivar"),

                   width=8),
                 ) #close fluidRow
        ) #close tabPanel
        
      ) #close navbarPage
  ) #close fluidPage
)#close shinyUI
  
