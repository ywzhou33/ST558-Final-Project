#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(fluidPage(
    withMathJax(),
    titlePanel("Employee Future Prediction"),
    tabsetPanel(
        tabPanel("About",
                 mainPanel(
                     h3("Purpose"),
                     htmlOutput("text1"),
                     h3("Dataset"),
                     htmlOutput("text2"),
                     h3("Page Information"),
                     htmlOutput("tab"), 
                     imageOutput("image")
                 )
        ),
        tabPanel("Data Exploration", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("var", 
                                        h5("Choose a variable of interest."),
                                        selected = "Gender",
                                        choices = c("Education",
                                                    "JoiningYear",
                                                    "City", 
                                                    "PaymentTier",
                                                    "Age",
                                                    "Gender",
                                                    "EverBenched",
                                                    "ExperienceInCurrentDomain")
                         ),
                         br(),
                         radioButtons("butt1",
                                      h5("Choose the file type to download the plot."),
                                      choices = list("png",
                                                     "pdf")
                         ),
                         br(),
                         downloadButton("downloadPlotEDA", "Download Button")
                     ),
                     mainPanel(fluidRow(
                         plotOutput("plot"),
                         tableOutput("statSum")
                         
                     )),
                 )
        ),
        tabPanel("Modeling", fluid = TRUE,
                 tabsetPanel(     
                     tabPanel("Modeling Info", fluid = TRUE,
                              mainPanel(
                                  h3("Generalized Linear Regression Model"),
                                  uiOutput("glmdes"),
                                  h3("Classification Tree"),
                                  uiOutput("ctdes"),
                                  h3("Random Forest"),
                                  uiOutput("rfdes"), 
                                  imageOutput("image")
                                  
                              ) 
                     )
                 )
        )
    )
))