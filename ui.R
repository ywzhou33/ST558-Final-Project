##
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
                                   h5("Choose a variable of interest:"),
                                   selected = "Gender",
                                   choices = c("Education",
                                               "JoiningYear",
                                               "City", 
                                               "PaymentTier",
                                               "Gender",
                                               "EverBenched",
                                               "ExperienceInCurrentDomain")
                    ),
                    br(),
                    radioButtons("butt1",
                                 h5("Choose the file type to download the plot:"),
                                 choices = list("png",
                                                "pdf")
                    ),
                    br(),
                    downloadButton("downloadPlotEDA", "Download Button")
                ),
                mainPanel(fluidRow(
                    plotOutput("barPlot"),
                    br(),
                    tableOutput("statSum")
                ))
            )
        ),
        tabPanel("Modeling", fluid = TRUE,
            tabsetPanel(
                tabPanel("Modeling Information", fluid = TRUE,
                    mainPanel(
                    h3("Generalized Linear Regression Model"),
                    htmlOutput("glmDes"),
                    uiOutput("glmFunc"),
                    h3("Classification Tree"),
                    htmlOutput("ctDes"),
                    imageOutput("ctImage"),
                    h3("Random Forest"),
                    htmlOutput("rfDes"),
                    imageOutput("rfImage")
                    )    
                ),
                tabPanel("Model Fitting", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("prop",
                                        "Select the Proportion of data to split:",
                                        min = 0,
                                        max = 1,
                                        value = 0.75),
                            br(),
                            selectizeInput("modelType", 
                                                     h5("Choose a Model Type:"),
                                                     selected = "Generalized Linear Regression Model",
                                                     choices = c("Generalized Linear Regression Model",
                                                                 "Classification Tree",
                                                                 "Random Forest")
                            ),
                            br(),
                            #lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=dat)
                            checkboxGroupInput("modelVars", 
                                                h5("Choose Predictors:"),
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
                            actionButton("train", "Train Model")
                        ),
                        mainPanel(fluidRow(
                            verbatimTextOutput("glm")
                        ))
                    )    
                ),
                tabPanel("Prediction", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            selectizeInput("modelType", 
                                            h5("Choose a Model Type:"),
                                            selected = "Generalized Linear Regression Model",
                                            choices = c("Generalized Linear Regression Model",
                                                        "Classification Tree",
                                                        "Random Forest")
                            ),
                            checkboxGroupInput("modelVars", 
                                                h5("Choose Predictors:"),
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
                            actionButton("Predict", "Make Prediction")
                        ),
                        mainPanel(fluidRow(
                            verbatimTextOutput("pred") 
                        ))
                    )
                )
            )
        ),
        tabPanel("Dateset", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var",
                        h5("Choose a variable to filter data table:"),
                            selected = "PaymentTier",
                            choices = c("PaymentTier",
                                        "Age",
                                        "ExperienceInCurrentDomain")
                    ),
                    br(),
                    downloadButton("downloadData", "Download Data Table")
                ),
                mainPanel(fluidRow(
                    dataTableOutput("dataTable")
                ))
            )
        )
    )
))