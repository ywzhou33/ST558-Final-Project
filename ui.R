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
        # get the about page
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
        # get the EDA page
        tabPanel("Data Exploration", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    selectizeInput("var", 
                                   h5("Choose a variable of interest:"),
                                   choices = c("Education",
                                               "JoiningYear",
                                               "City", 
                                               "PaymentTier",
                                               "Gender",
                                               "EverBenched",
                                               "ExperienceInCurrentDomain")
                    )
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
                                        step = 0.05,
                                        value = 0.75),
                            br(),
                            sliderInput("cv", 
                                        h5("Select the number of folds for Cross Validation:"),
                                        min = 3,
                                        max = 10,
                                        value = 10),
                            br(),
                            numericInput("ctP", 
                                         h5("Enter values of tuning parameter for Classification Tree"),
                                         min = 0.001, max = 0.1, value = 0.001),
                            br(),
                            sliderInput("rfP", 
                                        h5("Select the number of tuning parameter for Randon Forest"),
                                        min = 3,
                                        max = 15,
                                        step = 1,
                                        value = 3),
                            br(),
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
                            actionButton("train", "Train Model"),
                            br(),
                            actionButton("test", "Test and Compare Models")
                        ),
                        mainPanel(fluidRow(
                            textOutput("warning"),
                            htmlOutput("head1"),
                            tableOutput("comTable"),
                            h3("Generalized Linear Regression Model"),
                            verbatimTextOutput("glm"),
                            h3("Classification Tree Model"),
                            verbatimTextOutput("ct"),
                            plotOutput("ctPlot"),
                            h3("Random Forest Model"),
                            verbatimTextOutput("rf"),
                            plotOutput("rfPlot")
                        ))
                    )    
                ),
                tabPanel("Prediction", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            selectizeInput("modelType", 
                                            h5("Model Type:"),
                                            choices = c("Generalized Linear Regression Model" = "glm",
                                                        "Classification Tree" = "classTree",
                                                        "Random Forest" = "ranForest")
                            ),
                            checkboxGroupInput("best", h5("Model Type by Accuracy:"),
                                               choices = "Select Model with Best Prediction Accuracy!"),
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
                            actionButton("predict", "Make Prediction")
                        ),
                        mainPanel(fluidRow(
                                    column(width = 3, "Prediction Results", 
                                           tableOutput("predResult")), 
                                    column(width = 4 , "Prediction Summary", 
                                           tableOutput("predSum")),
                            textOutput("predDes")
                        ))
                    )
                )
            )
        ),
        tabPanel("Dateset", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    checkboxInput("subset", h5("Subset data on Payment Tier")
                    ),
                    br(),
                    conditionalPanel(condition = "input.subset", 
                                     selectizeInput("varType",
                                                    h5("Choose a variable Type to filter data table:"),
                                                    choices = c("Payment Tier 1" = "1",
                                                                "Payment Tier 2" = "2",
                                                                "Payment Tier 3" = "3"))
                                     
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