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
                    plotOutput("barPlot"), # display a barplot
                    br(),
                    tableOutput("statSum") #display statistical summary on slected variable.
                ))
            )
        ),
        # setup the Modeling page
        tabPanel("Modeling", fluid = TRUE,
            tabsetPanel(
                # 1st sub-page about Model Information
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
                # 2nd sub-page about Model Fitting
                tabPanel("Model Fitting", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            # setup input to decide the proportion to split
                            sliderInput("prop",
                                        "Select the proportion of data to split:",
                                        min = 0,
                                        max = 1,
                                        step = 0.05,
                                        value = 0.75),
                            br(),
                            # setup number input for cross validation
                            sliderInput("cv", 
                                        h5("Select the number of folds for Cross Validation:"),
                                        min = 3,
                                        max = 10,
                                        value = 10),
                            br(),
                            # setup tuning parameter for Classification Tree 
                            numericInput("ctP", 
                                         h5("Enter values of tuning parameter for Classification Tree"),
                                         min = 0.001, max = 0.1, value = 0.001),
                            br(),
                            # setup tuning parameter for Randon Forest
                            sliderInput("rfP", 
                                        h5("Select the number of tuning parameter for Randon Forest"),
                                        min = 3,
                                        max = 15,
                                        step = 1,
                                        value = 7),
                            br(),
                            # give the option to select predictors for the model fits
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
                            h5("Click to train model"),
                            actionButton("train", "Train Model"), # click to train model
                            br(),
                            h5("Click to test model performance"),
                            actionButton("test", "Test and Compare Models") # click to test model performance
                        ),
                        # display all three model fit summaries on the main Panel
                        mainPanel(fluidRow(
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
                # 3rd sub-page about Model Prediction
                tabPanel("Prediction", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            # give user the option to choose which model to use for prediction
                            selectizeInput("modelType", 
                                            h5("Model Type:"),
                                            choices = c("Generalized Linear Regression Model" = "glm",
                                                        "Classification Tree" = "classTree",
                                                        "Random Forest" = "ranForest")
                            ),
                            # give user the option to use model with highest Accuracy
                            checkboxGroupInput("best", h5("Model Type by Accuracy:"),
                                               choices = "Select Model with Best Prediction Accuracy!"),
                            # give the option to select predictors for prediction
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
                            h5("Click to make prediction"),
                            actionButton("predict", "Make Prediction") # click to make prediction
                        ),
                        # display prediciton results 
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
        # setup the Dataset page
        tabPanel("Dateset", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    # give user the option to subset data based on Payment Tier
                    checkboxInput("subset", h5("Subset data on Payment Tier")
                    ),
                    br(),
                    # choose PaymentTier value to subset
                    conditionalPanel(condition = "input.subset", 
                                     selectizeInput("varType",
                                                    h5("Choose a variable Type to filter data table:"),
                                                    choices = c("Payment Tier 1" = "1",
                                                                "Payment Tier 2" = "2",
                                                                "Payment Tier 3" = "3"))
                                     
                    ),
                    br(),
                    # allow download of both full and subset data
                    downloadButton("downloadData", "Download Data Table")
                ),
                # display data table
                mainPanel(fluidRow(
                    dataTableOutput("dataTable")
                ))
            )
        )
    )
))