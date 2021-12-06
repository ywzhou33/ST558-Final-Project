#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# List of Packages used
library(shiny)
library(DT) 
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)

shinyServer(function(input, output, session){
    
    # get the full dataset
    getAlldata <- reactive({ 
        emp <- read.csv('Employee.csv')
        anyNA(emp)
        emp
    })
    
    # get as factors to plot
    getPlotdata <- reactive({ 
        emp <- read.csv('Employee.csv')
        emp$LeaveOrNot <- as.factor(emp$LeaveOrNot)
        emp$Education <- as.factor(emp$Education)
        emp$JoiningYear <- as.factor(emp$JoiningYear)
        emp$City <- as.factor(emp$City)
        emp$PaymentTier <- as.factor(emp$PaymentTier)
        emp$Gender <- as.factor(emp$Gender)
        emp$EverBenched <- as.factor(emp$EverBenched)
        emp$ExperienceInCurrentDomain <- as.factor(emp$ExperienceInCurrentDomain)
        emp
    })
    
    # intro on About page
    output$text1 <- renderText({
        p1 <- ("A company's HR department is interested in predicting the future of their employees -- whether the employee leaves the company.")
    })
    # dataset source background on About page
    output$text2 <- renderText({
        p2 <- ("The dataset Employee.csv contains information about this company's past and current employees. There are 9 columns in this dataset.")
        url <- "https://www.kaggle.com/tejashvi14/employee-future-prediction?select=Employee.csv"
        link <- paste("For more information about the dataset, use this ", a("link", href = url), " to the dataset source.")
        HTML(paste(p2, sep = '<br/> <br/>'), "<br/> <br/>",
             "<ul>
        <li>Education - Education level.</li>
        <li>JoiningYear - Year of joining the company.</li>
        <li>City - City office where posted.</li>
        <li>PaymentTier - 1 is the highest; 2 is mid-level; 3 is the lowest.</li>
        <li>Age - Current age.</li>
        <li>Gender - Gender of employee.</li>
        <li>EverBenched - Whether the employee ever kept out of projects for 1 month or more.</li>
        <li>ExperienceInCurrentDomain - Year of experience in the current field.</li>
        <li>LeaveOrNot - Whether the employee leaves the company</li>
        </ul>", "<br/> <br/>", link)
        })
    # intro for purpose of each tap
    output$tab <- renderText({
        p3 <- ("For each tab page of this app: ")
        HTML(paste(p3, sep = '<br/> <br/>'), "<br/> <br/>",
             "<ul>
        <li>About page - The about page describes background information of the app.</li>
        <li>Data Exploration page - This page will perform the EDA. Summary statistics and graphical summaries will be displayed.</li>
        <li>Modeling page - Three supervised learning models are fitted and compared. Model with the highest accruacy will be selected and will be used for prediction.</li>
        <li>Data page - This page will display the data table and the data is available for download.</li>
        </ul>")
    })
    
    # display an image on About page
    output$image <- renderImage({
        filename <- normalizePath(file.path('aboutImage.png'))
        list(src = filename)
    }, deleteFile = FALSE)
    
    # Get the barPlot with logic statements
    output$barPlot <- renderPlot({
        barplotD <- getPlotdata()
        if (input$var == "Gender") {
            ggplot(data = barplotD, aes(x = Gender)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else if (input$var == "Education") {
            ggplot(data = barplotD, aes(x = Education)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else if (input$var == "JoiningYear") {
            ggplot(data = barplotD, aes(x = JoiningYear)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else if (input$var == "City") {
            ggplot(data = barplotD, aes(x = City)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else if (input$var == "PaymentTier") {
            ggplot(data = barplotD, aes(x = PaymentTier)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else if (input$var == "EverBenched") {
            ggplot(data = barplotD, aes(x = EverBenched)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        } else {
            ggplot(data = barplotD, aes(x = ExperienceInCurrentDomain)) + 
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
        }
    })
    
    # get the statistic summary table
    output$statSum <- renderTable({
        data <- getAlldata()
        var <- input$var
        
        emp_stat <- 
            data %>% 
            group_by(as.factor(LeaveOrNot), as.factor(var)) %>% #Education City EverBenched
            summarize(Avg = mean(PaymentTier), 
                      Sd = sd(PaymentTier)) %>% as_tibble()
        emp_stat
    })
    
    # get GLM information for the model intro tab
    output$glmDes <- renderText({
        glm <- ("Generalized Linear Regression Model allows for responses from non-normal distributions and it also allows for both continuous and categorical predictors. This data is predicted by Logistic Generalized Linear Regression Model, which the response is in 0 or 1, 1 for success (employee leaves the company) and 0 for failure (employee do not leave the company). Therefore the prediction values are also between 0 and 1, representing the probability of whether the employee leaves the company.")
        HTML(paste(glm, sep = '<br/> <br/>'), "<br/> <br/>", 
             "<b>Assumption: </b>", "<br/> <br/>", 
             "<ul>
              <li>Response must be binary or ordinal.</li>
              <li>Observations are independent of each other.</li>
              <li>There is little or no multicollinearity among the predictors.</li>
              <li>Predictors are linearly related to the log odds.</li>
              <li>A relatively large sample size.</li>
              </ul>", 
             "<b>Advantages: </b>", "<br/> <br/>", 
             "<ul>
              <li>Does not require data to meet normality and constant variance assumptions.</li>
              <li>Flexible in choosing link function.</li>
              <li>Has optimal properties due to MLE. </li>
              <li>Easy to interpret based on coeficient estimates.</li>
              </ul>", 
             "<b>Disadvantages:</b>", "<br/> <br/>", 
             "<ul>
              <li>Only allows for linear predictors.</li>
              <li>Unable to account for dependency.</li>
              </ul>", "<br/> <br/>",)
    })
    # get example GLM equation with MathJax
    output$glmFunc <- renderUI({
        withMathJax(
            helpText('GLM Example: $$Ln(Y) = \\beta_{0}\\ + \\beta_{1}x_{1}\\ + \\dots\\ + \\beta_{k}x_{k}\\ + e$$')
        )
    })
    # get Classification Tree information for the model intro tab
    output$ctDes <- renderText({
        ct <- ("The Classification Tree is a tree-based model that make prediction based on decision tree, which allows categorical response and predicts class membership (0 or 1) or probability of membership (between 0 and 1). For fitting classification trees, the residual sum of squares is not good for splits, we should use Gini index or entropy/deviance instead. The criteria is to choose nodes with cv, and nodes classifies well if p is near 0 or 1. ")
        HTML(paste(ct, sep = '<br/> <br/>'), "<br/> <br/>", 
             "<b>Advantages: </b>", "<br/> <br/>", 
             "<ul>
              <li>Works well with both categorical and numerical data as well as linear and non-linear relationship.</li>
              <li>Forest generate uncorrelated decision trees and accounts for dependency.</li>
              <li>Predictors do not need to be scaled or transformed.</li>
              <li>No statistical assumptions necessary.</li>
              <li>Able to built in variance selection and generaly provide high accuracy in prediction.</li>
              </ul>", 
             "<b>Disadvantages:</b>", "<br/> <br/>", 
             "<ul>
              <li>Small changes in data can vastly change tree.</li>
              <li>Greedy algorithm necessary implies no optimal algorithm.</li>
              <li>Need pruning parameter.</li>
              <li>Lose interpretability.</li>
              </ul>", "<br/> <br/>",)
    })
    
    # show visual example of classification tree
    output$ctImage <- renderImage({
        filename <- normalizePath(file.path('ctImage.jpg'))
        list(src = filename, 
             width = 681, 
             height = 300,
             align="center")
    }, deleteFile = FALSE)
    
    # get Random Forest Model information for the model intro tab
    output$rfDes <- renderText({
        rf <- ("Random Forest is an ensemble tree-based model that helps to solve regression and classification problems. Its algorithm consists of a collection of decision trees, which its forest is generated and train through a bootstrapped aggregation that use a random subset of predictors for each bootstrap tree fit, expanding the forest improves its precision. The model choosing criteria is to start the first split on strong predictor, then use m = sqrt(p) for classification and m=p/3 for regression to randomly selected predictors. The value of m is determined through OOB error.")
        HTML(paste(rf, sep = '<br/> <br/>'), "<br/> <br/>", 
             "<b>Advantages: </b>", "<br/> <br/>", 
             "<ul>
              <li>Works well with both categorical and numerical data as well as linear and non-linear relationship.</li>
              <li>Forest generate uncorrelated decision trees and accounts for dependency.</li>
              <li>Predictors do not need to be scaled or transformed.</li>
              <li>No statistical assumptions necessary.</li>
              <li>Able to built in variance selection and generaly provide high accuracy in prediction.</li>
              </ul>", 
             "<b>Disadvantages:</b>", "<br/> <br/>", 
             "<ul>
              <li>Costly in terms of computation time.</li>
              <li>It is like a black box algorithm, small changes in data can vastly change tree and we have no control over the process.</li>
              <li>Need pruning parameter.</li>
              <li>Lose interpretability.</li>
              </ul>", "<br/> <br/>",)
    })
    
    #show visual example of random forest
    output$rfImage <- renderImage({
        filename <- normalizePath(file.path('rfImage.png'))
        list(src = filename, 
             width = 781, 
             height = 482,
             align="center")
    }, deleteFile = FALSE)
    
    # get the train data set
    getTraindata <- reactive({ 
        emp <- read.csv('Employee.csv')
        emp$LeaveOrNot <- as.factor(emp$LeaveOrNot)
        emp$Education <- as.factor(emp$Education)
        emp$City <- as.factor(emp$City)
        emp$Gender <- as.factor(emp$Gender)
        emp$EverBenched <- as.factor(emp$EverBenched)
        # set seed
        set.seed(234)
        index <- createDataPartition(y = emp$LeaveOrNot, p = input$prop, list = F)
        train <- emp[index, ] # training set
        train
    })
    
    # get the test data set
    getTestdata <- reactive({ 
        emp <- read.csv('Employee.csv')
        emp$LeaveOrNot <- as.factor(emp$LeaveOrNot)
        emp$Education <- as.factor(emp$Education)
        emp$City <- as.factor(emp$City)
        emp$Gender <- as.factor(emp$Gender)
        emp$EverBenched <- as.factor(emp$EverBenched)
        
        # set seed
        set.seed(234)
        index <- createDataPartition(y = emp$LeaveOrNot, p = input$prop, list = F)
        test <- emp[-index, ] # test set
        test
    })

    getFglm <- reactive({ 
        train <- getTraindata()
        set.seed(345)
        ctrl <- trainControl(method = "cv", number = input$cv)
        # train the GLM model
        glm <- train(as.formula(paste("LeaveOrNot ~ ", paste(input$modelVars, collapse = "+"))), 
                     data = train, method = "glm", family = "binomial", 
                     preProcess =c("center", "scale"), trControl = ctrl)
        glm
    })
    
    getFct <- reactive({ 
        train <- getTraindata()
        ctGrid <- expand.grid(cp = seq(from = 0, to = 0.1, by = input$ctP))
        # train the Classification Tree model
        classTree <- train(as.formula(paste("LeaveOrNot ~ ", paste(input$modelVars, collapse = "+"))), 
                           data = train, method = "rpart", trControl = ctrl, 
                           preProcess = c("center", "scale"), tuneGrid = ctGrid)
        classTree
    })
    
    getFrf <- reactive({ 
        rfGrid <- expand.grid(mtry = seq(from = 1, to = input$rfP, by = 1))
        # train the Random Forest model
        ranForest <- train(as.formula(paste("LeaveOrNot ~ ", paste(input$modelVars, collapse = "+"))), 
                           data = train, method = "rf", trControl = ctrl, 
                           preProcess = c("center", "scale"), tuneGrid = rfGrid )
        ranForest
    })
    # if the train action bottom is pressed then return the results and plots for all three models
    observeEvent(input$train, {
        
        train <- getTraindata()
        glm <- getFglm()
        classTree <- getFct()
        ranForest <- getFrf()
        
        if(input$train){
            # GLM model
            output$glm <- renderPrint({
                glmSum <- summary(glm)
                glmSum$coefficients
            })
            # Clssification Tree model
            output$ct <- renderPrint({
                classTree$results
            })
            output$ctPlot <- renderPlot({
                ggplot(classTree)
            })
            # Random Forest model
            output$rf <- renderPrint({
                ranForest$results
            })
            output$rfPlot <- renderPlot({
                ggplot(ranForest)
            })
            }
    })
    # get the model comparing dataframe
    getCompare <- reactive({
        
        test <- getTestdata()
        glm <- getFglm()
        classTree <- getFct()
        ranForest <- getFrf()
        
        testglm <- confusionMatrix(data = test$LeaveOrNot, reference = predict(glm, newdata = test)) 
        testct <- confusionMatrix(data = test$LeaveOrNot, reference = predict(classTree, newdata = test)) 
        testrf <- confusionMatrix(data = test$LeaveOrNot, reference = predict(ranForest, newdata = test))
        
        allCompare <- data.frame(Models= c("glm", "classTree","ranForest"), 
                                  Accuracy = c(testglm$overall[1],
                                               testct$overall[1], 
                                               testrf$overall[1]))
        allCompare
    })
    # display the model comparing dataframe if the test action bottom is pressed 
    observeEvent(input$test,{
        output$comTable <- renderTable({getCompare()})
    })
  
    # display the prediction results if the test action bottom is pressed 
    observeEvent(input$predict,{
        
        test <- getTestdata()
        glm <- getFglm()
        classTree <- getFct()
        ranForest <- getFrf()
        
        if (input$modelType == "classTree"){
            output$predResult <- renderTable({
                predict(classTree, newdata = test, type = "prob")
            }) 
            output$predSum <- renderTable({
                x <- summary(predict(classTree, newdata = test))
                data.frame(Outcome = c("Employee leaves", "Employee Stay"),
                           Counts = c(x[[2]], x[[1]]))
            }) 
            output$predDes <- renderText({
                x <- summary(predict(classTree, newdata = test))
                paste("With the selected predictors, the Classification Tree Model predicts that ", 
                      round((x[[2]]/(x[[1]]+x[[2]]))*100, 2), 
                      "% of the current employees will leave the company in next two years.")
            })
        } else if (input$modelType == "ranForest"){ 
            output$predResult <- renderTable({
                predict(ranForest, newdata = test, type = "prob")
            }) 
            output$predSum <- renderTable({
                x <- summary(predict(ranForest, newdata = test))
                data.frame(Outcome = c("Employee leaves", "Employee Stay"),
                           Counts = c(x[[2]], x[[1]]))
            }) 
            output$predDes <- renderText({
                x <- summary(predict(ranForest, newdata = test))
                paste("With the selected predictors, the Random Forest Model predicts that ", 
                      round((x[[2]]/(x[[1]]+x[[2]]))*100, 2), 
                      "% of the current employees will leave the company in next two years.")
            })
        } else {
            output$predResult <- renderTable({
                predict(glm, newdata = test, type = "prob")
            })
            output$predSum <- renderTable({
                x <- summary(predict(glm, newdata = test))
                data.frame(Outcome = c("Employee leaves", "Employee Stay"),
                           Counts = c(x[[2]], x[[1]]))
            }) 
            output$predDes <- renderText({
                x <- summary(predict(glm, newdata = test))
                paste("With the selected predictors, the Generalized Linear Regression Model predicts that ", 
                      round((x[[2]]/(x[[1]]+x[[2]]))*100, 2), 
                      "% of the current employees will leave the company in next two years.")
            })
        }
    })
    
    # get the sub data to download
    getSubdata <- reactive({ 
        emp <- getAlldata()
        subdata <- emp %>% filter(PaymentTier == input$varType)
        subdata
    })
    
    # if user choose subset data, then the subset will be displayed and available for download
    # default it to display and download the full dataset.
    observeEvent(input$subset, {
        if (input$subset) { # subset
            output$dataTable <- renderDataTable({
                getSubdata()
            })
            output$downloadData <- downloadHandler(
                filename = function() {
                    paste("subEmployee", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(getSubdata(), file, row.names = FALSE)
                }
            )
        } else { # full dataset.
            output$dataTable <- renderDataTable({
                getAlldata()
            })
            output$downloadData <- downloadHandler(
                filename = function() {
                    paste("fullEmployee", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(getAlldata(), file, row.names = FALSE)
                }
            )
            
        }
    })

    
})















