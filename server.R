#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)

shinyServer(function(input, output, session){
    
    getAlldata <- reactive({ 
        emp <- read.csv('Employee.csv')
        anyNA(emp)
        emp
    })
    getSubdata <- reactive({ 
        emp <- read.csv('Employee.csv')
        emp$LeaveOrNot <- as.factor(emp$LeaveOrNot)
        emp$Education <- as.factor(emp$Education)
        emp$City <- as.factor(emp$City)
        emp$Gender <- as.factor(emp$Gender)
        emp$EverBenched <- as.factor(emp$EverBenched)
        subdata <- emp %>% filter(Gender = input$var)
        subdata
    })
    
    output$text1 <- renderText({
        p1 <- ("A company's HR department is interested in predicting the future of their employees -- whether the employee leaves the company.")
    })
    
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
    
    output$image <- renderImage({
        filename <- normalizePath(file.path('job-hopping_ygkecr.png'))
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$stat <- renderTable({
        data <- getAlldata()
        summary(select(data, where(is.integer)))
    })
    
    output$plot <- renderPlot({
        data <- getAlldata()
        # temporally treat Education as numeric since Education degrees has low-to-high relation
        temp.data <- data %>% 
            mutate(Education = if_else(Education == "Bachelors", 1,
                                       if_else(Education == "Masters", 2, 3)))
        
        num <- lapply(select(temp.data, is.numeric), as.numeric) %>%  as_tibble() %>% rename(Experience = ExperienceInCurrentDomain)
        
        # check correlations
        cor <- cor(num)
        
        # plot a correlation plot
        ggcorrplot(cor, hc.order = TRUE, type = "lower", lab = TRUE)
    }) 
    observeEvent(input$var, {
        subdata <- getSubdata()
        if (input$var == "Education"){
            output$plot <- renderPlot({
                subdata <- getsubdata()
                ggplot(data = subdata, aes(x = Education)) +
                geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                scale_fill_discrete()
            })
        } else if (input$var == "JoiningYear"){
            output$plot <- renderPlot({
                subdata <- getsubdata()
                ggplot(data = subdata, aes(x = as.factor(JoiningYear))) +
                    geom_bar(aes(fill = LeaveOrNot), position = "dodge") +
                    scale_fill_discrete()
            })
        }
    })
    output$glmdes <- renderUI({
        g <- ("A company's HR department is interested in predicting the future of their employees -- whether the employee leaves the company.")
    })
    
    output$ctdes <- renderUI({
        c <- ("The dataset Employee.csv contains information about this company's past and current employees. There are 9 columns in this dataset.")
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
    
    output$rfdes <- renderUI({
        r <- ("For each tab page of this app: ")
        HTML(paste(p3, sep = '<br/> <br/>'), "<br/> <br/>",
             "<ul>
        <li>About page - The about page describes background information of the app.</li>
        <li>Data Exploration page - This page will perform the EDA. Summary statistics and graphical summaries will be displayed.</li>
        <li>Modeling page - Three supervised learning models are fitted and compared. Model with the highest accruacy will be selected and will be used for prediction.</li>
        <li>Data page - This page will display the data table and the data is available for download.</li>
        </ul>")
    })
    
    getTraindata <- reactive({ 
        emp <- read.csv('Employee.csv')
        # set seed
        set.seed(234)
        index <- createDataPartition(y = emp$LeaveOrNot, p = input$p, list = F)
        train <- emp[index, ] # training set
    })
    
    getTestdata <- reactive({ 
        emp <- read.csv('Employee.csv')
        # set seed
        set.seed(234)
        index <- createDataPartition(y = emp$LeaveOrNot, p = input$p, list = F)
        test <- emp[-index, ] # test set
    })
})