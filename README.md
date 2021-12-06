# ST558-Final-Project

The purpose of this repository is to create a R Shiny app application for the class ST558 Final Project. 

### List of packages used:      

```{}
library(shiny)
library(DT) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
```
### Code to install packages

```{}
if (!require('shiny')) {install.packages("shiny")}
else if (!require('DT')) {install.packages("DT")}
else if (!require('dplyr')) {install.packages("dplyr")}
else if (!require('ggplot2')) {install.packages("ggplot2")}
else if (!require('tidyverse')) {install.packages("tidyverse")}
else if (!require('caret')) {install.packages("caret")}
else if (!require('randomForest')) {install.packages("randomForest")}
else if (!require('rpart')) {install.packages("rpart")}
```
### Code to run app

```{}
shiny::runGitHub("ST558-Final-Project
", "ywzhou33")
```
