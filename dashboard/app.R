#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("devtools"
                      ,"shinydashboard"
                      ,"shiny"
                      ,"dashboardthemes"
                      ,"data.table"
                      ,"httr"
                      ,"ggplot2"
                      ,"rsdmx"
                      ,"psych"
                      ,"reshape2"
                      ,"dplyr"
                      ,"ggthemes"
                      ,"highcharter"
                      ,"lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
library(shinydashboard)
library(shiny)
library(dashboardthemes)
library(data.table)
library(httr)
library(ggplot2)
library(rsdmx)
library(psych)
library(reshape2)
library(dplyr)
library(ggthemes)
library(highcharter)
library(lubridate)

legend_colors <- c("blue", "red", "green")

choiceVec <- c("Food" = 41,
               "Household Goods" = 42,
               "Clothing" = 43,
               "Department Stores" = 44,
               "Others" = 45,
               "Cafes & Restaurants" = 46)

choiceVec2 <- c("Original" = 10,
               "Seasonal" = 20,
               "Trend" = 30)

theme <- list(colors = c("#0266C8", "#F90101", "#F2B50F", 
                         "#00933B"), 
              chart = list(style = list(fontFamily = "Calibri", 
                                        color = "#FFFFFF"), backgroundColor = "transparent"), 
              title = list(style = list(color = "#FFFFFF")), 
              subtitle = list(style = list(color = "#666666")),
              legend = list(itemStyle = list(color = "#C0C0C0")), 
              xAxis = list(gridLineWidth = 0, 
                           lineColor = "#FFFFFF",
                           lineWidth = 0,
                           tickWidth = 0,
                           labels = list(style = list(color = "#FFFFFF")),
                           title = list(style = list(color = "#FFFFFF"))), 
              yAxis = list(gridLineWidth = 0,
                           lineColor = "#FFFFFF", 
                           lineWidth = 0,
                           tickWidth = 0,
                           labels = list(style = list(color = "#FFFFFF")),
                           title = list(style = list(color = "#FFFFFF"))), 
              legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
              background2 = "#505053", 
              dataLabelsColor = "#FFFFFF", 
              textColor = "#FFFFFF", 
              contrastTextColor = "#F0F0F3", 
              maskColor = "rgba(255,255,255,0.3)")
hc_theme_custom <- structure(theme, class = "hc_theme")

header <- dashboardHeader(title = "..::|::..", titleWidth = 230)
sidebar <- dashboardSidebar(
  #sidebarMenuOutput(outputId = "menu")
  sidebarMenu(
    menuItem("Dashboard",
             tabName = "dashboard"
    ),
    menuItem("",
             tabName = "Blank0")
    ,
    menuItem("",
             tabName = "Blank1")
    ,
    menuItem("",
             tabName = "Blank2")
    ,
    menuItem("",
             tabName = "Blank3")
    ,hr()
  ),
  sidebarMenuOutput(outputId = "menu"),
  conditionalPanel(condition = "input.tabbox1 === 'charts'",
                   selectInput(inputId = "industry", label = "Industry:",
                               choices = c("Food" = 41,
                                           "Household Goods" = 42,
                                           "Clothing" = 43,
                                           "Department Stores" = 44,
                                           "Others" = 45,
                                           "Cafes & Restaurants" = 46),
                               selected = c("Food" = 41)),
                   dateInput(inputId = "date1", label = "Start Date:", value = "2008-01-01"),
                   dateInput(inputId = "date2", label = "End Date:", value = Sys.Date())
  )
  ,
  conditionalPanel(condition = "input.tabbox1 === 'data'",
                   selectInput(inputId = "industry1", label = "Industry:",
                               choices = c("Food" = 41,
                                           "Household Goods" = 42,
                                           "Clothing" = 43,
                                           "Department Stores" = 44,
                                           "Others" = 45,
                                           "Cafes & Restaurants" = 46),
                               selected = c("Food" = 41)),
                   selectInput(inputId = "seriesType", label = "Series Type:",
                               choices = c("Original" = 10,
                                           "Seasonal" = 20,
                                           "Trend" = 30),
                               selected = c("Seasonal" = 20))
  )
)
body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  fluidRow(
    tabItems(
      tabItem(tabName = "dashboard",
              tabBox(
                title = "",
                id = "tabbox1",
                #height = "700px",
                width = 12,
                tabPanel("Data", value = "data",
                         fluidRow(
                           h2(textOutput("dataTitle"), align = "center"),
                           h3(textOutput("dataSubheading"), align = "center"),
                           div(tableOutput("printed_data"), align = "center")
                         ),
                         fluidRow( 
                           column(width = 4, infoBoxOutput("infoBox1", width = 12)),
                           column(width = 4, infoBoxOutput("infoBox3", width = 12)),
                           column(width = 4, infoBoxOutput("infoBox2", width = 12))
                         )),
                tabPanel("Charts", value = "charts", highchartOutput("distPlot", width = "100%" , height = "700px"))
              ))
    )
  )
)

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body)

# server <- function(input, output) { 
#   output$menu <- renderMenu({
#     sidebarMenu(
#       id = "main_menu"
#       , menuItem("Compare all states", tabName = "linebystate", icon = icon("map-marker"))
#       , menuItem("Compare over 3 years", tabName = "linebyyears", icon = icon("calendar"))
#       , menuItem("Compare by property type", tabName = "linebyptype", icon = icon("home"))
#       , menuItem("Distribution of lag_months", tabName = "histbyptype", icon = icon("bar-chart"))
#       , menuItem("Changing things up!", tabName = "changes", icon = icon("pencil-square"))
#       , hr()
#     )
#   })
#   output$printed_object <- renderPrint({
#     "print me"
#   })
#   
# }


server <- function(input, output, session) {
  setwd("/Users/anuj/Documents/UTS/Citi")
  reactive_data <- reactiveFileReader(
    intervalMillis = 1000000,
    session = session,
    filePath = "RT_Monthly.csv",
    readFunc = function(filePath){
      fread(filePath, sep = ",")
    }
  )
  
  reactive_data_forecast <- reactiveFileReader(
    intervalMillis = 1000000,
    session = session,
    filePath = "RT_Monthly_Forecast.csv",
    readFunc = function(filePath){
      fread(filePath, sep = ",")
    }
  )
  output$printed_data <- renderTable({
    dt <- reactive_data_forecast()
    dt <- dt[IND_R == input$industry1 & TSEST == input$seriesType]
    dt <- tail(dt[order(obsTime)], 5)
    namesList <-  c("obsTime", "C20", "C20_Adjusted")
    namesListDisp <-  c("Date","Actual Revenue($M)", "Predicted Revenue($M)", "Error(%)")
    dt <- dt[,namesList, with = FALSE]
    setorderv(dt,"obsTime")
    lastrow <- nrow(dt)
    if(lastrow > 0){
    predictedgrowth <- (dt[lastrow, C20_Adjusted] - dt[lastrow-1, C20_Adjusted])*100/dt[lastrow-1, C20_Adjusted]
    actualgrowth <- (dt[lastrow-1, C20] - dt[lastrow-2, C20])*100/dt[lastrow-2, C20]
    predictedgrowthprior <- (dt[lastrow-1, C20_Adjusted] - dt[lastrow-2, C20_Adjusted])*100/dt[lastrow-2, C20_Adjusted]
    upcolor = "green"
    downcolor = "red"
    upicon = icon("arrow-circle-up")
    downicon = icon("arrow-circle-down")
    
    dispcolor1 <- upcolor
    dispicon1 <- upicon
    
    dispcolor2 <- upcolor
    dispicon2 <- upicon
    
    dispcolor3 <- upcolor
    dispicon3 <- upicon
    
    if(actualgrowth<0)
    {
      dispcolor1 <- downcolor
      dispicon1 <- downicon
    } else {
      dispcolor1 <- upcolor
      dispicon1 <- upicon
    }
    if(predictedgrowth<0)
    {
      dispcolor2 <- downcolor
      dispicon2 <- downicon
    } else {
      dispcolor2 <- upcolor
      dispicon2 <- upicon
    }
    
    if(predictedgrowthprior<0)
    {
      dispcolor3 <- downcolor
      dispicon3 <- downicon
    } else {
      dispcolor3 <- upcolor
      dispicon3 <- upicon
    }
    
    predictedgrowth <- abs(predictedgrowth)
    actualgrowth <- abs(actualgrowth)
    predictedgrowthprior <- abs(predictedgrowthprior)
    disppredictedgrowth <- paste0(formatC(predictedgrowth, big.mark = ",", format = "f", digits = 2), "%")
    dispactualgrowth <- paste0(formatC(actualgrowth, big.mark = ",", format = "f", digits = 2), "%")
    disppredictedgrowthprior <- paste0(formatC(predictedgrowthprior, big.mark = ",", format = "f", digits = 2), "%")
    
    dispmonth1 <- month.abb[month(date(max(dt$obsTime))%m-% months(1))]
    dispyear1 <- year(date(max(dt$obsTime))%m-% months(1))
    
    dispmonth2 <- month.abb[month(max(dt$obsTime))]
    dispyear2 <- year(max(dt$obsTime))
    
    dispText1 <- paste0("Actual Revenue Growth - ", dispmonth1, "'", dispyear1)
    dispText2 <- paste0("Predicted Revenue Growth - ", dispmonth2, "'", dispyear2)
    dispText3 <- paste0("Predicted Revenue Growth - ", dispmonth1, "'", dispyear1)
    
    output$infoBox1 <- renderInfoBox({
      infoBox(dispText1, dispactualgrowth, icon = dispicon1, fill = TRUE, color = dispcolor1)
    })
    output$infoBox2 <- renderInfoBox({
      infoBox(dispText2, disppredictedgrowth , icon = dispicon2, fill = TRUE, color = dispcolor2)
    })
    output$infoBox3 <- renderInfoBox({
      infoBox(dispText3, disppredictedgrowthprior , icon = dispicon3, fill = TRUE, color = dispcolor3)
    })
    dt[, errorrate := (abs(C20_Adjusted-C20)*100)/C20]
    dt[, errorrate := formatC(errorrate, big.mark = ",", format = "f", digits = 2)]
    dt[, C20 := formatC(C20, big.mark = ",", format = "f", digits = 2)]
    dt[, C20_Adjusted := formatC(C20_Adjusted, big.mark = ",", format = "f", digits = 2)]
    }
    else{
      output$infoBox1 <- renderInfoBox({
        infoBox("", "0%", icon = icon("arrow-circle-up"), fill = TRUE, color = "yellow")
      })
      output$infoBox2 <- renderInfoBox({
        infoBox("", "0%" , icon = icon("arrow-circle-up"), fill = TRUE, color = "yellow")
      })
      output$infoBox3 <- renderInfoBox({
        infoBox("", "0%" , icon = icon("arrow-circle-up"), fill = TRUE, color = "yellow")
      })
      dt[, errorrate := NA]
    }
    names(dt) <- namesListDisp
    return(dt)
  })
  
  output$dataTitle <- renderText({
    paste0("Actual vs Predicted Growth - ",names(choiceVec)[choiceVec == input$industry1])
  })
  output$dataSubheading <- renderText({
    paste0("",names(choiceVec2)[choiceVec2 == input$seriesType])
  })
  
  
  output$distPlot <- renderHighchart({
    dt <- reactive_data()
    dt[,obsTime := as.Date(paste(obsTime, "01", sep = "-"))]
    dt[,TSEST := as.factor(TSEST)]
    dt <- dt[IND_R == input$industry & obsTime >= input$date1 & obsTime <= input$date2]
    title_txt <- paste0("Retail Trade Industry - ",names(choiceVec)[choiceVec == input$industry], "\n")
    hchart(dt, 'line', hcaes(x = 'obsTime', y = 'obsValue', group = "TSEST" ), lineWidth = 1, backgroundColor = "transparent") %>%
      hc_title(text = title_txt) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Revenue ($ million)")) %>%
      hc_legend(labelFormatter = JS("function () {
                                    switch (this.name) {
                                    case '10':
                                    val = 'Original';
                                    break;
                                    case '20':
                                    val = 'Seasonal';
                                    break;
                                    case '30':
                                    val = 'Trend';
                                    }
                                    return val;
  }"))%>%
      hc_tooltip(shared = TRUE, formatter = JS("function () {
                                               var d = new Date(this.x)
                                               var month = new Array(12);
                                               month[0] = 'January';
                                               month[1] = 'February';
                                               month[2] = 'March';
                                               month[3] = 'April';
                                               month[4] = 'May';
                                               month[5] = 'June';
                                               month[6] = 'July';
                                               month[7] = 'August';
                                               month[8] = 'September';
                                               month[9] = 'October';
                                               month[10] = 'November';
                                               month[11] = 'December';
                                               var n = month[d.getUTCMonth()];
                                               var o = new Date(this.x).getUTCFullYear()
                                               
                                               function seriesNameLoookup(caseval) {
                                               switch (caseval) {
                                               case '10':
                                               val = 'Original';
                                               break;
                                               case '20':
                                               val = 'Seasonal';
                                               break;
                                               case '30':
                                               val = 'Trend';
                                               }
                                               return val;
                                               }
                                               
                                               arrLen = this.points.length;
                                               text = '<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />';
                                               text = text + '<b>' + n + ' '+ o  + '</b><br>'
                                               for (i = 0; i < arrLen; i++) {
                                               text += '<span style=\"color:'+ this.points[i].point.color +'\"><p>\u25CF <p></span>' + seriesNameLoookup(this.points[i].point.series.name) + ': <b>' + this.points[i].y + '</b><br>'
                                               }
                                               return text;
      }"))%>%
      hc_add_theme(hc_theme_custom)
  })
  
}
shinyApp(ui, server)