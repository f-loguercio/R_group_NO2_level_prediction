# shiny ui
if(!"shiny" %in% installed.packages()) {
  install.packages("shiny")} 
if(!"shinydashboard" %in% installed.packages()) {
  install.packages("shinydashboard")} 
if(!"shinyjs" %in% installed.packages()) {
  install.packages("shinyjs")} 
if(!"ggplot2" %in% installed.packages()) {
  install.packages("ggplot2")} 
if(!"data.table" %in% installed.packages()) {
  install.packages("data.table")} 
if(!"plotly" %in% installed.packages()) {
  install.packages("plotly")} 
if(!"leaflet" %in% installed.packages()) {
  install.packages("leaflet")} 
if(!"DT" %in% installed.packages()) {
  install.packages("DT")} 

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(data.table)
library(plotly)
library(leaflet)
library(DT)


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Air Pollution in Madrid", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(width = 300,
      menuItem("Know the Data", tabName = "data", icon = icon("database")),
      menuItem("Station Level Data", tabName = "stations", icon = icon("map")),
      menuItem("Exploratory Analysis", tabName = "analysis", icon = icon("search")),
      menuItem("Factors Affecting NO2", tabName = "model", icon = icon("align-left"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .sidebar > ul > li> a i{
                              width: 45px !important;
                              font-size: 20px;
                              }
                              .sidebar > ul > li> a span{
                              font-size: 16px;
                              white-space: normal;
                              }
                              .shiny-text-output {
                                margin: 20px auto;
                                font-size: 18px;
                                color: #e08e0b;
                                text-align: center;
                              }
                              .main-header .logo {
                                font-weight: bold;

                              }
                              '))),
    tabItems(
      # Data tab content
      tabItem(tabName = "data",
              fluidRow(
                box(
                  width = 8,
                  htmlOutput("desc")
                ),
                
                box(
                  width = 4,
                  title = "Paramater",
                  selectInput(inputId= "parm","Select the Parameter",
                              choices=c("NO2","O3","PM2.5","SO2"), selected="NO2")
                )
              )
      ),
      
      # Stations tab content
      tabItem(tabName = "stations",
              fluidRow(
                box(
                  width = 8,
                  leafletOutput("map")
                ),
                
                box(
                  width = 4,
                  title = "Year",
                  selectInput(inputId= "yearmap","Select the Year",
                              choices=c(2011,2012,2013,2014,2015,2016),selected=2011)
                )
              )
      ),
      
      # Analysis tab content
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput("box"),
                  p(textOutput("boxdesc"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput("hm"),
                  p(textOutput("hmdesc"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput('line1'),
                  sliderInput('dateslider', 
                              "Select Date Range",
                              min = as.Date("2011-01-01"),
                              max = as.Date("2016-12-31"),
                              value=c(as.Date("2011-01-01"), as.Date("2016-12-31")),
                              timeFormat = "%Y-%m")
                )
              )
      ),
      
      # Model tab content
      tabItem(tabName = "model",
              fluidRow(
                box(
                  width = 12,
                  dataTableOutput("modeldt"),
                  p(textOutput("concl"))
                )
              )
      )
      
    )
    )
    )
