# shiny server
source("helpers.R") # data import and most of the preparation is performed in a separate helpers file

function (input, output) {   # session is included for using shinyjs functionalities

  
  # Tab 1
  output$desc<-renderText({
    if (input$parm == "NO2") {
      paste("<p></p>",
            "<h3>NO2</h3>",
            "<p>NO2 is a reddish brown gas that is emitted from all combustion engines. There are two main nitrogen based compounds that are emitted from combustion engines: NO2 and nitric oxide (NO). Collectively these two pollutants are referred to as NOx or oxides of nitrogen.
                At the point of emission (i.e. the exhaust pipe), the proportion of NOx is around 90% NO and 10% NO2.
                After a few hours in the atmosphere and in the presence of volatile organic compounds (VOCs) the NO is converted to NO2. This reaction can occur over a couple of seconds to a few hours.
                NO2 reacts further with other substances in the air to form nitric acid, particulate matter and substances called PANs (peroxyacyl nitrates).
                Also with sunlight NO2 can convert back to NO and produce ozone (O3) as a side pollutant. Because of the potential of NO2 to produce these 'secondary' pollutants it is important to monitor and regulate NO2.</p>"
    )
    }
    else if (input$parm == "O3") {
      paste0("<p></p>",
            "<h3>O3</h3>",
            "<p>Ground-level ozone (O3), 
            unlike other pollutants mentioned, is not emitted directly into the atmosphere, but is a secondary pollutant produced by reaction between nitrogen dioxide (NO2), hydrocarbons(42 TCH or 44 NMCH) and sunlight.</p>"
      )
    }
    else if (input$parm == "PM2.5") {
      paste("<p></p>",
            "<h3>PM2.5</h3>",
            "<p>The principal source of airborne PM2.5 matter in European cities is road traffic emissions, particularly from diesel vehicles.</p>"
      )
      
    }
    else {
      paste("<p></p>",
            "<h3>SO2</h3>",
            "<p>Fossil fuels contain traces of sulphur compounds, and SO2 is produced when they are burnt. The majority of the SO2 emitted to the air is from power generation, and the contribution from transport sources is small (shipping being an exception(but Madrid is not a coastal city)).</p>"
      )
    }
    
  })
  
  # Tab 2 
  output$map <- renderLeaflet({
    model_data_year <- model_data_ws[year(date) == input$yearmap]
    dtws <- aggregate(NO2~station+lat+lon+name+address,data=model_data_year,FUN = mean)
    ie_icon <- makeIcon(
      iconUrl = "https://static-frm.ie.edu/school-human-sciences-technology/wp-content/themes/ie_generico/icon/favicon.ico",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0)
    m <-leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircles(data = dtws, lng = ~lon, lat = ~lat, weight = 1, stroke = TRUE,color = 'yellow',
                 opacity = 1,
                 fillOpacity = 0.5,
                 radius = ~NO2*12,
                 popup = sprintf('<font size="2" face="Arial">
                                Station: %s <br />
                                NO2 Level: %s <br />
                                Address: %s <br />
                                </font>',
                                 dtws$name, round(dtws$NO2,2), dtws$address))  %>%
      addMarkers(lng = -3.688976, lat = 40.437713, icon = ie_icon, popup = sprintf(
        '<font size="2" face="Arial">
                  <p>The Place You Are Suffering At</p>
                  </font>')
      )

    m
  })
  
  # Tab 3

  output$box <- renderPlotly({
    
    subplot(g1,g2,g3,g4,g5,g6,g7,g8,
            nrows = 2, margin = c(0.03,0.03, 0.08,0.08)) %>% 
      layout (title = "")
    
  })
  
  output$boxdesc <- renderText("Since there are a lot of zeroes in Precipitation, the boxplot shows several outliers.
                               Average wind speed, SO2 and PM2.5 also have several outliers.")
  
  
  output$hm <- renderPlotly({
    plot_ly(x = colnames(model_data_s)[2:ncol(model_data_s)],
            y = colnames(model_data_s)[2:ncol(model_data_s)],
            z = cor(model_data_s[,2:ncol(model_data_s)]), type= "heatmap",
            colorscale = "Greys") %>%
      layout(title = "Correlations") 
  })
  
  output$hmdesc <- renderText("Minimum, maximum and average temperature are quite predictably highly correlated.")
  
  output$line1 <- renderPlotly({
   
    monthly_average_s <- aggregate(master_wide_s, by = list(format(as.Date(master_wide_s$date), "%Y-%m")), FUN = mean)
    monthly_average_sub <- subset(monthly_average_s, date >= input$dateslider[[1]] & date <= input$dateslider[[2]])

    plot_ly(data = monthly_average_sub, x = ~date, y = ~NO2, name = 'NO2', type = 'scatter', mode = 'lines', line=list(color="#FC4E07"))%>%
      add_trace(y = ~O3, name = 'O3', mode = 'lines', line = list(color = 'lightblue')) %>%
      add_trace(y = ~SO2, name = 'SO2', mode = 'lines', line=list(color = 'tan2')) %>% 
      add_trace(y = ~PM2.5, name = "PM2.5", mode = 'lines', line=list(color = 'grey50'))  %>%
      layout(title = 'Key Pollutants - Monthly Trend',
             xaxis = list(title = ' ', showgrid = F, zeroline = F),
             yaxis = list(title = 'ug/m3', showgrid = F, zeroline = F))
    
  })
  
  # Tab 4
  
  output$concl <- renderText({
    "NO2 levels in the air are affected by multiple different factors. 
     Let's take a look at how Pollutants such as O3, PM2.5 and SO2 as well as 
     weather parameters such as Wind Speed, Humidity, Temperature and Precipitation 
     have a significant impact on NO2."
  })
  
  modeldf <- data.frame(`Beta Coefficients` = round(model1$coefficients[2:length(model1$coefficients)],2))
  modeldf$Interpretation <- c(paste("One unit increase in", rownames(modeldf)[1], "REDUCES NO2 in the air by", abs(round(modeldf[1,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[2], "INCREASES NO2 in the air by", abs(round(modeldf[2,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[3], "REDUCES NO2 in the air by", abs(round(modeldf[3,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[4], "REDUCES NO2 in the air by", abs(round(modeldf[4,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[5], "INCREASES NO2 in the air by", abs(round(modeldf[5,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[6], "INCREASES NO2 in the air by", abs(round(modeldf[6,],2)), "units"),
                              paste("One unit increase in", rownames(modeldf)[7], "REDUCES NO2 in the air by", abs(round(modeldf[7,],2)), "units"))
  
  output$modeldt <- renderDataTable(
    datatable(modeldf)
  )
  observeEvent(input$tabs, {
    if(input$tabs=="data"){
      show("parm")
      hide("yearmap")
    }})
  observeEvent(input$tabs, {
    if(input$tabs=="station"){
      show("yearmap")
      hide("parm")
    }})
  observeEvent(input$tabs, {
    if(input$tabs=="eda"){
      hide("parm")
      hide("yearmon")
    }})
  observeEvent(input$tabs, {
    if(input$tabs=="model"){
      hide("parm")
      hide("yearmap")
    }})
  
}
