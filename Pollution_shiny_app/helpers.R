wd <- getwd()

model_data_s <- fread('model_data_shiny.csv') # data for most outputs

model_data_ws <- fread('model_data_ws.csv') # data for map output

master_wide_s <- readxl::read_xlsx('master_wide.xlsx') # data for monthly time series line plot
master_wide_s$date <- as.Date(master_wide_s$date)

# Workaround for plotting all boxplots in a single grid.
a1 <- list(
  text = "Temperature",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a2 <- list(
  text = "Precipitation",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a3 <- list(
  text = "Relative Humidity",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a4 <- list(
  text = "Average Wind Speed",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
a5 <- list(
  text = "SO2",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a6 <- list(
  text = "PM2.5",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a7 <- list(
  text = "O3",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

a8 <- list(
  text = "NO2",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

g1<- ggplotly(ggplot(data = model_data_s )+
                geom_boxplot(aes('Min Temp',temp_min), fill='darkseagreen1')+
                geom_boxplot(aes('Avg Temp',temp_avg), fill='slategray1')+
                geom_boxplot(aes('Max Temp',temp_max), fill='coral')+
                labs(#title = 'Temperature', 
                  y = '°C', x = NULL)+
                coord_flip()+
                theme_minimal()) %>% layout(annotations = a1)
g2 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('Precipitation',precipitation), fill='skyblue3')+
                 labs(#title = 'Precipitation', 
                   y = 'cm', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a2)

g3 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('Relative Humidity',humidity), fill='skyblue3')+
                 labs(title = 'Relative Humidity', y = '%', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a3)

g4 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('Average Wind Speed',wind_avg_speed), fill='aliceblue')+
                 labs(title = 'Average Wind Speed', y = "km/h", x = NULL)+
                 theme_minimal()) %>% layout (annotations = a4)
g5 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('SO2',SO2), fill='tan2')+
                 labs(title = 'SO2', y = 'ug/m3', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a5)

g6 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('PM2.5',PM2.5), fill='gray50')+
                 labs(title = 'PM2.5', y = 'ug/m3', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a6)

g7 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('O3',O3), fill='lightskyblue1')+
                 labs(title = 'O3', y = 'ug/m3', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a7)

g8 <- ggplotly(ggplot(data = model_data_s)+
                 geom_boxplot(aes('NO2',NO2), fill='brown')+
                 labs(title = 'NO2', y = 'ug/m3', x = NULL)+
                 theme_minimal()) %>% layout (annotations = a8)

# Tab 4
# Split data into train and test dataset:
set.seed = 1
train <- sample.int(nrow(model_data_s), size = 0.8*nrow(model_data_s))
md_train <- model_data_s[train,]
md_test <- model_data_s[-train,]

model1 <- lm(NO2~., data=md_train[, !c('date','temp_max','temp_min')])



