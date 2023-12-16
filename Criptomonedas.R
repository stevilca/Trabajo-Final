library(httr)
library(jsonlite)
library(corrr)
library(plotly)
#install.packages("tseries")
library(tseries)
#install.packages("httr2")
#library(httr2)
#install.packages("pracma")
library(pracma)
# Load the required library for setnames function
library(data.table)


library(ggplot2)
library(forecast)# Contiene el modelo ARIMA


#Column 1 (Timestamp): Unix timestamp or time in milliseconds (Time at the start of the interval).
#Column 2 (Open): Opening price at the beginning of the interval.
#Column 3 (High): Highest price during the interval.
#Column 4 (Low): Lowest price during the interval.
#Column 5 (Close): Closing price at the end of the interval.
#Column 6 (Volume): Volume of the traded asset during the interval.
#Column 7 (Close Time): The time at the end of the interval.
#Column 8 (Quote Asset Volume): The volume of the asset being traded against during the interval.
#Column 9 (Number of Trades): The number of trades that occurred during the interval.
#Column 10 (Taker Buy Base Asset Volume): Taker buy base asset volume during the interval.
#Column 11 (Taker Buy Quote Asset Volume): Taker buy quote asset volume during the interval.
#Column 12 (Ignore): This column might represent some auxiliary or ignored data.
#You can interpret this data as OHLCV (Open, High, Low, Close, Volume) candlestick data that's commonly used in financial analysis, particularly in the context of cryptocurrency trading.




# Specify the API endpoint and parameters
endpoint <- "https://api.binance.com/api/v3/klines"
start_time <- as.numeric(as.POSIXct("2023-01-01"))  * 1000
#end_time <- as.numeric(as.POSIXct("2023-10-01")) 
params <- list(symbol = "BTCUSDT", interval = "1d",  limit=1000)# startTime = start_time , endTime = end_time),
# Make a GET request to the Binance API
response <- GET(url = endpoint, query = params)

# Parse JSON response
if (status_code(response) == 200) {
  data <- content(response, "text")
  parsed_data <- fromJSON(data)
  # The parsed_data will contain OHLCV data and other information
  
  # Convert the matrix into a dataframe
  df <- as.data.frame(parsed_data)
  
  # Assign appropriate column names
  colnames(df) <- tolower(c("Timestamp", "Open", "High", "Low", "Close", "Volume", "Close_Time", "Quote_Asset_Volume", "Number_of_Trades", "Taker_Buy_Base_Asset_Volume", "Taker_Buy_Quote_Asset_Volume", "Ignore"))
  
  # Convert timestamp to a readable date-time format
  df$timestamp <- as.POSIXct(as.numeric(df$timestamp) / 1000, origin = "1970-01-01")
  data <- df[, c("timestamp", "open", "high", "low", "close", "volume")]
  
  
} else {
  print("Error fetching data")
}



variables_numericas <- data[, c("open", "high", "low", "close", "volume")]

variables_numericas <- sapply(variables_numericas, as.numeric)

variables_numericas%>% 
  correlate() %>% 
  rplot()

#Stella, para ver los valores de las correlaciones
m_cor <- cor(variables_numericas)
round(m_cor,2)
#la correlacion es negativa entre volume y el resto de las variables, ej: al incrementar "close", disminuye "volumne" y viceversa
#esto es diferente con la guia porque en ella la correlacion es positiva entre volume y el resto de las variables
#la correlación da igual a 1 entre close y el resto de las variables


# Create a candlestick chart
candlestick <- plot_ly(df, type = "candlestick", x = ~timestamp, open = ~open, high = ~high, low = ~low, close = ~close)

# Customize the chart layout 
candlestick <- candlestick %>% layout(title = "Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Show the chart
candlestick
#

#Perform a Dickey-Fuller test to evaluate stationary
df$timestamp <- as.POSIXct(df$timestamp)
close_prices <- data$close
close_time_series <- ts(close_prices)
result <- adf.test(close_time_series)
print(result)

# p-value is greater that 0.05 hence we fail to reject the null hyphotesis. The time series is non-stationary

plot(close_time_series)

data$close <- as.numeric(data$close)
# Calculate the first differences of the 'Close' column
#diff_close <- diff(data$close)
diff_close <- c(NA, diff(data$close))
# Remove any NA values resulting from differencing
diff_close_no_na <- diff_close[!is.na(diff_close)]
#
result <- adf.test(diff_close_no_na)
print(result)
# we got a p-value of 0.01 so we can reject the null hyphotesis and conclude that the time series is now stationary
# Therefore, it is necessary to model the first difference of the series:
data$diff_close <- diff_close

#### Decomposition
#The stl() function performs decomposition of the time series data into seasonal, trend, and remainder components using a seasonal-trend decomposition procedure based on Loess.
#The s.window = "periodic" argument specifies that a periodic window should be used for the seasonal component.
# Convert 'close' column to numeric
data$close <- as.numeric(as.character(data$close))

data$close <- as.ts(df$close)


# Plot the time series
plot(data$close)



seasonal_decompose <- function(data_column, frequency = 12) {
  # Convert the column to a time series
  ts_data <- ts(data_column, frequency = frequency)
  
  # Apply seasonal decomposition
  decomposed_data <- stl(ts_data, s.window = "periodic")
  
  # Access the decomposed components
  seasonal <- decomposed_data$time.series[, "seasonal"]
  trend <- decomposed_data$time.series[, "trend"]
  remainder <- decomposed_data$time.series[, "remainder"]
  
  # Set names for the components for plotting
  #setnames(decomposed_data$time.series, c("seasonal", "trend", "remainder"), 
  #         c("Componente Estacional", "Componente de Tendencia", "Componente de Residuo"))
  
  # Establecer un diseño de gráficos en 3 filas y 1 columna
  par(mfrow = c(4, 1))
  
  # Plot the original serie
  plot(data_column)
  # Plot the decomposed components
  plot(decomposed_data$time.series[, "seasonal"], main = "Componente Estacional")
  plot(decomposed_data$time.series[, "trend"], main = "Componente de Tendencia")
  plot(decomposed_data$time.series[, "remainder"], main = "Componente de Residuo")
}

#call the function for the complete close
seasonal_decompose(data$close, frequency = 7)  

# call the function for the last month
last_month_close<- tail(data$close, 30)

seasonal_decompose(last_month_close, frequency = 7) 

#There is a weekly seasonality, but its contribution is very small, so it can be neglected.

#####FE with tsfeatures

#install.packages('tsfeatures', dependencies = TRUE)
#install.packages("devtools")
#devtools::install_github("robjhyndman/tsfeatures")
#library(tsfeatures)
#mylist <- list(data$close,data$diff_close)
#myfeatures <- tsfeatures(mylist)
#myfeatures

#agregar algo de FE o shadow price upper and lower(ver notebook guia)

##### 4.5. Get target, training, validation and test datasets for ML models¶

get_train_valid_test_ts <- function(df, forecasting_days, target = "close") {
  # Get training, validation, and test datasets with target for Time Series models
  
  # Data preparing
  df <- na.omit(df)
  df <- df[, c("timestamp", "close")]
  names(df) <- c("ds", "y")
  
  N <- nrow(df)
  train <- df[1:(N - 2 * forecasting_days - 1), ]
  valid <- df[(N - 2 * forecasting_days):(N - forecasting_days - 1), ]
  test <- df[(N - forecasting_days):N, ]
  
  # Train+valid - for optimal model training
  train_valid <- rbind(train, valid)
  
  cat(sprintf("Origin dataset has %d rows and %d features\n", nrow(df), ncol(df)))
  cat(sprintf("Get training dataset with %d rows\n", nrow(train)))
  cat(sprintf("Get validation dataset with %d rows\n", nrow(valid)))
  cat(sprintf("Get test dataset with %d rows\n", nrow(test)))
  
  return(list(train = train, valid = valid, test = test, train_valid = train_valid))
}

forecasting_days <- 10
result <- get_train_valid_test_ts(data, forecasting_days) 


# Access the resulting data frames
train <- result$train
valid <- result$valid
test <- result$test
train_valid <- result$train_valid





#### ARIMA
# ARIMA stands for Autoregressive Integrated Moving Average Model. It belongs to a class of models that explains a given time series based on its own past values -i.e.- its own lags and the lagged forecast errors. The equation can be used to forecast future values. Any ‘non-seasonal’ time series that exhibits patterns and is not a random white noise can be modeled with ARIMA models. So, ARIMA, short for AutoRegressive Integrated Moving Average, is a forecasting algorithm based on the idea that the information in the past values of the time series can alone be used to predict the future values. ARIMA Models are specified by three order parameters: (p, d, q),
# where,

#ARIMA es un modelo que utiliza la información de los valores pasados de la serie de tiempo para predecir los valores futuros.
#Cualquier serie de tiempo "no estacional" que muestre patrones y no sea un ruido blanco aleatorio se puede modelar con modelos ARIMA.
#Su nombre es un abreviatura de “AutoRegressive Integrated Moving Average” en inglés, donde "AR" (siglas para Autoregressive)
#, MA (siglas para Moving Average) e I (siglas para Integrated).

#El modelo usa 3 parámetros p, q y d(no necesariamente deben estar todos presentes)
# p es el orden del término AR
# d es el número de diferenciaciones necesarias para que la serie de tiempo se vuelva estacionaria
# q es el orden del término MA

# AR(p) Autoregresión – un modelo de regresión que utiliza la relación dependiente entre una observación actual y las observaciones durante un período anterior.
#Un componente autorregresivo (AR(p)) se refiere al uso de los valores pasados en la ecuación de regresión de la serie de tiempo.

# I(d) Integración – utiliza la diferenciación de observaciones.
#La diferenciación implica la resta de los valores actuales de una serie con sus valores anteriores un número determinado de veces.

# MA(q) Media Móvil– Un componente de media móvil representa el error del modelo como una combinación de términos de errores anteriores. 
#El orden q representa el número de términos que se incluirán en el modelo.



#1. Cómo calcular el orden de diferenciación (d) 

#El objetivo de la diferenciación es hacer que la serie de tiempo sea estacionaria. 
#Debemos tener cuidado de no diferenciar demasiado la serie porque poría afectar el modelo
#El orden correcto de diferenciación es la diferenciación mínima requerida para obtener una serie estacionaria.
#Si la serie de tiempo es estacionaria, d es igual a cero y no requerie que se realice diferenciacion. Una serie es estacionaria cuando su media, varianza y autocovarianza son invariantes en el tiempo
#Si la serie no se vuelve estacionaria con 1 o 2 diferencias (en niveles), se recomienda usar diferencias con logaritmo.


#transformamos en tipo serie de tiempo a train 
train_time_series <-ts(train$y)

#en el grafico observamos que podria no ser una serie estacionaria
#par(mar = c(1, 1, 1, 1))
plot(train_time_series)

#realizamos el test Augmented Dickey Fuller (Prueba ADF) para ver si la serie de tiempo es estacionaria
#La hipótesis nula (Ho) de la prueba ADF es que la serie temporal no es estacionaria
result <- adf.test(train_time_series)
print(result)
#el p-valor es 0.6539 es mayor a 0.05, en consecuencia no rechazamos la Ho (la serie NO es estacionaria)


#calculamos la primera diferencia

train$y <- as.numeric(train$y)
train_time_series_d1 <-c(NA, diff(train$y))
train_time_series_d1_na <- diff_close[!is.na(train_time_series_d1)]

#verificamos con el test Augmented Dickey Fuller (Prueba ADF)
result <- adf.test(train_time_series_d1_na)
print(result)
#el p-valor es 0.01 es menor a 0.05, con una diferenciación logramos que la serie de tiempo sea estacionaria

#En conclusión, el valor para el parámetro de orden de diferencia(d) del modelo ARIMA es igual a 1

#2. Cómo calcular el orden del término AR(p) 

#Para identificar si el modelo necesita algún término AR, analizamos el gráfico de autocorrelación parcial (PACF).
#El PACF brinda información sobre la correlación pura entre un retraso(lag) y la serie, es decir la correlación entre la serie y su rezago, después de excluir las contribuciones de los lags intermedios
#La autocorrelación parcial del lag (k) de una serie es el coeficiente de ese lag en la ecuación de autorregresión de Y.

#Yt=α0+α1Yt−1+α2Yt−2+α3Yt−3

# si Y_t es la serie actual e Y_t-1 es el lag 1 de Y, entonces la autocorrelación parcial del lag 3 (Y_t-3) es el coeficiente
#α3 de Y_t-3 en la ecuación anterior.

#Calculamos el orden del término AR como igual a tantos lags crucen el límite de significancia en el gráfico PACF.


pacf(train_time_series_d1_na, main='Función de autocorrelación parcial (PACF) -Serie de Tiempo')

#el valor de (p) es 3

#3 Cómo calcular el orden del término MA(q) 

#Para encontrar el número de términos de AR, utilizamos el gráfico ACF.
#El gráfico ACF indica cuántos términos MA se requieren para eliminar cualquier autocorrelación en la serie estacionaria.


acf(train_time_series_d1_na,frequency =1, main='Función de autocorrelación parcial (PACF) -Serie de Tiempo')

#el valor de (q) es 0


#4. Luego de identificar los parámetros podemos ajustar el modelo ARIMA

#usamos arima del paquete y libreria forecast , donde el order es p,d,q
modelo1 = arima(as.numeric(train_time_series),order = c(3,1,0))
summary(modelo1)

tsdisplay(residuals(modelo1))

#para ver si el modelo esta bien ajustado tiene que haber ruido blanco, los errores deben tener media igual a cero, no estar correlacionados
#y la varianza debe ser constante.
#Usamos el test de Ljung-Box para el cual si el p-valor > a 0.05 indica que hay ruido blanco
Box.test(modelo1$residuals, lag = 1, type = "Ljung-Box") 
#si p-valor es 0.979, es mayor a 0.05 hay ruido blanco

shapiro.test(modelo1$residuals)
#el p-valor es menor a 0.05 los residuos siguen una distribucion normal

#CONCLUSION EL MODELO ESTA BIEN AJUSTADO

# 5. Construcción del modelo ARIMA automaticamente
#Utilizamos la opción auto.arima para luego compararlo con el modelo que hicimos identificando a mano los parámetros

modelo2=auto.arima(as.numeric(train_time_series))
summary(modelo2)
#los parámetros elegidos automaticamente son p=0, d=1 y q =0
tsdisplay(residuals(modelo2))

#validacion de los residuos
Box.test(modelo2$residuals, lag = 1, type = "Ljung-Box")
#si hay ruido blanco

shapiro.test(modelo2$residuals) 
#los residuos siguen una distribucion normal

#CONCLUSION DE RMSE y MAE, el modelo ARIMA para el cual calculamos los parámetros a mano
#tuvo un menor RMSE con un valor de 1230.916 y un menor MAE con un valor de 784.3979


#predicciones
forecast1=predict(modelo1, h=30)
forecast2=predict(modelo2, h=30)
pronostico1 <- forecast::forecast(modelo1,h=30)
plot(pronostico1)

pronostico2 <- forecast::forecast(modelo2,h=30)
plot(pronostico2)

#Utilizando el set de test
test_time_series <-ts(test$y)
modelo1.test <- Arima(as.numeric(test_time_series), model = modelo1)
accuracy(modelo1.test)
#CONCLUSION, el RMSE fue 940.0318 y el MAE fue 603.1536. Los valores son menores con test.


###### prophet
# Results of all models
result <- data.frame(
  name_model = character(),
  type_data = character(),
  r2_score = numeric(),
  rmse = numeric(),
  mape = numeric(),
  params = I(list()),
  ypred = I(list())
)

################ functions required
# Function to calculate metrics
calc_metrics <- function(type_score, list_true, list_pred) {
  # Calculation score with type=type_score for list_true and list_pred 
  if (type_score == 'r2_score') {
    score <- cor(list_true, list_pred)^2
  } else if (type_score == 'rmse') {
    score <- sqrt(mean((list_true - list_pred)^2))
  } else if (type_score == 'mape') {
    score <- mean(abs((list_true - list_pred) / list_true)) * 100
  }
  return(score)
}

# Function to add metrics to the result dataframe
result_add_metrics <- function(result, n, y_true, y_pred) {
  # Calculation and addition metrics into dataframe result[n, :]
  
  result[n, 'r2_score'] <- calc_metrics('r2_score', y_true, y_pred)
  result[n, 'rmse'] <- calc_metrics('rmse', y_true, y_pred)      # in coins
  result[n, 'mape'] <- calc_metrics('mape', y_true, y_pred)  # in %
  
  return(result)
}


################
#install.packages('prophet')
library(prophet)
cryptocurrency <- 'BTCUSDT'
holidays_df <- NULL
prophet_modeling <- function(result, 
                             cryptocurrency, 
                             train, 
                             test, 
                             holidays_df, 
                             period_days,
                             fourier_order_seasonality,
                             forecasting_period,
                             name_model,
                             type_data) {
  # Performs FB Prophet model training for the given train dataset, holidays_df, and seasonality_mode
  # Performs forecasting with a period by this model, visualization, and error estimation
  # df - dataframe with real data in the forecasting_period
  # can be such combinations of parameters: train=train, test=valid, or train=train_valid, test=test
  # Save results into the dataframe result
  
  # Build Prophet model with parameters and structure 
  model <- prophet(
    daily.seasonality = FALSE,
    weekly.seasonality = FALSE,
    yearly.seasonality = FALSE,
    changepoint.range = 1,
    changepoint.prior.scale = 0.5,
    holidays = holidays_df,
    seasonality.mode = 'multiplicative'
  )
  
  model <- add_seasonality(
    model,
    name = 'seasonality',
    period = period_days,
    fourier.order = fourier_order_seasonality,
    mode = 'multiplicative',
    prior.scale = 0.5
  )
  
  # Training model for df
  model <- fit.prophet(model, train)
  
  # Make a forecast
  future <- make_future_dataframe(model, periods = forecasting_period)
  forecast <- predict(model, future)
  
  # Draw plot of the values with forecasting data
  plot(model, forecast, xlabel = 'Date', ylabel = paste0(name_model, " for ", cryptocurrency))
  
  # Draw plot with the components (trend and seasonalities) of the forecasts
  plot(model, forecast, components = c('trend', 'seasonal'))
  
  # Output the prediction for the next time on forecasted_days
  # forecast[['yhat_lower', 'yhat', 'yhat_upper']] = forecast[['yhat_lower', 'yhat', 'yhat_upper']].round(1)
  # forecast[['ds', 'yhat_lower', 'yhat', 'yhat_upper']].tail(forecasting_period)
  
  # Forecasting data by the model
  ypred <- forecast$yhat[(nrow(forecast) - forecasting_period + 1):nrow(forecast)]
  print(ypred)
  # Save results
  n <- nrow(result)
  result[n, 'name_model'] <- paste0("Prophet_", name_model)
  result[n, 'type_data'] <- type_data
  result[n, 'params'] <- c(period_days, fourier_order_seasonality)
  result[n, 'ypred'] <- ypred
  
  #return(list(result = result, ypred = ypred))
  return (result)
}



is_Prophet <- TRUE

if (is_Prophet) {
  for (period_days in c(4, 5, 7, 14)) {
    for (fourier_order_seasonality in c(3, 12)) {
      result <- prophet_modeling(result,
                                 cryptocurrency,
                                 train,
                                 valid,
                                 holidays_df,
                                 period_days,
                                 fourier_order_seasonality,
                                 forecasting_days,
                                 paste0(period_days, "_days_", fourier_order_seasonality, "_order"),
                                 'valid')
    }
  }
}