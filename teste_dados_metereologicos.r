# # Instalar e carregar pacotes necessários
# install.packages("httr")
# install.packages("jsonlite")
library(httr)
library(jsonlite)
#Definir a chave da API e coordenadas para Minas Gerais (substituir por lat/lon reais)
api_key <- "741011b19edddf62da42cc534a8ed124"
lat <- "-19.9167"
lon <- "-43.9345"

# Função para fazer a chamada da API e obter previsões diárias
get_weather_forecast <- function(api_key, lat, lon) {
  url_base <- "https://api.openweathermap.org/data/3.0/onecall?"

  # Parâmetros da requisição
  params <- list(
    latitude = lat,
    longitude = lon,
    exclude = "minutely,hourly,current,alerts", # Excluindo dados desnecessários
    units = "metric",
    lenguage = "pt_br",
    appid = api_key
  )
    ?lat={lat}&lon={lon}&exclude={part}&appid={API key}&lang=pt_br
  # Fazer a requisição à API
  response <- GET(url_base, query = params)
  
  # Verificar se a requisição foi bem-sucedida
  if (status_code(response) == 200) {
    # Converter o resultado para JSON
    data_json <- fromJSON(content(response, as = "text"), flatten = TRUE)
    
    # Extrair dados diários
    daily_data <- data_json$daily
    
    # Criar um dataframe com informações relevantes para a fazenda
    weather_df <- data.frame(
      data = as.POSIXct(daily_data$dt, origin = "1970-01-01", tz = "UTC"),
      temperatura_dia = daily_data$temp$day,
      temperatura_min = daily_data$temp$min,
      temperatura_max = daily_data$temp$max,
      umidade = daily_data$humidity,
      vento_velocidade = daily_data$wind_speed,
      chuva_probabilidade = daily_data$pop,
      chuva_volume = ifelse(is.null(daily_data$rain), 0, daily_data$rain)
    )
    
    return(weather_df)
  } else {
    print(paste("Erro na requisição:", status_code(response)))
    return(NULL)
  }
}

# Coletar dados de previsão para os próximos 7 dias
weather_forecast <- get_weather_forecast(api_key, lat, lon)

# Exibir os dados de previsão
if (!is.null(weather_forecast)) {
  print(weather_forecast)
}

