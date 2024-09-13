# # Instalar e carregar pacotes necessários
# install.packages("httr")
# install.packages("jsonlite")
acao_dados_metereologicos()<- function() {
    library(httr)
    library(jsonlite)
    library(dplyr) # uso para tratar o dataframe
    # Sys.setlocale("LC_ALL", "C")
    print("Acessando API... aguarde por favor")
    # Definindo a chave da API e coordenadas para Minas Gerais
    api_key <- "741011b19edddf62da42cc534a8ed124"
    latitude <- "-19.9167"
    longitude <- "-43.9345"
    unidade <- "metric"
    language <- "pt_br"

    url_base <- "https://api.openweathermap.org/data/2.5/forecast?"

    url_completa <- paste0(
        url_base,
        "lat=", latitude,
        "&lon=", longitude,
        "&units=", unidade,
        "&lang=", language,
        "&appid=", api_key
    )

    response <- GET(url_completa)

    # Verificar se a requisição foi bem-sucedida
    if (status_code(response) == 200) {
        # Converter o resultado para JSON
        print("Acesso bem sucedido!")
        # trazendo json
        data_json <- fromJSON(content(response, as = "text"), flatten = TRUE)

        # Transformar o json em dataframe
        forecasts <- data_json$list
        df <- as.data.frame(forecasts)

        # Alterar os nomes dos campos
        df <- df %>%  rename(
            visibilidade = visibility,
            probabilidade_precipitacao = pop,
            data_e_hora = dt_txt,
            temperatura = main.temp,
            sensacao_termica = main.feels_like,
            temperatura_minima = main.temp_min,
            temperatura_maxima = main.temp_max,
            pressao = main.pressure,
            pressao_nivel_do_mar = main.sea_level,
            pressao_nivel_do_solo = main.grnd_level,
            umidade = main.humidity,
            correcao_temperatura = main.temp_kf,
            nuvens = clouds.all,
            velocidade_do_vento = wind.speed,
            direcao_do_vento = wind.deg,
            rajada_de_vento = wind.gust,
            periodo_do_dia = sys.pod
        )
        avg_precipitacao <- mean(df$probabilidade_precipitacao, na.rm = TRUE)
        avg_temperatura <- mean(df$temperatura, na.rm = TRUE)
        avg_vento <- mean(df$velocidade_do_vento, na.rm = TRUE)
        avg_umidade <- mean(df$umidade, na.rm = TRUE)

        return(list(
            avg_precipitacao = avg_precipitacao,
            avg_temperatura = avg_temperatura,
            avg_vento = avg_vento,
            avg_umidade = avg_umidade
        ))
    } else {
        print("Tente novamente mais tarde!")
        return(NULL)
    }
}