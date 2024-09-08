# # Instalar e carregar pacotes necessários
# install.packages("httr")
# install.packages("jsonlite")
acao_dados_metereologicoss <- function() {
    library(httr)
    library(jsonlite)
    library(dplyr) # uso para tratar o dataframe
    Sys.setlocale("LC_COLLATE", "Portuguese_Brazil")
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
        df <- df %>% 
            rename(
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

        exibir_menu <- function(df) { 
            opcoes <- c(
                "Probabilidade média de chuva", 
                "Temperatura média", 
                "Velocidade média de vento", 
                "Volume médio de umidade",
                "Voltar para menu principal"
            )
            opcao <- menu(opcoes, title = "Selecione uma das opções para exibir a sua previsão média para os próximos dias:")
            return(opcao)
        }

        repeat {
            Sys.sleep(2) 
            # Exibindo o menu e capturando a escolha
            opcao <- exibir_menu(df)
            # Calculando e exibindo a média do campo selecionado
            if (opcao == 1) {
                cat(rep("\n", 50))
                # Média da Probabilidade de Precipitação
                media <- mean(df$probabilidade_precipitacao, na.rm = TRUE)
                cat("A previsão média da Probabilidade de Precipitação é:", media, "\n")
            } else if (opcao == 2) {
                cat(rep("\n", 50))
                # Média da Temperatura
                media <- mean(df$temperatura, na.rm = TRUE)
                cat("A previsão média da Temperatura é:", media, "\n")
            } else if (opcao == 3) {
                cat(rep("\n", 50))
                # Média da Velocidade do Vento
                media <- mean(df$velocidade_do_vento, na.rm = TRUE)
                cat("A previsão média da Velocidade do Vento é:", media, "\n")
            } else if (opcao == 4) {
                cat(rep("\n", 50))
                # Média da Umidade
                media <- mean(df$umidade, na.rm = TRUE)
                cat("A previsão média de umidade é:", media, "\n")
            } else if (opcao == 5) {
                cat(rep("\n", 50))
                cat("Saindo do menu.\n")
                break
            } else {
                cat(rep("\n", 50))
                cat("Você selecionou errado, por favor tente novamente selecionando uma das opções numéricas listadas.\n")
            }
        }
    } else {
        print("Tente novamente mais tarde!")
    }
}
acao_dados_metereologicoss()