# Reação das Expectativas de Mercado (PIB) reage a Choques da Selic, 
# Desemprego e Câmbio #

#Importando e Carregando pacotes
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(rbcb)
library(readxl)
library(tseries)   # ADF
library(vars)      # VAR, IRF, FEVD
library(urca)      # testes de raiz unitária Johansen, PP
library(tsibble)
library(feasts)
library(writexl)


#Coleta e tratamento de dados

# Definir data inicial
start_date <- "2010-01-01"

# 1. IPCA mensal (série oficial do BCB/IBGE)
# Código 433: IPCA - índice geral
ipca <- get_series(433, start_date = start_date) %>%
  rename(ipca = `433`)


# 2. Selic mensalizada (série 4189: taxa acumulada ao mês)
selic <- get_series(4189, start_date = start_date) %>%
  rename(selic_mensal = `4189`)

head(selic)

# 3. Expectativas de PIB anual (Focus/BCB)
pib_exp <- get_market_expectations(
  type = "quarterly",
  indic = "PIB Total",
  start_date = start_date,
  top_n = 20
) %>%
  dplyr::select(Data, Media) %>%   # usa os nomes corretos
  dplyr::arrange(Data)

head(pib_exp)
#Agregando pela média
pib_exp_clean <- pib_exp %>%
  group_by(Data) %>%
  summarise(pib_media = mean(Media, na.rm = TRUE)) %>%
  arrange(Data)

#Ajustando a frequência
#Expectativa média para o PIB naquele mês
pib_mensal <- pib_exp_clean %>%
  mutate(mes = floor_date(Data, "month")) %>%
  group_by(mes) %>%
  summarise(pib_exp_mensal = mean(pib_media, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = mes)

head(pib_mensal)

#Dados de desemprego
desemprego <- read_excel(
  "C:/Users/m-hen/OneDrive/Área de Trabalho/desemprego.xlsx",
  col_types = c("date", "numeric", "numeric")
)

# Garantir que a coluna de data esteja em formato Date
desemprego <- desemprego %>%
  mutate(Date = as.Date(Date))

# Filtrar e renomear
desemprego <- desemprego %>%
  dplyr::select(Date, pnad_sa) %>%
  dplyr::filter(Date >= as.Date("2010-01-01")) %>%
  dplyr::rename(date = Date)

head(desemprego)

# Dados do Câmbio, Taxa de câmbio - Livre - Dólar americano (compra) - Média de período - mensal 
cambio <- get_series(3697, start_date = start_date) %>%
  rename(cambio = `3697`)
head(cambio)

#Hiato do Produto, coletado do Banco Central
hiato <- read_excel("C:/Users/m-hen/OneDrive/Área de Trabalho/rpm202509anp.xlsx",
                    sheet = "Graf 2.2.8", skip = 8) %>%
  mutate(
    Date = zoo::as.yearqtr(Trimestre),   # certifique-se do nome exato
    hiato = as.numeric(`Cenário de referência`) # nome exato da coluna
  ) %>%
  dplyr::select(Date, hiato) %>%
  dplyr::rename(date = Date)

head(hiato)

# Converter yearqtr para Date (primeiro mês do trimestre)
hiato <- hiato %>%
  mutate(date = as.Date(date)) %>%       # converte yearqtr para Date
  filter(!is.na(date), !is.na(hiato))    # remove linhas com NA

# Se quiser expandir para frequência mensal (repete valor do trimestre)
hiato_mensal <- hiato %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by = "month")) %>%
  tidyr::fill(hiato, .direction = "down")
  
#Filtrando data
  hiato_mensal <- hiato_mensal%>%
  filter(date >= as.Date("2010-01-01"))


head(hiato_mensal)

#Juntando tabelas e criando data frame
# União das tabelas pela coluna "date"
dados <- pib_mensal %>%
  left_join(selic, by = "date") %>%
  left_join(ipca, by = "date") %>%
  left_join(desemprego, by = "date") %>%
  left_join(cambio, by = "date") %>%
  left_join(hiato_mensal, by = "date")

# Visualizar primeiras linhas
head(dados)
colSums(is.na(dados))

#Tratando dados ausentes
#Hiato, Desemprego, IPCA e Câmbio
dados$hiato <- zoo::na.locf(dados$hiato, na.rm = FALSE)
dados$pnad_sa <- zoo::na.locf(dados$pnad_sa, na.rm = FALSE)
dados$cambio <- zoo::na.locf(dados$cambio, na.rm = FALSE)
dados$ipca <- zoo::na.locf(dados$ipca, na.rm = FALSE)

#Renomenando colunas
dados <- dados%>%
    rename(expec_pib = pib_exp_mensal)%>%
    rename(selic = selic_mensal)%>%
    rename(desemprego = pnad_sa)
head(dados)
str(dados)

#Exportando base de dados
write_xlsx(dados, "C:/Users/m-hen/OneDrive/Área de Trabalho/dados.xlsx")

#Visualizando variaveis
ggplot(dados, aes(x = date, y = expec_pib)) + geom_line() + theme_minimal()
ggplot(dados, aes(x = date, y = cambio)) + geom_line() + theme_minimal()
ggplot(dados, aes(x = date, y = desemprego)) + geom_line() + theme_minimal()
ggplot(dados, aes(x = date, y = hiato)) + geom_line() + theme_minimal()
ggplot(dados, aes(x = date, y = ipca)) + geom_line() + theme_minimal()
ggplot(dados, aes(x = date, y = selic)) + geom_line() + theme_minimal()

# Teste ADF para cada variável separadamente
adf.test(dados$expec_pib, alternative = "stationary")
adf.test(dados$selic, alternative = "stationary")
adf.test(dados$ipca, alternative = "stationary")
adf.test(dados$desemprego, alternative = "stationary")
adf.test(dados$cambio, alternative = "stationary")
adf.test(dados$hiato, alternative = "stationary")
#Apenas variavel IPCA é estacionaria em nivel
#Aplicando primeira diferença nas demais
dados$selic_diff <- c(NA, diff(dados$selic))
dados$desemprego_diff <- c(NA,diff(dados$desemprego))
dados$hiato_diff <- c(NA,diff(dados$hiato))
dados$expec <- c(NA,diff(dados$expec_pib))
#Aplicando log nivel no cambio
dados <- dados %>%
  mutate(cambio_log = c(NA, diff(log(cambio))))

dados%>%
  rename(expec_pib_diff = expec)

#Criando base de dados para o VAR
dados_diff <- dados %>%
  mutate(
    selic_diff       = c(NA, diff(selic)),
    desemprego_diff  = c(NA, diff(desemprego)),
    hiato_diff       = c(NA, diff(hiato)),
    expec_pib_diff   = c(NA, diff(expec_pib)),   # cria a coluna que faltava
    cambio_log       = c(NA, diff(log(cambio))),
    ipca             = ipca
  )

dados_diff
dados_diff <- na.omit(dados_diff)

dados_diff <- dados_diff %>%
  dplyr::select(date, selic_diff, desemprego_diff, hiato_diff, expec_pib_diff, cambio_log, ipca)
dados_diff
#Exportando base de dados estacionarizados
write_xlsx(dados_diff, "C:/Users/m-hen/OneDrive/Área de Trabalho/dados_diff.xlsx")

#Testado estacionariedade novamente
adf.test(dados_diff$expec_pib_diff, alternative = "stationary")
adf.test(dados_diff$selic_diff, alternative = "stationary")
#Ipca ja é estacionaria
adf.test(dados_diff$desemprego_diff, alternative = "stationary")
adf.test(dados_diff$cambio_log, alternative = "stationary")
adf.test(dados_diff$hiato_diff, alternative = "stationary")

#Seleção de defasagens do VAR
VARselect(dados_diff[, -1], lag.max = 12, type = "const")

#Modelo Var01, Critério HQ -> 4 Defasagens
var_model01 <- VAR(dados_diff[, -1], p = 4, type = "const")
summary(var_model01)

#Modelo Var02, Critério AIc -> 12 Defasagens
var_model02 <- VAR(dados_diff[, -1], p = 12, type = "const")
summary(var_model02)

#Modelo VAR03, Meio termo entre HQ e AIc -> 6 defasagens
var_model03 <- VAR(dados_diff[, -1], p = 6, type = "const")
summary(var_model03)

#Vamos manter com o modelo 03

#Aanalise dos residuos
#- Testar autocorrelação dos resíduos
serial.test(var_model03, lags.pt = 6, type = "PT.asymptotic")
#Residuos são autocorrelacionados

#- Testar normalidade dos resíduos
normality.test(var_model03)
#Residuos são não normais, problemas de caudas pesadas, outliers?

#- Testar heteroscedasticidade (ARCH)
arch.test(var_model03, lags.multi = 6)
#Modelo possui heterocedasticidade
#Estabilidade do VAR03
plot(stability(var_model03))

#Criando variaveis dummies para evento (crise economica 2016) e evento (pandemia 2020/2021)
dados_diff <- dados_diff%>%
  mutate(
    # Crise 2016
    crise16_step  = as.integer(date >= as.Date("2016-01-01") & date <= as.Date("2017-12-01")),
    crise16_pulse = as.integer(date == as.Date("2016-08-31")), #Impeachment Dilma Rousseff,
    
    # Pandemia 2020–2021
    pandemia_step  = as.integer(date >= as.Date("2020-03-01") & date <= as.Date("2021-12-01")),
    pandemia_pulse = as.integer(date %in% as.Date(c("2020-03-01","2021-06-01")))# Dois Impulsos, primeiro para o LockDown, segundo para o mês e ano mais critico da pandemia
  )

head(dados_diff)

#Reestimando modelo com dummies de eventos politicos-economicos
# Endógenas: todas menos a coluna de data
dados_diff <- dados_diff %>% dplyr::select(-date)

# Variaveis exógenas(eventos politicos e choques economicos)
dummies <- cbind(
  crise16_step  = dados_diff$crise16_step
  #crise16_pulse = dados_diff$crise16_pulse,
  #pandemia_step  = dados_diff$pandemia_step
  #pandemia_pulse = dados_diff$pandemia_pulse
)

# Estimar com 6 defasagens (pode testar 8–12 se a autocorrelação persistir)
var_dummies <- VAR(dados_diff, p = 4, type = "const", exogen = dummies)
summary(var_dummies)

#Reestimar modelo com variaveis exógenas, o modelo final será um VAR ajustado 
#por eventos politicos e choque pandemico explicando como esse ajuste
#ajuda a explicar os choques de incerteza da expectativa do PIB em relação as 
#demais variaveis.





