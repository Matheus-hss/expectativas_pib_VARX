#Modelando VAR com dummies exógenas para eventos politicos e economicos
# dummies estáticas step/pulse e dinâmicas
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



dados_diff <- read_excel("C:/Users/m-hen/OneDrive/Área de Trabalho/Modelagem_Econométrica/modelo_VARX/dados_diff.xlsx", 
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))
head(dados_diff)


#Criando variaveis dummies para eventos socioeconomicos e politicos
#Analise baseada no artigo<"Box, G. E. P., & Tiao, G. C. (1975). 
#"Intervention Analysis with Applications to Economic and Environmental Problems">
# 1. Criação das Dummies Estáticas E Dinâmicas
dados_var <- dados_diff %>%
  arrange(date) %>%
  mutate(
    # --- DUMMIES ESTÁTICAS E DE PULSO ---
    
    # Evento 1: Impeachment (Step e Pulso para Decaimento)
    dummy_impeachmentDilma_step = ifelse(date >= as.Date("2016-09-01"), 1, 0),
    dummy_impeachmentDilma_pulse = ifelse(date == as.Date("2016-09-01"), 1, 0),
    
    # Evento 2: Eleição Jair Bolsonaro (Step - Captura a mudança de patamar pós-eleição)
    #dummy_eleicaoBolsonaro_step = ifelse(date >= as.Date("2018-12-01"), 1, 0),
    
    # Evento 2.1: Antecipação Eleição Bolsonaro (Pulse)
    # 3 meses antes (Ago, Set, Out 2018)
    #dummy_antecipacao_Bolsonaro = ifelse(
      #date == as.Date("2018-09-01") | 
        #date == as.Date("2018-10-01") |
        #date == as.Date("2018-11-01"), 1, 0),
    
    # Evento 3: Choque Inicial COVID (Pulso)
    dummy_covid_pulse = ifelse(date == as.Date("2020-03-01") | date == as.Date("2020-04-01"), 1, 0),
    
    # Evento 4: Fase Pandêmica (Step Temporário)
    dummy_covid_phase = ifelse(date >= as.Date("2020-03-01") & date <= as.Date("2021-12-01"), 1, 0),
    
    # Evento 5: Eleição 3° mandato Lula (Step e Pulso para Decaimento)
    dummy_eleicaoLula_step = ifelse(date >= as.Date("2023-01-01"), 1, 0),
    dummy_eleicaoLula_pulse = ifelse(date == as.Date("2023-01-01"), 1, 0),
    
    # --- DUMMIES DINÂMICAS (Decaimento e Lags) ---
    
    # Lags de decaimento para Impeachment
    dummy_impeachment_l1 = lag(dummy_impeachmentDilma_pulse, 1),
    dummy_impeachment_l2 = lag(dummy_impeachmentDilma_pulse, 2),
    dummy_impeachment_l3 = lag(dummy_impeachmentDilma_pulse, 3),
    
    # Lags de decaimento para eleição Lula
    dummy_eleicaoLula_l1 = lag(dummy_eleicaoLula_pulse, 1),
    dummy_eleicaoLula_l2 = lag(dummy_eleicaoLula_pulse, 2),
    dummy_eleicaoLula_l3 = lag(dummy_eleicaoLula_pulse, 3)
    
  )

# 2. Separar Variáveis Endógenas e Exógenas
# Garante que os valores iniciais NULL ou NA criados pelo lag() sejam substituídos por 0
dados_var[is.na(dados_var)] <- 0

# Endógenas (Variáveis do Sistema VAR)
vars_endogenas <- dados_var %>%
  dplyr::select(expec_pib_diff, selic_diff, desemprego_diff, hiato_diff, cambio_log, ipca)

# Exógenas (Dummies Estáticas e Dinâmicas)
vars_exogenas <- dados_var %>%
  dplyr::select(dummy_impeachmentDilma_step,dummy_impeachmentDilma_pulse,
                dummy_covid_pulse,dummy_covid_phase,dummy_eleicaoLula_step,
                dummy_eleicaoLula_pulse,dummy_impeachment_l1, dummy_impeachment_l2,
                dummy_impeachment_l3,dummy_eleicaoLula_l1,dummy_eleicaoLula_l2,
                dummy_eleicaoLula_l3) # <-- Dummies Adicionadas (primeiros as 
                                            #estáticas depois as dinâmicas)

# 3. Transformar em Time Series (ts) e Matriz (próximo passo)
ts_endogenas <- ts(vars_endogenas, start = c(2010, 2), frequency = 12)
ts_exogenas <- as.matrix(vars_exogenas)

# 4. Seleção de Lags (Critérios de Informação)
# O VARselect sugere quantos meses para trás influenciam o hoje
lag_selection <- VARselect(ts_endogenas, lag.max = 12, type = "const", exogen = ts_exogenas)
print(lag_selection$selection)


# Vamos assumir que o AIC sugeriu 'p' lags (ex: p=2). 
# Substitua p_optimo pelo valor sugerido (ex: 2 ou 3)
#p_optimo <- lag_selection$selection["AIC(n)"]

#Testando 3 modelos diferentes
# -> Var01: Modelo com 12 defasagens (AIC e FPE: 12)
# -> Var02: Modelo com 4 defasagens (HQ: 4)
# -> Var03: Modelo com 1 defasagem (SC: 1)
# 4. Estimar o Modelo VARX
modelo_var01 <- VAR(ts_endogenas, p = 12, type = "const", exogen = ts_exogenas)
modelo_var02 <- VAR(ts_endogenas, p = 4, type = "const", exogen = ts_exogenas)
modelo_var03 <- VAR(ts_endogenas, p = 1, type = "const", exogen = ts_exogenas)

# Resumo focado na equação da Expectativa do PIB
summary(modelo_var01$varresult$expec_pib_diff)
summary(modelo_var02$varresult$expec_pib_diff)
summary(modelo_var03$varresult$expec_pib_diff)

#Modelo Escolhido VAR02*

#Analise dos residuos do VAR03
#- Testar autocorrelação dos resíduos
serial.test(modelo_var01, lags.pt = 24, type = "PT.asymptotic")
serial.test(modelo_var02, lags.pt = 12, type = "PT.asymptotic")
serial.test(modelo_var03, lags.pt = 12, type = "PT.asymptotic")
#Residuos são autocorrelacionados, isso gera problemas de inferencia

#- Testar normalidade dos resíduos
normality.test(modelo_var01)
normality.test(modelo_var02)
normality.test(modelo_var03)
#Residuos são não normais, problemas de caudas pesadas, outliers?
#Gráfico QQ-PLOT
residuos01 <- residuals(modelo_var01)
residuos02 <- residuals(modelo_var02)
residuos03 <- residuals(modelo_var03)
#hist(residuos01, main = "Histograma dos resíduos", xlab = "Resíduos")
qqnorm(residuos01); qqline(residuos01, col = "red")
qqnorm(residuos02); qqline(residuos02, col = "red")
qqnorm(residuos03); qqline(residuos03, col = "red")
#- Testar heteroscedasticidade (ARCH)
arch.test(modelo_var01, lags.multi = 12)
arch.test(modelo_var02, lags.multi = 12)
arch.test(modelo_var03, lags.multi = 12)
#Modelo possui heterocedasticidade
#Estabilidade do VAR03
plot(stability(modelo_var01))
plot(stability(modelo_var02))
plot(stability(modelo_var03))


#Posição,Variável,Justificativa Imposta (Meu Raciocínio)
#1ª,cambio_log,"Choque Externo: É a variável mais exógena. Um choque (e.g., tarifário) no câmbio acontece sem reação contemporânea das demais variáveis domésticas."
#2ª,ipca,Transmissão Imediata: A inflação é a primeira a absorver o choque cambial (dólar mais caro = produtos importados mais caros) no mesmo mês.
#3ª,selic_diff,Política Monetária Reativa: O Banco Central ajusta a Selic para controlar a inflação e o câmbio. A decisão de política é reativa ao cambio_log e ao ipca.
#4ª,desemprego_diff,"Custo Social: O mercado de trabalho (emprego) é imediatamente afetado pelas decisões de juros (Selic) do Banco Central, reagindo mais rápido que o hiato."
#5ª,hiato_diff,"Reação da Atividade: O hiato do produto (o ""esfriamento"" da economia) reage de forma mais agregada e lenta que o mercado de trabalho individualizado."
#6ª,expec_pib_diff,"Sentimento/Conclusão: É a variável mais endógena. As expectativas reagem a todo o processo: Choque Cambial, Reação do BC, Inflação e sinais de desaceleração (emprego/hiato)."

# Reordene as colunas dos dados endógenos para a ordem de Cholesky
vars_endogenas_ordenadas <- dados_var %>%
  dplyr::select(cambio_log, ipca, selic_diff, desemprego_diff, hiato_diff, expec_pib_diff)

# Transforme em Time Series (ts) novamente, na ordem correta
ts_endogenas_ordenadas <- ts(vars_endogenas_ordenadas, start = c(2010, 2), frequency = 12)

# Re-estime o modelo VAR02 com a ordem correta das variáveis
modelo_var02_cholesky <- VAR(ts_endogenas_ordenadas, p = 4, type = "const", exogen = ts_exogenas)
summary(modelo_var02_cholesky$varresult$expec_pib_diff)

# 1. Calcule as IRFs usando o Bootstrap
# periods = 12: Analisaremos o efeito de 1 ano (horizonte comum)
# n.boot = 500: Número de repetições para o Bootstrap
# runs = T: As IRFs são acumuladas (interpretação mais fácil para choques permanentes)
irf_bootstrap <- irf(
  modelo_var02_cholesky,
  n.boot = 1000,
  ci = 0.95,
  boot = TRUE,
  cumulative = FALSE, # Usaremos FALSE para ver a resposta mês a mês
  #runs = TRUE,
  ortho = TRUE, #já usará a ordem definida na estimação (Cholesky)
  periods = 12 # Ajuste conforme a necessidade
)

# 2. Plote as IRFs. Vamos analisar os dois choques mais importantes:
# A) Choque no Câmbio (o choque exógeno)
# B) Choque na Selic (a resposta da política)


# 3. Analisar o efeito do Câmbio sobre a Expectativa de PIB
plot(irf_bootstrap, impulse = "cambio_log", response = "expec_pib_diff")
# 4. Analisar o efeito da Selic sobre o Desemprego
plot(irf_bootstrap, impulse = "selic_diff", response = "desemprego_diff")
# 5. Plota as respostas de todas as outras 5 variáveis a um choque no Câmbio
plot(irf_bootstrap, impulse = "cambio_log")


#Decomposição da variancia
fevd_res <- fevd(modelo_var02_cholesky, n.ahead = 12)
plot(fevd_res)

# 2. Transformar o FEVD em um dataframe tidy
fevd_df <- lapply(names(fevd_res), function(var){
  as.data.frame(fevd_res[[var]]) %>%
    mutate(h = 1:n()) %>%    # horizonte
    pivot_longer(cols = -h, names_to = "shock", values_to = "contrib") %>%
    mutate(variable = var)
}) %>% bind_rows()

# 3. Plot personalizado (cores, legendas, tema)
ggplot(fevd_df, aes(x = h, y = contrib, fill = shock)) +
  geom_bar(stat = "identity", position = "stack",alpha = 0.85) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +   # paleta bonita e profissional
  labs(
    title = "Decomposição da Variância do Erro de Previsão (FEVD)",
    x = "Horizonte (meses)",
    y = "Contribuição",
    fill = "Choque"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )