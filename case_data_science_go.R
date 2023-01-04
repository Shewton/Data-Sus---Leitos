#importando dados + pacotes
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
dados <- fetch_datasus(year_start = 2006, month_start = 1, year_end = 2021, month_end = 12, uf = "GO",information_system = "CNES-LT")
library(tidyverse)
install.packages("anytime") 
library("anytime")
library(ggplot2)
library(gapminder)
library(dplyr)


#Limpando dataset
excluir <- c("TERCEIRO" ,"REGSAUDE","NIVEL_HIER", "MICR_REG","DISTRSAN","DISTRADM","TPGESTAO","PF_PJ","CPF_CNPJ","NIV_DEP","CNPJ_MAN","ESFERA_A","ATIVIDAD","RETENCAO","NATUREZA","CLIENTEL","TP_UNID", "TURNO_AT", "NIV_HIERTERCEIRO", "TP_LEITO")
dados <- dados[,!(names(dados)%in% excluir)]
        
##Prerrogativas
#1 - Entender a evolução dos leitos no tempo (mês e anos)
#2 - Avaliar essa evolução de leitos SUS e não SUS
#3 - Ter uma visão geral de como estavam os leitos pré pandemia e depois da pandemia


#Formatando datas
dados$COMPETEN <- anydate(dados$COMPETEN)
dados$COMPETEN <- as.Date(dados$COMPETEN)
dados["Ano"] <- c(format(dados$COMPETEN, format = "%Y"))
dados["mês"] <- c(format(dados$COMPETEN, format = "%Y/%m"))

##1) leitos por mes de pandemia
leitos_month <- dados %>%
   group_by(mês, Ano) %>%
  summarise(
  leitos_existentes = sum(unique(QT_EXIST), na.rm = T),
  leitos_contratados = sum(unique(QT_CONTR), na.rm = T),
  leitos_sus = sum(unique(QT_SUS), na.rm = T),
  leitos_nsus = sum(unique(QT_EXIST-QT_SUS), na.rm = T)) %>%
  filter(mês >= "2019/01", mês <= "2021/12")

##visão leitos por mes
leitos_month %>% ggplot(aes(x = mês, y = leitos_sus, group = 1)) +
  geom_line() +
  geom_point ()

## leitos ao ano
leitos_year <- dados %>%
  group_by(Ano) %>%
  summarise(
    leitos_existentes = sum(unique(QT_EXIST), na.rm = T),
    leitos_contratados = sum(unique(QT_CONTR), na.rm = T),
    leitos_sus = sum(unique(QT_SUS), na.rm = T),
    leitos_nsus = sum(unique(QT_EXIST-QT_SUS), na.rm = T)) %>%
  filter(Ano >= "2006")

##2 - leitos ao ano (Visão de leitos SUS e não SUS)

leitos_year_agr <- leitos_year %>%
  group_by(Ano) %>%
  summarise(
    num_leitos = max(unique(leitos_sus), na.rm = T),
    num_leitos_nsus = max(unique(leitos_nsus), na.rm = T))

leitos_year_agr %>% ggplot(aes(x = Ano, y = num_leitos , group = 1)) +
  geom_line(aes(col = "leitos sus")) +
  geom_line(aes(y=num_leitos_nsus,col = "leitos não sus"))
  geom_point ()

##----------------------------------------------------------------------------------------


##3 - leitos por municipio antes e depois da pandemia
leitos_mun_ant_pandemia <- dados %>%
  group_by(CODUFMUN, Ano) %>%
  summarise(
    leitos_existentes_pre = sum(unique(QT_EXIST), na.rm = T),
    leitos_contratados_pre = sum(unique(QT_CONTR), na.rm = T),
    leitos_sus_pre = sum(unique(QT_SUS), na.rm = T),
    leitos_nsus_pre = sum(unique(QT_EXIST-QT_SUS), na.rm = T)) %>%
  filter(Ano == 2014)

leitos_mun_pos_pandemia <- dados %>%
  group_by(CODUFMUN, Ano) %>%
  summarise(
    leitos_existentes_pos = sum(unique(QT_EXIST), na.rm = T),
    leitos_contratados_pos = sum(unique(QT_CONTR), na.rm = T),
    leitos_sus_pos = sum(unique(QT_SUS), na.rm = T),
    leitos_nsus_pos = sum(unique(QT_EXIST-QT_SUS), na.rm = T)) %>%
  filter(Ano == 2021)

leitos_pre_pos_1 <- merge(leitos_mun_ant_pandemia,leitos_mun_pos_pandemia, by.x = 'CODUFMUN', by.y = 'CODUFMUN', all.x = TRUE )

leitos_saldo <- leitos_pre_pos_1 %>%
  group_by(CODUFMUN) %>%
  summarise(
    leitos_sus_saldo = mean(leitos_sus_pos-leitos_sus_pre, na.rm = T),
    leitos_negativos = ifelse(leitos_sus_saldo < 0, 1, 0)
  )

leitos_saldo_geral <- leitos_saldo %>%
  summarise(
    media = mean(leitos_sus_saldo, na.rm = T),
    max = max(leitos_sus_saldo, na.rm = T),
    min = min(leitos_sus_saldo, na.rm = T),
    desvio = sd(leitos_sus_saldo, na.rm = T),
    leitos_negativos = sum(leitos_negativos, na.rm = T),
    leitos_positivos = n() - sum(leitos_negativos, na.rm = T)
  ) 

#Conclusões a partir de prerrogativas

#1 
- #Entre 2019 e 2020 vemos um crescimento no número de leitos
- #Com uma queda nos meses de março em ambos os anos (justamente os meses onde a pandemia se comportou pior, segundo reportagens publicadas)
- #No entanto em março de 2020, apõs a queda, ela voltou a sua ascensão no número de leitos
  
#2 
- #Fica claro a visão de crescimento no número de leitos nos últimos anos devido a pandemia (corroborando com a reportagem do link do case)
- #Com um número maior de leitos SUS em comparação com leitos não SUS
- #Leitos não SUS tiveram um crescimento mais padronizado,uma curva ascendente
- # Leitos SUS embora tenha crescido (até mais que não SUS), foi uma curva menos padronizada com alguns picos negativos e positivos, na escalada.

#3 
- # Comparou-se 2014 (pré pandemia) x 2021 (final de pandemia) - Entender evolução de leitos
- # Uma boa quantidade de municipios, teve um número menor de leitos, quando comparamos com o período pós pandemia
- # É bom lembrar que comparamos com o melhor Ano pré pandemia, se compararmos com 2018 por exemplo isso seria bem diferentes
- # Fica claro a partir dos dados do dataset "leitos_saldo_geral" que muitos municipios nessa comparação 2014-2021 tiveram um saldo negativo
  # Porém predominantemente,houve um saldo positivo (crescimento) nessa comparação
  

#Sugestões de estudo
- # Entender o pq dessa variação dos leitos nos municipios durante anos
- # Estados com saldo negativo, entender e levantar hipóteses de pq isso ocorreu




