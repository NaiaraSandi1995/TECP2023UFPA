#####Carregar a Base de Dados######
sen2018 <- read.csv2("dados/sen2018.csv")

#####Pacotes#####
library(tidyverse)

#####Tabelas de Frequência#####
sen2018 %>%
  count(SIGLA_PARTIDO) %>%
  print(n = 50)

table(sen2018$SIGLA_PARTIDO)

#####Tabela de Contingência######
sen2018 %>% 
    group_by(SIGLA_PARTIDO, DESCRICAO_SEXO) %>%
    summarise(n = n()) %>%
    spread(DESCRICAO_SEXO, n) %>%
    replace(is.na(.), 0)%>%
    mutate(Total = MASCULINO + FEMININO) %>%
    print(n = 50)

table(sen2018$SIGLA_PARTIDO, sen2018$DESCRICAO_SEXO) 

#####Proporções#####
######Total######
prop.table(table(sen2018$SIGLA_PARTIDO, sen2018$DESCRICAO_SEXO)) 

######Linhas######
round(prop.table(table(sen2018$SIGLA_PARTIDO, sen2018$DESCRICAO_SEXO), 1)*100, 1)

######Colunas######
prop.table(table(sen2018$SIGLA_PARTIDO, sen2018$DESCRICAO_SEXO), 2)

######Usando o Tidy######
sen2018 %>% 
  group_by(SIGLA_PARTIDO, DESCRICAO_SEXO) %>%
  summarise(n_cand = n()) %>% #é usada para calcular estatísticas resumidas dentro de cada grupo definido nas etapas anteriores
  ungroup(DESCRICAO_SEXO) %>% # remover a estrutura de grupos criada anteriormente
  mutate(pr_cand = round((n_cand/sum(n_cand))*100, 1)) %>%  #criar uma nova coluna chamada "pr_cand" e calcular a porcentagem de candidatos em relação ao total de candidatos em cada grupo. 
  pivot_wider(names_from = DESCRICAO_SEXO, #transformar os dados de forma que as categorias únicas de "DESCRICAO_SEXO" se tornem colunas no conjunto de dados. 
              values_from = c(n_cand, pr_cand), #Os valores de "n_cand" e "pr_cand" são extraídos e preenchidos com zeros onde não há dados disponíveis.
              values_fill = 0) %>% 
  flextable() #Criar uma tabela

#Flextable https://ardata-fr.github.io/flextable-book/index.html#build-with-a-data.frame


####Tabelas com 3 variáveis####
######Recodificação######
sen2018 <- sen2018 %>%
  mutate(
    cor_raca = case_when(
      DESCRICAO_COR_RACA == 'PARDA' | DESCRICAO_COR_RACA == 'PRETA' ~ 'Negra',
      DESCRICAO_COR_RACA == 'BRANCA' ~ 'Branca',
      TRUE ~ 'Outras')
  ) 
tab <- sen2018 %>% 
        filter(cor_raca != "Outras") %>%
        group_by(SIGLA_PARTIDO, DESCRICAO_SEXO, cor_raca) %>%
        summarise(n_cand = n()) %>% 
        ungroup(DESCRICAO_SEXO, cor_raca) %>%
        mutate(pr_cand = round((n_cand/sum(n_cand))*100, 1)) %>%
        pivot_wider(names_from = c(DESCRICAO_SEXO, cor_raca),  
                    values_from = c(n_cand, pr_cand), 
                    values_fill = 0) %>%
        print(n = 50)

#####Tabelas em HTML####
install.packages("flextable")
library(flextable)

sen2018 %>% 
  filter(cor_raca != "Outras") %>%
  group_by(SIGLA_PARTIDO, DESCRICAO_SEXO, cor_raca) %>%
  summarise(n_cand = n()) %>% 
  ungroup(DESCRICAO_SEXO, cor_raca) %>%
  mutate(pr_cand = round((n_cand/sum(n_cand))*100, 1)) %>%
  pivot_wider(names_from = c(DESCRICAO_SEXO, cor_raca),  
              values_from = c(n_cand, pr_cand), 
              values_fill = 0) %>%
  flextable()

#####Medidas de Tendência Central####
######Média#####
mean(sen2018$IDADE_DATA_POSSE)

######Mediana#####
median(sen2018$IDADE_DATA_POSSE)

######Moda######
calcular_moda <- function(vetor) {
  contagem <- table(vetor)
  moda <- names(contagem[contagem == max(contagem)])
  return(as.numeric(moda))
}

calcular_moda(sen2018$IDADE_DATA_POSSE)

#####Summary#####
summary(sen2018$IDADE_DATA_POSSE)

######Histograma Idade######
hist(sen2018$IDADE_DATA_POSSE)

#####Teste de Normalidade#####
shapiro.test(sen2018$IDADE_DATA_POSSE)



# Shapiro-Wilk normality test
# 
# data:  sen2018$IDADE_DATA_POSSE
# W = 0.98955, p-value = 0.02514

#####Medidas de Dispersão#####
######Variância######
var(sen2018$TOTAL_RECEITAS)
options(scipen = 999)
var(sen2018$TOTAL_RECEITAS)

######Desvio Padrão#####
sd(sen2018$TOTAL_RECEITAS)


######Dispersão dos dados######
mean(sen2018$TOTAL_RECEITAS)
hist(sen2018$TOTAL_RECEITAS)

#####Escore Z#####
######Escore Z do Jader Barbabalho######
jader_z <- (74-mean(sen2018$IDADE_DATA_POSSE))/
  sd(sen2018$IDADE_DATA_POSSE)

jader_z #[1] 1.693606

######Calcular todos os Escores Z da base#####
sen2018$idade_z <- (sen2018$IDADE_DATA_POSSE - 
                      mean(sen2018$IDADE_DATA_POSSE))/
                    sd(sen2018$IDADE_DATA_POSSE)

summary(sen2018$idade_z)
mean(sen2018$idade_z)

# > summary(sen2018$idade_z)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -2.518651 -0.726202 -0.009222  0.000000  0.707758  2.500208 
# > mean(sen2018$idade_z) #Explicar a leitura do valor em notação
# [1] 9.594753e-17

#####Risco Relativo#####
install.packages("epitools")
library(epitools)
sen2018$DESCRICAO_SEXO <- as.factor(sen2018$DESCRICAO_SEXO)
sen2018$DESCRICAO_SEXO <- relevel(sen2018$DESCRICAO_SEXO, "MASCULINO")
sen2018$DESC_SIT_TOT_TURNO <- as.factor(sen2018$DESC_SIT_TOT_TURNO)
sen2018$DESC_SIT_TOT_TURNO <- relevel(sen2018$DESC_SIT_TOT_TURNO, "NÃO ELEITO")

epitab(table(sen2018$DESCRICAO_SEXO, sen2018$DESC_SIT_TOT_TURNO),
       method = "riskratio")


