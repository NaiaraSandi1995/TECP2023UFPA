#Regressão####
##Linear simples####

#FDefinir o diretório de trabalho
getwd()
setwd("Dados")

#Abrir a base 
library(readr)
sen2018 <- read_delim("sen2018.csv", delim = ";", 
                      escape_double = FALSE, locale = locale(decimal_mark = ",", 
encoding = "Latin1"), trim_ws = TRUE)

#OR
#sen2018 <- read.csv2("sen2018.csv", fileEncoding = "latin1")


#Pacotes utilizados
install.packages("")

library(ggplot2)
library(sjPlot)
library(dplyr)
library(ggfortify)
library(olsrr)
library(coefplot)
library(ggpmisc)
library(psych)

#Visualização da base de dados####
View(sen2018)   


colnames(sen2018)
# [1] "SQ_CANDIDATO"             "SIGLA_UF"                 "NOME_CANDIDATO"          
# [4] "NUMERO_PARTIDO"           "SIGLA_PARTIDO"            "IDADE_DATA_POSSE"        
# [7] "DESCRICAO_SEXO"           "DESCRICAO_GRAU_INSTRUCAO" "DESCRICAO_ESTADO_CIVIL"  
# [10] "DESCRICAO_COR_RACA"       "DESCRICAO_OCUPACAO"       "DESC_SIT_TOT_TURNO"      
# [13] "SITUACAO_REELEICAO"       "DES_SITUACAO_CANDIDATURA" "votos"                   
# [16] "TOTAL_RECEITAS"          

table(sen2018$DES_SITUACAO_CANDIDATURA)
# APTO 
# 311 

table(sen2018$DESC_SIT_TOT_TURNO)
# ELEITO NÃO ELEITO 
# 54           257 

#O total de votos está relacionado com ter sido eleito? 
#O que será que influencia o total de votos?
#Será que a receita pode ser uma variável preditiva 

#Icialmente temos que verificar se há relação entre 
# total de votos 
# e receita (e essa, como podemos chamá-la?)


#Gráfico dispersão####
#Análise gráfica entre receitas e votos
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point()
#Imagem 1 


#Notem que os valores das receitas são tão altos 
#que aparecem em notação científica, queremos alterar isso?
#Se sim, podemos fazer o seguinte:
options(scipen = 999)
#Imagem 2 

#Organização do gráfico
ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receitas", y= "Votos", title = "Receitas por votos") +
  theme_classic()
#Imagem 3

Gra1 <- ggplot(data = sen2018, aes(y = votos, x= TOTAL_RECEITAS )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T) +
  labs(x ="Receita", y= "Votos", title = "Receitas por votos") +
  theme_classic()


#Imagem 4 - aprimorando
#inserção da equação da reta 

#################################################
#FINAL DA AULA
#pessoal, vejam que agora eu inseri mais algumas linhas 
#no gráfico, essas linhas novas estão marcadas com um "#"
#São para a inserção da equação da reta do modelo 1 de regressão 
# no gráfico, a função utilizada é a " stat_poly_eq", do pacote 
# ggpmisc, então para conseguir rodar esse gráfico, instalem 
#e ativem o pacote. 


Gra1 <- ggplot(data = sen2018, 
               aes(y = votos, x= TOTAL_RECEITAS,
                   size = TOTAL_RECEITAS, 
                   color =votos)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",  se = T, color = "red")+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label..,# stat_poly_eq adiciona uma equação de regressão ao gráfico.
                                 sep = "*plain(\",\")~~")),# aes(label = ...) especifica o rótulo da equação, que inclui ..eq.label.. para a equação de regressão e ..adj.rr.label.. para o coeficiente de determinação ajustado (R² ajustado).
               label.x = 0.05, label.y = 400,# label.x e label.y definem as coordenadas onde a equação de regressão será exibida no gráfico.
               parse = TRUE, coef.digits = 2)+# parse = TRUE permite a interpretação de texto, o que é útil para formatar a equação.
  labs(x ="Receita", y= "Votos", 
       title = "Receitas por votos")+ theme_classic() 

#sep = "*plain(\",\")~~" especifica o separador a ser usado entre os elementos. Vamos entender isso detalhadamente:

# *plain(\",\")~ ~:  é um formato especial usado para formatar o separador. 
# plain(\",\") indica que o separador é a vírgula (,).
# ~~ é usado para criar uma quebra de linha (espaço).

#olhar o gráfico 
Gra1

formatacao <- theme(text = element_text(family = "serif", size = 14),
                    title = element_text(color = "black"),
                    axis.line = element_line(color = "black")) +
  theme(legend.position="none") 

# “left”,“top”, “right”, “bottom” and none

Gra2 <- Gra1 + formatacao

#Olhar o gráfico 
Gra2

EquaRet <- lm(votos ~ TOTAL_RECEITAS, data = sen2018)
summary(EquaRet)

summary(sen2018$votos)
#####################################################

#Certo, essa já uma visualização gráfica mais próxima 
#do real, no entanto, vejam que os valores tem escalas
#são bastante distintas?

#Então para melhorar a visualização e tornar estatísticamente
#mais proporcional os dados, podemos realizar uma transformação logarítmica
#utilizando a função log


sen2018$log.votos <- log(sen2018$votos)
sen2018$log.receitas <- log(sen2018$TOTAL_RECEITAS)

ggplot(data = sen2018, aes(y = log.votos, x= log.receitas )) +
  geom_point()

#Construção do modelo

Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#Apresentação do Erro
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#  NA/NaN/Inf in 'y'

#Análise da base para identificar o erro

#Correção do erro
sen2018[is.na(sen2018) | sen2018== "Inf"] =NA

sen2018[is.na(sen2018) | sen2018== "-Inf"] =NA

sen2018[is.na(sen2018) | sen2018== "NAN"] =NA

#Modelo após a correção
Model1 <- lm(sen2018$log.votos ~ sen2018$log.receitas)

#OU
Model1.1 <- lm(log.votos ~ log.receitas, data = sen2018)

summary(Model1.1)
#Interpretação no slide


#Construção da tabela que poderá ser apresentada junto ao texto
#Usando a função tab_model do pacote sjPlot
tab_model(Model1.1, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

#Análise de resíduos####


resid <- (cbind(sen2018$log.votos, predict(Model1.1), 
                residuals(Model1.1)))


View(resid)

sen2018[252, c(3, 15, 16)]
#84940        R$2.250.000

#Análise de resíduos através de gráficos#### 
#Função autoplot

obj1 <- autoplot(Model1,
                 nrow = 2,
                 ncol = 2) 

obj1 + theme_classic() 

#Gráficos:
#interpretação no slide 

#Linear multipla#### 

#Quais outras variáveis podem ser interessantes para análise 
#da quantidade de votos?
#Idade na época da posse?
#Sexo?
#Grau de instrução?
#Raça?


#Então incialmente vamos rapidamente olhar e organizar essas variáveis 
#Para pode inserir no teste

summary(sen2018$IDADE_DATA_POSSE)
sen2018$Idade <- sen2018$IDADE_DATA_POSSE
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 27.0    47.0    55.0    55.1    63.0    83.0 

table(sen2018$DESCRICAO_SEXO)
sen2018$Sexo <- sen2018$DESCRICAO_SEXO
# FEMININO MASCULINO 
# 56       255 

table(sen2018$DESCRICAO_GRAU_INSTRUCAO)

sen2018$Instrução <- sen2018$DESCRICAO_GRAU_INSTRUCAO


sen2018 <- sen2018 %>%
  mutate(Instrução = case_when(
    Instrução %in% c("ENSINO FUNDAMENTAL INCOMPLETO", "ENSINO FUNDAMENTAL COMPLETO") ~ "Baixa",
    Instrução %in% c("ENSINO MÉDIO COMPLETO", "ENSINO MÉDIO INCOMPLETO") ~ "Média",
    Instrução %in% c("SUPERIOR COMPLETO", "SUPERIOR INCOMPLETO") ~ "Superior",
    TRUE ~ Instrução
  ))
table(sen2018$Instrução)

sen2018$Instrução <- relevel(sen2018$Instrução,
                             ref = "Baixa")


table(sen2018$DESCRICAO_COR_RACA)
sen2018$Raca <- sen2018$DESCRICAO_COR_RACA
sen2018$Raca <-  as.character(sen2018$DESCRICAO_COR_RACA)

#Modelo de regressão 
Model2 <- lm(log.votos ~ log.receitas + Idade + 
               Sexo + Instrução + Raca, data = sen2018)

summary(Model2)

tab_model(Model2, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "numeric_stars")

#Condionantes para o teste de regressão####
#VIF: Variance Inflation Factors

library(olsrr) # Pacote para VIF



ols_vif_tol(Model2)

#Condition Index####

ols_eigen_cindex(Model2)



#############
#ARRUMANDO O MODELO#########

sen2018 <- sen2018 %>%
  mutate(Instrução = case_when(
    Instrução %in% c("ENSINO FUNDAMENTAL INCOMPLETO", 
                     "ENSINO FUNDAMENTAL COMPLETO", 
                     "ENSINO MÉDIO COMPLETO", 
                     "ENSINO MÉDIO INCOMPLETO") ~ "Média e Baixa",
    Instrução %in% c("SUPERIOR COMPLETO", "SUPERIOR INCOMPLETO") ~ "Superior",
    TRUE ~ Instrução
  ))
table(sen2018$Instrução)


table(sen2018$DESCRICAO_COR_RACA)
sen2018$Raca <- sen2018$DESCRICAO_COR_RACA

sen2018 <- sen2018 %>%
  mutate(Raca = case_when(
    Raca %in% c("AMARELA", 
                "INDÍGENA", 
                "PARDA", 
                "PRETA") ~ "Não Branco",
    Raca %in% c("BRANCA") ~ "Branco",
    TRUE ~ Raca
  ))
table(sen2018$Raca)

sen2018$Raca <- as.factor(sen2018$Raca)
sen2018$Raca <- relevel(sen2018$Raca,
                        ref = "Não Branco")


#Modelo de regressão 
Model2 <- lm(log.votos ~ log.receitas + Idade + 
               Sexo + Instrução + Raca, data = sen2018)

summary(Model2)

tab_model(Model2, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "numeric_stars")

#Condionantes para o teste de regressão####
#VIF: Variance Inflation Factors
library(olsrr) # Pacote para VIF
ols_vif_tol(Model2)

#Condition Index####
ols_eigen_cindex(Model2)


#Coefplot###### 
#O Coefplot é um tipo de gráfico que pode ser utilizado
#para apresentar graficamente os resultados de um teste 
#de regressão #pacote coefplot 

obj1 <- coefplot(Model2, title = "Análise dos votos a senado em 2018",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 5)

obj2 <- obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                            alpha=105) 
obj2 + formatacao 

#Outras possibilidades de análise
plot_model(Model2) 
plot_model(Model2, type = "pred", terms = c("log.receitas"))


coefplot1 <-  plot_model(Model2,
                         show.intercept = F,
                         show.p = T,
                         show.values = T,
                         value.size = 4,
                         digits = 2,
                         vline.color = "pink",
                         sort.est = TRUE,
                         width = 0.05)  

coefplot1 + theme_classic() +
  scale_color_sjplot("system")
 


show_sjplot_pals()

####Análise da correlação entre todas e a Vdependente
GraficoModelos <- sen2018 %>%
  select(log.votos, log.receitas, Instrução,
         Sexo, Idade, Raca)


pairs.panels(GraficoModelos)

