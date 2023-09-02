#Aula número 2 - programação em R 

#Slide 2####
Y <- "Gato"
Y <- 5
Y <- 5.321

print(Y)

print(Y, digits = 1)
print(Y, digits = 2)

#Boas práticas*
Z = 5:60
Z = seq(5,60, by=5)

print(Z)

#Escolher diretório de trabalho e salvar objeto criado
getwd()
setwd("C:/Users/nayar/OneDrive/Tópicos especiais em ciência política/Aula 2")

save(A, file = "A.RData")
load("A.RData")


#Slide 3####

2+5

2-5

2/5

2*5

2^5

2<5

2>5

2<=5

2>=5

#Utilização de parênteses

(5-2) * (3+2)


5 - 2 * 3 +2 

5 == 2

2 != 5 # (! mais =)


(10 > 5) & (10 > 8) #Shift + e


(2 < 5) | (7 < 8)

A <- 2

B <- 5


#Slide 5 a 8####
#Tipos de variáveis e criação de base de dados 
#Idade


#Criação de base de dados

#Criação dos vetores
Id <-  c(1:4)
Nomes <- c("Ana", "Gilberto", "Rodrigo", "Marcela")
Peso <- c(75.6, 99, 62.8, 102)
Idades <- c(25, 18, 44, 23)
Escolaridade <- c("Graduação", "Mestrado", "Primário", "Graduação")
Exerc_Recomend <- c("Natação", "Pilates", "Musculação", "Corrida")
Comidas_preferidas <- c("Chocolate", "Sorvete", "Milho", "Pão")

#Criação do data frame
Ficha_Pacientes <- data.frame(Id, Nomes, Peso, 
                              Idades, Escolaridade, 
                              Exerc_Recomend, Comidas_preferidas)
#Função para ver o data frame
View(Ficha_Pacientes)

## Função salvar#####
#Função para salvar o data frame
save(Ficha_Pacientes, file = "Ficha_Pacientes.RData")

#Slide 9- ####
# Organização dos vetores e variáveis

#ver a base####
#Função para análise das classes de cada variável
str(Ficha_Pacientes)
# 'data.frame':	4 obs. of  7 variables:
#   $ Id                : int  1 2 3 4
# $ Nomes             : chr  "Ana" "Gilberto" "Rodrigo" "Marcela"
# $ Peso              : num  75.6 99 62.8 102
# $ Idades            : num  25 18 44 23
# $ Escolaridade      : chr  "Graduação" "Mestrado" "Primário" "Graduação"
# $ Exerc_Recomend    : chr  "Natação" "Pilates" "Musculação" "Corrida"
# $ Comidas_preferidas: chr  "Chocolate" "Sorvete" "Milho" "Pão"

#A base de dados deverá ter um nome de coluna e linhas 
#Para saber esses nomes use as funções rownames e colnames
rownames(Ficha_Pacientes)
#[1] "1" "2" "3" "4"
colnames(Ficha_Pacientes)#Usando a função names também 
#Conseguimos ver todas as colunas
# [1] "Id"                 "Nomes"              "Peso"               "Idades"            
# [5] "Escolaridade"       "Exerc_Recomend"     "Comidas_preferidas"


print(Ficha_Pacientes)
# Id    Nomes  Peso Idades Escolaridade Exerc_Recomend Comidas_preferidas
# 1  1      Ana  75.6     25    Graduação        Natação          Chocolate
# 2  2 Gilberto  99.0     18     Mestrado        Pilates            Sorvete
# 3  3  Rodrigo  62.8     44     Primário     Musculação              Milho
# 4  4  Marcela 102.0     23    Graduação        Corrida                Pão

#Análise das médias
print(mean(Ficha_Pacientes$Idades))
#[1] 27.5
summary(Ficha_Pacientes$Idades)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   21.75   24.00   27.50   29.75   44.00 
summary(Ficha_Pacientes$Exerc_Recomend)
# Length     Class      Mode 
#  4 character character 
table(Ficha_Pacientes$Escolaridade)
# Graduação  Mestrado  Primário 
# 2          1         1 

#Seleção####
#Selação dos dados indicando 1º a linha e 2º a coluna
Ficha_Pacientes[2,5]
#[1] "Mestrado"
Ficha_Pacientes[2,5, drop=F]
# Escolaridade
# 2     Mestrado
Ficha_Pacientes[2,c("Peso", "Idades", "Nomes")]
# Peso Idades    Nomes
# 2   99     18 Gilberto

#Exclusão####
#Exclusão da primeira linha
Ficha_Pacientes[-1, ]
#Exclusão da primeira linha e última coluna 
Ficha_Pacientes[ -1 , -7]

#Exclusões de colunas
## deleta quant_filhos
Ficha_Pacientes$Exerc_Recomendad <- NULL

## deleta colunas específicas, 
#O mesmo pode ser feito para linhas
Ficha_Pacientes <-  Ficha_Pacientes[, c(-4, -6)]


#Inserção####
#Inserir novos dados em toda a base 
Ficha_Pacientes$Sexo <-  c("F", "M", "M", "F")
Ficha_Pacientes$Quant_filhos <-  c(4:7)


#TIDYVERSE####

#install.packages("tidyverse")
library(tidyverse)

#Substituindo o cbind pelo pipe e mutate

Ficha_Pacientes <- Ficha_Pacientes %>%
  mutate(Prim_emprego = c("sim", "nao", "nao", "sim"))

#Filtros####

#Quero uma base de dados com apenas 5 variáveis
# Selecionar colunas específicas
Base_menor <- Ficha_Pacientes %>%
  select(Id, Nomes, Peso, Idades, Sexo)

# Filtrar por sexo masculino
Base_menor <- Ficha_Pacientes %>%
  filter(Sexo == "M") %>%
  select(Id, Nomes, Peso, Idades, Sexo)

# Filtrar por idade maior ou igual a 20 anos
Base_menor <- Ficha_Pacientes %>%
  filter(Idades >= 20) %>%
  select(Id, Nomes, Peso, Idades, Sexo)

#Filtro usando o filter 
# seleciona apenas colunas numéricas
Filter(is.numeric, Ficha_Pacientes)
# Id  Peso Idades
# 1  1  75.6     25
# 2  2  99.0     18
# 3  3  62.8     44
# 4  4 102.0     23

# seleciona apenas colunas de texto
Filter(is.character, Ficha_Pacientes)
#        Nomes Escolaridade Exerc_Recomend Comidas_preferidas
# 1      Ana    Graduação        Natação          Chocolate
# 2 Gilberto     Mestrado        Pilates            Sorvete
# 3  Rodrigo     Primário     Musculação              Milho
# 4  Marcela    Graduação        Corrida                Pão

#Separando dados das colunas####
library(stringr)

#Inserindo uma variável com 2 informações
Ficha_Pacientes$Nome_Mãe <- 
  c('Maria, Costa', 'Marcos, Ferreira', 'Joana, Silva', 'Pedro, Santo')


#Separando
  str_split_fixed(Ficha_Pacientes$Nome_Mãe, ", ", 2)

#salvando na base 
Ficha_Pacientes$Nomes <- 
  str_split_fixed(Ficha_Pacientes$Nome_Mãe, ", ", 2)

#Alterar (Renomear) o nome das variáveis nome 1 e nome 2 
Ficha_Pacientes <- Ficha_Pacientes %>%
  mutate(Prim.Nome = Nomes[, 1], Seg.Nome = Nomes[, 2])


# Ficha_Pacientes <- Ficha_Pacientes %>%
#   mutate(Prim.Nome = Nomes[, 1], Seg.Nome = Nomes[, 2])%>% 
#   select(-Nomes) # remover a variável "Nomes" 






