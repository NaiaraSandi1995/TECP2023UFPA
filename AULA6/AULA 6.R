#TÓPICO 5 - ####

#Apresentação gráfica####
#Boxplot####

#Abertura da base de dados
library(readr)
sen2018 <- read_delim("C:/Users/nayar/OneDrive/Tópicos especiais em ciência política/Aulas/TECPquant/Dados/sen2018.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                      trim_ws = TRUE)
View(sen2018)

#Nós vimos que a versão mais simples do boxplot já 
#Vem com a mediana, mas podemos inserir também  
#o ponto da média

#No R base nós fazemo isso
#através da criação de um objeto contendo a média
#Que depois será inserido junto ao gráfico
Média <- mean(sen2018$IDADE_DATA_POSSE)

boxplot(sen2018$IDADE_DATA_POSSE)
points(Média, pch=16,col="red")

#PCH significa Plot character, ou plotagem de caracter
#é a função que define qual será o caracter plotado 
#existem 25 modelos, você pode escrever pch no help 
#Para encontrar todos os tipos
#Col é a cor que o seu ponto terá

#Agora vamos etender como isso funciona no ggplot 
#Documentação do pacote 

#Pacote para criação de gráficos 
install.packages("ggplot")
library(ggplot2)

#Antes de enteder como criar um boxplot 
#É preciso entender que qualquer projeção gráfica utilizando 
#o ggplot é criada através de uma série de camadas específicas 
#Cada camada irá inserir um detalhe ao gráfico 

#Além disso, existe um mapeamento geral que compõe cada gráfico
#Vamos ver como isso funciona

#1º versão:
ggplot(sen2018, aes(y= IDADE_DATA_POSSE)) + 
  geom_boxplot()


#2ª versão:
ggplot(sen2018, aes(y= IDADE_DATA_POSSE)) + 
  geom_boxplot() + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem")


#todos os gráficos possuem em seu mapeamento o eixo y e o eixo x
#No caso do boxplot o ideal é que em um dos eixos
#exista uma variável qualitativa, quando temos a 
#intenção de comparar categorias entre si, inserimos uma variável
#específica, quando não temos a inteção de trabalhar com variáveis 
#cruzadas temos a intenção de fazer uma análise cruzada
#podemos não inserir nada, porém isso fará diferença quando quisermos
#realizar certas análises, como a média
#Por isso, inserimos algum outra variável no eixo faltante
#Pode ser uma contagem com a função count ou então uma única
#informação

#3ª versão: Inserção da média

sen2018$V <- c("pessoa")

sen2018$DES_SITUACAO_CANDIDATURA

#*Falar sobre as cores
ggplot(sen2018, aes(y= IDADE_DATA_POSSE, 
                    x = DES_SITUACAO_CANDIDATURA)) + 
  geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ #Cores do gráfico
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem")


#Explicar porque precisa indicar para contar ou ter
#Uma variável constante
sen2018$V <- c("pessoa")

sen2018$DES_SITUACAO_CANDIDATURA


#Versão 4
ggplot(sen2018, aes(y= IDADE_DATA_POSSE, 
                    x = DES_SITUACAO_CANDIDATURA)) + 
  geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
               shape=16, size=4, color="#87C4EB") 

#Versão 5
#Fonte####
Fonte <-  theme(text = element_text(family = "serif", size = 13),
                title = element_text(color = "#8b0000"),
                axis.line = element_line(color = "#8b0000"), 
                axis.text = element_text(colour = "#8b0000", size = rel(0.5))) 
#rel() is used to specify sizes relative



#Análise cruzada
#Versão 1

sen2018$DESCRICAO_SEXO

ggplot(sen2018, aes(y= IDADE_DATA_POSSE, x=DESCRICAO_SEXO)) + 
  geom_boxplot() 



#Versão 2
ggplot(sen2018, aes(y= IDADE_DATA_POSSE, x=DESCRICAO_SEXO,
                         fill= DESCRICAO_SEXO)) + 
  geom_boxplot()
#

#Versão 3
ggplot(sen2018, aes(y= IDADE_DATA_POSSE, x=DESCRICAO_SEXO,
                         color= DESCRICAO_SEXO)) + 
  geom_boxplot()

#Mudar a idade apenas para ter um outlier
sen2018$IDADE_DATA_POSSE[311] <- 95 

ggplot(sen2018, aes(y= IDADE_DATA_POSSE, x=DESCRICAO_SEXO)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3, 
               fill= c("blue", "red"), alpha = 0.2)

#Versão 5
#Salvar em um objeto
Gráfico <- ggplot(sen2018, aes(y= IDADE_DATA_POSSE, x=DESCRICAO_SEXO)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3, 
               fill= c("blue", "red"), alpha = 0.2)
Gráfico

#Versão 6
Gráfico1 <-   Gráfico + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="IDADE_DATA_POSSE", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
               shape=16, size=2, color="red")



#Versão 7
Gráfico1 + Fonte


#Histograma####
#Versão1
sen2018$Id <- c(1:1500)

Gráfico1 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE))+
  geom_histogram()

Gráfico1

#Versão2
Gráfico2 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE, 
                                fill= DESCRICAO_SEXO))+
  geom_histogram()

Gráfico2

#Versão3
Gráfico3 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE, fill= DESCRICAO_SEXO))+
  geom_histogram(position = "dodge") #Para colocar lado a lado as barras

Gráfico3

#Versão4
Gráfico4 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE, fill= DESCRICAO_SEXO))+
  geom_histogram(position = "dodge", binwidth = 5) #Para colocar lado a lado as barras
#tamanho das barras
Gráfico4

#Versão5 - cor
# https://colorbrewer2.org/#type=qualitative&scheme=Pastel2&n=3

Gráfico5 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE, fill= DESCRICAO_SEXO))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")

Gráfico5

#Versão6 

Gráfico6 <- ggplot(sen2018, aes(x= IDADE_DATA_POSSE, 
                                fill= DESCRICAO_SEXO))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")+
  scale_y_continuous(limits = c(0, 50))+
  labs(title = "Idade por sexo", 
       y="Contagem", x= "Idade") +
  theme_classic()

Gráfico6

#versão7
Gráfico6 + Fonte

