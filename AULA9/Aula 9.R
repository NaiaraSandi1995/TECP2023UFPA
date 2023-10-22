####Preparação do Ambiente de Trabalho####

#####Pacores####
library(tidyverse)

#####Dados#####
sen2018 <- read.csv2("Dados/sen2018.csv", fileEncoding = "latin1")

cor(sen2018$TOTAL_RECEITAS, sen2018$votos)

cor.test(sen2018$TOTAL_RECEITAS, sen2018$votos)

plot(sen2018$TOTAL_RECEITAS, sen2018$votos)

options(scipen = 999)

sen2018 <- sen2018[-285, ]

cor.test(sen2018$TOTAL_RECEITAS, sen2018$votos)

###LOG####
sen2018$logrec <- log(sen2018$TOTAL_RECEITAS)
sen2018$logvot <- log(sen2018$votos)

plot(sen2018$logrec, sen2018$logvot)


ex2 <- ggplot(sen2018, aes(x = logrec, y = logvot))
ex2 + geom_point() +
  labs(x = "Log Total de Receitas dos Candidatos ao Senado",
       y = "Log Votos Recebidos",
       title = "Votos Recebidos por Total de Receitas")

###separar a base####
sen2018cor <- sen2018[c("IDADE_DATA_POSSE",
                        "logvot", "logrec")]

###retirar os 0's####
sen2018cor <- subset(sen2018cor, logvot > 0)

###matriz de correlação####
install.packages("Hmisc")
library(Hmisc)
mcor <- rcorr(as.matrix(sen2018cor))
mcor

###gráfico de calor####
install.packages("corrplot")
library(corrplot)

corrplot(mcor$r, type = "upper", 
         order = "hclust", tl.col = "black", 
         tl.srt = 50)


