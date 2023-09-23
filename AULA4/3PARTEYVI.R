#Base de dados aluna - orientanda Bruno

library(dplyr)
library(writexl)#Salvar tudo em XLSX
library(lubridate)#Para o cálculo da média
library(openxlsx) #abertura de base xlsx

BASE <- BASET

# Calcular a média de quantas vezes cada nome apareceu ao longo das candidaturas

media_por_nome <- BASE %>% 
  group_by(NOME, LEGISLATURA) %>% 
  summarize(contagem = n()) %>% 
  group_by(NOME) %>% 
  summarize(media = round(mean(contagem), 1))


# Divide os dados em grupos com base nas variáveis NOME e LEGISLATURA.
# Calcula a contagem de observações em cada grupo.
# Novamente, divide os dados em grupos apenas com base na variável NOME.
# Calcula a média das contagens em cada grupo resultante.
# O resultado final é uma tabela que mostra a média das contagens para cada valor único de NOME.

# Exibir o resultado
print(media_por_nome)

# Calcular a idade atual
BASE$IDADE.Correta <- 2023 - BASE$Nascimento


# Calcular a média das idades por nome
media_idades <- aggregate(IDADE.Correta ~ NOME, 
                          data = BASE, FUN = mean)


# Exibir o resultado
print(media_idades)

#Juntar a base das  media_por_nome na base
BASE <- merge(BASE, media_por_nome, by= "NOME")


#e se quiser saber o tempo em anos das legislaturas de 
#cada candidato
#opção 1- preciso fazer a subtração do ano final 
#da legislatura pelo ano inicial?
#depois tira a média por pessoa ?
#Opção 2- Cada legislatura não tem 3 anos? 
#Então será que podemos multiplicar 3 pelo número de candidaturas?

BASE$ANOSCANDIDATURA <- 3 * BASE$media
  
media_por_nome$ANOSCANDIDATURA <- 3 * media_por_nome$media

BaseIeL <- merge(media_idades, media_por_nome, by ="NOME")

summary(BaseIeL$IDADE.Correta)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 46.00   65.00   72.00   72.79   79.00  105.00 

summary(BaseIeL$ANOSCANDIDATURA)

# Salvar a nova base de dados em um arquivo xlsx

writexl::write_xlsx(BASE, path = "BASE.xlsx")


####
# Carregar a base de dados original do arquivo Excel existente
arquivo_excel <- "BASE.xlsx" 
base_original <- read.xlsx(arquivo_excel, sheet = 1)

# Criar as duas novas bases de dados
nova_base1 <- data.frame(media_idades)
nova_base2 <- data.frame(media_por_nome)

# Criar um novo arquivo Excel
novo_arquivo_excel <- "NOVO_BASE2.xlsx"
wb <- createWorkbook()

# Adicionar a base original em uma aba
addWorksheet(wb, sheetName = "BaseOriginal")
writeData(wb, sheet = "BaseOriginal", x = base_original)

# Adicionar a nova base 1 em uma aba
addWorksheet(wb, sheetName = "NovaBase1")
writeData(wb, sheet = "NovaBase1", x = nova_base1)

# Adicionar a nova base 2 em uma aba
addWorksheet(wb, sheetName = "NovaBase2")
writeData(wb, sheet = "NovaBase2", x = nova_base2)

# Salvar o novo arquivo Excel
saveWorkbook(wb, novo_arquivo_excel)
