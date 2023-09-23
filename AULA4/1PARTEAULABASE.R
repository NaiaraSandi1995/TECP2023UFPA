#1ª parte da aula 

#Salvamentos e abertura da base de dados####
#salvamento####

#Vamos salvar a base que criamos em csv E xlsx 
#Criação dos vetores####
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
#CSV salvamento####
write.csv2(Ficha_Pacientes, file = "Ficha2.csv", 
           quote = F, #Dividir por ponto e vírgulas
           row.names = F,#remover a primeira coluna com id do sistema
           fileEncoding = "latin1")#função pra definir a linguagem 

#CSV abertura####

#Precisaremos do pacote readr
library(readr)
#abertura
Ficha2 <- read_delim("Ficha2.csv", delim = ";", 
                     locale = locale(encoding = "Latin1"))
View(Ficha2)

#Xlsx Salvamento#### 

#Precisaremos do pacote 
library(writexl)

#salvando
writexl::write_xlsx(Ficha_Pacientes, path = "Ficha3.xlsx")

#Xlsx Abertura####
#Pacote:
library(readxl)

Ficha3 <- read_excel("Ficha3.xlsx")

View(Ficha3) 

#Abertura de dados reais####
#Abertura em SAV####

#Pacote:
library(haven)

#Como o nome da base é bastate grande, vamos chamar por um 
#nome mais resumido
Brasil2014 <- read_sav("863896541Brazil LAPOP AmericasBarometer 2014 Espanol v3.0_W.sav")
#Após realizar as alterações na base
#se quiser salvar no mesmo formato, use:
write_sav(data = Brasil2014, path = "Brasil2014.sav")

#Abertura em DTA####
#Use o mesmo pacote haven
Brasil2014dta <-  
  read_dta("LAPOP 2014/636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")



#Após realizar as alterações na base
#se quiser salvar no mesmo formato, use:
write_dta(data = Brasil2014dta, path = "Brasil2014dta.dta")

################################
