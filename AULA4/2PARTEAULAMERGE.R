
#AULA 4 

#INNER JOIN####

# Criar a tabela de funcionários
funcionarios <- data.frame(
  IDFuncionario = c(1, 2, 3, 4, 5),
  PrimeiroNome = c("João", "Maria", "Pedro", "Ana", "Carlos"),
  Sobrenome = c("Silva", "Santos", "Oliveira", "Ferreira", "Rodrigues"),
  IDDepartamento = c(101, 102, 101, 103, 102)
)

# Criar a tabela de departamentos
departamentos <- data.frame(
  IDDepartamento = c(101, 102, 103),
  NomeDepartamento = c("Vendas", "TI", "RH")
)

colnames(departamentos)
# Realizar o INNER JOIN entre as tabelas 
resultado <- merge(funcionarios, departamentos, 
                   by= "IDDepartamento", all = F)


# Exibir o resultado
print(result)

#Outra forma usando a própria função inner join

library(dplyr)

# Realizar o INNER JOIN entre as tabelas funcionarios e departamentos
resultado2 <- funcionarios %>% 
  inner_join(departamentos, by= "IDDepartamento")


# Exibir o resultado
print(resultado)

#LEFT JOIN####

# Alterando a base de dados
funcionarios <- data.frame(
  IDFuncionario = c(1, 2, 3, 4, 5),
  PrimeiroNome = c("João", "Maria", "Pedro", "Ana", "Carlos"),
  Sobrenome = c("Silva", "Santos", "Oliveira", "Ferreira", "Rodrigues"),
  IDDepartamento = c(15, 102, 15, 103, 102) #Alterei para 15 
)

# Criar a tabela de departamentos
departamentos <- data.frame(
  IDDepartamento = c(101, 102, 103, 104),
  NomeDepartamento = c("Vendas", "TI", "RH", "Logística") #
)
# Supondo que você já tenha as bases de dados 'funcionarios' e 'departamentos' definidas.

# Realizar o LEFT JOIN entre as tabelas funcionarios e departamentos
resultado3 <- merge(funcionarios, departamentos, all.x = TRUE)


# Exibir o resultado
print(resultado3)

#Outra forma
library(dplyr)

# Realizar o LEFT JOIN entre as tabelas funcionarios e departamentos
resultado4 <- funcionarios %>% 
  left_join(departamentos, by= "IDDepartamento")


#E se as colunas estiverem com nomes diferentes ?

#Alterando a coluna de junção
funcionarios$Nomedpt <- funcionarios$IDDepartamento
funcionarios <- funcionarios[ ,-4]

#Indicando as colunas no merge
resultado5 <- merge(funcionarios, departamentos, 
              by.x = "Nomedpt",
              by.y = "IDDepartamento", all.x = TRUE)

#Dizendo quais colunas são iguais
resultado6 <- funcionarios %>%
  left_join(departamentos, by = c("Nomedpt" = "IDDepartamento"))



#RIGHT JOIN####
library(dplyr)

# Usando  a mesma base de dados de funcionários, mas 
#Sem a coluna do ID do departamento
funcionarios_right <- data.frame(
  IDFuncionario = c(101, 2, 102, 4, 104),
  PrimeiroNome = c("João", "Maria", "Pedro", "Ana", "Carlos"),
  Sobrenome = c("Silva", "Santos", "Oliveira", "Ferreira", "Rodrigues")
)

# dpt

departamentos_right <- data.frame(
  IDDepartamento = c(101, 102, 103, 104),
  NomeDoDepartamento = c("Vendas", "TI", "RH", "Logística")
)


# Realizar o RIGHT JOIN entre as tabelas departamentos_right 
#e funcionarios_right

resultado7 <- merge(departamentos_right, 
                    funcionarios_right,
                    by.x = "IDDepartamento",
                    by.y = "IDFuncionario", 
                    all.x = TRUE)

#Outra forma
library(dplyr)

resultado8 <- funcionarios_right %>% 
  right_join(departamentos_right,
             by= c("IDFuncionario" = "IDDepartamento"))


#CROSS JOIN####
# Criar a primeira tabela
tabela1 <- data.frame(
  Produto = c("A", "B", "C"),
  Preco = c(10, 15, 20)
)


# Criar a segunda tabela
tabela2 <- data.frame(
  Cliente = c("X", "Y"),
  Quantidade = c(2, 3)
)

# Realizar um CROSS JOIN entre as tabelas tabela1 e tabela2
resultado9 <- merge(tabela1, tabela2, by = NULL) # 1


resultado10 <- merge(tabela2, tabela1, by = NULL) #2

#FULL JOIN####
# Base de Dados 1 - Eleições 2018
eleicoes_2018 <- data.frame(
  NomePrefeito = c("João", "Maria", "Pedro", "Ana", "Carlos"),
  TotalVotos = sample(10000:50000, 5),
  SucessoEleitoral = c("Eleito", "Não eleito", "Eleito", "Não eleito", "Eleito"),
  Sexo = c("Masculino", "Feminino", "Masculino", "Feminino", "Masculino"),
  Idade = c(45, 38, 52, 41, 49),
  PartidoPolitico = c("PT", "MSB", "PSDB", "PL", "PDT"),
  AnoEleicao = 2018
)

# Base de Dados 2 - Eleições 2020
eleicoes_2020 <- data.frame(
  NomePrefeito = c("Lúcia", "Ricardo", "Fernanda", "Roberto", "Sara"),
  TotalVotos = sample(10000:50000, 5),
  SucessoEleitoral = c("Eleito", "Não eleito", "Eleito", "Não eleito", "Eleito"),
  Sexo = c("Feminino", "Masculino", "Feminino", "Masculino", "Feminino"),
  Idade = c(39, 47, 42, 56, 33),
  PartidoPolitico = c("PT", "MSB", "PSDB", "PL", "PDT"),
  AnoEleicao = 2020
)

# Base de Dados 3 - Eleições 2022
eleicoes_2022 <- data.frame(
  NomePrefeito = c("Paulo", "Isabel", "Marcelo", "Larissa", "Fernando"),
  TotalVotos = sample(10000:50000, 5),
  SucessoEleitoral = c("Eleito", "Não eleito", "Eleito", "Não eleito", "Eleito"),
  Sexo = c("Masculino", "Feminino", "Masculino", "Feminino", "Masculino"),
  Idade = c(48, 35, 50, 44, 37),
  PartidoPolitico = c("PT", "MSB", "PSDB", "PL", "PDT"),
  AnoEleicao = 2022
)

# Realizar um FULL JOIN entre as bases de dados usando merge()
Bese1 <- merge(eleicoes_2018, eleicoes_2020, all = TRUE)
Basetotal <- merge(Bese1, eleicoes_2022, all = TRUE)



# Realizar a junção das bases de dados
library(dplyr)

Basetotal2 <- full_join(eleicoes_2018, 
                        eleicoes_2020, 
                        by= NULL) %>% 
  full_join(eleicoes_2022, by = NULL)



# mais uma forma 
Basetotal3 <-   bind_rows (eleicoes_2018, 
                           eleicoes_2020, 
                          eleicoes_2022)

#E se fosse uma pesquisa de painel e a intenção fosse colocar
#Uma base ao lado da outra?

# mais uma forma 
Basetotal4 <- bind_cols(eleicoes_2018, 
                        eleicoes_2020, 
                        eleicoes_2022)

############################

#MERGE####

library(readr)

#Votos
votos_sen <- read_delim("votos_sen.csv", 
                        delim = ";", escape_double = FALSE, 
                        col_types = cols(`Soma de Votos nominais` = col_number()), 
                        trim_ws = TRUE)

View(votos_sen) 

votos_sen$NOME <-  votos_sen$`Rótulos de Linha`



#excluir essa coluna renomeada
votos_sen <- votos_sen[ ,-1]


# Retirar acentos e outros caracteres
# install.packages("stringi")
 library(stringi)

# 

# Removendo todos os caracteres que não 
# são letras (maiúsculas ou minúsculas), 
# números ou espaços. 
# A expressão regular [^a-zA-Z0-9 ] corresponde 
# a todos os caracteres que não são letras 
# (maiúsculas ou minúsculas), números ou espaços 
# e substitui-os por uma string vazia ""
# 
# a opção "ASCII//TRANSLIT" ajuda a substituir 
# os caracteres 
# acentuados por suas versões não acentuadas.
#                     "ASCII//TRANSLIT"

votos_sen$NOME <- gsub("[^a-zA-Z0-9 ]", "", 
                  iconv(votos_sen$NOME, 
                        to = "ASCII//TRANSLIT"))
#candidatos
cand_sen <- read_delim("cand_sen.csv", delim = ";", 
                       escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                       trim_ws = TRUE)


cand_sen$NOME <- cand_sen$NM_CANDIDATO
#excluir essa coluna renomeada
cand_sen <- cand_sen[ , -18]

#remover os acentos...
cand_sen$NOME <- gsub("[^a-zA-Z0-9 ]", "", 
                       iconv(cand_sen$NOME, 
             to = "ASCII//TRANSLIT"))

#receita
receita_sen <- read_delim("receita_sen.csv", 
delim = ";", escape_double = FALSE, #Não usaremos aspasduplas
locale = locale(decimal_mark = ","), trim_ws = TRUE) #trim_ws 
#retira os espaços em branco extras ao redor dos valores 
#serão removidos durante a leitura dos dados. Isso é útil para garantir que não haja espaços em branco indesejados nos dados lidos.

receita_sen$NOME <- receita_sen$`Rótulos de Linha`

#excluir essa coluna renomeada
receita_sen <- receita_sen[ , -1]

#remover os acentos...
receita_sen$NOME <- gsub("[^a-zA-Z0-9 ]", "", 
                      iconv(receita_sen$NOME, 
                            to = "ASCII//TRANSLIT"))


#MERGE
#POR NOME
Base1 <- merge(cand_sen, receita_sen, by = "NOME") #nulos
BaseSenado1 <- merge(Base1, votos_sen, by=  "NOME")

#JUNTANDO TUDO, INDEPENDENTE DO NOME
Base2 <- merge(cand_sen, receita_sen, all = T)
BaseSenado2 <- merge(Base2, votos_sen, all = T)


#Precisaremos do pacote 
library(writexl)

#salvando
writexl::write_xlsx(BaseSenado1, path = "Teste1.xlsx")

writexl::write_xlsx(BaseSenado2, path = "Teste2.xlsx")

#Depois de arrumar verificamos que foram 7 casos
#com nomes realmente diferentes

# https://sig.tse.jus.br/ords/dwapr/r/seai/sig-candidaturas/cargo?session=9974857748415

#Explicar certinho para os alunos o q acontece quando repete 
#as colunas, como limpar, como organizar, enfim, para deixar 
#a base ok para as análises! 
