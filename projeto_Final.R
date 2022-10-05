pacotes <- c("tidyverse","readxl","MASS","rpart","neuralnet","fastDummies")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando as bases
applications <- read.csv("/Users/iurysalles/Desktop/MBA DATA SCIENCE/TCC/Projeto Final/Dataset/application_record.csv")
behavior <- read.csv("/Users/iurysalles/Desktop/MBA DATA SCIENCE/TCC/Projeto Final/Dataset/credit_record.csv")

#entendendo a estrutura das informações
head(applications)
head(behavior)

#avaliando se tem elemento N/A
applications[is.na(applications) == TRUE]
behavior[is.na(behavior) == TRUE]



#DUMMY COLUNA SEXO
#applications <- applications%>% mutate(M = case_when(CODE_GENDER=="M"~ 1, TRUE ~ 0))

#Dummy nas colunas
applications <- dummy_columns(.data = applications,
                                   select_columns = c("CODE_GENDER","FLAG_OWN_CAR","FLAG_OWN_REALTY","NAME_INCOME_TYPE","NAME_FAMILY_STATUS","NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)


#Renomeando as colunas
colnames(applications)

colnames(applications)[15] <- "NAME_INCOME_TYPE_COMERCIAL_ASSOCIATE"
colnames(applications)[16] <- "NAME_INCOME_TYPE_PENSIONER"
colnames(applications)[17] <- "NAME_INCOME_TYPE_STATE_SERVANT"
colnames(applications)[18] <- "NAME_INCOME_TYPE_STUDENT"
colnames(applications)[19] <- "NAME_FAMILY_STATUS_CIVIL_MARRIAGE"
colnames(applications)[20] <- "NAME_FAMILY_STATUS_SEPARATATED"
colnames(applications)[21] <- "NAME_FAMILY_STATUS_SINGLE"
colnames(applications)[22] <- "NAME_FAMILY_STATUS_WIDOW"
colnames(applications)[23] <- "NAME_EDUCATION_TYPE_ACADEMIC_DEGREE"
colnames(applications)[24] <- "NAME_EDUCATION_TYPE_HIGHER_EDUCATION"
colnames(applications)[25] <- "NAME_EDUCATION_TYPE_INCOMPLETE_HIGHER"
colnames(applications)[26] <- "NAME_EDUCATION_TYPE_LOWER_SECUNDARY"
colnames(applications)[27] <- "NAME_HOUSING_TYPE_COOP_APARTMENT"
colnames(applications)[28] <- "NAME_HOUSING_TYPE_MUNICIPAL_APARTMENT"
colnames(applications)[29] <- "NAME_HOUSING_TYPE_OFFICE_APARTMENT"
colnames(applications)[30] <- "NAME_HOUSING_TYPE_RENTED_APARTMENT"
colnames(applications)[31] <- "NAME_HOUSING_TYPE_WITH_PARENTS"


head(applications)

#distinct(applications,NAME_HOUSING_TYPE,keep_all=FALSE)
#distinct(applications,FLAG_MOBIL,keep_all=FALSE)

#criando classificação de Bom e mal pagador
behavior_status = behavior %>% mutate(bom_pagador = case_when(STATUS=="C"|STATUS=="X"|STATUS==0   ~ 1, TRUE ~ 0))

#Removendo colunas indesejadas
behavior_status$MONTHS_BALANCE = NULL
behavior_status$STATUS = NULL

applications$OCCUPATION_TYPE = NULL
applications$FLAG_MOBIL = NULL

#resumo da base
summary(behavior_status)

nova_base_behavior = behavior_status



head(nova_base_behavior)

#agrupando por ID e entendendo quantos por ID e status de pagador
nova_base_behavior = group_by(nova_base_behavior,ID) %>%  count(bom_pagador)

#entendendo a estrutura da nova base mergeada
head(nova_base_behavior)

#filtrando todos os mal pagadores
bad_borrowers <- nova_base_behavior[nova_base_behavior$bom_pagador == 0,] 

#filtrando todos os bom pagadores que não são mals pagadores
good_borrowers = nova_base_behavior %>%
  filter(!ID %in% bad_borrowers$ID)

#unindo bons pagadores com mals pagadores
final_base = union(bad_borrowers,good_borrowers)

#ordenando por id
final_base_behavior <-final_base[order(final_base$ID),]

#join das bases de application e mal/bom pagador
base_mergeada <- left_join(applications, final_base_behavior,
                            by = "ID")

#verificando se tem N/A
base_mergeada[is.na(base_mergeada) == TRUE]

#retirando os clientes que não tem histórico
#base_final <- base_mergeada %>%  filter(!is.na(cg))
base_final <- na.omit(base_mergeada)  

base_final[is.na(base_final) == TRUE]
base_final_copia <- base_final

#Separando amostra de treino e teste
index = sample(1:nrow(base_final),round(0.70*nrow(base_final)))
train_data <- as.data.frame(base_final[index,])
test_data <- as.data.frame(base_final[-index,])

#árvore
fit_tree <- rpart(bom_pagador~.,method="anova", data=train_data)
tree_predict <- predict(fit_tree,test_data)
mse_tree <- mean((tree_predict - test_data$bom_pagador)^2)

### NeuralNet
set.seed(0)
max_data <- apply(base_final, 2, max) 
min_data <- apply(base_final, 2, min)
scaled <- scale(base_final,center = min_data, scale = max_data - min_data)

index = sample(1:nrow(scaled),round(0.70*nrow(scaled)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])



#nn <- neuralnet(medv~.,data=train_data,hidden=c(5,4,3,2),linear.output=T)
head(train_data)
colnames(train_data)[13] <- "COMERCIAL_ASSOCIATE"

nn <- neuralnet(bom_pagador~.,data=train_data,hidden=c(5,4,3,2),linear.output=T)
plot(nn)

nnpredict <- predict(nn,test_data)
mse_tree <- mean((nnpredict - test_data$cg)^2)

# De forma geral, o dataset já está bastante organizado
# As observações são os jogos que acontecetaram ao longo do campeonato
# As variáveis são as informações pertinentes aos jogos

# Uma variável que não consta na lista é o time vencedor do jogo
# Vamos criar usando o mutate e a função case_when para definir a fórmula
# O case_when funciona como um "se -> então"

jogos <- jogos %>% mutate(time_vencedor = case_when(
  c(jogos$team_home_score-jogos$team_away_score)==0 ~ "empate",
  c(jogos$team_home_score-jogos$team_away_score)>0 ~ "mandante",
  c(jogos$team_home_score-jogos$team_away_score)<0 ~ "visitante")) %>% 
  relocate(time_vencedor, .after = team_away_score)

# Vamos gerar um gráfico para visualizar melhor a informação resultante

ggplot(jogos) + 
  geom_bar(aes(x = time_vencedor)) + 
  labs(x = "Vencedor",
       y = "Contagem") + 
  theme_light()

# Vamos identificar as fases da competição e fazer uma análise mais específica:

fases <- word(jogos$stage, 1)

# Note que, ao pedirmos a primeria palavra, o comando retornou "vazio"
# Ao analisar o dataset, nota-se que existe um espaço antes do texto
# Podemos simplesmente pedir a segunda palavra:

fases <- word(jogos$stage, 2)
print(fases)

# Também poderíamos excluir o espaço e pedir a primeira palavra
# Ambos apresentam o mesmo resultado

ajuste <- sub("^.", "", jogos$stage)
fases_2 <- word(ajuste, 1)
print(fases_2)

# Vamos adicionar a variável ao dataset, mas renomeando as categorias

jogos <- jogos %>% mutate(fases = recode(fases,
                                         "Group"=1,
                                         "Round"=2,
                                         "Quarter-finals"=3,
                                         "Semi-finals"=4,
                                         "Final"=5)) %>% 
  relocate(fases, .after = stage)

# Vamos analisar o gráfico de acordo com as fases da competição:

ggplot(jogos) + 
  geom_bar(aes(x = interaction(time_vencedor, fases), fill=factor(fases))) + 
  labs(x = "Vencedor por Fase",
       y = "Contagem") + 
  scale_fill_brewer(palette=18)

# Uma informação interessante seria identificar os jogadores que fizeram os gols
# Esta informação está na variável "events_list" que é uma string mais complexa
# Precisamos retirar a informação específica, então vamos procurar um padrão
# A informação que queremos está sempre após -- 'Goal', 'action_player_1': ' --

extrai_gol <- str_extract_all(jogos$events_list, 
                              "'Goal', 'action_player_1': '\\w*(.*?)\\w*\\'",
                              simplify = TRUE)

# Acima, utilizamos regex (regular expression), úteis para trabalhar em strings
# Embora não seja nosso foco, é importante conhecer a existência
# O str_extract_all pede para extrair em todas as ocorrências do padrão

# Pedimos para extrair qualquer palavra (\w) contida entre as extremidades:
# Extremidade 1: 'Goal', 'action_player_1': '
# Extremidade 2: ' (só o apóstrofo)
# A seguir, apenas faremos uma limpeza no texto

extrai_gol <- gsub("'Goal', 'action_player_1': ", "", extrai_gol)
extrai_gol <- gsub("'", "", extrai_gol)

# O mesmo critério vamos usar para extrair os gols de pênalti

extrai_penalti <- str_extract_all(jogos$events_list,
                                  "'event_type': 'Penalty', 'action_player_1': '\\w*(.*?)\\w*\\'",
                                  simplify = TRUE)

extrai_penalti <- gsub("'event_type': 'Penalty', 'action_player_1': ", "", extrai_penalti)
extrai_penalti <- gsub("'", "", extrai_penalti)

# Por fim, podemos pedir uma tabela de frequências dos gols

sort(table(cbind(extrai_gol, extrai_penalti)), decreasing = T)

