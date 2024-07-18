# ensemble learning 
# multiplos algoritmos usando varios modelos
# Random Forest -> Varias arvores de decisões


install.packages("randomForest", dependencies = T)
library(randomForest)

dataset <- read.csv(file.choose(),sep=";",header = T)

View(dataset)


### Carregando os conjuntos de Treino e Teste da Maquina1 Gen ###



pnewsTreinoMaq <- read.csv(file="treinoGenMaq.csv", sep=";", header = T)
pnewsTesteMaq <- read.csv(file="testeGenMaq.csv", sep=";", header = T)


View(pnewsTreinoMaq)
View(pnewsTesteMaq)


######################

datasettreinoMaq <- dataset[dataset$id %in% pnewsTreinoMaq$id,]
datasettesteMaq <- dataset[dataset$id %in% pnewsTesteMaq$id,]

View(datasettreinoMaq)
View(datasettesteMaq)

nrow(pnewsTreinoMaq)
nrow(pnewsTesteMaq)
nrow(datasettreinoMaq)
nrow(datasettesteMaq)

######### Iniciando a execucao ##########
#########################################

  
  datasettreinoMaq$rotulo = as.factor(datasettreinoMaq$rotulo)

  datasettesteMaq$rotulo = as.factor(datasettesteMaq$rotulo)

  modelo  = randomForest(rotulo ~ tweetCount + retweetCount + favoriteCount, data = datasettreinoMaq,
                         ntree = 100)

  
  
  previsao = predict(modelo, datasettesteMaq)
  
    
  confusao = table(datasettesteMaq$rotulo,previsao)
  
  
  acuracia = (confusao[1]  + confusao[4])/ sum(confusao)
  
    
  print(confusao)
  print(acuracia)
    

