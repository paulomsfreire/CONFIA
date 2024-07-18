
#################################
# Classificação com Naive bayes
#################################

install.packages("e1071", dependencies = T)
library(e1071)

# Conjunto de dados chamado credito.


credito <-  read.csv(file.choose(), sep=",", header = T, stringsAsFactors = TRUE)
dim(credito)

# Exibir os dados do credito
View(credito)

unique(credito$class)

# Dividir os dados historicos em dois grupos 
# Grupo de treino e o grupo de teste
# Amostras -> 70%, 30% Holdout Validação cruzada 100 10/10 3-1 2
 
amostra = sample(2, 1000, replace = T, prob=c(0.7, 0.3))
amostra


# Dividindo em dois conjunto de creditos
# treino, teste
creditotreino = credito[amostra ==1, ]
creditoteste = credito[amostra ==2, ]

# analisar a dimensão
dim(creditotreino)
dim(creditoteste)

# Criação do modelo em relação ao vetor de treinamento de credito
# Usa probabilidade e só faz Classificação
# função naiveBayes () -> 
help("naiveBayes")

# 1 -> Formula 
# 2 -> conjunto de treinamento

modelo <-  naiveBayes(class ~., creditotreino)

# Modelo de um objeto do R
# Pode Gravar o modelo
modelo

View(creditoteste)

# Previsão

predicao = predict(modelo, creditoteste)
predicao

# Usar tabela de confusão para medir a eficiencia do 
# algoritmo

confusao <-  table(creditoteste$class, predicao)
confusao

acuracia  = (confusao[1] + confusao[4]) / sum(confusao)
acuracia



################################################
# Classificação com SVM
################################################

install.packages("e1071", dependencies = T)
library(e1071)

# SVM espera receber um factor com parâmetro para classificação e numeric para regressão
# a partir do R 4.0 é preciso usar stringsAsFactors = TRUE para que read.csv leia 
# string como factor
# Fontes: https://community.rstudio.com/t/problems-when-creating-svm-model-with-linear-kernel/64975/4
# https://sparkbyexamples.com/r-programming/r-read-csv-file-with-examples/
#help("svm")

dim(creditotreino)
dim(creditoteste)

modelo1 <-  svm(class ~.,
                data = creditotreino)

modelo2 <-  svm(class ~ checking_status + duration + purpose + credit_history,
               data = creditotreino)



predicao = predict(modelo1, creditoteste)
predicao

predicao = predict(modelo2, creditoteste)

confusao = table(creditoteste$class, predicao)
confusao

acuracia = (confusao[1] + confusao[4]) / sum(confusao)

acuracia





