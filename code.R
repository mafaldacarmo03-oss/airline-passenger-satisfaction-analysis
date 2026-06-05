##### TRABALHO MEDM  #####

#packages necessarios

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(knitr)
library(MASS)
library(cluster)
library(clusterCrit)
library(mclust)
library(pROC)
library(rpart)
library(rpart.plot)
library(caret)
library(class)

# 1. LEITURA DO DATASET

data <- read_excel("airlinepassengersatisfaction_trabalho.xlsx")
View(data)

dim(data)
str(data)

# remover variável id (não tem significado)
data <- data[, -1]

# garantir formato numérico da variável Arrival Delay
data$`Arrival Delay in Minutes` <- as.numeric(as.character(data$`Arrival Delay in Minutes`))


# separar variável resposta
satisfaction_vector <- data$satisfaction
data_s_t <- subset(data, select = -satisfaction)


table(is.na(data_s_t)) #tem 393 valores em falta de 2856967+393=2857360 valores no total
#ou seja, 0.014% dos valores totais estão em falta,
#portanto podemos descartá-los 

# remover NA (percentagem pequena)
complete_idx <- complete.cases(data_s_t)
data_s_t <- data_s_t[complete_idx, ]
satisfaction_vector <- satisfaction_vector[complete_idx]




#Univariate analysis


#variáveis quantitativas contínuas: boxplots
#age: quantitativa discreta (registada em anos)
#foi tratada como variável quantitativa contínua devido ao elevado número de observações e amplitude dos valores


par(mfrow = c(2, 2))

# Age
boxplot(
  data_s_t$Age,
  main = "Age",
  col = "lightgray",
  ylab = "Years"
)

# Flight Distance
boxplot(
  data_s_t$`Flight Distance`,
  main = "Flight Distance",
  col = "lightblue",
  ylab = "Distance"
)

# Departure Delay
boxplot(
  data_s_t$`Departure Delay in Minutes`,
  main = "Departure Delay",
  col = "lightgreen",
  ylab = "Minutes"
)

# Arrival Delay
boxplot(
  data_s_t$`Arrival Delay in Minutes`,
  main = "Arrival Delay",
  col = "lightpink",
  ylab = "Minutes"
)

par(mfrow = c(1,1))


#Estatísticas descritivas das variáveis quantitativas

dados_continuos <- data.frame(
  Age = data_s_t$Age,
  Flight_Distance = data_s_t$`Flight Distance`,
  Departure_Delay = data_s_t$`Departure Delay in Minutes`,
  Arrival_Delay = data_s_t$`Arrival Delay in Minutes`
)


# Estatísticas descritivas
estatisticas <- data.frame(
  Variavel = names(dados_continuos),
  
  Media = sapply(dados_continuos,
                 function(x) mean(x, na.rm = TRUE)),
  
  Mediana = sapply(dados_continuos,
                   function(x) median(x, na.rm = TRUE)),
  
  Minimo = sapply(dados_continuos,
                  function(x) min(x, na.rm = TRUE)),
  
  Q1 = sapply(dados_continuos,
              function(x) quantile(x, 0.25, na.rm = TRUE)),
  
  Q3 = sapply(dados_continuos,
              function(x) quantile(x, 0.75, na.rm = TRUE)),
  
  Maximo = sapply(dados_continuos,
                  function(x) max(x, na.rm = TRUE)),
  
  Desvio_Padrao = sapply(dados_continuos,
                         function(x) sd(x, na.rm = TRUE))
)

# Arredondar valores
estatisticas[, -1] <- round(estatisticas[, -1], 2)

# Mostrar tabela

kable(
  estatisticas,
  caption = "Estatísticas descritivas-dispersao"
)



#Variáveis quantitativas ordinais: Gráficos de barras 


vars_ordinais <- data_s_t %>%
  select(
    `Inflight wifi service`,
    `Seat comfort`,
    `Food and drink`,
    `Departure/Arrival time convenient`,
    `Ease of Online booking`,
    `Gate location`,
    `Online boarding`,
    `Inflight entertainment`,
    `On-board service`,
    `Leg room service`,
    `Baggage handling`,
    `Inflight service`,
    `Cleanliness`,
    `Checkin service`,
  )

vars_long <- vars_ordinais %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variavel",
    values_to = "Valor"
  )


ggplot(vars_long, aes(x = factor(Valor))) +
  geom_bar(fill = "lightblue") +
  facet_wrap(~Variavel) +
  labs(
    title = "Distribuição das Variáveis Ordinais",
    x = "Avaliação",
    y = "Frequência"
  ) +
  theme_minimal()



#variaveis categóricas- gráficos de barras 

par(mfrow = c(2,3))


# Gender
barplot(
  table(data_s_t$Gender),
  col = "lightblue",
  main = "Gender"
)

# Customer Type
barplot(
  table(data_s_t$`Customer Type`),
  col = "lightgreen",
  main = "Customer Type",
  las=2
)

# Type of Travel
barplot(
  table(data_s_t$`Type of Travel`),
  col = "lightpink",
  main = "Type of Travel",
  las=2
)

# Class
barplot(
  table(data_s_t$Class),
  col = "lightyellow",
  main = "Class"
)

#satisfaction
barplot(
  table(satisfaction_vector),
  col = "orange",
  main = "Satisfaction"
)

par(mfrow = c(1,1))



#Bivariate analysis

#matriz de correlação entre variáveis numéricas(não ordinais)



vars_cor <- data_s_t %>%
  select(
    `Age`,
    `Flight Distance`,
    `Departure Delay in Minutes`,
    `Arrival Delay in Minutes`,
  )

cor_matrix <- cor(vars_cor, method="spearman")

corrplot(cor_matrix, 
          method = "color",
          type = "upper", 
          tl.col = "black", 
          tl.srt = 45 )



# Tabelas de contingência:


# Class × Satisfaction

tabela_class <- table(
  data_s_t$Class,
  satisfaction_vector
)

# Proporções por classe

prop.table(tabela_class, margin = 1)


chisq.test(tabela_class)



# Type of Travel × Satisfaction

tabela_travel <- table(
  data_s_t$`Type of Travel`,
  satisfaction_vector
)


# Proporções por tipo de viagem

prop.table(tabela_travel, margin = 1)

chisq.test(tabela_travel)





#Multivariate analysis

# CODIFICAÇÃO DE VARIÁVEIS CATEGÓRICAS

# variáveis binárias → 0/1
data_s_t$Gender <- ifelse(data_s_t$Gender == "Female", 0, 1)
data_s_t$`Customer Type` <- ifelse(data_s_t$`Customer Type` == "Loyal Customer", 0, 1)
data_s_t$`Type of Travel` <- ifelse(data_s_t$`Type of Travel` == "Business travel", 0, 1)

# variável Class (3 categorias) → one-hot encoding
data_s_t$classECO <- ifelse(data_s_t$Class == "Eco", 1, 0)
data_s_t$classECOPLUS <- ifelse(data_s_t$Class == "Eco Plus", 1, 0)
data_s_t$classBUSINESS <- ifelse(data_s_t$Class == "Business", 1, 0)

data_s_t$Class <- NULL




# VERIFICAÇÃO DE CORRELAÇÕES
# ACP é útil quando existem relações lineares consideráveis entre variáveis

cor(data_s_t)


# elevada correlação entre delays → informação redundante
data_s_t$`Arrival Delay in Minutes` <- NULL

# variável com baixa contribuição estrutural no PCA
data_s_t$Gender <- NULL


# APLICAÇÃO DO PCA

# standardização necessária devido a diferentes escalas das variáveis
pca <- prcomp(data_s_t, scale. = TRUE)

# Critério da variância explicada (objetivo ~80%)
summary(pca)

# gráfico do cotovelo (elbow method)
plot(pca)

# critério de Kaiser (eigenvalues > 1)
eigenvalues <- pca$sdev^2
eigenvalues


# decisão: manter 7 componentes principais
# baseada em variância acumulada, elbow e Kaiser


# LOADINGS (INTERPRETAÇÃO DAS COMPONENTES)

loadings <- round(pca$rotation[, 1:7], 3)
threshold <- 0.30

par(mfrow = c(3, 3), mar = c(5, 5, 2, 1))

for (i in 1:7) {
  pc_loadings <- loadings[, i]
  
  bar_colors <- ifelse(abs(pc_loadings) >= threshold, "steelblue", "grey70")
  
  barplot(pc_loadings,
          main = paste("PC", i),
          las = 2,
          col = bar_colors,
          ylim = c(-1, 1))
  
  abline(h = c(-threshold, threshold), col = "red", lty = 2)
}


# PC1 – associado à qualidade global do serviço a bordo
# PC2 – associado à conveniência e serviços digitais
# PC3 – associado ao conforto e serviços complementares
# PC4 – associado ao tipo de viagem e classe
# PC5 – associado ao perfil do passageiro
# PC6 – associado à classe do voo
# PC7 – associado à pontualidade (atrasos)


# ANÁLISE GRÁFICA

scores <- as.data.frame(pca$x)

scores$satisfaction <- factor(
  satisfaction_vector,
  levels = c("neutral or dissatisfied", "satisfied"),
  labels = c("Neutral/Dissatisfied", "Satisfied")
)



# PC1 vs PC2 (principal separação)
ggplot(scores, aes(PC1, PC2, color = satisfaction)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal() +
  labs(title = "ACP - PC1 vs PC2",
       x = "PC1",
       y = "PC2")




                                  ###KMEANS


#devemos ter dados esfericos primeiro
scores = pca$x[,1:7] #escolhemos as 7 componentes principais
scores_sphered <- scores %*% diag(1 / pca$sdev[1:7]) #divide cada peso de cada componente pelo seu desvio padrao


set.seed(1)

# criar amostra para conseguir aplicar índices de validação interna
#caso contrário não são computacionalmente eficientes


idx <- sample(1:nrow(scores_sphered), 10000)
scores_sub <- scores_sphered[idx, ]

par(mfrow=c(1,1))

#para escolher o valor de k, atraves do Total within-cluster sum of squares

wss = numeric(10)

for(i in 1:10){
  wss[i] = kmeans(scores_sub, centers=i, nstart=20)$tot.withinss #soma das distâncias (ao quadrado) de cada ponto ao centro do seu cluster
}

plot(1:10, wss, type="b") 

#escolhemos k=4/5, pois a partir desse momento adicionar clusters nao melhora
#muito mais o modelo (olhando para wss)
#nao escolhemos k=10, que e o que tem menor valor de wss,
#porque isso leva a overfitting, muitos clusters pequenos e com pouca interpretabilidade


ks <- 2:7

d <- dist(scores_sub)

# vetores
ch_vals  <- numeric(length(ks))
bh_vals  <- numeric(length(ks))
sil_vals <- numeric(length(ks))

# loop único
for (i in seq_along(ks)) {
  
  k <- ks[i]
  
  km <- kmeans(scores_sub, centers = k, nstart = 15)
  
  # CH
  ch_vals[i] <- intCriteria(
    as.matrix(scores_sub),
    km$cluster,
    "Calinski_Harabasz"
  )
  
  # BH
  bh_vals[i] <- intCriteria(
    as.matrix(scores_sub),
    km$cluster,
    "Ball_Hall"
  )
  
  # Silhouette
  sil <- silhouette(km$cluster, d)
  sil_vals[i] <- mean(sil[, 3])
}

# melhores k
k_CH  <- ks[which.max(ch_vals)]
k_BH  <- ks[which.min(bh_vals)]
k_SIL <- ks[which.max(sil_vals)]

# plots
par(mfrow = c(1,3))

plot(ks, ch_vals, type="b", pch=19,
     main="Calinski-Harabasz",
     xlab="k", ylab="CH score")
abline(v=k_CH, col="red", lty=2)

plot(ks, bh_vals, type="b", pch=19,
     main="Ball-Hall",
     xlab="k", ylab="BH score")
abline(v=k_BH, col="red", lty=2)

plot(ks, sil_vals, type="b", pch=19,
     main="Silhouette",
     xlab="k", ylab="Average silhouette width")
abline(v=k_SIL, col="red", lty=2)

# resultados
cat("CH escolheu K =", k_CH, "\n")           #deu 4
cat("BH escolheu K =", k_BH, "\n")           #deu 6
cat("Silhouette escolheu K =", k_SIL, "\n")  #deu 2


#decidimos escolher k=4


# modelo final aplicado ao conjunto completo de observações
set.seed(123)
km = kmeans(scores_sphered, centers = 4, nstart = 20, iter.max = 100) #nstart define quantas vezes tentamos encontrar a melhor solução
km


# validação externa complementar através do ARI
# objetivo: avaliar concordância entre clusters e variável de satisfação


library(mclust)

true_labels <- satisfaction_vector

ari_4 <- adjustedRandIndex(km$cluster, true_labels)

c(ari_4)





                            ###CLUSTERING HIERARQUICO
#nao usar dados esfericos aqui- apenas os scores
#usar pcs, reduz redundância, elimina correlações fortes e simplifica a 
#estrutura dos dados

set.seed(1)
idx <- sample(1:nrow(scores[, 1:7]), 10000)
scores_sub <- scores[idx, 1:7]
dist_matrix = dist(scores_sub) #distancia euclidiana

#comparar vários linkages

hc_ward = hclust(dist_matrix, method = "ward.D2") 
hc_average = hclust(dist_matrix, method = "average") 
hc_complete = hclust(dist_matrix, method = "complete") 
hc_single = hclust(dist_matrix, method = "single") 


par(mfrow = c(2,2))
plot(hc_ward, labels = FALSE, 
     main = paste("Dendrograma - Ward linkage"))

plot(hc_average, labels = FALSE, 
     main = paste("Dendrograma - Average linkage"))


plot(hc_complete, labels = FALSE, 
     main = paste("Dendrograma - Complete linkage"))


plot(hc_single, labels = FALSE,
     main = paste("Dendrograma - Single linkage"))

#ward.D2 revelou-se o mais consistente


#utilizando índices internos  

set.seed(1)

idx <- sample(1:nrow(scores[,1:7]), 10000)
scores_sub <- scores[idx,1:7 ]

d <- dist(scores_sub)

hc <- hclust(d, method = "ward.D2")

ks <- 2:7

sil_scores   <- numeric(length(ks))
db_scores    <- numeric(length(ks))
gamma_scores <- numeric(length(ks))

for (i in seq_along(ks)) {
  
  clusters <- cutree(hc, k = ks[i])
  
  # Silhouette (maior = melhor)
  sil_scores[i] <- mean(silhouette(clusters, d)[,3])
  
  # Davies-Bouldin (menor = melhor)
  db_scores[i] <- intCriteria(
    as.matrix(scores_sub),
    as.integer(clusters),
    "Davies_Bouldin"
  )$davies_bouldin
  
  # Gamma index (maior = melhor)
  gamma_scores[i] <- intCriteria(
    as.matrix(scores_sub),
    as.integer(clusters),
    "Gamma"
  )$gamma
}

# melhores K
k_sil   <- ks[which.max(sil_scores)]
k_db    <- ks[which.min(db_scores)]
k_gamma <- ks[which.max(gamma_scores)]

# plots
par(mfrow=c(1,3))

plot(ks, sil_scores, type="b", pch=19,
     main="Silhouette (↑ melhor)")
abline(v = k_sil, col="red", lty=2)

plot(ks, db_scores, type="b", pch=19,
     main="Davies-Bouldin (↓ melhor)")
abline(v = k_db, col="red", lty=2)

plot(ks, gamma_scores, type="b", pch=19,
     main="Gamma Index (↑ melhor)")
abline(v = k_gamma, col="red", lty=2)

# resultados
cat("Silhouette escolheu K =", k_sil, "\n")           # 5
cat("Davies-Bouldin escolheu K =", k_db, "\n")        # 7
cat("Gamma escolheu K =", k_gamma, "\n")              # 5



#distribuição

hc <- hc_ward

grupos_ward5 <- cutree(hc, k = 5)
grupos_ward7 <- cutree(hc, k = 7)


table(grupos_ward5)
table(grupos_ward7)


#visualização dos clusters no dendograma

par(mfrow=c(1,1))
plot(hc_ward, labels = FALSE, 
     main = paste("Dendrograma - Ward linkage"))


rect.hclust(hc, k=5, border="red")


#utilizando indices externos


# labels reais da amostra
true_labels_sub <- true_labels[idx]

# clustering
d <- dist(scores_sub)
hc <- hclust(d, method = "ward.D2")

# clusters
clust5 <- cutree(hc, k = 5)
clust7 <- cutree(hc, k = 7)

# ARI
ari_5 <- adjustedRandIndex(clust5, true_labels_sub)
ari_7 <- adjustedRandIndex(clust7, true_labels_sub)

# output
cat("ARI para K=5 :", ari_5, "\n")   
cat("ARI para K=7 :", ari_7, "\n")   
#valores pequenos




                               #MODEL BASED CLUSTERING

#usando as variáveis originais-apenas as continuas


# selecionar variáveis contínuas
vars_cont <- data[, c(
  "Age",
  "Flight Distance",
  "Departure Delay in Minutes",
  "Arrival Delay in Minutes"
)]

# remover linhas com NA
vars_cont <- na.omit(vars_cont)


# ajustar modelo GMM
gmm_model <- Mclust(vars_cont)

# resumo
summary(gmm_model)


#sem escalar dá 9- modelo VEV
#escalando dá 9- modelo VEV tbm



#usando os scores do pca


gmm <- Mclust(scores[,1:7])


summary(gmm) # o número de clusters escolhido foi 9 e o modelo VVV



gmm2<-Mclust(scale(scores[,1:7]))
summary(gmm2) 


#sem escalar dá 9-modelo VVV
#escalando dá 9- modelo VVV


#critério bic
plot(gmm, what = "BIC")


# número de observações por cluster
table(gmm$classification)

# proporções
prop.table(table(gmm$classification))


#Relação com satisfaction
tab_gmm <- prop.table(
  table(gmm$classification, satisfaction_vector),
  margin = 1
)
round(tab_gmm, 3)


#SUPERVISIONADAS

                                    #Analise Discriminante


#LDA
dim(data_s_t)
sample_id = sample(1:dim(data_s_t), 1000)
satisfaction_vector_01 = ifelse(satisfaction_vector == "satisfied", 1, 0)
pairs(data_s_t[sample_id, 1:11], col = satisfaction_vector_01 + 1)
pairs(data_s_t[sample_id, 12:22], col = satisfaction_vector_01 + 1)
#não conseguimos ver nenhum par de variáveis com uma separação de grupos
#muitas observações sobrepostas pois temos muitas variaveis binarias e discretas


set.seed(123)
idx <- sample(1:nrow(scores), 0.7*nrow(scores))
train <- data_s_t[idx, ]
test  <- data_s_t[-idx, ]
train_y <- satisfaction_vector_01[idx]
test_y  <- satisfaction_vector_01[-idx]

lda_model <- lda(as.factor(train_y) ~ . ,
                 data = train)
lda_pred <- predict(lda_model, test)$class

confusion_table = table(Real = test_y, Previsto = lda_pred)
confusion_table
accuracy <- sum(diag(confusion_table)) / sum(confusion_table)
accuracy #87%

#agora com PCs
scores <- as.data.frame(pca$x)
pairs(scores[sample_id, 1:7], col = satisfaction_vector_01 + 1)
#nao conseguimos ver clara distincao de satisfeitos com insatisfeitos
#talvez por causa da natureza dos dados, que sao subjetivos (sao opinioes e gostos 
#pessoais), parece haver uma maior distincao entre PC4 E PC6

set.seed(123)
idx <- sample(1:nrow(scores), 0.7*nrow(scores))
train_scores <- scores[idx, ]
test_scores  <- scores[-idx, ]
train_y <- satisfaction_vector_01[idx]
test_y  <- satisfaction_vector_01[-idx]

lda_model <- lda(as.factor(train_y) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7,
                 data = train_scores)

lda_pred <- predict(lda_model, test_scores)$class

confusion_table = table(Real = test_y, Previsto = lda_pred)
confusion_table
accuracy <- sum(diag(confusion_table)) / sum(confusion_table)
accuracy #83%

#visualizacao
x <- seq(min(scores$PC6), max(scores$PC6), length = 200) #do grafico PC4 vs PC6
y <- seq(min(scores$PC4), max(scores$PC4), length = 200)

grid <- expand.grid(PC6 = x, PC4 = y) 

lda_2d <- lda(as.factor(satisfaction_vector_01) ~ PC4 + PC6, data = scores)
grid_pred <- predict(lda_2d, grid)$class
plot(scores$PC6,
     scores$PC4,
     col = satisfaction_vector_01 + 1,
     pch = 19,
     xlab = "PC6",
     ylab = "PC4",
     main = "LDA com PC6 e PC4")
contour(x,
        y,
        matrix(as.numeric(grid_pred), 200, 200),
        levels = 1.5,
        add = TRUE,
        drawlabels = FALSE,
        lwd = 2)

#Apesar de não existir separação visual clara entre pares de componentes principais,
#o LDA conseguiu encontrar uma combinação linear das 7 PCs que 
#separa razoavelmente os passageiros satisfeitos dos insatisfeitos.
#Isso sugere que a separação existe num espaço multidimensional, 
#mesmo não sendo visível em projeções bidimensionais.



#QDA
set.seed(123)
idx <- sample(1:nrow(data_s_t), 0.7*nrow(data_s_t))
train <- data_s_t[idx, ]
test  <- data_s_t[-idx, ]
train_y <- satisfaction_vector_01[idx]
test_y  <- satisfaction_vector_01[-idx]

qda_model <- qda(as.factor(train_y) ~ . ,
                 data = train)
#Deu erro: Error in qda.default(x, grouping, ...) : rank deficiency in group 0
#Significa que não consegue estimar/inverter a matriz de covariancia da classe 0
#Possivel problema: multicolineariedade
#Solução: usar PCA

set.seed(123)
idx <- sample(1:nrow(scores), 0.7*nrow(scores))
train_scores <- scores[idx, ]
test_scores  <- scores[-idx, ]
train_y <- satisfaction_vector_01[idx]
test_y  <- satisfaction_vector_01[-idx]

qda_model <- qda(as.factor(train_y) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7,
                 data = train_scores)
qda_pred <- predict(qda_model, test_scores)$class
confusion_table = table(Real = test_y, Previsto = qda_pred)
confusion_table
accuracy <- sum(diag(confusion_table)) / sum(confusion_table)
accuracy #83%
#QDA não acrescentou capacidade discriminativa relativamente à LDA
#as classes parecem aproximadamente linearmente separáveis no espaço das PCs

#visualizacao
x <- seq(min(scores$PC6), max(scores$PC6), length = 200) 
y <- seq(min(scores$PC4), max(scores$PC4), length = 200)

grid <- expand.grid(PC6 = x, PC4 = y) 

qda_2d <- qda(as.factor(satisfaction_vector_01) ~ PC4 + PC6, data = scores)
grid_pred <- predict(qda_2d, grid)$class
plot(scores$PC6,
     scores$PC4,
     col = satisfaction_vector_01 + 1,
     pch = 19,
     xlab = "PC6",
     ylab = "PC4",
     main = "LDA com PC6 e PC4")
contour(x,
        y,
        matrix(as.numeric(grid_pred), 200, 200),
        levels = 1.5,
        add = TRUE,
        drawlabels = FALSE,
        lwd = 5,
        col = "yellow")

#Conclusão do dataset:
#A separação entre satisfeitos e insatisfeitos não ocorre de forma simples 
#em pares de variáveis, existe num espaço multidimensional.
#As classes parecem ser aproximadamente linearmente separáveis.





                                 #ARVORES DE DECISAO

tree_data <- data_s_t
tree_data$satisfaction <- as.factor(satisfaction_vector)

set.seed(123)
idx <- sample(1:nrow(tree_data),
              0.7*nrow(tree_data))
train <- tree_data[idx, ]
test  <- tree_data[-idx, ]

tree_model <- rpart(satisfaction ~ .,
                    data = train,
                    method = "class")

#visualizar #temos referencia
rpart.plot(tree_model)
printcp(tree_model)
#xerror (erro estimado por validação cruzada) mais pequeno esta na ultima linha, 
#portanto a arvore ja e otima
#O algoritmo rpart já aplica um critério de complexidade (cp) 
#durante o crescimento da árvore. Por isso, a árvore inicial já estava
#relativamente otimizada e o pruning posterior não alterou significativamente 
#a estrutura (nós experimentamos podar a arvore para ver o que acontecia).

#A árvore final ficou relativamente simples, com poucos nós de decisão, 
#o que sugere que algumas variáveis de serviço conseguem explicar grande parte
#da satisfação dos passageiros: Online Boarding, Inflight Wifi Service 
#e Type Of Travel.

#prever
tree_pred <- predict(tree_model,
                     test,
                     type = "class")
confusion_table <- table(Real = test$satisfaction,
                         Previsto = tree_pred)
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #89%

#agora com PCA
tree_data_pca <- scores[,1:7]
tree_data_pca$satisfaction <- as.factor(satisfaction_vector)

set.seed(123)
idx <- sample(1:nrow(tree_data_pca),
              0.7*nrow(tree_data_pca))
train <- tree_data_pca[idx, ]
test  <- tree_data_pca[-idx, ]

tree_model_pca <- rpart(satisfaction ~ .,
                    data = train,
                    method = "class")
#Ao utilizar apenas as primeiras componentes principais, 
#a árvore de decisão tornou-se extremamente simples, indicando 
#que a PCA conseguiu condensar a informação mais relevante para 
#a classificação da satisfação em poucas dimensões. No entanto, 
#perdeu-se interpretabilidade direta, porque os nós passam a 
#representar componentes principais em vez de variáveis concretas do serviço.

#visualizar
rpart.plot(tree_model_pca)
printcp(tree_model_pca)

#prever
tree_pred_pca <- predict(tree_model_pca,
                     test,
                     type = "class")
confusion_table <- table(Real = test$satisfaction,
                         Previsto = tree_pred_pca)
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #82%
#Nao fez sentido aplicar com PCA. Perdemos muita interpretabilidade e informação.

#Conclusões do dataset: Algumas variáveis de serviço conseguem explicar grande
#parte da satisfação dos passageiros: Online Boarding, Inflight Wifi Service 
#e Type Of Travel.


                              #LOGISTIC REGRESSION

#nao podemos aplicar multinomial regression, pois a target so tem 2 categorias

lr_data = data_s_t
set.seed(123)
idx <- sample(1:nrow(lr_data),
              0.7*nrow(lr_data))
train <- lr_data[idx, ]
test  <- lr_data[-idx, ]
gl = glm(satisfaction_vector_01[idx]~., data=train,family=binomial)
proba.train = predict(gl,newdata=train,type="response")
proba.test = predict(gl,newdata=test,type="response")
predicted_train = as.numeric(proba.train > 0.5)
predicted_test = as.numeric(proba.test > 0.5)
confusion_table = table(predicted_train, satisfaction_vector_01[idx])
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #87%
confusion_table = table(predicted_test, satisfaction_vector_01[-idx])
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #88
summary(gl) #Conseguimos perceber quais são as variaveis mais significativas 
            #para a classificação e a direção


roc_curve = roc(satisfaction_vector_01[-idx], proba.test)
plot(roc_curve, col = "blue", main = "ROC Curve", print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "red")
#Temos que AUC=0.928 está muito perto de 1, portanto o modelo está muito bem ajustado 
#aos dados

#Aplicando PCA:
lr_data_pca = scores[,1:7]
set.seed(123)
idx <- sample(1:nrow(lr_data_pca),
              0.7*nrow(lr_data_pca))
train <- lr_data_pca[idx, ]
test  <- lr_data_pca[-idx, ]

gl = glm(satisfaction_vector_01[idx]~., data=train,family=binomial)
proba.train = predict(gl,newdata=train,type="response")
proba.test = predict(gl,newdata=test,type="response")
predicted_train = as.numeric(proba.train > 0.5)
predicted_test = as.numeric(proba.test > 0.5)

confusion_table = table(predicted_train, satisfaction_vector_01[idx])
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #84%
confusion_table = table(predicted_test, satisfaction_vector_01[-idx])
confusion_table
accuracy <- sum(diag(confusion_table))/sum(confusion_table)
accuracy #84%
summary(gl)

roc_curve = roc(satisfaction_vector_01[-idx], proba.test)
plot(roc_curve, col = "blue", main = "ROC Curve", print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "red")
#AUC=0.905. Usar a PCA não melhorou nada.

#Conclusão do dataset:
#As variáveis menos significativas para a classificação são:
#.Flight Distance, que contribui negativamente para a satisfação, p-value=0.08
#.Gate Location, que contribui positivamente, p-value=0.02
#A componente principal menos significativa é:
#. PC5, associado ao perfil do passageiro, p-value= 0.06


                                      #KNN
#temos referencia para o codigo usado
#temos referencia para a conclusao

sample_id = sample(1:nrow(data_s_t), 10000)
knn_data = data_s_t[sample_id,] #vamos usar uma amostra porque knn
                                #é muito dispendisoso computacionalmente
set.seed(123)
idx <- sample(1:nrow(knn_data),
              0.7*nrow(knn_data))
train <- knn_data[idx, ]
test  <- knn_data[-idx, ]

mod_knn = train(x = train,
                y = satisfaction_vector[sample_id][idx],
                method = "knn", #aplicamos knn
                preProcess = c("center", "scale"), #dados centrados e normalizados - estandardização completa dos dados
                #temos de normalizar as variáveis, pois tem escalas diferentes de valores
                #e o método dos KNN é um método que usa a distância entre os dados para avaliar
                #a classe de cada dado, portanto é importante normaliza-los
                
                tuneGrid = data.frame(k = c(1:15)), #valores de K que vamos experimentar
                trControl = trainControl(method = "repeatedcv", #repeated cross-validation
                                         number = 5, #number de folds
                                         repeats = 2) #how many times the k-fold split must be repeated
)
mod_knn
#o melhor valor de K foi 3 e obtivemos 91% de accuracy

pred_cls = predict(mod_knn, test)
ct = table(Actl = satisfaction_vector[sample_id][-idx], Pred = pred_cls)
ct
accuracy = sum(diag(ct))/sum(ct)
accuracy #92% 

#Vamos experimentar aplicar PCA para ver se melhora

sample_id = sample(1:nrow(scores), 10000)
knn_pca = scores[sample_id, 1:7] 
set.seed(123)
idx <- sample(1:nrow(knn_pca),
              0.7*nrow(knn_pca))
train <- knn_pca[idx, ]
test  <- knn_pca[-idx, ]
mod_knn_pca = train(x = train,
                y = scores$satisfaction[sample_id][idx],
                method = "knn", 
                preProcess = c("center", "scale"), 
                tuneGrid = data.frame(k = c(1:15)), 
                trControl = trainControl(method = "repeatedcv", #repeated cross-validation
                                         number = 5, #number de folds
                                         repeats = 2) #how many times the k-fold split must be repeated
)
mod_knn_pca
#melhor valor de K=7, com accurary=90%
pred_cls = predict(mod_knn_pca, test)
ct = table(Actl = scores$satisfaction[sample_id][-idx], Pred = pred_cls)
ct
accuracy = sum(diag(ct))/sum(ct)
accuracy #90%

#A aplicação de PCA antes do KNN não conduziu a melhoria da accuracy, 
#mantendo-se valores próximos de 90%. Este resultado sugere que as variáveis 
#originais já continham informação discriminativa relevante e relativamente 
#pouco ruído. Embora a PCA tenha reduzido dimensionalidade e eliminado correlações,
#também poderá ter removido informação útil para classificação, 
#uma vez que o método maximiza variância total e não separação entre classes. 
#Assim, neste conjunto de dados, o KNN apresentou desempenho semelhante utilizando 
#tanto as variáveis originais como as componentes principais.

#Conclusão no dataset: as variáveis do dataset fazem uma boa divisão entre classes. 