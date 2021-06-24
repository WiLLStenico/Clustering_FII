
########################################
#
#   Ativando Libs
#
########################################

library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl) #trabalhar com xls
library(PerformanceAnalytics) #para grafico de correlacoes
library(plotly) #ggplot

################################################################################

#Dados obtidos em 21/06/2021 em: https://fiis.com.br/lupa-de-fiis/ 

#Tratado diretamente no .csv os separadores decimais e de milhar

################################################################################

#LEITURA DOS DADOS
fiis <- read.table("dados/FIIs.csv", sep = ";", header = T, dec = ",")
head(fiis)

#Definindo Ticker como ID de nosso Dataset
rownames(fiis) <- fiis[,1]
fiis <- fiis[,-1]
head(fiis)



################################################################################

#Criando um dataset contendo apenas váriáveis QUANTITATIVAS.
#Lembrando que a clusterização é o agrupamento de váriáveis baseado em sua distância vetorial.
#Qualquer calculo numérico utilizando váriáveis que não sejam quantitativas, ou seja QUALITATIVAS, 
#é considerada uma ponderação arbitrária, com excessão as variáveis binarias. 

################################################################################

#Entendendo os tipos de dados do nosso dataset
str(fiis)

################################################################################
#Aqui estamos considerando:
#     Ticker	| Último Rend. (%) |	Rend. Méd. 12m (%) |	Patrimônio/Cota |	Cotação/VP |	Nº negócios/mês |	Número Cotistas |	Patrimônio
################################################################################

fiis.quantitativos <- fiis[,c(5,	9,	10,	11,	12,	14, 15)]

str(fiis.quantitativos)
head(fiis.quantitativos)

#Criação de grafico de correlação entre as váriaveis. Lembrando que quanto menos váriáveis, melhor para nosso modelo.
chart.Correlation(fiis.quantitativos[, ], histogram = T)

#Analisando as correlações
fiis.quantitativos.cor <- cor(fiis.quantitativos)
fiis.quantitativos.cor

################################################################################
#Conforme demonstrado o numero de negocios X quantidade de cotistas são duas váriaveis super correlacionadas, o que faz total sentido. Para otimizarmos nosso algoritmo, 
#vamos remover a quantidade de cotistas ficando então com:

#     Ticker	| Último Rend. (%) |	Rend. Méd. 12m (%) |	Patrimônio/Cota |	Cotação/VP |	Nº negócios/mês |	Número Cotistas |	Patrimônio
################################################################################

fiis.quantitativos <- fiis.quantitativos [,c(1,	2,	3,	4,	5,7)]
#str(fiis.quantitativos)
chart.Correlation(fiis.quantitativos[, ], histogram = T)


# #Visualizando a distribuição dos dados
# rend_medio_12m_perc.hist <- ggplot(data = fiis.quantitativos) +
#   geom_histogram(aes(x = fiis.quantitativos$rend_medio_12m_perc))
# 
# ultimo_rendimento_perc.hist <- ggplot(data = fiis.quantitativos) +
#   geom_histogram(aes(x = fiis.quantitativos$ultimo_rendimento_perc))
# 
# cotacao_por_vp.hist <- ggplot(data = fiis.quantitativos) +
#   geom_histogram(aes(x = fiis.quantitativos$cotacao_por_vp))
# 
# Patrimonio.hist <- ggplot(data = fiis.quantitativos) +
#   geom_histogram(aes(x = fiis.quantitativos$Patrimonio))
# 
# patrimonio_por_cota.hist <- ggplot(data = fiis.quantitativos) +
#   geom_histogram(aes(x = fiis.quantitativos$patrimonio_por_cota))
# 
# #Imprimir graficos na mesma tela
# grid.arrange(rend_medio_12m_perc.hist, ultimo_rendimento_perc.hist, cotacao_por_vp.hist, Patrimonio.hist, patrimonio_por_cota.hist , nrow = 2)


#############################
#Padronizando variaveis
#############################
fiis.pad <- scale(fiis.quantitativos)
fiis.pad[is.na(fiis.pad)] <- 0

head(fiis.pad)
chart.Correlation(fiis.pad, histogram = T)


################################################################################
# Clusterização Hierárquica
################################################################################


#VERIFICANDO ELBOW: Objetivo é identificar a quantidade de grupos em que a variancia dentro de cada grupo comece a diminuir.
# Lembrando que quanto mais grupos, menor será a variancia.
fviz_nbclust(fiis.pad, FUN = hcut, method = "wss") 

# Pelo método "silhouette" 2 parece um bom número, porém nosso objetivo é criar grupos menores para facilitar nossa busca por fiis
# 6 também parece uma boa escolha...
fviz_nbclust(fiis.pad, FUN = hcut, method = "silhouette")


CLUSTERS <- 7 # Vamos iniciar com 7 e ver o que acontece....

#CALCULANDO MATRIZ DE DISTANCIAS 
distancias <- dist(fiis.pad, method = "euclidean")


#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"
# Aqui utilizaremos o método "single"(vizinho mais próximo) para geração dos Clusters
hc1 <- hclust(distancias, method = "single" )


#DESENHANDO O DENDOGRAMA
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = CLUSTERS)


#criando grupos 
fiis_clust <- cutree(hc1, k = CLUSTERS)

# RESULTADO: Infelizmente o este primeiro resultado não foi muito útil para nossa 
# analise, onde tivemos 1 grupo com 301 amostras 1 com 4 e 5 grupos com apenas 1 FII.
# Será que podemos fazer algo para melhorar?
table(fiis_clust)


################################################################################
# 
# DESISTIR É PARA OS FRACOS....
#
################################################################################

# Aqui podemos tomar algumas decisões, por exemplo, reduzir a quantidade de váriáveis
# analisadas através de suas correlações, aumentar o número de clusters, alterar o algoritmo de agrupamento para Não-Hierarquico, etc...
# 
# Outra opção, que tentaremos aqui será alterar o método de agrupamento de SINGLE(vizinho mais próximo) para o WARD.
# 
# Lembrando: nós tivemos um problema com a quantidade de amostras dentro de cada agrupamento, sendo que a grande maioria ficou no grupo 1.
# O método WARD, baseado em analise de variâncias, tende a criar agrupamentos com as mesmas quantidades de amostras. Parece que o WARD se encaixa 
# perfeitamente no nosso problema...


CLUSTERS <- 7

#metodos disponiveis "average", "single", "complete" e "ward.D"
hc2 <- hclust(distancias, method = "ward.D" ) #Esta é a unica linha que precisamos alterar...


#DESENHANDO O DENDOGRAMA
plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = CLUSTERS)

#COMPARANDO DENDOGRAMAS
#comparando o metodo single com ward
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list <- dendlist(dend1, dend2) 
#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list),2)))


#criando grupos 
fiis_clust <- cutree(hc2, k = CLUSTERS)

################################################################################
# NOVO RESULTADO: Apesar de não alcançarmos grupos tão homogeneos em quantidade, 
# podemos considerar que, de fato, temos algum agrupamento.
################################################################################
table(fiis_clust)

################################################################################
# Observe os grupos 5,6 e 7. Estes grupos estão com poucas amostras...
# Lembra da nossa analise com Elbow e Silhouette? 
# -Elbow sugeriu 7 amostras
# -Silhouette 2 ou 6
# 
# O que acontece se mudarmos a quantidade de Clusters para 6?
################################################################################

CLUSTERS <- 6 #Alterado para 6

#metodos disponiveis "average", "single", "complete" e "ward.D"
hc2 <- hclust(distancias, method = "ward.D" ) 


#DESENHANDO O DENDOGRAMA
plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = CLUSTERS)


#criando grupos 
fiis_clust <- cutree(hc2, k = CLUSTERS)

#Resultado: As amostras que estavam no grupo 6 e 7 foram agrupadas... Por ser uma quantidade muito pequena, irei mantê-los agrupados.
table(fiis_clust)

#transformando em data frame a saida do cluster
fiis_clust_df <- data.frame(fiis_clust)

#juntando com a base original
fiis_fim <- cbind(fiis, fiis_clust_df)

######################################
#Conclusão: O Grupo 3 possui 3 FIIs que eu já tenho em carteira, oq faz com que eu de foco em analisar os outros 16 restantes...
######################################

# Uma forma de tentar entender os grupos é calcular a média das váriaveis quantitativas de cada grupo

#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo
mediagrupo_Fiis <- fiis_fim %>% 
  group_by(fiis_clust) %>% 
  summarise(n = n(),
            ultimo_rendimento_perc = mean(ultimo_rendimento_perc), 
            rend_medio_12m_perc = mean(rend_medio_12m_perc),
            cotacao_por_vp  = mean(cotacao_por_vp), 
            patrimonio_por_cota    = mean(patrimonio_por_cota ),   
            Patrimonio  = mean(Patrimonio), 
            nro_negocios_por_mes  = mean(nro_negocios_por_mes)
            )
mediagrupo_Fiis


#Caso quiséssemos salvar o nosso data frame 'dados' em formato *.csv:
#write.csv(fiis_fim, file = "dados/dados_FIM.csv", row.names = TRUE)

##################################################################################################################################################################
#Executando estudo com modelo não hierarquico
##################################################################################################################################################################

####################
#Utilizando K-Means
####################


#VERIFICANDO ELBOW 
fviz_nbclust(fiis.pad, kmeans, method = "wss")

#Rodar o modelo
fiis.k2 <- kmeans(fiis.pad , centers = 2)
fiis.k4 <- kmeans(fiis.pad , centers = 4)
fiis.k6 <- kmeans(fiis.pad , centers = 6)
fiis.k7 <- kmeans(fiis.pad , centers = 7)

#Criar graficos
G1 <- fviz_cluster(fiis.k2, geom = "point", data = fiis.pad) + ggtitle("k = 2")
G2 <- fviz_cluster(fiis.k4, geom = "point", data = fiis.pad) + ggtitle("k = 4")
G3 <- fviz_cluster(fiis.k6, geom = "point",  data = fiis.pad) + ggtitle("k = 6")
G4 <- fviz_cluster(fiis.k7, geom = "point",  data = fiis.pad) + ggtitle("k = 7")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#Juntando os Dados
fiis.fit <- data.frame(fiis.k6$cluster)

#Agrupar cluster e base
fiis_fim  <-  cbind(fiis_fim, fiis.fit)

#Caso quiséssemos salvar o nosso data frame 'dados' em formato *.csv:
write.csv(fiis_fim, file = "dados/dados_FIM.csv", row.names = TRUE)






### método dbscan

#Calcular o Cluster
dbscan <- fpc::dbscan(fiis.pad,eps = 0.25, MinPts = 3)

fiis_fim$dbscan <- dbscan$cluster

#visualizando em cores os clusters
fiis_fim %>% ggplot() +
  geom_point(aes(x = rend_medio_12m_perc,
                 y = patrimonio_por_cota,
                 color = as.factor(dbscan)),
             size = 3)



#Agrupar cluster e base
fiis_fim  <-  cbind(fiis_fim, fiis.fit)

#Caso quiséssemos salvar o nosso data frame 'dados' em formato *.csv:
write.csv(fiis_fim, file = "dados/dados_FIM.csv", row.names = TRUE)





