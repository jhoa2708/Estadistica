library(readxl )
Datos<- read_excel("D:/TESIS_rev/TESIS_DATA/Datos_estadistica_ordenados_1.xlsx")
#Convierto datos en dataframe
Datos <- as.data.frame(Datos)
Datos <- data.frame(Datos[,-1], row.names=Datos[,1])
#################################
#Creo las variables iniciales
#Datos concentraciones totales
Datos_CT<-Datos[,c(1:7,12,17,22,27,32,37,42,47,52,53)]
#Concentraciones totales solo numeros
Datos_Num <-Datos[,c(1:51)]
#Concentraciones totales solo numeros
Datos_Num_CT<-Datos_Num[,c(1:7,12,17,22,27,32,37,42,47)]
#Temporada seca  solo numeros
Datos_Num_TS<-Datos_Num[c(0:10),]
#Temporada humeda  solo numeros
Datos_Num_TH<-Datos_Num[c(11:20),]
#################################
#Correlacion de datos(Solo se puede usar datos numericos)
#Todos los datos
Corr<-round(cor(Datos_Num),2)
#Temporada seca
Corr_TS<-round(cor(Datos_Num_TS),2)
#Temporada humeda
Corr_TH<-round(cor(Datos_Num_TH),2)
#Concentraciones totales
Corr_CT<-round(cor(Datos_Num_CT),2)
# Solo triangulo superior para concentraciones totales
Upper_Corr_CT<-Corr_CT
Upper_Corr_CT[lower.tri(Upper_Corr_CT)]<-""
Upper_Corr_CT<-as.data.frame(Upper_Corr_CT)
print(Upper_Corr_CT)
#De ahora en adelante se usará los valores para la correlacion de concentraciones totales
#Tabla de valores de correlacion y pearson
library("Hmisc")
Corr_CT_1<- rcorr(as.matrix(Datos_Num_CT))
# flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(Corr_CT_1$r, Corr_CT_1$P)
#Tabla con codificacion simbolica
symnum(Corr_CT, abbr.colnames = FALSE)
library(corrplot)
library(GGally)
library(qgraph)
# Create a new pdf device
pdf("Correlacion_todo.pdf") 
#Grafica sin numeros
corrplot(Corr_CT,type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
#Grafica con numeros y mejor dividida
# Insignificant correlations are leaved blank
corrplot(Corr_CT_1$r, type="upper", order="hclust", 
         p.mat = Corr_CT_1$P, sig.level = 0.01, insig = "blank")
#Correlacion con Qgraph
qgraph(cor(Datos_Num_CT))
qgraph(cor(Datos_Num_CT), layout="spring", posCol="darkgreen", negCol="darkmagenta")
#Grafico con histogramas y  significancia 
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = Corr_CT, col = col, symm = TRUE)
#Heatmap con "TRUE" or "FALSE" to see how the correlation matrix changes
ggcorr(Corr_CT,label = TRUE,label_alpha = FALSE)
dev.off() # Close the pdf device
pdf("Heatmap.pdf") 
# scale data to mean=0, sd=1 and convert to matrix
Datos_scaled <- as.matrix(scale(Datos_Num))
# create heatmap and don't reorder columns
heatmap(Datos_scaled, Colv=F, scale='none')
# cluster rows
hc.rows <- hclust(dist(Datos_scaled))
plot(hc.rows)
# transpose the matrix and cluster columns
hc.cols <- hclust(dist(t(Datos_scaled)))
plot(hc.cols)
# draw heatmap for first cluster
heatmap(Datos_scaled[cutree(hc.rows,k=2)==1,], Colv=as.dendrogram(hc.cols), scale='none')
# draw heatmap for second cluster
heatmap(Datos_scaled[cutree(hc.rows,k=2)==2,], Colv=as.dendrogram(hc.cols), scale='none')
dev.off() # Close the pdf device
#######################################################
#Principal component analysis
#PCA con princomp
pdf("PCA_1.pdf") 
Datos_CT_pca<- princomp(Datos_Num_CT, cor=T)
screeplot(Datos_CT_pca)
biplot(Datos_CT_pca, col=c("black","red"), cex=c(0.7,0.8))
dev.off() # Close the pdf device
pdf("PCA_2.pdf") 
# en vez de Datos_1[,c(1:51)] puede ser Datos_1_Num
Datos.pca <- prcomp(Datos[,c(1:51)], center = TRUE,scale. = TRUE)
library(ggbiplot)
ggbiplot(Datos.pca,labels=rownames(Datos))
#Se crean caracteres de las columnas con las que se quieren hacer elipses
a<-unlist(Datos[52], recursive = TRUE, use.names = TRUE)
b<-unlist(Datos[53], recursive = TRUE, use.names = TRUE)
ggbiplot(Datos.pca,ellipse=TRUE,  labels=rownames(Datos), groups=a)
ggbiplot(Datos.pca,ellipse=TRUE,  labels=rownames(Datos), groups=b)
dev.off() # Close the pdf device
pdf("PCA_3.pdf") 
#PCA con Principal
library("psych")
#Sin rotacion
Datos_CT_pca_unrot <- principal(Datos_Num_CT, nfactors=3, rotate="none")
qg_pca_unrot<-qgraph(loadings(Datos_CT_pca_unrot), posCol="darkgreen", layout="spring", negCol="darkmagenta", edge.width=2)
qgraph(qg_pca_unrot, layout="spring", posCol="darkgreen", negCol="darkmagenta", edge.width=2)
#Con rotacion varimax
Datos_CT_pca_rot <- principal(Datos_Num_CT, nfactors=3, rotate="varimax")
qg_pca_rot<-qgraph(loadings(Datos_CT_pca_rot), posCol="darkgreen", layout="spring", negCol="darkmagenta", edge.width=2)
qgraph(qg_pca_rot, layout="spring", posCol="darkgreen", negCol="darkmagenta", edge.width=2)
dev.off() # Close the pdf device
pdf("PCA_4.pdf") 
library("FactoMineR")
library("factoextra")
Datos_1.pca <- PCA(Datos_1_Num_CT, graph = FALSE)
fviz_eig(Datos_1.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(Datos_1.pca, col.var = "black")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(Dat.pca, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(Dat.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Change the transparency by cos2 values
fviz_pca_var(Dat.pca, alpha.var = "cos2")
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)  
# Contributions of variables to PC1
fviz_contrib(Dat.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(Dat.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(Dat.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(Dat.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)






















