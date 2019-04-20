
library(readxl)
Datos_1 <- read_excel("D:/TESIS_rev/TESIS_DATA/Datos_estadistica_ordenados_1.xlsx")

Datos_1 <- as.data.frame(Datos_1)
Datos_1 <- data.frame(Datos_1[,-1], row.names=Datos_1[,1])

Datos_1.pca <- prcomp(Datos_1[,c(1:51)], center = TRUE,scale. = TRUE)

summary(Datos_1.pca)

library(ggbiplot)
ggbiplot(Datos_1.pca)

library(ggbiplot)
ggbiplot(Datos_1.pca,labels=rownames(Datos_1))

a<-unlist(Datos_1[52], recursive = TRUE, use.names = TRUE)

b<-unlist(Datos_1[53], recursive = TRUE, use.names = TRUE)

ggbiplot(Datos_1.pca,ellipse=TRUE,  labels=rownames(Datos_1), groups=a)

ggbiplot(Datos_1.pca,ellipse=TRUE,  labels=rownames(Datos_1), groups=b)


