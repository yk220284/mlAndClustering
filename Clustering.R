library(MASS)
library(stats)
library(rgl)
library(car)
library(dplyr)
select <- dplyr::select
data_ready <- read.csv(file.choose())

#deleting identical features
data_ready_target <- select(data_ready,-X,-X1,-Asset_Option)

#PCA 
f1 = prcomp(data_ready_target, retx = TRUE, scale.=T)

#calculate kmean
fk = kmeans(f1$x[, 1:11], centers = 11)

#viewing different clusters with different colors in pc1-pc2-pc3 space
cols = c("black", "red", "green", "orange", "purple", "blue", "cyan", "yellow", "brown", "grey",
         "pink")
scatter3d(x = f1$x[,1], y = f1$x[,2], z = f1$x[,3], point.col = cols[fk$cluster], pch = 16, surface
          = FALSE, xlab = "PC1", ylab = "PC2", zlab = "PC3")

#projecting the data set onto pc2-pc3 plane 
plot(f1$x[,1:2])

#standard dev for each pc
barplot(f1$sdev)

#viewing loading matrix
rotation <- f1$rotation

#adding a new feature to the table which is the clustering classification
data_ready_cl <- cbind(data_ready_target,fk[["cluster"]])
names(data_ready_cl)[94]<-paste("cl_label")


#run hierarchical clustering with Single Linkage on the dataset as follows
hc1 = hclust(dist(scale(f1$x[,1])), method = "single")
plot(hc1)
#??? vec mem running out???

#analyse data
data_cl <- select(data_ready_cl,-(X2:X67))


#init analysis matrix
cl1 <- data_cl[which(data_ready_cl$cl_label=='1'),]
cl1 <- colMeans(cl1)
analysis_matrix <- cl1

#for loop
for (i in 2:11) {
  cl <- data_cl[which(data_ready_cl$cl_label == i ),]
  cl <- colMeans(cl)
  analysis_matrix <- cbind(analysis_matrix,cl)
}

#change col names accordingly
colnames(analysis_matrix) <- (1:11)
analysis_matrix <- t(analysis_matrix)
write.csv(analysis_matrix,"analysis_matrix.csv")



