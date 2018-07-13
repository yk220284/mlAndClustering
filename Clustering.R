library(MASS)
library(stats)
library(rgl)
library(car)
data_ready <- read.csv(file.choose())

f1 = prcomp(data_ready, retx = TRUE )
fk = kmeans(f1$x[, 1:11], centers = 11)
cols = c("black", "red", "green", "orange", "purple", "blue", "cyan", "yellow", "brown", "grey",
         "pink")
scatter3d(x = f1$x[,1], y = f1$x[,2], z = f1$x[,3], point.col = cols[fk$cluster], pch = 16, surface
          = FALSE, xlab = "PC1", ylab = "PC2", zlab = "PC3")
table(Z, fk$cluster)


f1$x
plot(f1$x[,2:3])
barplot(f1$sdev)
rotation <- f1$rotation
fk = kmeans(f1$x[, 1:10], centers = 5)

output <- matrix(cl,ncol = 1000, byrow = T)
data_ready_cl <- cbind(data_ready,fk[["cluster"]])
