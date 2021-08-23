library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages('cluster')
install.packages('factoextra')
library(cluster)
library(factoextra)

"""Assignment Batch 3 Day 18: PCA + K-Means

Dataset: Iris
Task: Lakukan PCA sehingga mereduksi data Iris menjadi 2 kolom saja
kemudian, lakukan K-Means Clustering dari data Iris yang telah berhasil 
direduksi. 

Soal:
a. Berapa nilai eigenvalue 1 dan eigenvalue 2 dari dataset Iris?
Jawab
Eigenvalue 1 = 1.7084
Eigenvalue 2 = 0.9560

b. Berapa informasi yang masih bisa 'dijelaskan' oleh data yang telah direduksi?
Jawab
Data yang dapat dijelaskan oleh informasi tersebut dapat dilihat dari 
Cumulative proposition PC2 yang merupakan total dari proportion varians dari 
pricipels yang inging kita gunakan karena principel yang kita gunakan PC1 dan PC2
maka total variance yang dapat digunakan yaitu PC1+PC2 =0.7296 + 0.2285 = 0.9581. 
Sehingga informasi yang dapat dijelaskan sebesar 95,81%.

c. Cari nilai 'k' yang optimal berdasarkan Elbow dan Silhouette method.
Apakah nilai k = 3 masih merupakan nilai 'k' yang terbaik?
Jawab : 
Berdasarkan Metode Elbow K = 3 merupakan nilai terbaik karena pola siku
dan untuk K selanjutnya mengalami penurunan yang tidak terlalu signifikan.

Berdasarkan metode Silhouttte K = 3 bukan nilai yang terbaik karena nilai
tertinggi berada pada di K = 2.untuk menentukan K terbaik maka gunakan
metode tambahan sekaligus menjadi penentu apakah 2 atau 3 yang terbaik.

Bedasarkan metode GAP Statistic justru K= 6 lah yang terbaik tentu ini tidak
bisa digunakan sebagai jawaban, untuk mengetahui jawabannya maka gunakan visualisasi.

Bedasarkan pengamatan saya menggunakan fviz_cluster, k= 2 lah yang terbaik yang
dapat digunakan untuk klustering.

"""

# Import data iris
data(iris)

# Hilangkan kolom 'label' bunga sehingga data 'terkesan' seperti unlabeled
df <- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)

# Buatlah PCA dari df
pca_iris = prcomp(df, center = T, scale = T)

# Lihat summary model PCA untuk menentukan eigenvalue 1 dan 2
summary(pca_iris)

# Reduksilah data iris menjadi 2 kolom saja
iris_transform = as.data.frame(-pca_iris$x[,1:2])
iris_transform

# Kemudian, lakukan evaluasi k-means terhadap iris_transform
fviz_nbclust(df, kmeans, method = 'wss')
fviz_nbclust(df, kmeans, method = 'silhouette')
fviz_nbclust(df, kmeans, method = 'gap_stat')

# Pilihlah nomor k yang menurut anda paling optimal
k = kmeans(df, centers = 2, nstart = 50)

# Visualisasikan k-means clustering pada data Iris yang telah tereduksi
kmeans_iris = kmeans(iris_transform, centers = 2, nstart = 50)
fviz_cluster(kmeans_iris, data = iris_transform)
