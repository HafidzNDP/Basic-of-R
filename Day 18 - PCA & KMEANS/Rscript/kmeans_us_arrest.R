library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages('cluster')
install.packages('factoextra')
library(cluster)
library(factoextra)

data("USArrests")

df <- USArrests

# Hilangkan baris yang bernilai NA (not available - kosong)
df <- na.omit(df)


# Pertama, skala-kan datanya terlebih dahulu

df <- scale(df)
head(df)

# Kemudian, buat model k-means clustering

k_2 = kmeans(df, centers = 2, nstart = 50)
k_2

# Dapat dilihat bahwa model berhasil memecahkan negara bagian menjadi...
# ...2 kelompok. Namun, apakah k=2 adalah yg terbaik?

fviz_cluster(k_2, data = df)

# Evaluasi dengan Metode Elbow

fviz_nbclust(df, kmeans, method = 'wss')

# Evaluasi dengan Metode Silhouette

fviz_nbclust(df, kmeans, method = 'silhouette')

# Evaluasi dengan Gap Metric

fviz_nbclust(df, kmeans, method = 'gap_stat')

# Final K-Means

k_4 = kmeans(df, centers = 4, nstart = 50)
fviz_cluster(k_4, data = df)
