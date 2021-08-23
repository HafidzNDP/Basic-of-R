library(tidyverse)
library(ggplot2)
data("USArrests")

## Buat model PCA dengan memanggil command prcomp pada data USArrests
## Pastikan center dan scale dibuat TRUE untuk melakukan standarisasi pada data
pca_crime <- prcomp(USArrests, center = TRUE, scale = TRUE)

## Tampilkan Summary dari model PCA yang dibuat
summary(pca_crime)

"""
Standard Deviation adalah nilai eigenvalue dari masing-masing principal component
Eigenvalue 1 = 1.5749
Eigenvalue 2 = 0.9949
Eigenvalue 3 = 0.59713
Eigenvalue 4 = 0.41645

Proportion of variance adalah seberapa banyak 'varian' yang dijelaskan oleh 
principal component yang bersangkutan. Anggap ini sebagai skala seberapa 'penting'
suatu Principal Component.

PC1 memiliki 0.62 proportion of variance.
Artinya, PC1 mampu menjelaskan 62% dari variansi yang terjadi dalam dataset

Cummulative proportion adalah jumlah kumulatif dari varians yang dijelaskan
oleh Principal Component secara beruntun. Artinya:

PC1 = 0.6201
PC2 = 0.2474

Bila kita 'hanya' menggunakan PC1 dan PC2, maka total varians yang dapat
dijelaskan oleh kedua PC ini adalah 0.6201 + 0.2474 = 0.8675 = 86.75%.

Secara kasar, artinya hanya dengan 2 Principal Component, kita mampu menjelaskan
86.75% dataset. Informasi yang 'hilang' akibat reduksi dimensi ini kurang dari 15%.
"""
# Untuk melihat data yang telah ditransformasi ke PC1 dan PC2, jalankan kodingan berikut
transformed_data = as.data.frame(-pca_crime$x[,1:2])

# Mengapa diberi tanda negatif? Karena secara default di R, eigenvalue bernilai negatif.
# Agar pada saat visualisasi, nilai positif tetap mengacu pada nilai positif, maka kita berikan tanda negatif

#################
## Visualisasi ##

PC1 <- transformed_data$PC1
PC2 <- transformed_data$PC2
usa_crime = data.frame(State = row.names(USArrests), PC1, PC2)

ggplot(usa_crime, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("Sumbu-x : PC1") + 
  ylab("Sumbu-y : PC2") + 
  ggtitle("PCA Pada Data USArrests")
