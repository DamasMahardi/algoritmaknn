# Contoh k-NN (k-Nearest Neighbor)
# menggunakan library(class) dan library(dbscan)
# oleh: Arif Rahman Hakim

library(class)
# install.packages("dbscan")  #install package dbscan jika belum terinstall
library(dbscan)

tinggi <- c(175,165,166,178,174,169,168,170)
berat <- c(75,60,77,73,62,68,64,70)
size <- factor(c("L","M","L","L","M","M","M","L"))

# menggabungkan tinggi dan berat
orang <- cbind(tinggi,berat)
orang

# mendifinisikan nama untuk masing-masing orang
rownames(orang) <- c("Richard","Deby","Mike","Tom","Bella","John","Ann","Jack")
orang

# membuat plot
plot(orang, col=ifelse(size=="L","red","blue"), cex=2, lwd=4)

# mempercantik plot agar tidak terlalu melebar
plot(orang, col=ifelse(size=="L","red","blue"), cex=2, lwd=4, asp=1)
plot(orang, col=ifelse(size=="L","red","blue"), cex=0.3, lwd=0.2, asp=1)
text(orang, labels = row.names(orang), cex = 2)

# mendefinisikan Ryan
orangbaru <- cbind(173,69)
points(orangbaru, cex = 2, lwd =4)
rownames(orangbaru) <- c("Ryan")
orangbaru

# klasifikasi orang baru (Ryan) dengan knn dari library(class)
# mencari ukuran kaos untuk orang baru
# knn = untuk mencari ukuran kaos dengan algoritma k-NN
# orang = data training
# orangbaru = data testing
# size = target
# k = banyaknya tetangga
# prob = probabilitas / peluang
klasifikasi <- knn(orang, orangbaru, size, k = 3, prob = TRUE)
klasifikasi
klasifikasi <- knn(orang, orangbaru, size, k = 5, prob = TRUE)
klasifikasi

# sebaiknya menggunakan k sebanyak berapa?
# apakah 1, 3, 5 dst

# mengetahui persekitaran / tetangga terdekat dari si Ryan
# menggabungkan si Ryan dengan teman-temannya
semuaorang <- rbind(orang, orangbaru)
semuaorang

# melihat tetangga terdekat
tetangga <- kNN(semuaorang, k = 3) 
tetangga
# struktur tetangga dengan kNN
str(tetangga)

# mendefinisikan Ryan dan melihat tetangga terdekat si Ryan
i <- 9
tetangga$id[i,]
# tetangga terdekatnya yaitu:
# 1. => 8 [Jack]
# 2. => 6 [John]
# 3. => 1 [Richard]

# membuat plot untuk semua orangorang
plot(semuaorang, col = ifelse(1:nrow(semuaorang) %in% tetangga$id[i,], "red", "black"), cex = 2, lwd = 2, asp = 1)

# menampilkan nama-nama pada scatter plot
text(semuaorang, labels = row.names(semuaorang), cex = 1)

# melihat tetangga terdekat dari si Ryan dengan lingkaran dari tiga tetangga terjauh si Ryan
dist(rbind(semuaorang[9,], semuaorang[1,]), method = "euclidian")
circle <- function(x,y,rad = 1,nvert = 500, ...){
  rads <- seq(0,2*pi,length.out=nvert)
  xcoords <- cos(rads)*rad+x
  ycoords <- sin(rads)*rad+y
  polygon(xcoords, ycoords, ...)
}
circle(173,69,6.3246)

# size si Ryan = L, masukkan ke dalam list size
sizetambahan <- factor(c("L"))
size <- unlist(list(size, sizetambahan))
size

# klasifikasi dari semuaorang k=3
klasifikasi3 <- knn(semuaorang, semuaorang, size, k = 3, prob = TRUE)
klasifikasi3

# melihat kebenaran klasifikasi
100*sum(size==klasifikasi3)/100
table(klasifikasi3, size)


# klasifikasi dari semuaorang k=5
klasifikasi5 <- knn(semuaorang, semuaorang, size, k =5, prob = TRUE)
klasifikasi5

# melihat kebenaran klasifikasi
100*sum(size==klasifikasi5)/100
table(klasifikasi5, size)


