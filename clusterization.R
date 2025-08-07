library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
par(mfrow=c(1,1))
#Wykonaj klasteryzacjê stanów (USArrests) metod¹ k-means. Wybierz liczbê klastrów i uzasadnij swój
#wybór (za³¹cz wykresy). Zwizualizuj klastry w dwóch wymiarach. Zinterpretuj wybrany klaster. Czym
#ró¿ni siê od pozosta³ych? Jak liczne s¹ klastry?
# Zadanie 2 ---------------------------------------------------------------
#metoda k-means
data_frame <- USArrests 
data_frame <- na.omit(data_frame)
set.seed(1000)
#wybieram ile klastrÃ³w?
fviz_nbclust(data_frame, FUNcluster = kmeans, method = "wss", k.max = 10)

fviz_nbclust(data_frame, FUNcluster = kmeans, method = "silhouette", k.max = 10)

gap_stat <- clusGap(data_frame, FUNcluster = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#na oko (HCA)
data <- as.data.frame(scale(data_frame))
diss <- dist(data, method = "euclidian") 
hc <- hclust(diss, method = "ward.D2") 
plot(hc, cex = 0.6, hang = -1, 
     main = "Ward's minimum variance dendrogram", sub = "", xlab = "")

#wybieram 4 klastry, bo 6 jest moim zdaniem za duÅ¼o (trudno interpretowaÄ‡), a 4 ma doÅ›Ä‡ wysoki wskaÅºnik silhoutte)

set.seed(1000)
km <- kmeans(data_frame, 4, nstart = 50, iter.max = 100)
print(km)

aggregate(data_frame, by=list(cluster=km$cluster), mean)

km$size
#doÅ›Ä‡ rÃ³wna liczebnoÅ›Ä‡ klastrÃ³w
km$centers
#liczebnoÅ›ci - 16(1 klaster), 14(2 klaster), 10(3 i 4 klastry)
fviz_cluster(km,
             data = data_frame,
             main = "4 clusters",
             labelsize = 8,
             pointsize = 0.5)
#1 klaster - stany z najwyÅ¼szym poziomem morderstw i napadÃ³w (najniebezpieczniejsze)
#2 klaster - najwyÅ¼szy odsetek miejskiej ludnoÅ›ci i rÃ³wnieÅ¼ wysoki poziom morderstw
#3 klaster - umiarkowanie bezpieczne
#4 klaster - najbezpieczniejsze (tylko poÅ‚owa to mieszkaÅ„cy miast, porÃ³wnywalne niski wskaÅºnik morderstw i napadÃ³w)