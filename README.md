# Modul2_Probstat_502521114
Berisi penjelasan terhadap praktikum probstat

## Identitas
- Adam Haidar Azizi
- 5025211114

```
1. Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴 
Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen  dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.
```
Masukkan Data
```r
#X (sebelum melakukan aktivitas)
X<-c(78, 75, 67, 77, 70, 72, 28, 74, 77)
#Y (sesudah melakukan aktivitas)
Y<-c(100, 95, 70, 90, 90, 90, 89, 90, 100)
```
- A. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

Untuk mencari standar deviasi dari data selisih pasangan
```r
difff <- c(22, 20, 3, 13, 20, 18, 11, 16, 23)
sd(difff)
```
Sehingga didaptakan
![1a](https://user-images.githubusercontent.com/90259304/207295611-c2702522-6ffa-498c-9db1-726293c1e0b9.png)

Berdasarkan gambar didapat bahwa hasil dari standar deviasi = 6.359695

- B. carilah nilai t (p-value)

Untuk mencari nilai t(p-value) digunakan cara

```r
meanX <- mean(X)
meanY <- mean(Y)
sdX <- sd(X)
sdY <- sd(Y)
variansX <- sdX ^ 2
variansY <- sdY ^ 2
abs(meanX - meanY) / sqrt((variansX/9) + (variansY/9))
```
Sehingga didapatkan
![1b](https://user-images.githubusercontent.com/90259304/207296895-1d58fe2f-4bba-49dc-9720-af34d06cf232.png)

Berdasarkan gambar didapat bahwa nilai t = 3.632278

- C. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
```r
t.test(X, Y)
```

Didapat
![1c](https://user-images.githubusercontent.com/90259304/207297890-6e3d2148-9eff-4d60-bb02-23542003d73d.png)

Berdasarkan hasil tersebut disimpulkan melihat nilai p kita bisa dibilang bahwa H0 tolak dan H1 diterima dengan H1 = ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴.

```
2. Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan library seperti referensi pada modul). 
```

```r
install.packages("BSDA")
library(BSDA)
rata_rata <- 23500
standar_deviasi <- 3900
pemilik_mobil <- 100
tsum.test(
  mean.x = rata_rata, 
  sd(standar_deviasi), 
  n.x = pemilik_mobil, 
  var.equal = TRUE)
```

![2ab](https://user-images.githubusercontent.com/90259304/207300165-05b71759-26ca-41b8-821f-576a4fac7ec4.png)


- A. Apakah Anda setuju dengan klaim tersebut?
Setuju

- B. Jelaskan maksud dari output yang dihasilkan!
 
 Berdasar hasil
 H0 : μ = 20000
 H1 : μ > 20000
 
 - C. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
```r
dt.mean <- 235000
dt.a <- 20000
dt.sd <- 3900
dt.n <- 100
z <- (dt.mean-dt.a)/(dt.sd/sqrt(dt.n))
2*pnorm(-abs(z))
```
 ![2c](https://user-images.githubusercontent.com/90259304/207301297-b6ff844d-b8af-4b61-80b3-254f7f0fe158.png)

Hasil h0 diterima


```
3. Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.
Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)? Buatlah :
```

- A. H0 dan H1(3)
H0 : Tidak ada perbedaan rata - rata antara Bandung dan Bali
H1 : ada perbedaan rata - rata antara Bandung dan Bali

- B. Hitung Sampel Statistik(3)
```r
bandung <- list("saham"=19, "mean"=3.64, "sd" =1.67)
bali <- list("saham" =27, "mean" =2.79, "sd" =1.32)
tsum.test(
  n.x = bandung$saham,
  n.y = bali$saham,
  mean.x = bandung$mean,
  mean.y = bali$mean,
  s.x = bandung$sd,
  s.y = bali$sd,
  var.equal = TRUE,
  alternative = "two.sided",
)
```
![3b](https://user-images.githubusercontent.com/90259304/207303017-19746213-9852-49bd-b7ea-a2bedc2eb1b4.png)

- C. Lakukan Uji Statistik (df =2)(5)
t = 1.9267
- D.
```r
qt(p =0.05, df =2, lower.tail = FALSE)
```
![3c](https://user-images.githubusercontent.com/90259304/207303613-e48bfb98-27e0-4b47-8314-dde9f209e2a3.png)

qt = 2.919986

- E Keputusan(3)
H0 diterima karena berada di antara kedua nilai kritikal
- F Kesimpulan
Tidak ada perbedaan rata - rata antara Bandung dan Bali

```
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data  tiga spesies kucing yaitu kucing oren, kucing hitam dan kucing putih dengan panjangnya masing-masing. 
Jika : 
diketahui dataset  https://intip.in/datasetprobstat1 
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama    
```

- A. Buatlah masing masing jenis spesies menjadi  3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.

```r
data <- read.table("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt", h=T)
head(data)
data$Group <- as.factor(data$Group)
table(data$Group)
str(data)
data$Group = factor(data$Group,labels = c("Kucing Oren", "Kucing Hitam", "Kucing Putih"))
Group1 <- subset(data, Group == "Kucing Oren")
Group2 <- subset(data, Group == "Kucing Hitam")
Group3 <- subset(data, Group == "Kucing Putih")
#Plot Kuantil
qqnorm(Group1$Length)
qqline(Group1$Length)
qqnorm(Group2$Length)
qqline(Group2$Length)
qqnorm(Group3$Length)
qqline(Group3$Length)
```

![4a](https://user-images.githubusercontent.com/90259304/207306573-6154ab19-19f1-409d-bfd8-4f791bd40e32.png)
![grup 1](https://user-images.githubusercontent.com/90259304/207307050-3892f93b-0d8c-4c10-9414-bdbe139559e5.png)
![grup 2](https://user-images.githubusercontent.com/90259304/207307082-1eb582bb-052a-47c2-989d-5b341a98b7a3.png)
![grup 3](https://user-images.githubusercontent.com/90259304/207307101-71128cff-5255-4039-8b74-199f178c3308.png)

- B. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

```r
bartlett.test(Length ~ Group, data = data)
```
![4b](https://user-images.githubusercontent.com/90259304/207307645-00bc5636-c293-46e2-a376-858cb1f7d411.png)

H0 = Semua Variansi Sama
H1 = Ada 1 populasi dengan Variansi berbeda
P-value = 0.8054

- C. Untuk uji ANOVA, buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.
```r
model1 = lm(Length ~ Group, data = data)
anova(model1)
model1_1 = aov(Length ~ Group, data=data)
summary(model1_1)
```













