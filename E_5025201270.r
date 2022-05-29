Responden <-c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100,95,70,90,90,90,89,90,100)

#Soal 1
#1A
df <- data.frame(Responden,X,Y)
df

selisih <- df[,3]-df[,2]
selisih

stdv_1a<- sd(selisih)
stdv_1a

#1B
x1bar_1b <- mean(X)
x2bar_1b <- mean(Y)
n1_1b<-length(X)
n2_1b<-length(Y)
S1kdrt_1b <- var(X)
S2kdrt_1b <- var(Y)
Spkdrt_1b <- ((n1_1b-1)*S1kdrt_1b+(n2_1b-1)*S2kdrt_1b)/((n1_1b-1)+(n2_1b-1))
Spkdrt_1b

t_1b<-((x1bar_1b-x2bar_1b)-stdv_1a)/sqrt(Spkdrt_1b*((1/n1_1b)+(1/n2_1b)))
t_1b                                

#1C
xbar = mean(Y)          
mu0 = mean(X)
s = sd(Y)
n = length(Y)
t = (xbar-mu0)/(s/sqrt(n)) 
t

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha)

pval <- 2*pt(t, df=n-1)
pval

cat("pvalue > 0,05 atau pvalue > alpha maka keputusan gagal tolak H0","\n", "tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas")

#Soal 2
#2A
cat("H0 : mu <= 20.000","\n","H0 : mu > 20.000")
xbar2 = 23500         
mu02 = 20000
sd2 = 3900
n2 = 100
z2 = (xbar2-mu02)/(sd2/sqrt(n2)) 
z2

alpha2 =0.05
z.alpha2 = qnorm(1-alpha2) 
z.alpha2 

#2A
cat("Setuju dengan klaim tersebut karena setelah diuji ternyata tolak H0 (z>Z.alpha) sehingga rata-rata mobil dikemudikan per tahun lebih dari 20.000km")
#2B
cat("Berdasarkan yang sudah tertulis diatas nilai 8,974359 merupakan nilai dari zhitung(z2 dalam syntax ini) melebihi nilai ztabel(z.alpha2) sehingga keputusan yang diambil adalah Tolak H0","\n", "sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km")
#2C
pval = pnorm(z2, lower.tail=FALSE) 
pval 
cat("Dikarenakan nilai pvalue < alpha(0,05) maka keputusan yang diambil adalah Tolak H0","\n", "sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km")

#Soal 3
#3A
cat("H0 : mu = mu0","\n","mu !=(tidak sama dengan) mu0")
#3C
xbar3 = 2.79
mu03 = 3.64
s3 = 1.32
n3 = 27              
t3 = (xbar3-mu03)/(s3/sqrt(n3)) 
t3  

#3D Nilai Kritis
alpha3 = 0.05 
t.alpha3 = qt(1-alpha3, df=2) 
t.alpha3 

#3E
cat("Keputusan : Gagal Tolak H0")

#3F
cat("Kesimpulan : Tidak ada perbedaan pada rata-rata jumlah saham perusahaan di dua kota tersebut")

#Soal 4
#4A
my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))


grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)

qqnorm(grup2$Length)

qqnorm(grup3$Length)

#Berdasarkan plot kuantil normal di atas, tidak ditemukan outlier utama pada homogenitas varians

#4B
bartlett.test(Length ~ Group, data = my_data)

#4C
model1 <- aov(Length ~ Group, data = my_data)
summary(model1)

#4D
#Nilai P adalah 0.0013 dimana kurang dari 0.005, sehingga H0 ditolak

#4E
TukeyHSD(model1)

#4F
library("ggplot2")
ggplot(my_data, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "white", colour = "black") + 
  scale_x_discrete() + xlab("Group") + ylab("Length")


#Soal 5
libray(dplyr)
library(multcompView)
gtl <- read.csv(file.choose())

#5A
qplot(x = Temp, y = Light, geom = "point", data = gtl) +
  facet_grid(.~Glass, labeller = label_both)

#5B
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gtl)

gtlaov <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(gtlaov)

#5C
data_summary <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)

#5D
tukey <- TukeyHSD(gtlaov)
print(tukey)

#5E
tukey.cld <- multcompLetters4(gtlaov, tukey)
print(tukey.cld)
