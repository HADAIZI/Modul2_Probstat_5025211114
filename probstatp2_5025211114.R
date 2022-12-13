#1
#X (sebelum melakukan aktivitas)
X<-c(78, 75, 67, 77, 70, 72, 28, 74, 77)
#Y (sesudah melakukan aktivitas)
Y<-c(100, 95, 70, 90, 90, 90, 89, 90, 100)
#1a
difff <- c(22, 20, 3, 13, 20, 18, 11, 16, 23)
sd(difff)
#1b
meanX <- mean(X)
meanY <- mean(Y)
sdX <- sd(X)
sdY <- sd(Y)
variansX <- sdX ^ 2
variansY <- sdY ^ 2
abs(meanX - meanY) / sqrt((variansX/9) + (variansY/9))
#1c
t.test(X, Y)


#soal 2
#A. Setuju
#B
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
#C
dt.mean <- 235000
dt.a <- 20000
dt.sd <- 3900
dt.n <- 100
z <- (dt.mean-dt.a)/(dt.sd/sqrt(dt.n))
2*pnorm(-abs(z))


#soal 3
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
qt(p =0.05, df =2, lower.tail = FALSE)

#4
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
#b
bartlett.test(Length ~ Group, data = data)
#4c
model1 = lm(Length ~ Group, data = data)
anova(model1)
model1_1 = aov(Length ~ Group, data=data)
summary(model1_1)
#d.p-value = 0.8054
#e
plot(TukeyHSD(model1_1))
TukeyHSD(aov(model1))
TukeyHSD(aov(model1_1))
#f
library(ggplot2)
ggplot(data, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  
  ylab("Length (cm)")

#soal 5
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
GTL <- read_csv("C:\\Users\\OMEN\\OneDrive\\Desktop\\Progamming\\Probstat\\Prak 2\\GTL.csv")
head(GTL)
str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

# No 5b
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

# No 5c
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# No 5d
tukey <- TukeyHSD(anova)
print(tukey)

# No 5e
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")