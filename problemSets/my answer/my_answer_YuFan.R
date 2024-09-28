#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
##1.Find a 90% conﬁdence interval for the average student IQ in the school.

sample_mean <- mean(y)
sample_sd <- sd(y)
n <- length(y)
t_score <- qt(0.95, df = n-1)
lower_CI <- sample_mean - (t_score * sample_sd / sqrt(n))
upper_CI <- sample_mean + (t_score * sample_sd / sqrt(n))
confint90 <- c(lower_CI, upper_CI)
confint90
#> confint90
#[1]  93.95993 102.92007

##2.Next,the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score(100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α=0.05.
#H0: Average IQ of students in schools <= 100  H1: Average IQ of students in schools > 100
t_test <- t.test(y, mu = 100, alternative = "greater")
print(t_test)
#t = -0.59574, df = 24, p-value = 0.7215
#alternative hypothesis: true mean is greater than 100
#p>0.05,so the null hypothesis cannot be rejected. 
#there is insufficient statistical evidence to support that the average IQ of the school's students is significantly higher than the national average IQ.

#####################
# Problem 2
#####################
##1.the relationships among Y, X1, X2, and X3
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.relationships_YuFan.pdf")
pairs(expenditure[, c("Y", "X1", "X2", "X3")], main = "Scatterplot Matrix")
dev.off()

matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")])
print(matrix)
summary(expenditure)
sink("summary.txt")
print(summary(expenditure) )
sink()

# Create correlation matrix
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
cor_matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")])
print(cor_matrix)

#Y and X1
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.Y.X1_YuFan.pdf")
plot(x = expenditure$X1, y = expenditure$Y, main = "Y vs X1 Scatter Plot", xlab = "X1", ylab = "Y", pch = 19, col = "blue")  
dev.off()

#Y and X2
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.Y.X2_YuFan.pdf")
plot(x = expenditure$X2, y = expenditure$Y, main = "Y vs X2 Scatter Plot", xlab = "X2", ylab = "Y", pch = 19, col = "blue")  
dev.off()

#Y and X3
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.Y.X3_YuFan.pdf")
plot(x = expenditure$X3, y = expenditure$Y, main = "Y vs X3 Scatter Plot", xlab = "X3", ylab = "Y", pch = 19, col = "blue")  
dev.off()

#X1 and X2
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.X1.X2_YuFan.pdf")
plot(x = expenditure$X1, y = expenditure$X2, main = "X1 vs X2 Scatter Plot", xlab = "X1", ylab = "X2", pch = 19, col = "blue")  
dev.off()

#X1 and X3
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.X1.X3_YuFan.pdf")
plot(x = expenditure$X1, y = expenditure$X3, main = "X1 vs X3 Scatter Plot", xlab = "X1", ylab = "X3", pch = 19, col = "blue")  
dev.off()

#X2 and X3
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
pdf("plot.X2.X3_YuFan.pdf")
plot(x = expenditure$X2, y = expenditure$X3, main = "X2 vs X3 Scatter Plot", xlab = "X2", ylab = "X3", pch = 19, col = "blue")  
dev.off()

##2. plot the relationship between Y and Region
library(ggplot2) 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
boxplotYregion <- ggplot(expenditure, aes(x = Region, y = Y, fill = Region)) + 
  geom_point() +
  theme_minimal() +
  labs(title = "Boxplot of Expenditure on Housing Assistance by Region",
       x = "Region",
       y = "Expenditure (Y)",
       fill = "Region")
ggsave("boxplot.Y.Region_YuFan.pdf", plot = boxplotYregion, width = 8, height = 6)

#which region has the highest per capita expenditure on housing assistance?
barplotYregion <- ggplot(expenditure, aes(x = Region, y = Y, fill = Region)) + 
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(title = "Average Expenditure on Housing Assistance by Region",
       x = "Region",
       y = "Average Expenditure (Y)",
       fill = "Region")
ggsave("barplot.Y.Region_YuFan.pdf", plot = barplotYregion, width = 8, height = 6)

##3 plot the relationship between Y and X1
library(ggplot2)
expenditure$Region <- as.factor(expenditure$Region)
YX1region <- ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) + 
  geom_point() +  
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(1, 2, 3, 4)) +  
  theme_minimal() + 
  labs(title = "Relationship between Y and X1 by Region",
       x = "X1",
       y = "Y",
       color = "Region") 
ggsave("plot_Y_X1_by_Region.pdf", plot = YX1region, width = 8, height = 6)
