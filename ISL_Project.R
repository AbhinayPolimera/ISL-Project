library(readr)
winequality_red <- read_csv("Downloads/winequality-red.csv")
View(winequality_red)
library(MASS)
library(ISLR2)
head(winequality_red)
attach(winequality_red)
lm.fit <- lm(`total sulfur dioxide` ~ `residual sugar`, data = winequality_red)
lm.fit <- lm(`total sulfur dioxide` ~ `residual sugar`)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
plot(`residual sugar`, `total sulfur dioxide`)
abline(lm.fit, lwd = 2, col = "green")
par(mfrow = c(2, 2))
plot(lm.fit)

## Multi-Linear

lm.fit <- lm(`total sulfur dioxide` ~ `residual sugar` + `volatile acidity`, data = winequality_red)
summary(lm.fit)
lm.fit <- lm(`total sulfur dioxide` ~ ., data = winequality_red)
summary(lm.fit)
lm.fit1 <- lm(`total sulfur dioxide` ~ . - sulphates, data = winequality_red)
summary(lm.fit)
summary(lm(`total sulfur dioxide` ~ `residual sugar` * sulphates, data = winequality_red))
lm.fit1 <- lm(`total sulfur dioxide` ~ `residual sugar` + I(`residual sugar`^2))
summary(lm.fit1)
anova(lm.fit, lm.fit1)
par(mfrow = c(2, 2))
plot(lm.fit1)
lm.fit5 <- lm(`total sulfur dioxide` ~ poly(`residual sugar`, 5))
summary(lm.fit5)
summary(lm(`total sulfur dioxide` ~ log(`volatile acidity`), data = winequality_red))

#Step-2
library(readr)
heart_disease <- read_csv("Downloads/heart_disease.csv")
View(heart_disease)
regfit.fwd <- regsubsets(heart_disease$chol ~ ., data = heart_disease, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(heart_disease$chol ~ ., data = heart_disease, nvmax = 19, method = "backward")
summary(regfit.bwd)
reg.summaryfwd <- summary(regfit.fwd)
names(reg.summaryfwd)
reg.summaryfwd$rsq

par(mfrow = c(2, 2))
plot(reg.summaryfwd$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summaryfwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
plot(reg.summaryfwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
points(11, reg.summaryfwd$adjr2[11], col = "red", cex = 2, pch = 20)
par(mfrow = c(1, 1))
plot(reg.summaryfwd$adjr2, xlab = "Number of Variables",ylab = "Adjusted RSq", type = "l")
points(11, reg.summaryfwd$adjr2[11], col = "red", cex = 2,pch = 20)
reg.summarybwd <- summary(regfit.bwd)
par(mfrow = c(2, 2))
plot(reg.summarybwd$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summarybwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")


#Step-3
#part-1
library(readr)
data <- read_csv("Downloads/data.csv")
View(data)
library(ISLR2)
names(data)
dim(data)
summary(data)
plot(data$radius_mean)
glm.fits <- glm(
  +     data$fractal_dimension_mean  ~ data$smoothness_mean + data$area_mean + data$compactness_mean + data$concavity_mean,
  +     data = data, family = binomial
   )
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]

#part-2
attach(heart_disease)
library(caTools)
library(ggplot2)
library(Metrics)
library(e1071)
head(heart_disease)
split = sample.split(heart_disease$cp, SplitRatio = 0.7)
trainingset = subset(heart_disease, split == TRUE)
testset= subset(heart_disease, split == FALSE)
tree.exang <- tree(as.factor(exang) ~ cp+age+sex+chol+fbs+thalch, trainingset)
plot(tree.exang)
text(tree.exang, pretty = 0)
tree.pred <- predict(tree.exang, testset, type = "class")
table(tree.pred, testset$exang)
data = data.frame(x = heart_disease$thalch, y = as.factor(heart_disease$exang), z = heart_disease$chol)
df = data.frame(x = heart_disease$thalch, y = as.factor(heart_disease$exang), z = heart_disease$chol)
split = sample.split(df$z, SplitRatio = 0.7)
trainset = subset(df, split == TRUE)
tset = subset(data, split == FALSE)
svmfit <- svm(y ~ x+z, data = trainset, kernel = "linear", cost = 1, scale = TRUE)
summary(svmfit)
plot(svmfit, trainset)
tune.out <- tune(svm, y ~ ., data = trainset, kernel = "linear", 
                 +                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)






