library(readxl)
library(glmnet)
library(ggplot2)
library(graphics)
library(foreign)
library(dplyr)
setwd('/Users/nang/Desktop/낭/교류학술제/regression')

blue <- read_excel('청와대_final2.xlsx')
class(blue)
blue <- as.data.frame(blue)
blue <- blue[,-c(3,4)] # category, start 삭제
head(blue,5) ; dim(blue)

y <- blue$count
X <- as.matrix(blue[,-c(1,2)])

# split train-test data
set.seed(sample(1:1000, 1))
train <- sample(1:nrow(X), nrow(X)*0.7)
X_test <- (-train)
y_test <- y[X_test]

# lasso regression
lasso <- cv.glmnet(X[train,], y[train], alpha = 1,
                   nfolds = 8,
                   family = 'poisson')
lasso$glmnet.fit

plot(lasso, main = '')
title(main = list('lasso regression k=8', cex = 0.8, col = 'blue'))

best.lambda <- lasso$lambda.min

best_lasso <- glmnet(X[train,], y[train], alpha = 1,
                     lambda = best.lambda,
                     family = 'poisson')

pred <- predict(best_lasso, s = best.lambda,
                newx = X[X_test,])
head(cbind(y_test, exp(pred)),20)

coef(best_lasso, s = lasso$lambda.min)
summary(best_lasso$beta)





# elasticnet
y <- assembly$count
X <- as.matrix(assembly[,-c(1,2)])
set.seed(sample(1:1000,1))
train <- sample(1:nrow(X), nrow(X)*0.3)
X_test <- (-train)
y_test <- y[X_test]

alpha <- seq(0.8,0.99,0.01)
par(mfrow = c(3,7))
best.lambda_min <- rep(0,length(alpha))
best.lambda_1se <- rep(0,length(alpha))
for (i in 1:length(alpha)){
  k = cv.glmnet(X[train,], y[train], alpha = alpha[i],
                nfolds = 7,
                family = 'poisson')
  
  plot(k)
  best.lambda_min[i] <- k$lambda.min
  best.lambda_1se[i] <- k$lambda.1se
  print(c('alpha :', alpha[i],'best lambda - min: ', best.lambda_min[i],
          'best lambda - 1se: ', best.lambda_1se[i]))
}

min(best.lambda_min)
which.min(best.lambda_min)
best.elasticnet <- glmnet(X[train,], y[train], alpha = alpha[which.min(best.lambda_min)],
                          lambda = best.lambda_min[which.min(best.lambda_min)],
                          family = 'poisson')
pred <- predict(best.elasticnet, s = best.lambda_min[which.min(best.lambda_min)],
                newx = X[X_test,])

head(cbind(y_test,exp(pred)),20)

coef(best.elasticnet, s = best.lambda_min[which.min(best.lambda_min)])
summary(best.elasticnet$beta)
