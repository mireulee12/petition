# scad regression
library(ncvreg)
library(readxl)
setwd("D:/Summer 2021/교류학술제/SCAD_regression")

################################################
## assembly
################################################
# 1. import data
assembly <- data.frame(read_excel("국회_final(2).xlsx"))
assembly_X <- assembly[, seq(from = 5, to = ncol(assembly), by = 1)]
assembly_y <-  log(assembly$count)
rm(assembly)

interaction = assembly_X$anger * assembly_X[, 3:ncol(assembly_X)]
assembly_X = cbind(assembly_X, interaction)

# 2. determine lambda
cvfit <- cv.ncvreg(data.frame(assembly_X), assembly_y, type = "cve")
plot(cvfit)

min_index = summary(cvfit)$min
lambdas = summary(cvfit)$lambda
assembly_l = lambdas[min_index]

# cve의 변화가 0.0001이하가 되는 람다 값을 찾는 과정
max_index = which.max(summary(cvfit)$cve)
diff = c()
for(i in 1:99){
  diff[i] = summary(cvfit)$cve[i] - summary(cvfit)$cve[i+1]
}
which(abs(diff) <= 0.001); print(max_index)

min_index = 36
lambdas = summary(cvfit)$lambda
assembly_l = lambdas[min_index]

# 3. fit regression
fit <- ncvfit(assembly_X, assembly_y,
              penalty = "SCAD",
              gamma = 3.7,lambda = assembly_l)
fit$beta

cv_fit <- ncvreg(assembly_X, assembly_y,
                 family="poisson", # or gaussian
                 penalty="SCAD", gamma=3.7)
cv_fit$beta



################################################
## blue
################################################
# 1. .import data
blue <- data.frame(read_excel("청와대_final(2).xlsx"))
blue_X <- blue[, seq(from = 5, to = ncol(blue), by = 1)]
blue_y <-  log(blue$count)
rm(blue)

interaction = blue_X$anger * blue_X[, 3:ncol(blue_X)]
blue_X = cbind(blue_X, interaction)

# 2. determine lambda
cvfit <- cv.ncvreg(data.frame(blue_X), blue_y, type = "cve")
plot(cvfit)

min_index = summary(cvfit)$min
lambdas = summary(cvfit)$lambda
blue_l = lambdas[min_index]


diff = c()
for(i in 1:99){
  diff[i] = summary(cvfit)$cve[i] - summary(cvfit)$cve[i+1]
}
which(abs(diff) <= 0.001)

min_index = 37
lambdas = summary(cvfit)$lambda
blue_l = lambdas[min_index]

# 3. fit regression
fit <- ncvfit(data.frame(blue_X), blue_y,
  penalty = "SCAD",
  gamma = 3.7,  lambda = blue_l)
fit$beta

cv_fit <- ncvreg(blue_X, blue_y,
                 family="poisson", # or gaussian
                 penalty="SCAD", gamma=3.7)
cv_fit$beta

