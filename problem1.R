# Visualization of the BAS bodyfat data
#
#install.packages("ggplot2")
library(ggplot2)
#install.packages("BAS")
library(BAS)
library(glmnet)

# prepare data
data(bodyfat)
sn=252
bodyfat <- bodyfat[ , ! names(bodyfat) %in% c("Density")] 
for(i in 1:ncol(bodyfat)) {      
  #beta0[i] = mean(bodyfat[ , i])
  bodyfat[ , i] <- bodyfat[ , i] - mean(bodyfat[ , i]) 
}

bf = data.matrix(bodyfat)
bft = t(bf)
cov_bf = bft%*% bf/(sn-1)

ev <- eigen(cov_bf)
# extract components
(values <- ev$values)
(vectors <- ev$vectors)

pca_data = bf %*% t(vectors)
print(pca_data[,1:4])
x = pca_data[,2:4]
y = pca_data[,1]

lambdas <- 10^seq(3, -3, by = -.1)

# L2
cv_ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
optimal_lambda = .1
ridge_reg = glmnet(x, y, nlambda = 1, alpha = 0, family = 'gaussian', lambda = optimal_lambda)
pred_y <- predict(ridge_reg, s = optimal_lambda, newx = x)
print(ridge_reg$beta)
(rms = sqrt(sum((y-pred_y)^2)/sn))

# L1
cv_L1 <- cv.glmnet(x, y, alpha = 1, lambda = lambdas)
optimal_lambda = .1
ridge_L1 = glmnet(x, y, nlambda = 1, alpha = 1, family = 'gaussian', lambda = optimal_lambda)
pred_y <- predict(ridge_L1, s = optimal_lambda, newx = x)
print(ridge_L1$beta)
(rms = sqrt(sum((y-pred_y)^2)/sn))

bf_svd <- svd(bf)
vectors = bf_svd$u

svd_data = bf %*% t(vectors)
print(svd_data[,1:4])
x = svd_data[,2:4]
y = svd_data[,1]

# L2
cv_ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
optimal_lambda = .1
ridge_reg = glmnet(x, y, nlambda = 1, alpha = 0, family = 'gaussian', lambda = optimal_lambda)
pred_y <- predict(ridge_reg, s = optimal_lambda, newx = x)
print(ridge_reg$beta)
(rms = sqrt(sum((y-pred_y)^2)/sn))

# L1
cv_L1 <- cv.glmnet(x, y, alpha = 1, lambda = lambdas)
optimal_lambda = .1
ridge_L1 = glmnet(x, y, nlambda = 1, alpha = 1, family = 'gaussian', lambda = optimal_lambda)
pred_y <- predict(ridge_L1, s = optimal_lambda, newx = x)
print(ridge_L1$beta)
(rms = sqrt(sum((y-pred_y)^2)/sn))
