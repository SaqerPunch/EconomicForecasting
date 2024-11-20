install.packages("ISLR")
library(ISLR)
data("College")
fix(College)

college = na.omit(College)

install.packages("glmnet")
library(glmnet)

x = model.matrix(Apps~Private+Accept, College)[,-1]
y = College$Apps
summary(x)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]


lm.fit=lm(Apps~., data=College, subset=train)
summary(lm.fit)
mean((summary(lm.fit)$residuals^2))
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
bestlam =cv.out$lambda.min

ridge.mod=glmnet(x,y, alpha=0, lambda =bestlam)
coef(ridge.mod)
predict(ridge.mod, type="coefficients")
ridge.pred=predict (ridge.mod, newx=x[test ,])
mean((ridge.pred -y.test)^2)


lasso.mod=glmnet(x,y, alpha=0, lambda =bestlam)
coef(lasso.mod)
predict(lasso.mod, type="coefficients")
lasso.pred=predict (lasso.mod, newx=x[test ,])
mean((lasso.pred -y.test)^2)


