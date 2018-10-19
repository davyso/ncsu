Nodes=read.csv(file="./nodes.csv", colClasses=c("SKU"="factor"), header=TRUE)


# Quantile Regression for Sum ~ Degrees relationship
qs = 1:3/4
qr2 <- rq(Sum ~ Degrees, data=Nodes, tau = qs)
ggplot(Nodes, aes(Sum, Degrees)) + geom_point() + geom_quantile(quantiles = qs)

coef(qr2)

plot(summary(qr2), parm="Degrees")





qs=c(0.01,0.9)
data = data.frame(
  x=c(1,2,3,4,5,6,7,8,9,10),
  y=c(1,1,1,4,5,6,20,19,18,17))
qr <- rq(y ~ x, data=data, tau = qs)

ggplot(data, aes(x,y)) + 
  geom_point() + 
  geom_quantile(quantiles=qs)
#plot(summary(qr), parm="x")
