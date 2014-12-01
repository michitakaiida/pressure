d<-read.csv("output.txt")
d<-d[7:length(d$X1),]　//余計なテストデータの削除
names(d) <- c('id','id2',"--","-","condition","timestamp","press")

condition.glm <- glm(condition~press, data=d, family=binomial)
summary(condition.glm)
plot(d$press,d$condition)
lines(d$press, fitted(condition.glm), col="blue")

x <- seq(min(d$press), max(d$press), by = 0.01)
eta.pred <- condition.glm$coefficients["(Intercept)"] +
+ condition.glm $coefficients["press"]*x
p.pred <- 1/(1 + exp(-eta.pred))
lines(x, p.pred) 



xaxis1 <- condition.glm $linear.predictor 
xaxis2 <- seq(from=min(xaxis1), to=max(xaxis1), length=200)　
yaxis2 <- exp(xaxis2)/(1+exp(xaxis2))
plot(xaxis1, d$condition)
lines(xaxis2, yaxis2) 

