#R flow example for prediction of DDM-CDA-RF
#All DDMs can be trained wiht caret package (see Kunh, 2019)

#load .RData of Data-in-CDA-RF
load(file.choose())
#load .RData of Download CDA-RF DDM folder
load(file.choose())

###training adjustment of DDM-RF
print(rffim)

##testing adjustment of DDM-RF
library(caret)
est.rf <- predict(rffim)
pred.rf <- predict(rffim, newdata = test[,2:length(train)])
apply(as.matrix(pred.rf), 2, postResample, obs = test$N)

#Training residues
plot(train$N,(train$N-est.rf),pch=19,cex=.4,xlab='Standart GWL (m)', ylab='Residues', cex.lab=1.5,cex.axis=1.5)
abline(0,0)

#Testing residues
plot(test$N,(test$N-pred.rf),pch=19,cex=.4,xlab='Standart GWL (m)', ylab='Residues', cex.lab=1.5,cex.axis=1.5)
abline(0,0)

#All predictions graphs of test stage by lat
for (i in 1:length(unique(test$lat))){
  tests=subset(test, lat==unique(test$lat)[i] &
                 lon==unique(test$lon)[i])
  pred.rf <- predict(rffim, newdata =tests[,2:length(tests)[1]])
  print(apply(as.matrix(pred.rf), 2, postResample, obs = tests$N))
  plot(seq(1:length(tests[,1])),tests$N,type='l',xlab=tests$lat)
  lines(seq(1:length(tests[,1])),pred.rf,col='red')
}
