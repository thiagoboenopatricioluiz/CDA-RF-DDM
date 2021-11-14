#R flow example for prediction of DDM-CDA-RF
#All DDMs can be trained wiht caret package (see Kunh, 2019)

#load .RData of Data-in-CDA-RF
load(file.choose())
#load .RData of Data-out-CDA-RF-model folder
load(file.choose())

###training adjustment of DDM-ANN
print(rnafim)

##testing adjustment of DDM-ANN
library(caret)
est.rna <- predict(rnafim)
pred.rna <- predict(rnafim, newdata = test[,2:length(train)])
apply(as.matrix(pred.rna), 2, postResample, obs = test$N)

#Training residues
plot(train$N,(train$N-est.rna),pch=19,cex=.4,xlab='Standart GWL (m)', ylab='Residues', cex.lab=1.5,cex.axis=1.5)
abline(0,0)

#Testing residues
plot(test$N,(test$N-pred.rna),pch=19,cex=.4,xlab='Standart GWL (m)', ylab='Residues', cex.lab=1.5,cex.axis=1.5)
abline(0,0)

#All predictions graphs of test stage by lat
for (i in 1:length(unique(test$lat))){
  tests=subset(test, lat==unique(test$lat)[i] &
                 lon==unique(test$lon)[i])
  pred.rna <- predict(rnafim, newdata =tests[,2:length(tests)[1]])
  print(apply(as.matrix(pred.rna), 2, postResample, obs = tests$N))
  plot(seq(1:length(tests[,1])),tests$N,type='l',xlab=tests$lat)
  lines(seq(1:length(tests[,1])),pred.rna,col='red')
}
