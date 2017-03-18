library(e1071)
library(crisprpred)
library(randomForest)
library(DAAG)
library(ROCR)
alldata = read.csv('finalfeaturizedcheckins.csv')
alldata = data.frame(alldata)
alldata$stationdistance=unlist(lapply(alldata$stationdistance, function(x){if(is.infinite(x)){0}else{x}}))
trueranking = alldata[order(-alldata[,2]),]
for(i in 1:length(alldata[,1])){
  alldata$temporal[i] = max(alldata$morning[i],alldata$noon[i], alldata$afternoon[i], alldata$evening[i])
}
trueranking$rank = as.numeric(factor(-trueranking$ncheckins))
trueranking$ncheckins = NULL
trueranking$venuenames = NULL

features = names(trueranking)
fformula = featureformula(features)
#size = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80)
size = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)
len = length(alldata[,1])
cbank = c("red","blue","black","green","brown")
for(i in 1:length(size)){
  trueranking$rank[1:(size[i]*len)] = 1
  trueranking$rank[(size[i]*len+1):len] = 0
  trainingdata = sample(trueranking)
  svmRES = svm(as.formula(fformula),trainingdata, kernel = "linear", cross = 10)
  predValue = as.vector(predict(svmRES, trainingdata))
  truev = as.vector(trainingdata$rank)
  svmprediction = prediction(predValue, truev > 0.4)
  svmROC = performance(svmprediction,"tpr","fpr")
  svmPR = performance(svmprediction,"prec","rec")
  cat("i=",i,"\n")
  if(i==1){
    apred = svmprediction
    aROC = svmROC
    aPR = svmPR
  }else if(i==2){
    bpred = svmprediction
    bROC = svmROC
    bPR = svmPR
  }else if(i==3){
    cpred = svmprediction
    cROC = svmROC
    cPR = svmPR
  }else if(i==4){
    dpred = svmprediction
    dROC = svmROC
    dPR = svmPR
  }else if(i==5){
    epred = svmprediction
    eROC = svmROC
    ePR = svmPR
  }else if(i==6){
    fpred = svmprediction
    fROC = svmROC
    fPR = svmPR
  }
}

legend("bottomright",
       inset=.01,
       cex = 1,
       #title="Legend",
       c("5%","10%","15%","20%","25%","30%"),
       horiz=FALSE,
       lty=c(1,2,3,4,5,6),
       lwd=c(2,2),
       col=c("red","blue","black","green","brown","cyan"))
# pdf("svmaccuracygraph.pdf")
#dev.off ()


   plot.new()
   par(mar = c(4, 5, 1.98, 1))
 
 plot(
   svmROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1, add = TRUE, col = cbank[i]
 )

 plot.new() 
 cbank = c("red","blue","black","green","brown","cyan")
 par(mar = c(4, 5, 1.98, 1))
 plot(
   aROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[1]
 )
 plot(
   bROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[2], add= TRUE
 )
 plot(
   cROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[3], add= TRUE
 )
 plot(
   dROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[4], add= TRUE
 )
 plot(
   eROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[5], add= TRUE
 )
 plot(
   fROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[6], add= TRUE
 )
 legend("bottomright",
        inset=.01,
        cex = 1,
        #title="Legend",
        c("5%","10%","15%","20%","25%","30%"),
        horiz=FALSE,
        lty=c(1,2,3,4,5,6),
        lwd=c(2,2),
        col=c("red","blue","black","green","brown","cyan"))
 
 
 plot.new()
 cbank = c("red","blue","black","green","brown","cyan")
 par(mar = c(4, 5, 1.98, 1))
 plot(
   aPR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[1]
 )
 plot(
   bPR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[2], add= TRUE
 )
 plot(
   cPR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[3], add= TRUE
 )
 plot(
   dPR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[4], add= TRUE
 )
 plot(
   ePR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[5], add= TRUE
 )
 plot(
   fPR, main = "PR-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1,col = cbank[6], add= TRUE
 )
 
 legend("bottomleft",
        inset=.01,
        cex = 1,
        #title="Legend",
        c("5%","10%","15%","20%","25%","30%"),
        horiz=FALSE,
        lty=c(1,2,3,4,5,6),
        lwd=c(2,2),
        col=c("red","blue","black","green","brown","cyan"))
 
 #Length = 38312
 #AUROC = 0.9009666 ~ 0.9647305
 #AUPR = 0.8336043 ~ 0.636847
 #41.34 41.76 43.76 45.77 47.85 50.46
 #95.84, 91.67 87.22 82.5 77.6 72.6
 #ACCURACY = 72.6 ~ 95.84
 #Correlation = 0.6365053 ~ 0.3311955