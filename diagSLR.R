args <- read.table('something.txt',header = TRUE, sep = "\t")
filePath <-as.character(args$var[1])
csvData <- read.csv(filePath, header = TRUE)
args <- read.table('something.txt',header = TRUE, sep = "\t")
outTxt <- as.character(args$var[7])


#SLR Summary
xDataName <- as.character(args$var[3])
print(paste("X Variable: ",xDataName))
yDataName <- as.character(args$var[4])
print(paste("Y Variable: ",yDataName))

xData <- csvData[xDataName]
yData <- csvData[yDataName]
slrModel <- lm(as.formula(paste(yDataName,"~",xDataName,sep="")),data=csvData)

getSummary <- function(slrModel){
  b0 <- slrModel$coefficients[1]
  b0_pval <- summary(slrModel)$coefficients[1,4]
  b1 <- slrModel$coefficients[2]
  b1_pval <- summary(slrModel)$coefficients[2,4]
  slrSummaryTable <- cbind(c("b0","b1","p-val (b0)","p-val (b0)","R^2"),
                         c(b0,b1,b0_pval,b1_pval,summary(slrModel)$r.squared))
  return(slrSummaryTable)
}


#predictions
alpha <- as.numeric(as.character(args$var[6]))
strListChar <- as.character(args$var[5])
splitListChar <- strsplit(strListChar,split=",")[[1]]
xh_data <- data.frame(xhList = as.numeric(splitListChar)) 
xhConf <- predict(slrModel,setNames(xh_data,xDataName),interval='confidence',level=alpha)
xhPred <- predict(slrModel,setNames(xh_data,xDataName),interval='predict',level=alpha)



#anova
ei <- residuals(slrModel)
nVal <- nrow(csvData)
MSE <- sum(ei^2)/(nVal-2)
SST <- sum((yData - mean(yData))^2)
SSE <- MSE*(nVal-2)
SSR <- SST-SSE #here the SSR=MSR because df=1
fStat <- SSR/MSE
fHat <- qf(1-alpha, df1=1, df2=nVal-2) 
anovaTable <- cbind(c("SSE","SSR","SST","df (SSR)", "df (SSE)","MSR","MSE","F-statistic"),
                    c(SSE,SSR,SST,1,nVal-2,SSR,fStat))
#return(anovaTable)


#diagnostics (numeric)

shapiro <- shapiro.test(ei)[2]
med <- median(xData) 
group <- xData <= med
levene <- levene.test(ei,group)[2]
numericTable <- cbind(c("Shapiro","Levene"),c(shapiro,levene))
#small p value, not constant variance
#chi square for independence?

#diagnostics (plots)
#need gridExtra Package

eStar <- ei/sqrt(MSE)
scatPlot <- ggplot(data=csvData,aes(as.numeric(xData[[1]]),as.numeric(yData[[1]])))+
          geom_point() + geom_smooth(method=lm,se=F) + ggtitle("Scatter Plot")+
          xlab(xDataName)+ylab(yDataName)
resPlot <- ggplot(slrModel,aes(x=as.numeric(xData[[1]]),y=ei))+geom_point()+
          geom_hline(yintercept=0,color='red')+ggtitle("Residual Plot")+
          xlab(xDataName)+ylab("Residuals")
outlierPlot <- ggplot(slrModel,aes(x=as.numeric(xData[[1]]),y=eStar))+
          geom_point()+geom_hline(yintercept=0,color='red')+
          geom_hline(yintercept=c(-4,4),color='blue')+ggtitle("Residual Outlier Plot")+
          xlab(xDataName)+ylab("Semi-studentized Residuals")
normProbPlot <- ggplot(slrModel,aes(sample=ei))+stat_qq()+stat_qq_line()+
          ggtitle("Normal Probabilty Plot")






