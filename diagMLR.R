args <- read.table('something.txt',header = TRUE, sep = "\t")
filePath <-as.character(args$var[1])
csvData <- read.csv(filePath, header = TRUE)
outTxt <- as.character(args$var[7])


#MLR Summary
xDataWhole <- as.character(args$var[3])
xDataSplit <- strsplit(xDataWhole,split=",")[[1]]
print(paste("X Variables: ",xDataSplit))
yDataName <- as.character(args$var[4])
print(paste("Y Variable: ",yDataName))

#create matrix of all desired X variables. each col is a var.
xDataZeros <- vector(mode="numeric", length=nrow(csvData))
for(var in xDataSplit){
  xDataZeros <- cbind(xDataZeros,csvData[var])
  print(var)
}
xData <- xDataZeros[,-1]

#get y data
yData <- csvData[yDataName]

#concatenate strings for x formula of model
xFormula <-""
idx=1
for(var in xDataSplit){
  print(idx)
  if(idx != length(xDataSplit)){xFormula <- paste(xFormula,var,"+")}
  else{xFormula <- paste(xFormula,var)}
  idx=idx+1
}

#create model
mlrModel <- lm(as.formula(paste(yDataName,"~",xFormula)),data=csvData)

getSummary <- function(mlrModel){
  b0 <- mlrModel$coefficients[1]
  b0_pval <- summary(mlrModel)$coefficients[1,4]
  b1 <- mlrModel$coefficients[2]
  b1_pval <- summary(mlrModel)$coefficients[2,4]
  slrSummaryTable <- cbind(c("b0","b1","p-val (b0)","p-val (b0)","R^2"),
                           c(b0,b1,b0_pval,b1_pval,summary(mlrModel)$r.squared))
  return(slrSummaryTable)
}
tbl <- getSummary(mlrModel)

#predictions
alpha <- as.numeric(as.character(args$var[6]))
strListChar <- as.character(args$var[5])
splitListChar <- strsplit(strListChar,split=",")[[1]]
xh_data <- data.frame(xhList = as.numeric(splitListChar)) 
xhConf <- predict(mlrModel,setNames(xh_data,xDataName),interval='confidence',level=alpha)
xhPred <- predict(mlrModel,setNames(xh_data,xDataName),interval='predict',level=alpha)



#anova
#also find t p-values for independence
ei <- residuals(mlrModel)
nVal <- nrow(csvData)
nCoeff <- length(xDataSplit)+1 #this will be the p # of beta coefficients in model, NOT num of predictors
MSE <- sum(ei^2)/(nVal-2)
SST <- sum((yData[[1]] - mean(yData[[1]]))^2)
SSE <- MSE*(nVal-nCoeff)
SSR <- SST-SSE
MSR <- SSE/(nCoeff-1)
fStat <- SSR/MSE
pfVal <- pf(1-alpha, df1=nCoeff-1, df2=nVal-nCoeff) 
anovaTable <- cbind(c("SSE","SSR","SST","df (SSR)", "df (SSE)","MSR","MSE","F-statistic","P-Value"),
                    c(SSE,SSR,SST,nCoeff-1,nVal-nCoeff,MSR,MSE,fStat,pfVal))

#get t test p values from summary
#1-pt(t*,n-1)
ptVals <- list()
for (idx in nCoeff) {
  ptVals[idx] <- summary(mlrModel)$coefficients[idx,4]
  print(ptVals[idx])
  idx = idx + 1
}
ptTable <- cbind(xDataSplit,ptVals)


#diagnostics (numeric)

shapiro <- shapiro.test(ei)[2]
med <- median(xData[[1]]) 
group <- xData[[1]] <= med
levene <- levene.test(ei,group)[2]
numericTable <- cbind(c("Shapiro","Levene"),c(shapiro,levene))
#small p value, not constant variance
#chi square for independence?

#diagnostics (plots)
#need gridExtra Package

#do for each x var?
#do ggpair
eStar <- ei/sqrt(MSE)
scatPlot <- ggplot(data=csvData,aes(as.numeric(xData[[1]]),as.numeric(yData[[1]])))+
  geom_point() + geom_smooth(method=lm,se=F) + ggtitle("Scatter Plot")+
  xlab(xDataName)+ylab(yDataName)
resPlot <- ggplot(mlrModel,aes(x=as.numeric(xData[[1]]),y=ei))+geom_point()+
  geom_hline(yintercept=0,color='red')+ggtitle("Residual Plot")+
  xlab(xDataName)+ylab("Residuals")
outlierPlot <- ggplot(mlrModel,aes(x=as.numeric(xData[[1]]),y=eStar))+
  geom_point()+geom_hline(yintercept=0,color='red')+
  geom_hline(yintercept=c(-4,4),color='blue')+ggtitle("Residual Outlier Plot")+
  xlab(xDataName)+ylab("Semi-studentized Residuals")
normProbPlot <- ggplot(mlrModel,aes(sample=ei))+stat_qq()+stat_qq_line()+
  ggtitle("Normal Probabilty Plot")

grid.arrange(scatPlot, resPlot, outlierPlot, normProbPlot, nrow = 2)




