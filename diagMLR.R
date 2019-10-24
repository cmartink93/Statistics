csvData <- read.csv(file = "~/Desktop/applied_regression/diamonds.csv", header = TRUE)
args <- read.table('something.txt',header = TRUE, sep = "\t")

#SLR Summary
xDataName <- as.character(args$var[3])
yDataName <- as.character(args$var[4])

xData <- csvData[xDataName]
yData <- csvData[yDataName]
slrModel <- lm(yData~xData,data=csvData) #should be sum of x vars
#as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))

b0 <- slrModel$coefficients[1]
b0_pval <- summary(slrModel)$coefficients[1,4]
b1 <- slrModel$coefficients[2]
b1_pval <- summary(slrModel)$coefficients[2,4]
#find up to b_p
slrSummaryTable <- cbind(c("b0","b1","p-val (b0)","p-val (b0)","R^2"),
                         c(b0,b1,b0_pval,b1_pval,summary(slrModel)$r.squared))


#predictions
alpha <- 0.95#as.numeric(args[6])
xh_data <- data.frame(xData=c(4)) #data.frame(xData=c(as.numeric(args[5])))
predict(slrModel,xh_data,interval='confidence',level=alpha)
predict(slrModel,xh_data,interval='predict',level=alpha)
predictionTable <- rbind(c(""))

#anova
ei <- residuals(slrModel)
p <- 2
nVal <- nrow(csvData)
MSE <- sum(ei^2)/(nVal-2)
SST <- sum((yData - mean(yData))^2)
SSE <- MSE*(nVal-2)
SSR <- SST-SSE 
MSR <- SSR/(p-1)
fStat <- SSR/MSE
fHat <- qf(1-alpha, df1=p-1, df2=nVal-2) 
anovaTable <- cbind(c("SSE","SSR","SST","df (SSR)", "df (SSE)","MSR","MSE","F-statistic","P-value"),
                    c(SSE,SSR,SST,p-1,nVal-2,MSR,MSE,fStat,fHat))


#diagnostics (numeric)

shapiro <- shapiro.test(ei)[2]
med <- median(xData) 
group <- xData <= med
levene <- levene.test(ei,group)
#small p value, not constant variance
#chi square for independence?

#diagnostics (plots)
#need gridExtra Package
library(gridExtra)
eStar <- ei/sqrt(MSE)
scatPlot <- ggplot(slrModel,aes(x=xData,y=yData))+geom_point()
resPlot <- ggplot(slrModel,aes(x=xData,y=ei))+geom_point()+geom_hline(yintercept=0,color='red')
outlierPlot <- ggplot(slrModel,aes(x=xData,y=eStar))+geom_point()+geom_hline(yintercept=0,color='red')+geom_hline(yintercept=c(-4,4),color='blue')
normProbPlot <- ggplot(slrModel,aes(sample=ei))+stat_qq()+stat_qq_line()

pdf(str(args[7]) + "diagPlots.pdf") 
diagPlots <- grid.arrange(scatPlot, resPlot, outlierPlot, normProbPlot, nrow = 2)
dev.off()




