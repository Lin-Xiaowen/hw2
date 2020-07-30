getwd()
setwd('/Users/Katherine/R')

library(quantmod)
library(openxlsx)
stock <- read.xlsx('stockid.xlsx')

stockdata <- data.frame(date=1:82)
stockdata[, 1] <- read.xlsx('time.xlsx')

covid19_data_raw <- read.csv('COVID-19.csv', header = TRUE)
covid19_data <- covid19_data_raw[1:183,c(1, 20, 23)]



for(i in 2:31){
  setSymbolLookup(TEMP=list(name=stock$id[i]))
  getSymbols("TEMP", warnings=F, from='2020-02-01',to='2020-06-01')
  TEMP <- as.data.frame(TEMP)
  stockdata[, i] <- TEMP[, 2]
  colnames(stockdata)[i] <- substring(colnames(TEMP)[1], 1, nchar(colnames(TEMP)[1])-5)
}

stockdata <- as.data.frame(stockdata)
stockdata_2 <- stockdata
stockdata_2$deathIncrease <- c(1:82)
stockdata_2$positiveIncrease <- c(1:82)


for(i in 1:nrow(stockdata_2)){
  date1 <- stockdata_2[i, 1]
  for(j in 1:nrow(covid19_data)){
    date2 <- covid19_data[j, 1]
    if(date1 == date2){
      stockdata_2$deathIncrease[i] <- covid19_data$deathIncrease[j]
      stockdata_2$positiveIncrease[i] <- covid19_data$positiveIncrease[j]
    }
  }
}

pdf(file = "Scatter_plot.pdf")
for(i in 2:(ncol(stockdata_2)-2)){
  plot(x = stockdata_2[,i], y=stockdata_2$deathIncrease, xlab = colnames(stockdata_2)[i], ylab = 'deathIncrease')
  plot(x = stockdata_2[,i], y=stockdata_2$positiveIncrease, xlab = colnames(stockdata_2)[i], ylab = 'positiveIncrease')
  
}
dev.off()


cormatrix <- stockdata_2[1, 2:30]
for(i in 1:ncol(cormatrix)){
  cor_death <- cor(stockdata_2[, i+1], stockdata_2$deathIncrease)
  cormatrix[1, i] <- cor_death
}
rownames(cormatrix)[1] <- 'deathIncrease'

for(i in 1:ncol(cormatrix)){
  cor_positive <- cor(stockdata_2[, i+1], stockdata_2$positiveIncrease)
  cormatrix[2, i] <- cor_positive
}
rownames(cormatrix)[2] <- 'positiveIncrease'

cormatrix <-na.omit(t(as.matrix.data.frame(cormatrix)))

stockdata_1 <- as.data.frame(t(na.omit(t(stockdata))))[, 2:(ncol(stockdata)-1)]
res <- cor(stockdata_1)
library(corrplot)
corrplot(res, method = "shade",shade.col = NA, tl.col ="black", tl.srt = 45, order = "AOE")
corrplot(res, method = "shade",shade.col = NA, tl.col ="black", tl.srt = 45, order = "hclust")

library(pheatmap)
pheatmap(res, display_numbers = FALSE)


means <- sapply(stockdata_1, mean)
SD <- sapply(stockdata_1, sd)
dataScale <- scale(stockdata_1,center=means,scale=SD) 
Dist <- dist(dataScale,method="euclidean")
heatmap(as.matrix(Dist),labRow = F,labCol = F)  

ClusteModel <- hclust(Dist,method = "ward.D")
plot(ClusteModel, hang = -1,labels=FALSE)


