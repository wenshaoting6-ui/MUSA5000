install.packages('NbClust')
library(NbClust)
library(sf)
library(tidyverse)

data=read.csv("RegressionData.csv")
df <- data.frame(scale(data[-1:-0]))

set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans", index="all")

table(nc$Best.n[1,])

par(mfrow=c(1,1)) 
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#since we have 2 with 8 proposed and 15 with 6 proposed I will do a 2 k-mean cluster analysis and then do a 15 one
# when k=2
set.seed(1234)
fit.km <- kmeans(df, 2, nstart=25)
#Let's look at the number of observations in each cluster
fit.km$size
round(fit.km$centers, 2)
cbind(round(aggregate(data[-1:0], by=list(cluster=fit.km$cluster), mean),1),fit.km$size)



#when k=15
set.seed(1234)
fit.km <- kmeans(df, 15, nstart=25)
#Let's look at the number of observations in each cluster
fit.km$size
round(fit.km$centers, 2)
cbind(round(aggregate(data[-1:0], by=list(cluster=fit.km$cluster), mean),1),fit.km$size)
