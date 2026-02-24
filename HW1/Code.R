library(dplyr)
library(stargazer)
library(sf)
library(MASS)
#install.packages("DAAG")
library(DAAG)
data=read.csv("RegressionData.csv")
output=stargazer(data,type = "text")

#as the min value for PCTBACHMOR, PCTVACANT,PCTSINGLES, and NBELPOV100 is zero, we are going to use log(1+VAR) for them

data=data%>%
   mutate(
     LNMEDHVAL=log(MEDHVAL),
     LNPCBACHMORE=log(PCTBACHMOR+1),
     LNNBELPOV100=log(NBELPOV100+1),
     LNPCTVACANT=log(PCTVACANT+1),
     LNPCTSINGLES=log(PCTSINGLES+1)
   )

variable=c("MEDHVAL","PCTBACHMOR","NBELPOV100","PCTVACANT","PCTSINGLES")

for (v in variable) {
  hist(data[[v]], main = paste("Histogram of", v), xlab = v)
}

lnvariable=c("LNMEDHVAL","LNPCBACHMORE","LNNBELPOV100","LNPCTVACANT","LNPCTSINGLES")

for (v in lnvariable) {
 hist(data[[v]], main = paste("Histogram of", v), xlab = v)
}


finalvariable=c("LNNBELPOV100","PCTBACHMOR","PCTVACANT","PCTSINGLES")

dev.off() 
par(mfrow=c(2,2))
for (v in finalvariable) {
  plot(data$LNMEDHVAL,data[[v]],xlab = "LNMEDHVAL",ylab = v)
}

cor_data=data%>% 
  select("LNNBELPOV100","PCTBACHMOR","PCTVACANT","PCTSINGLES")
cor(cor_data)


data_sf=st_read("shapefile/RegressionData.shp")


r1=lm(LNMEDHVAL~PCTVACANT+LNNBELPOV100+PCTBACHMOR+PCTSINGLES,data=data)
summary(r1)
anova(r1)

data$fittedvalue=fitted(r1)
data$residual=residuals(r1)
data$stdresidual=rstandard(r1)

par(mfrow=c(1,1))
plot(data$fittedvalue,data$stdresidual)

stepmodel=step(r1)
stepmodel$anova

fullmodel=CVlm(
  data=data,
  form.lm = formula(LNMEDHVAL~PCTVACANT+LNNBELPOV100+PCTBACHMOR+PCTSINGLES),
  m=5)

partmodel=CVlm(
  data=data,
  form.lm = formula(LNMEDHVAL~PCTVACANT+MEDHHINC),
  m=5)
