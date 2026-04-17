library(aod)
library(ggplot2)
library(rms)
library(gmodels)
library(nnet)
library(DAAG)
library(ROCR)
library(xtable)
library(ResourceSelection)
library(stargazer)
library(Hmisc)

mydata <- read.csv("Logistic Regression Data.csv")

#proportion of crashes that involved adrunk driver
DRINKING_D.tab <- table(mydata$DRINKING_D)
prop.table(DRINKING_D.tab)



#Chi-Square test and cross tabulations
CrossTable(mydata$DRINKING_D,mydata$FATAL_OR_M, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$OVERTURNED, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$CELL_PHONE, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$SPEEDING, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$AGGRESSIVE, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$DRIVER1617, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRINKING_D,mydata$DRIVER65PLUS, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)

# Build the summary table manually from the CrossTable output
summary_table <- data.frame(
  Variable = c("FATAL_OR_M", "OVERTURNED", "CELL_PHONE", "SPEEDING",
               "AGGRESSIVE", "DRIVER1617", "DRIVER65PLUS"),
  Description = c(
    "Crash resulted in fatality or major injury",
    "Crash involved an overturned vehicle",
    "Driver was using cell phone",
    "Crash involved speeding car",
    "Crash involved aggressive driving",
    "Crash involved at least one driver who was 16 or 17 years old",
    "Crash involved at least one driver who was at least 65 years old"
  ),
  N_no_alcohol   = c(1181, 612, 426, 1261, 18522, 674, 4237),
  Pct_no_alcohol = c(2.89, 1.50, 1.04, 3.09, 45.31, 1.65, 10.37),
  N_alcohol      = c(188, 110, 28, 260, 916, 12, 119),
  Pct_alcohol    = c(7.57, 4.43, 1.13, 10.46, 36.86, 0.48, 4.79),
  N_total        = c(1369, 722, 454, 1521, 19438, 686, 4356)
)

# View the table
print(summary_table)

# Group means and SDs
tapply(mydata$PCTBACHMOR, mydata$DRINKING_D, mean)
tapply(mydata$PCTBACHMOR, mydata$DRINKING_D, sd)
tapply(mydata$MEDHHINC, mydata$DRINKING_D, mean)
tapply(mydata$MEDHHINC, mydata$DRINKING_D, sd)

# t-tests
t.test(mydata$PCTBACHMOR ~ mydata$DRINKING_D)
t.test(mydata$MEDHHINC ~ mydata$DRINKING_D)




# Build correlation matrix
vars <- mydata[, c("FATAL_OR_M","OVERTURNED","CELL_PHONE","SPEEDING",
                   "AGGRESSIVE","DRIVER1617","DRIVER65PLUS","PCTBACHMOR","MEDHHINC")]

corr_matrix <- rcorr(as.matrix(vars))
r <- corr_matrix$r
p <- corr_matrix$P

stargazer(r,
          title = "Pearson Correlation Matrix",
          type = "html",
          digits = 2,
          out = "correlation_table.html")









#logit model
mylogit1 <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING +
                  AGGRESSIVE + DRIVER1617 + DRIVER65PLUS + PCTBACHMOR + MEDHHINC,
                data = mydata, family = binomial)

output1=summary(mylogit1)

# Odds ratios and 95% CI merged matrix
or_ci=exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
finalloigit1=cbind(output1$coefficients,or_ci)
finalloigit1








#sensitivity analysis
predicted <- predict(mylogit1, type = "response")
cutoffs <- c(0.02, 0.03, 0.05, 0.07, 0.08, 0.09, 0.10, 0.15, 0.20, 0.50)
sen=data.frame(cutoffs=cutoffs,
               sensitivity     = NA,
               specificity     = NA,
               misclass_rate   = NA)

for (i in seq_along(cutoffs)) {
  cut = cutoffs[i]
  
  predicted_bin = ifelse(predicted >= cut, 1, 0) 
  
  TP <- sum(mydata$DRINKING_D == 1 & predicted_bin == 1)
  FN <- sum(mydata$DRINKING_D == 1 & predicted_bin == 0)
  TN <- sum(mydata$DRINKING_D == 0 & predicted_bin == 0)
  FP <- sum(mydata$DRINKING_D == 0 & predicted_bin == 1)
  
  sen$sensitivity[i]   = TP / (TP + FN)
  sen$specificity[i]   = TN / (TN + FP)      
  sen$misclass_rate[i] = (FP + FN) / (TP + FN + TN + FP)
}

write.csv(sen,"sensitivity_table.csv")










#Roc 
library(pROC)
roc1 <- roc(mydata$DRINKING_D, predicted)
# Plot ROC curve
plot(roc1,
     col = "blue",
     lwd = 2,
     main = "ROC Curve – Full Model",
     print.thres = "best",       # marks optimal cut-off
     print.auc = TRUE)           # prints AUC on plot

plot(roc1, col = "blue", lwd = 2, main = "ROC Curve – Full Model",
     print.thres = "best", print.auc = TRUE)
dev.off()

# AUC
auc(roc1)








#logit2
mylogit2 <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING +
                  AGGRESSIVE + DRIVER1617 + DRIVER65PLUS,
                data = mydata, family = binomial)

output2=summary(mylogit2)

# Odds ratios and 95% CI merged matrix
or_ci2=exp(cbind(OR = coef(mylogit2), confint(mylogit2)))
finalloigit2=cbind(output2$coefficients,or_ci2)
finalloigit2



# AIC comparison
AIC(mylogit1, mylogit2)
