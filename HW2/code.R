library(sf)
library(spdep)
library(spgwr)
library(tmap)
library(spatialreg)
library(whitestrap)
library(lmtest)
library(tseries)

shp=read_sf("Lecture 1 - RegressionData.shp/RegressionData.shp")

shp$LNNBELPOV=log(shp$NBelPov100+1)
shp$LNMEDHHINC <- log(shp$MEDHHINC + 1)
shp$LNMEDHVAL=log(shp$MEDHVAL+1)
shp$LNPCTVACANT <- log(shp$PCTVACANT + 1)

par(oma=c(0,0,2,0)) 
par(mfrow=c(1,1)) 

#define queen martix and calculate moran's I
queen<-poly2nb(shp, row.names=shp$POLY_ID)
queenlist<-nb2listw(queen, style = 'W')
moran(shp$LNMEDHVAL, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I` 
#Permutation test
moranMC<-moran.mc(shp$LNMEDHVAL, queenlist, nsim=999, alternative="two.sided")  #We use 999 permutations
moranMC
#permutation test historgam
moranMCres<-moranMC$res
hist(moranMCres, freq=10000000, nclass=100)   #Draws distribution of Moran's I's calculated from randomly permuted values
# Here, we draw a red vertical line at the observed value of our Moran's I
abline(v=moran(shp$LNMEDHVAL, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  
#moran's i plot
moran.plot(shp$LNMEDHVAL, queenlist) 
#local moran's I'
lmoran<-localmoran(shp$LNMEDHVAL, queenlist)
head(lmoran)
df.lmoran <-cbind(shp, as.data.frame(lmoran))
library(tmap)
tmap_mode("plot")
#Obtaining the Local Moran's P-Values (two-sided)
shp$lmp <- lmoran[, "Pr(z != E(Ii))"]
library(sf)
shp <- st_make_valid(shp) #Sometimes necessary if projection is off
#Creating the LISA Clusters
mp <- moran.plot(as.vector(scale(shp$LNMEDHVAL)), queenlist)

shp$quadrant <- NA
# high-high
shp[(mp$x >= 0 & mp$wx >= 0) & (shp$lmp <= 0.05), "quadrant"]<- 1
# low-low
shp[(mp$x <= 0 & mp$wx <= 0) & (shp$lmp <= 0.05), "quadrant"]<- 2
# high-low
shp[(mp$x >= 0 & mp$wx <= 0) & (shp$lmp <= 0.05), "quadrant"]<- 3
# low-high
shp[(mp$x <= 0 & mp$wx >= 0) & (shp$lmp <= 0.05), "quadrant"]<- 4
# non-significant
shp[(shp$lmp > 0.05), "quadrant"] <- 5


# LISA P-Value Map
p_vals <- tm_shape(shp) +
  tm_polygons(col = "lmp", title = "",
              breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
              palette = c("darkblue", "blue", "lightblue", "white")) +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 1,
    legend.title.size = 1,
    fontfamily = "Arial",
    title = "LISA P-Value Map",
    title.size = 1.2,
    frame = FALSE
  )

# LISA Cluster Map
clusters <- tm_shape(shp) +
  tm_fill(col = "quadrant", title = "",
          breaks = c(1, 2, 3, 4, 5, 6),
          palette = c("red", "blue", "lightpink", "skyblue2", "white"),
          labels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non-significant")) +
  tm_borders(alpha = 0.5) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.text.size = 1,
    legend.title.size = 1,
    fontfamily = "Arial",
    title = "LISA Cluster Map",
    title.size = 1.2
  )

clusters
p_vals


#run the regression
reg<-lm(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV, data=shp)
summary(reg)
#save the residual
standardised<-rstandard(reg)
shp$standardised <- standardised 
#get the surrounding residual
resnb<-sapply(queen, function(x) mean(standardised[x]))
#run the regression between residual and surrounding residual
res.lm <- lm(formula=standardised ~ resnb)
summary(res.lm)
#calculate the moran's i
moran(shp$standardised, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I` 
#permutation test
moranMC2<-moran.mc(standardised, queenlist, 999, alternative="two.sided")
#moran's i plot
moran.plot(standardised, queenlist)
#permutation test plot
OLS<-moranMC2$res
hist(OLS, freq=10000000, nclass=100) 
abline(v=moran(shp$standardised, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  





#spatial lag regression
lagreg<-lagsarlm(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV, data=shp, queenlist)
summary(lagreg)
#Moran'i for spatial residuals and permutation test
reslag<-lagreg$residuals
lagMoranMc<-moran.mc(reslag, queenlist,999, alternative="two.sided")
lagMoranMc
#moran's i plot
moran.plot(reslag, queenlist)
#permutation plot
Spatial_lag_regression_residuals<-lagMoranMc$res
hist(Spatial_lag_regression_residuals, freq=10000000, nclass=100) 
abline(v=moran(reslag, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  





#spatial error regression
errreg<-errorsarlm(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV, data=shp, queenlist)
reserr<-residuals(errreg)
errresnb<-sapply(queen, function(x) mean(reserr[x]))
summary(errreg)
#Moran's I and permutation test
errMoranMc<-moran.mc(reserr, queenlist, 999, alternative="two.sided")
errMoranMc
#moran's i plot
moran.plot(reserr, queenlist)
#permutation plot
Spatial_error_regression_residuals<-errMoranMc$res
hist(Spatial_error_regression_residuals, freq=10000000, nclass=100) 
abline(v=moran(reserr, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  





#GWR
#Setting an adaptive bandwidth
shps <- as(shp, 'Spatial')  #These analyses are easier to do when the data are of the SpatialPolygonsDataFrame class
class (shps)
bw<-gwr.sel(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV, 
            data=shps,
            method = "aic",
            adapt = TRUE)

#setting the fixed bandwidth
bw_fixed<-gwr.sel(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV, 
                  data=shps,
                  method = "aic",
                  adapt = FALSE)


#adaptive bandwidth GWR
gwrmodel<-gwr(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV,
              data=shps,
              adapt = bw, #adaptive bandwidth determined by proportion of observations accounted for
              gweight=gwr.Gauss,
              se.fit=TRUE, #to return local standard errors
              hatmatrix = TRUE)
gwrmodel

#local r2 map
shps_sf <- st_as_sf(shps)
gwrresults<-as.data.frame(gwrmodel$SDF)
shps_sf$coefPCTBACHMORst<-gwrresults$PCTBACHMOR/gwrresults$PCTBACHMOR_se
shps_sf$coefPCTVACANTst<-gwrresults$PCTVACANT/gwrresults$PCTVACANT_se
shps_sf$coefLNNBELPOVst<-gwrresults$LNNBELPOV/gwrresults$LNNBELPOV_se
shps_sf$coefPCTSINGLESst<-gwrresults$PCTSINGLES/gwrresults$PCTSINGLES_se

shps$gwrE<-gwrresults$gwr.e
shps$localR2<-gwrresults$localR2



tm_shape(shps_sf)+
  tm_fill(col='localR2',  breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8,0.9), n=5, palette = 'Blues')+
  tm_layout(frame=FALSE)

#Moran's I and permutation test
resgwr<-gwrmodel$SDF$gwr.e
gwrMoranMc<-moran.mc(resgwr, queenlist, 999, alternative="two.sided")
gwrMoranMc
#moran's i plot
moran.plot(resgwr, queenlist)
#permutation plot
GWR_residuals<-gwrMoranMc$res
hist(GWR_residuals, freq=10000000, nclass=100) 
abline(v=moran(resgwr, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  

#Coefficient map
coefPCTBACHMOR<-tm_shape(shps_sf)+
  tm_fill(col='coefPCTBACHMORst', breaks=c(-Inf, -2, 0, 2, Inf), title='Standardized coefficient of Pct of Bachelor', 
          palette ='RdBu')+
  tm_layout(frame=FALSE, title = 'Percentage of Bachelor Degree')
coefPCTBACHMOR

coefPCTVACANT<-tm_shape(shps_sf)+
  tm_fill(col='coefPCTVACANTst', breaks=c(-Inf, -2, 0, 2, Inf), title='Standardized coefficient of PCTVACANT', 
          palette='RdBu')+
  tm_layout(frame=FALSE, title = 'Percentage of Housing Vacant')
coefPCTVACANT

coefLNNBELPOV<-tm_shape(shps_sf)+
  tm_fill(col='coefLNNBELPOVst', breaks=c(-Inf, -2, 0, 2, Inf), title='Standardized coefficient of LNNBELPOV', 
          palette='RdBu')+
  tm_layout(frame=FALSE, title = 'Number of Household Below Poverty (Log)')
coefLNNBELPOV

coefPCTSINGLES<-tm_shape(shps_sf)+
  tm_fill(col='coefPCTSINGLESst', breaks=c(-Inf, -2, 0, 2, Inf), title='Standardized coefficient of PCTSINGLES', 
          palette='RdBu')+
  tm_layout(frame=FALSE, title = 'Percentage of Single Family Houses')
coefPCTSINGLES

tmap_arrange(coefPCTBACHMOR, coefPCTVACANT, coefLNNBELPOV, coefPCTSINGLES, ncol=4)








#fixed bandwidth
gwrmodel_fixed<-gwr(formula=LNMEDHVAL ~ PCTBACHMOR + PCTVACANT + PCTSINGLES + LNNBELPOV,
                    data=shps,
                    bandwidth = bw_fixed, #fixed bandwidth
                    gweight=gwr.Gauss,
                    se.fit=TRUE, #to return local standard errors
                    hatmatrix = TRUE)
gwrmodel_fixed

