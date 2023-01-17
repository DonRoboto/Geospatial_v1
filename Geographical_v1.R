



library(maptools)
library(sp)
library(readr)
library(RColorBrewer)

setwd("~/ENIGH")

dmap2 <- readShapePoly("~/ENIGH/shp_file/México_Estados.shp")  # Save only Geometry file from ESRI shapefile
dmap.csv <- read_csv("~/ENIGH/df_mxstate_2020.csv")   # Save Attributes file from CSV file; use the right encoding for local language encoding="UTF-8")
dmap.csv['CODIGO'] <- paste('MX', dmap.csv $region, sep = "")


dmap.sp <- merge(dmap2,dmap.csv)    # Integrate geometry with attributes
#save(dmap.sp, file="dmap.RData")    # Save the integrated data as RData

#par(mfrow=c(1,2))  # Generate a graphical layout with 1 row and 2 columns

# Mapping for polygon 
plot(dmap2, pbg="blue", axes = T)  # axes: add X and Y axis to the plot
title("Spatial polygon plot")   # Add title

# If you show a graph in a separate window
# options(device = "quartz")  # for Mac
# options(device = "windows")  # for Windows

dev.off()  # Remove plot



###############
#Choropleth Map
##############
my_colors <- brewer.pal(9, "Blues") 
my_colors <- colorRampPalette(my_colors)(30)

# Attribute the appropriate color to each country
class_of_country <- cut(dmap.sp@data$salud_2020/dmap.sp@data$ing_disponible_2020, 30)
my_colors <- my_colors[as.numeric(class_of_country)]

# Make the plot
plot(dmap.sp ,  col=my_colors)


###############
#spatial neighbor
##############
# (1) Adjacency-based spatial neighbor list
mexicoq_nb <- poly2nb(dmap.sp, queen= TRUE)  # Using SpatialPolygonsDataFrame; queen type
summary(mexicoq_nb)   # View how spatial neighbor list is constructed
mexicor_nb <- poly2nb(dmap.sp, queen= FALSE)  # Using SpatialPolygonsDataFrame; rook type
summary(mexicor_nb)   # View how spatial neighbor list is constructed

# (2) Graph-based spatial neighbor list
coords <- coordinates(dmap.sp)   # Get coordinates
ID <- row.names(as(dmap.sp, "data.frame"))   # Get IDs
mexico1_nb <- tri2nb(coords, row.names = ID) # triangulation type
mexico2_nb <- graph2nb(gabrielneigh(coords), row.names = ID) # Gabriel type
mexico3_nb <- graph2nb(relativeneigh(coords), row.names = ID) # Relative type

# (3) Distance-based spatial neighbor list (k-nearest neighbor)
mexico4_nb <- knn2nb(knearneigh(coords, k = 2), row.names = ID) # K=2


# Let's compare all the weight structures visually
par(mfrow=c(2,3))

# Queen type
plot(dmap.sp)
plot(mexicoq_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("Queen")


# Rook type
plot(dmap.sp)
plot(mexicor_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("Rook")

# Triangulation type
plot(dmap.sp)
plot(mexico1_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("Triangulation")

# Gabriel type
plot(dmap.sp)
plot(mexico2_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("Gabriel")

# Relative type
plot(dmap.sp)
plot(mexico3_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("Relative")

## K-nearest type
plot(dmap.sp)
plot(mexico4_nb, coords=coordinates(dmap.sp), pch=19, cex=0.1, col="blue", add=T)
title("K-nearest(k=2)")



## 2. Moran's I

# To create Moran's I, spatial weight matrix needs to be created first.

library(spdep)
d.W <- poly2nb(dmap.sp, queen=FALSE)  # Create a queen-type neighbor list
W <- nb2listw(d.W)               # Use the list to create spatial weight matrix
summary(W)                       # View how spatial weight matrix is constructed

# Calculate Moran's I for the variable "Out-of-pocket health expenditure"
# Get the variable "Out-of-pocket health expenditure" from st data (SpatialpolygonsData Frame)
V1 = dmap.sp$salud_2020/dmap.sp$ing_disponible_2020

# Moran's I Test
moran.test(V1, W, randomisation=TRUE, alternative="two.sided")
# Moran I = 0.13 (P<.001) confirms a significant level of spatial autocorrelation

# Let's visualize Moran's I plot
dev.off()
moran.plot(V1, W, pch=19, cex=0.5, col="red", xlab="health_expend", ylab="lagged health_expend", main ="Moran Scatter plot")
# High-high area: ID 1, 7, 15


## 3. LISA (Local Index of Spatial Autocorrelation)

listw <- nb2listw(d.W)  # Get neigbor list from spatial weight matrix 
lisa <- localmoran(V1, listw) # Calculate LISA for each local area with hypothesis test
summary(lisa)  # Check results

# For LISA map, standardize the variable and calculate spatial lag variable (mean of X's neigbors)
sV1 <-scale(V1)   # standardization of a variable
lag_sV1 <- lag.listw(listw, sV1)  # spatial lag variable
summary(sV1)
summary(lag_sV1)

# Using gbs variable and its lagged variable to identify high-high, low-low, high-low, low-high areas
quadrant <- vector(mode="numeric",length=nrow(lisa)) 
quadrant[sV1>0 & lag_sV1>0] <- 1    # high-high
quadrant[sV1<0 & lag_sV1<0] <- 2    # low-low  
quadrant[sV1>0 & lag_sV1<0] <- 3    # high-low
quadrant[sV1<0 & lag_sV1>0] <- 4    # low-high

# Determine level of significance for LISA testing (95% or 99%) 
signif <- 0.05 

# Add the areas with no statistical significance
quadrant[lisa[, 5]> signif] <- 5 

# Create a LISA map
colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95)) 
par(mar=c(0,0,1,0)) # sets margin parameters for plot space 
plot(dmap2, border="grey", col=colors[quadrant], 
     main = "LISA Cluster Map, household spending on health") 
legend("bottomright",legend=c("high-high","low-low","high-low","low-high"), 
       fill=colors, bty="n", cex=0.7, y.intersp=1, x.intersp=1) 

dev.off() 


# Another metric: local G-Statistics
library(spdep)
lgg = localG(V1,W)   # calculate local G-stat in each area

# Perform K-means clustering analysis for local G-Statistics
library(classInt)
par(family="NanumGothic")
pa = c("blue","skyblue2","white","lightpink","red")
qk = classIntervals(round(lgg, 4), n=5, style="kmeans")
qkp = findColours (qk, pa)
plot(qk, pal = pa, xlab="household spending on health", ylab="cumulative prob", main="Cumulative Probability Plot (K-Means)", axes = T)

# Create a map of local G-Statistics based on 5 clusters by k-means method
plot(dmap.sp, col = qkp)
title("5 Clusters by local G-Statistics: K-Means")
legend("topleft", fill=attr(qkp,"palette"), cex=0.8, legend= names(attr(qkp,"table")), bty="n")

# You may want adjust white spaces at the margin
par("mar")  # Check the current margins 
par(mar=c(0.5,0.5,1.5,0.5))  # c(bottom, left, top, right)




## 4. OLS regression (non-spatial)

lm(salud_2020 ~ mayores_2020 , data=dmap.sp)   # DV ~ IVs
mexico.lm <- lm(salud_2020 ~ mayores_2020, data=dmap.sp)
# Regression results are stored in mexico.lm
summary(mexico.lm)
# Significant variables: mayores_2020

# Descriptive statistics of variables: correlation and distribution
xx= data.frame(dmap.sp$salud_2020, dmap.sp$mayores_2020)
library(psych)
pairs.panels(xx)

# GLOBAL Moran'S I test for regression residuals

library(spdep)
d.W <- poly2nb(dmap.sp, queen=TRUE)   
W <- nb2listw(d.W)   # Spatial weight matrix (queen)

ct.moran <- lm.morantest(mexico.lm, W, alternative="two.sided") # Two-tail test
print(ct.moran)
# Moran I Statistic: 0.24 (P<.001) confirms the existence of spatial autocorrelation

# Moran I test under normality assumption (theoretically more correct)
ct.e <- resid(mexico.lm)
ct.morane <- moran.test(ct.e, W, randomisation = FALSE, alternative="two.sided")
print(ct.morane)   # P<.001




## 5. Spatial Autoregressive Models

## Typical procedure
## Step 1: Run OLS and LM diagnostics (LM-Lag, LM-Error)
## Step 2: Based on LM diagnostics results, run the appropriate model
##     If neither is significant -> Take OLS model
##     If only one is significant -> Take either SLM or SEM that is significant 
##     If both are significant -> Run robust LM -> Take either SLM or SEM that is significant 

library(spdep)
library(spatialreg)

## Langrange Multiplier (LM) Test for Spatial Autocorrelation
mexico.lagrange <- lm.LMtests(mexico.lm, W, test="all") # with all options, test all "Lmerr", "RLMerr", "LMlag", "RLMlag", "SARMA"
summary(mexico.lagrange)
# Both LM-lag (P<.001) and LM-err (P<.001) are significant
# RLMlag (p<.001) & RLMerr (not sig) -> Take Spatial Lag model

# Check heteroskedasticity of residuals by Breusch-Pagan Test
error.ct <- spatialreg::errorsarlm(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020 , data=dmap.sp, W)
spatialreg::bptest.Sarlm(error.ct)
# P<.01: confirm the existence of heteroskedasticity of residuals

# (1) Spatial Lag Model (SLM)

# Maximum Likelihood Estimation of the Spatial Lag Model
ct.lag <-spatialreg::lagsarlm(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, W)
summary(ct.lag)
# Rho (Spatial Autogressive Parameter) = 0.493 (P<.001)

# (2) Spatial Error Model (SEM)

# Maximum Likelihood Estimation of the Spatial Error Model
ct.err <-spatialreg::errorsarlm(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, W)
summary(ct.err)
# Lambda (Spatial Autoregressive Coefficient) 0.495 (P<.001)

# (3) Spatial Durbin Model (SDM)

ct.durbin <- spatialreg::lagsarlm(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, W, type="mixed")  # "mixed": SDM, "lag": SLM
summary(ct.durbin)
# Rho (Spatial Autogressive Parameter) = 0.468 (P<.001)
# Check the beta coefficients of lagged variables (spill-over effects)

## 6. Geographically Weighted Regression

library(spgwr); library(spData)

# For GWR, you must set the optimal bandwidth first
# Methods to select the optimal parameter for kernel function: CV (Cross-Validation) score method vs. AIC method

bw <- gwr.sel(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp) # Default: CV method
# Optimal bandwidth (CV)=  12.84483

bw2 <- gwr.sel(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, method="AIC") # AIC method
# Optimal bandwidth (AIC)= 23.0164

# GWR model #1 with the bandwidth from CV
gwr.model1 = gwr(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, bandwidth = bw, hatmatrix = T) #hatmatrix = T must be added
# hat matrix from regression modeling: hi = xi(xt wi x)-1XtWi
gwr.model1  # RSS=32799.55, R2=0.17

# GWR model #2 with the bandwidth from AIC
gwr.model2 = gwr(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, bandwidth = bw2, hatmatrix = T)
gwr.model2  # RSS=8179.871, R2=0.79
# Based on the results, the model #2 is better

# Visualize GWR results

gwr.model <- gwr(dmap.sp$salud_2020 ~ dmap.sp$mayores_2020, data=dmap.sp, bandwidth = bw2, hatmatrix = T)  
results<-as.data.frame(gwr.model$SDF)  # Save predicted values from GWR
head(results)  # Show some results
dmap.sp$pred <- results$pred  # predicted crime rate
dmap.sp$coefmayores <- results$dmap.sp.mayores_2020  # beta for CCTV variable
dmap.sp$coefmayoresese <- results$dmap.sp.mayores_2020_se  # standard error for CCTV coefficient

# Histogram of predicted crime rate
hist(dmap.sp$pred)

# Mapping of different GWR coefficients over spatial units
library(tmap)
tmap_mode("view")

# tm_shape("map name")+tm_polygons("variable name")
tm_shape(dmap.sp)+tm_polygons("pred", style="quantile", title ="Predicted health expenditures by GWR")  
# Quantile map of predicted crime rate

tm_shape(dmap.sp)+tm_polygons("mayores_2020", style="jenks", title ="GWR coefficients for age")
# Jenks' natural break map of CCTV coefficients

# These maps show the areas where GWR coefficient is not significant at 95%
# To show only significant areas, get the p-value for the coefficients
n <- length(dmap.sp)      # Sample size
dmap.sp$t <- dmap.sp$coefmayores/dmap.sp$coefmayoresese  # t statistics
dmap.sp$pvalue <- 2*pt(-abs(dmap.sp$t),df=n-1)       # p-value
dmap.sp.sig <- dmap.sp[dmap.sp$pvalue<0.013,]              # Get subset if p-value<0.05
dmap.sp.sig
# Now, create the maps highlighting only the areas where GWR coefficient is significant at 95%
tm_shape(dmap.sp.sig)+tm_polygons("mayores_2020", style="jenks", title ="GWR coefficients for age")



