#setting workspace
setwd("E:/final_year_project/Western_Ghats")

load(".rdata")
#importing csv file
sl_data1 <- read.csv("lorris_pts.csv", header = TRUE)

#fetching names of variables
names(sl_data1)

#creating boxplots to check for outliers

boxplot(sl_data1$dem,sl_data1$slope,sl_data1$aspect,sl_data1$lulc,sl_data1$Ndvi,sl_data1$Temp,
        sl_data1$pop,sl_data1$RF,names = c("alt","slope","aspect","LULC","ndvi","temp","PD","RF"))


#finding max value of Dem
max(sl_data1$dem)
max(sl_data1$lulc)
max(sl_data1$Ndvi)
max(sl_data1$Temp)
max(sl_data1$pop)
max(sl_data1$RF)
#1st filter
sl_new1 <- sl_data1[((sl_data1$dem >=0)&(sl_data1$dem <2070)),]

#checking again for outliers
boxplot(sl_new1$dem,sl_new1$slope,sl_new1$aspect,sl_new1$lulc,sl_new1$Ndvi,sl_new1$Temp,
        sl_new1$pop,sl_new1$RF,names = c("alt","slope","aspect","LULC","ndvi","temp","PD","RF"))

#finding max value of aspect
max(sl_new1$aspect)

#Multi filter

sl_final1 <- sl_new1[((sl_new1$aspect >=0)&(sl_new1$aspect <356)),]
sl_final2 <- sl_final1[((sl_final1$lulc >=0)&(sl_final1$lulc <40)),]
sl_final3 <- sl_final2[((sl_final2$Ndvi >=0)&(sl_final2$Ndvi <250)),]
sl_final4 <- sl_final3[((sl_final3$Temp >=0)&(sl_final3$Temp <25)),]
sl_final5 <- sl_final4[((sl_final4$pop >=0)&(sl_final4$pop <725)),]
sl_final6 <- sl_final5[((sl_final5$RF >=0)&(sl_final5$RF <526)),]

#final check
boxplot(sl_final6$dem,sl_final6$slope,sl_final6$aspect,sl_final6$lulc,sl_final6$Ndvi,sl_final6$Temp,
        sl_final6$pop,sl_final6$RF,names = c("alt","slope","aspect","LULC","ndvi","temp","PD","RF"))

#factorizing lulc variable
sl_final6$lulc <- factor(sl_final6$lulc)


#correlation plot
plot(data.frame(sl_final6$dem,sl_final6$slope,sl_final6$aspect,
                sl_final6$pop,sl_final6$Ndvi,sl_final6$lulc,sl_final6$Temp,sl_final6$RF))


#correlation coefficients
cor(data.frame(sl_final6$dem,sl_final6$slope,sl_final6$aspect,
                sl_final6$pop,sl_final6$Ndvi,sl_final6$Temp,sl_final6$RF))

#VIF calculation for cont variables
VIFcalc(data.frame(sl_final6$dem,sl_final6$slope,sl_final6$aspect,sl_final6$pop,sl_final6$Ndvi,
                   sl_final6$Temp,sl_final6$RF))
save.image()


#Removing Temp due to high vif & correlation
VIFcalc(data.frame(sl_final6$dem,sl_final6$aspect,sl_final6$pop,sl_final6$Ndvi,
                   sl_final6$slope,sl_final6$RF))

#logistic regression
log_model  <- glm(obser~dem+aspect+pop+Ndvi+slope+RF+lulc,data = sl_final6,family = "binomial")

#summary of the model 
summary(log_model)

#stepwise regression
step_model <- step(log_model)

#summary of step model
summary(step_model)

library(raster)

#importing only selected rasters
dem_ras <- raster("dem.tif")
RF_ras <- raster("RF_pcs.tif")

#ploting rasters
plot(dem_ras,main = "Elevation in Meters",xlab = "UTM Easting", ylab = "UTM Northing", col = terrain.colors(5))
plot(RF_ras,main ="Annual Mean Rainfall (MM)",xlab = "UTM Easting", ylab = "UTM Northing", col = rainbow(5))

#converting as dataframes
dem_df <- as.data.frame(dem_ras)
RF_df <- as.data.frame(RF_ras)

#combining all dataframes
sl_df1 <- data.frame(dem = dem_df$dem,RF = RF_df$Band_1)


#prediction using step model
sl_df1$pr <- predict(step_model,type = "response",sl_df1)

#prediction matrix
pr_matrix1<-matrix(sl_df1$pr,nrow=dem_ras@nrows,ncol=dem_ras@ncols,byrow=TRUE)

#prediction raster
pred_ras1<-raster(pr_matrix1, 
                 crs=dem_ras@crs, ## the coordinate reference system
                 xmn=dem_ras@extent@xmin,## the outer coordinates of the bounding box
                 ymn=dem_ras@extent@ymin,
                 xmx=dem_ras@extent@xmax,
                 ymx=dem_ras@extent@ymax)
plot(pred_ras1)

save.image()
