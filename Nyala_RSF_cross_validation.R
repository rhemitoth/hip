## ****************************************************
## Title: HiP Herbivore RSF Cross Validation
## Author: Rhemi Toth
## Date Created: 01/29/2023
## Email: rhemitoth@g.harvard.edu
##
## ****************************************************
##
## Notes:
##
##
## ****************************************************
##
## Loading Packages
library(ROCR)
##
## ****************************************************

#Loading Data----
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/Nyala_RSF.R")

#ROC Plot Information
palette <- c("#264653", "#2a9d8f","#e9c46a", "#f4a261", "#e76f51")
Models <- c("RSF9", "RSF1", "RSF10", "RSF3", "RSF2")
#Initializing RMSE results tables----

nyala_cv_results <- tibble(Model = "",
                            fold = rep(seq(1,10,1),times = 5),
                            RMSE = 0.001,
                            R2 = 0.001,
                            AUC = 0.001)

aucs <- list()

#Initializing ROC tables----
rsf5_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf7_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf12_roc_raw <- tibble(Model = "",
                        fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                        tpr = 0.001,
                fpr = 0.001)

rsf11_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf4_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)


#RSF5----
print("Executing RSF5 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  nyala_cv_results$Model[f] <- "RSF5"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.NyalaRSF.train <- data.NyalaRSF %>%
    filter(fold != f)

  data.NyalaRSF.test <- data.NyalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.NyalaRSF.train$veg1m <- raster::extract(veg1m, data.NyalaRSF.train[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.train <- na.omit(data.NyalaRSF.train)

  nyalaRSF5.fit.train <- glm(Used ~ veg1m,
                              data = data.NyalaRSF.train,
                              family = binomial(link = "logit"))

  hab1 <- stack(veg1m)
  names(hab1) <- c("veg1m")
  nyalaRSF5.predict.train <- terra::predict(object = hab1, model = nyalaRSF5.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.NyalaRSF.test$RSF <- raster::extract(nyalaRSF5.predict.train, data.NyalaRSF.test[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.test <- na.omit(data.NyalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.NyalaRSF.test$Used)
  sse <- sum((obs_data-data.NyalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  nyala_cv_results$R2[f] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.NyalaRSF.test$Used)-data.NyalaRSF.test$RSF)^2 ))

  nyala_cv_results$RMSE[f] <- rmse

  #*****ROC----
  pred <- prediction(data.NyalaRSF.test$RSF, data.NyalaRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf5_roc_raw <- rsf5_roc_raw%>%
      add_row(tibble_row(Model = "RSF5",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  nyala_cv_results$AUC[f] <- auc_val
}

#Deleting place-holder row
rsf5_roc_raw <- rsf5_roc_raw[-1,]

#RSF7----
print("Executing RSF7 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  nyala_cv_results$Model[f+10] <- "RSF7"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.NyalaRSF.train <- data.NyalaRSF %>%
    filter(fold != f)

  data.NyalaRSF.test <- data.NyalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  #Distance to Major Rivers
  data.NyalaRSF.train$rivers<- raster::extract(rivers, data.NyalaRSF.train[,1:2])

  #Avg NDVI
  data.NyalaRSF.train$ndvi <- raster::extract(ndvi, data.NyalaRSF.train[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.train <- na.omit(data.NyalaRSF.train)

  nyalaRSF7.fit.train <- glm(Used ~ ndvi + rivers,
                              data = data.NyalaRSF.train,
                              family = binomial(link = "logit"))

  coef(nyalaRSF7.fit.train)%>%exp()

  vif(nyalaRSF7.fit.train)

  hab1 <- stack(ndvi, rivers)
  names(hab1) <- c("ndvi", "rivers")
  nyalaRSF7.predict.train <- terra::predict(object = hab1, model = nyalaRSF7.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.NyalaRSF.test$RSF <- raster::extract(nyalaRSF7.predict.train, data.NyalaRSF.test[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.test <- na.omit(data.NyalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.NyalaRSF.test$Used)
  sse <- sum((obs_data-data.NyalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  nyala_cv_results$R2[f+10] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.NyalaRSF.test$Used)-data.NyalaRSF.test$RSF)^2 ))

  nyala_cv_results$RMSE[f+10] <- rmse

  #*****ROC----
  pred <- prediction(data.NyalaRSF.test$RSF, data.NyalaRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf7_roc_raw <- rsf7_roc_raw%>%
      add_row(tibble_row(Model = "RSF7",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  nyala_cv_results$AUC[f+10] <- auc_val
}

#Deleting place-holder row
rsf7_roc_raw <- rsf7_roc_raw[-1,]


#RSF12----
print("Executing RSF10 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  nyala_cv_results$Model[f+20] <- "RSF12"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.NyalaRSF.train <- data.NyalaRSF %>%
    filter(fold != f)

  data.NyalaRSF.test <- data.NyalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.NyalaRSF.train$veg1.5m <- raster::extract(veg1.5m, data.NyalaRSF.train[,1:2])

  #Pre post fire ndvi

  data.NyalaRSF.train$ndviFire <- NA

  for ( i in 1:length(data.NyalaRSF.train$X)){
    date <- data.NyalaRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.NyalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.NyalaRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.NyalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.NyalaRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.NyalaRSF.train <- na.omit(data.NyalaRSF.train)

  #*****Fittind training data to Model----

  nyalaRSF12.fit.train <- glm(Used ~ veg1.5m + ndviFire,
                              data = data.NyalaRSF.train,
                              family = binomial(link = "logit"))

  coef(nyalaRSF12.fit.train)%>%exp()

  vif(nyalaRSF12.fit.train)

  hab9_0722<- stack(ndvi_20140722, veg1.5m)
  names(hab9_0722) <- c("ndviFire", "veg1.5m")
  nyalaRSF12_0722.predict.train <- terra::predict(object = hab9_0722, model = nyalaRSF12.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, veg1.5m)
  names(hab9_0823) <- c("ndviFire", "veg1.5m")
  nyalaRSF12_0823.predict.train <- terra::predict(object = hab9_0823, model = nyalaRSF12.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.NyalaRSF.test$RSF <- NA

  for( i in 1:length(data.NyalaRSF.test$X)){
    if (data.NyalaRSF.test$ndviDate[i] == "2014-07-22"){
      data.NyalaRSF.test$RSF[i] <- raster::extract(nyalaRSF12_0722.predict.train, data.NyalaRSF.test[i,1:2])
    } else if(data.NyalaRSF.test$ndviDate[i] == "2014-08-23"){
      data.NyalaRSF.test$RSF[i] <- raster::extract(nyalaRSF12_0823.predict.train, data.NyalaRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.NyalaRSF.test <- na.omit(data.NyalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.NyalaRSF.test$Used)
  sse <- sum((obs_data-data.NyalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  nyala_cv_results$R2[f+20] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.NyalaRSF.test$Used)-data.NyalaRSF.test$RSF)^2 ))

  nyala_cv_results$RMSE[f+20] <- rmse

  #*****ROC----
  pred <- prediction(data.NyalaRSF.test$RSF, data.NyalaRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf12_roc_raw <- rsf12_roc_raw%>%
      add_row(tibble_row(Model = "RSF12",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  nyala_cv_results$AUC[f+20] <- auc_val
}

#Deleting place-holder row
rsf12_roc_raw <- rsf12_roc_raw[-1,]

#RSF11----
print("Executing RSF11 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  nyala_cv_results$Model[f+30] <- "RSF11"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.NyalaRSF.train <- data.NyalaRSF %>%
    filter(fold != f)

  data.NyalaRSF.test <- data.NyalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.NyalaRSF.train$veg1m <- raster::extract(veg1m, data.NyalaRSF.train[,1:2])

  #Pre post fire ndvi

  data.NyalaRSF.train$ndviFire <- NA

  for ( i in 1:length(data.NyalaRSF.train$X)){
    date <- data.NyalaRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.NyalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.NyalaRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.NyalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.NyalaRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.NyalaRSF.train <- na.omit(data.NyalaRSF.train)

  #*****Fittind training data to Model----

  nyalaRSF11.fit.train <- glm(Used ~ veg1m + ndviFire,
                              data = data.NyalaRSF.train,
                              family = binomial(link = "logit"))

  coef(nyalaRSF11.fit.train)%>%exp()

  vif(nyalaRSF11.fit.train)

  hab9_0722<- stack(ndvi_20140722, veg1m)
  names(hab9_0722) <- c("ndviFire", "veg1m")
  nyalaRSF11_0722.predict.train <- terra::predict(object = hab9_0722, model = nyalaRSF11.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, veg1m)
  names(hab9_0823) <- c("ndviFire", "veg1m")
  nyalaRSF11_0823.predict.train <- terra::predict(object = hab9_0823, model = nyalaRSF11.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.NyalaRSF.test$RSF <- NA

  for( i in 1:length(data.NyalaRSF.test$X)){
    if (data.NyalaRSF.test$ndviDate[i] == "2014-07-22"){
      data.NyalaRSF.test$RSF[i] <- raster::extract(nyalaRSF11_0722.predict.train, data.NyalaRSF.test[i,1:2])
    } else if(data.NyalaRSF.test$ndviDate[i] == "2014-08-23"){
      data.NyalaRSF.test$RSF[i] <- raster::extract(nyalaRSF11_0823.predict.train, data.NyalaRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.NyalaRSF.test <- na.omit(data.NyalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.NyalaRSF.test$Used)
  sse <- sum((obs_data-data.NyalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  nyala_cv_results$R2[f+30] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.NyalaRSF.test$Used)-data.NyalaRSF.test$RSF)^2 ))

  nyala_cv_results$RMSE[f+30] <- rmse

  #*****ROC----
  pred <- prediction(data.NyalaRSF.test$RSF, data.NyalaRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf11_roc_raw <- rsf11_roc_raw%>%
      add_row(tibble_row(Model = "RSF11",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  nyala_cv_results$AUC[f+30] <- auc_val
}

#Deleting place-holder row
rsf11_roc_raw <- rsf11_roc_raw[-1,]

#RSF4----
print("Executing RSF4 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  nyala_cv_results$Model[f+40] <- "RSF4"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.NyalaRSF.train <- data.NyalaRSF %>%
    filter(fold != f)

  data.NyalaRSF.test <- data.NyalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.NyalaRSF.train$veg1m <- raster::extract(veg1m, data.NyalaRSF.train[,1:2])

  #Avg NDVI
  data.NyalaRSF.train$ndvi <- raster::extract(ndvi, data.NyalaRSF.train[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.train <- na.omit(data.NyalaRSF.train)

  nyalaRSF4.fit.train <- glm(Used ~ veg1m + ndvi,
                              data = data.NyalaRSF.train,
                              family = binomial(link = "logit"))

  coef(nyalaRSF4.fit.train)%>%exp()

  vif(nyalaRSF4.fit.train)

  hab1 <- stack(ndvi, veg1m)
  names(hab1) <- c("ndvi", "veg1m")
  nyalaRSF4.predict.train <- terra::predict(object = hab1, model = nyalaRSF4.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.NyalaRSF.test$RSF <- raster::extract(nyalaRSF4.predict.train, data.NyalaRSF.test[,1:2])

  #Remove rows containing NA

  data.NyalaRSF.test <- na.omit(data.NyalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.NyalaRSF.test$Used)
  sse <- sum((obs_data-data.NyalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  nyala_cv_results$R2[f+40] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.NyalaRSF.test$Used)-data.NyalaRSF.test$RSF)^2 ))

  nyala_cv_results$RMSE[f+40] <- rmse

  #*****ROC----
  pred <- prediction(data.NyalaRSF.test$RSF, data.NyalaRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf4_roc_raw <- rsf4_roc_raw%>%
      add_row(tibble_row(Model = "RSF4",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  nyala_cv_results$AUC[f+40] <- auc_val
}

#Deleting place-holder row
rsf4_roc_raw <- rsf4_roc_raw[-1,]

# Average ROC Curve Function ----

average_roc <- function(fprs, tprs_data, m){
  xvals <- fprs %>%
    unique()%>%
    sort()

  roc_calc <- tibble(start = 0,
                     stop = 0)

  for(i in 1:(length(xvals)-1)){
    roc_calc <- roc_calc %>%
      add_row(tibble_row(start = xvals[i],
                         stop = xvals[i+1]))
  }

  roc_calc$mean_tpr <- NA

  roc_calc <- roc_calc[-1,]

  for(i in 1:length(roc_calc$start)){
    tprs <- tprs_data %>%
      filter(tprs_data$fpr >= roc_calc$start[i] & tprs_data$fpr <= roc_calc$stop[i])
    tprmean <- mean(tprs$tpr)
    roc_calc$mean_tpr[i] <- tprmean
  }

  roc_plot <- tibble(fpr = 0,
                     tpr = 0,
                     Model = m)

  for(i in 1: (length(roc_calc$start))){
    xcurrent <- roc_calc$start[i]
    xnext <- roc_calc$stop[i]
    mtpr <- roc_calc$mean_tpr[i]
    roc_plot <- roc_plot %>%
      add_row(tibble_row(fpr = xcurrent,
                         tpr = mtpr,
                         Model = m))
    roc_plot <- roc_plot %>%
      add_row(tibble_row(fpr = xnext,
                         tpr = mtpr,
                         Model = m))
  }

  roc_plot <- roc_plot[-1,]

  return(roc_plot)
}



#Calculating Average ROC Curves----

rsf5_roc <- average_roc(fprs = rsf5_roc_raw$fpr, tprs_data = rsf5_roc_raw, m = "RSF5")
rsf7_roc <- average_roc(fprs = rsf7_roc_raw$fpr, tprs_data = rsf7_roc_raw, m = "RSF7")
rsf12_roc <- average_roc(fprs = rsf12_roc_raw$fpr, tprs_data = rsf12_roc_raw, m = "RSF12")
rsf11_roc <- average_roc(fprs = rsf11_roc_raw$fpr, tprs_data = rsf11_roc_raw, m = "RSF11")
rsf4_roc <- average_roc(fprs = rsf4_roc_raw$fpr, tprs_data = rsf4_roc_raw, m = "RSF4")

nyala_rocs <- bind_rows(rsf5_roc,
                         rsf7_roc,
                         rsf12_roc,
                         rsf11_roc,
                         rsf4_roc)

nyala_cv_sum <- nyala_cv_results %>%
  group_by(Model) %>%
  summarise(mRMSE = mean(RMSE),
            mR2 = mean(R2),
            mAUC = mean(AUC))%>%
  merge(aics,
        by = "Model")

write.csv(nyala_cv_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/nyala_RSF_Model_Selection.csv")

nyala_rocs <- merge(x = nyala_rocs, y = nyala_cv_sum,
                     by = "Model")

nyala_rocs$legend <- NA

for(i in 1:length(nyala_rocs$Model)){
  nyala_rocs$legend[i] <- paste(nyala_rocs$Model[i]," ","(",nyala_rocs$mAUC[i],")",sep="")
}

nyala_roc_plot <- ggplot(nyala_rocs, aes(x = fpr, y = tpr))+
  geom_line(aes(color = legend))+
  labs(x = "False Positive Rate", y = "True Positive Rate")+
  ggtitle("Model ROC Curves (Nyala Data Only)")+
  scale_color_discrete(name = "Model (AUC)")


nyala_roc_plot

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/ROC/nyalaROC.png",
       plot = nyala_roc_plot,
       units = c("in"),
       width = 6,
       height = 5,
       dpi = 350)

trapint <- function(xs, ys){
  integral = 0
  for(i in 1:(length(xs)-1)){
    deltax <- xs[i+1] - xs[i]
    area <- deltax * (ys[i+1] + ys[i]) /2
    integral = integral+area
  }
  return(integral)
}

