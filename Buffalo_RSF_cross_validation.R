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
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/Buffalo_RSF.R")

#ROC Plot Information
palette <- c("#264653", "#2a9d8f","#e9c46a", "#f4a261", "#e76f51")
Models <- c("RSF9", "RSF1", "RSF10", "RSF3", "RSF2")
#Initializing RMSE results tables----

buffalo_cv_results <- tibble(Model = "",
                            fold = rep(seq(1,10,1),times = 10),
                            RMSE = 0.001,
                            R2 = 0.001,
                            AUC = 0.001)

aucs <- list()

#Initializing ROC tables----
rsf1_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                       fpr = 0.001)

rsf2_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                       fpr = 0.001)

rsf3_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                       fpr = 0.001)

rsf4_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                       fpr = 0.001)

rsf5_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf8_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf9_roc_raw <- tibble(Model = "",
                        fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                        tpr = 0.001,
                fpr = 0.001)

rsf10_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf11_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf12_roc_raw <- tibble(Model = "",
                        fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                        tpr = 0.001,
                        fpr = 0.001)


#RSF1----
print("Executing RSF1 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f] <- "RSF1"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Distance to Major Rivers
  data.buffaloRSF.train$rivers<- raster::extract(rivers, data.buffaloRSF.train[,1:2])

  #Avg NDVI
  data.buffaloRSF.train$ndvi <- raster::extract(ndvi, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF1.fit.train <- glm(Used ~ veg1m + ndvi +slope + rivers,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  coef(buffaloRSF1.fit.train)%>%exp()

  vif(buffaloRSF1.fit.train)

  hab1 <- stack(ndvi, slope, veg1m, rivers)
  names(hab1) <- c("ndvi", "slope", "veg1m", "rivers")
  buffaloRSF1.predict.train <- terra::predict(object = hab1, model = buffaloRSF1.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF1.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf1_roc_raw <- rsf1_roc_raw%>%
      add_row(tibble_row(Model = "RSF1",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  buffalo_cv_results$AUC[f] <- auc_val
}

#Deleting place-holder row
rsf1_roc_raw <- rsf1_roc_raw[-1,]

#RSF2----
print("Executing RSF2 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+10] <- "RSF2"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1.5m
  data.buffaloRSF.train$veg1.5m <- raster::extract(veg1.5m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Distance to Major Rivers
  data.buffaloRSF.train$rivers<- raster::extract(rivers, data.buffaloRSF.train[,1:2])

  #Avg NDVI
  data.buffaloRSF.train$ndvi <- raster::extract(ndvi, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF2.fit.train <- glm(Used ~ veg1.5m + ndvi +slope + rivers,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  coef(buffaloRSF2.fit.train)%>%exp()

  vif(buffaloRSF2.fit.train)

  hab1 <- stack(ndvi, slope, veg1.5m, rivers)
  names(hab1) <- c("ndvi", "slope", "veg1.5m", "rivers")
  buffaloRSF2.predict.train <- terra::predict(object = hab1, model = buffaloRSF2.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF2.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+10] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+10] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf2_roc_raw <- rsf2_roc_raw%>%
      add_row(tibble_row(Model = "RSF2",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  buffalo_cv_results$AUC[f+10] <- auc_val
}

#Deleting place-holder row
rsf2_roc_raw <- rsf2_roc_raw[-1,]

#RSF3----
print("Executing RSF3 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+20] <- "RSF3"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Avg NDVI
  data.buffaloRSF.train$ndvi <- raster::extract(ndvi, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF3.fit.train <- glm(Used ~ veg1m + ndvi +slope,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  coef(buffaloRSF3.fit.train)%>%exp()

  vif(buffaloRSF3.fit.train)

  hab1 <- stack(ndvi, slope, veg1m)
  names(hab1) <- c("ndvi", "slope", "veg1m")
  buffaloRSF3.predict.train <- terra::predict(object = hab1, model = buffaloRSF3.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF3.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+20] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+20] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf3_roc_raw <- rsf3_roc_raw%>%
      add_row(tibble_row(Model = "RSF3",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  buffalo_cv_results$AUC[f+20] <- auc_val
}

#Deleting place-holder row
rsf3_roc_raw <- rsf3_roc_raw[-1,]

#RSF4----
print("Executing RSF4 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+30] <- "RSF4"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  #Avg NDVI
  data.buffaloRSF.train$ndvi <- raster::extract(ndvi, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF4.fit.train <- glm(Used ~ veg1m + ndvi,
                               data = data.buffaloRSF.train,
                               family = binomial(link = "logit"))

  coef(buffaloRSF4.fit.train)%>%exp()

  vif(buffaloRSF4.fit.train)

  hab1 <- stack(ndvi, veg1m)
  names(hab1) <- c("ndvi", "veg1m")
  buffaloRSF4.predict.train <- terra::predict(object = hab1, model = buffaloRSF4.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF4.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+30] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+30] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

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

  buffalo_cv_results$AUC[f+30] <- auc_val
}

#Deleting place-holder row
rsf4_roc_raw <- rsf4_roc_raw[-1,]


#RSF5----
print("Executing RSF5 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+40] <- "RSF5"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF5.fit.train <- glm(Used ~ veg1m,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  hab1 <- stack(veg1m)
  names(hab1) <- c("veg1m")
  buffaloRSF5.predict.train <- terra::predict(object = hab1, model = buffaloRSF5.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF5.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+40] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+40] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

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

  buffalo_cv_results$AUC[f+40] <- auc_val
}

#Deleting place-holder row
rsf5_roc_raw <- rsf5_roc_raw[-1,]

#RSF8----
print("Executing RSF8 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+50] <- "RSF8"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1.5m <- raster::extract(veg1.5m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Avg NDVI
  data.buffaloRSF.train$ndvi <- raster::extract(ndvi, data.buffaloRSF.train[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  buffaloRSF8.fit.train <- glm(Used ~ veg1.5m + ndvi +slope,
                               data = data.buffaloRSF.train,
                               family = binomial(link = "logit"))

  coef(buffaloRSF8.fit.train)%>%exp()

  vif(buffaloRSF8.fit.train)

  hab1 <- stack(ndvi, slope, veg1.5m)
  names(hab1) <- c("ndvi", "slope", "veg1.5m")
  buffaloRSF8.predict.train <- terra::predict(object = hab1, model = buffaloRSF8.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- raster::extract(buffaloRSF8.predict.train, data.buffaloRSF.test[,1:2])

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+50] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+50] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf8_roc_raw <- rsf8_roc_raw%>%
      add_row(tibble_row(Model = "RSF8",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  buffalo_cv_results$AUC[f+50] <- auc_val
}

#Deleting place-holder row
rsf8_roc_raw <- rsf8_roc_raw[-1,]

#RSF9----
print("Executing RSF9 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+60] <- "RSF9"
  print(paste(toString(f/10*100), " %"))

  #******Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #******Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Distance to Major Rivers
  data.buffaloRSF.train$rivers<- raster::extract(rivers, data.buffaloRSF.train[,1:2])

  #Pre post fire ndvi

  data.buffaloRSF.train$ndviFire <- NA

  for ( i in 1:length(data.buffaloRSF.train$X)){
    date <- data.buffaloRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.buffaloRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.buffaloRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  #*****Fittind training data to Model----

  buffaloRSF9.fit.train <- glm(Used ~ veg1m + ndviFire+slope+rivers,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  coef(buffaloRSF9.fit.train)%>%exp()

  vif(buffaloRSF9.fit.train)

  hab9_0722<- stack(ndvi_20140722, slope, veg1m,rivers)
  names(hab9_0722) <- c("ndviFire", "slope", "veg1m", "rivers")
  buffaloRSF9_0722.predict.train <- terra::predict(object = hab9_0722, model = buffaloRSF9.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, slope, veg1m,rivers)
  names(hab9_0823) <- c("ndviFire", "slope", "veg1m", "rivers")
  buffaloRSF9_0823.predict.train <- terra::predict(object = hab9_0823, model = buffaloRSF9.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- NA

  for( i in 1:length(data.buffaloRSF.test$X)){
    if (data.buffaloRSF.test$ndviDate[i] == "2014-07-22"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF9_0722.predict.train, data.buffaloRSF.test[i,1:2])
    } else if(data.buffaloRSF.test$ndviDate[i] == "2014-08-23"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF9_0823.predict.train, data.buffaloRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+60] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+60] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf9_roc_raw <- rsf9_roc_raw%>%
      add_row(tibble_row(Model = "RSF9",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)
  buffalo_cv_results$AUC[f+60] <- auc_val

}

#Deleting place-holder row
rsf9_roc_raw <- rsf9_roc_raw[-1,]


#RSF10----
print("Executing RSF10 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+70] <- "RSF10"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  # Slope
  data.buffaloRSF.train$slope <- raster::extract(slope,data.buffaloRSF.train[,1:2])

  #Pre post fire ndvi

  data.buffaloRSF.train$ndviFire <- NA

  for ( i in 1:length(data.buffaloRSF.train$X)){
    date <- data.buffaloRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.buffaloRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.buffaloRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  #*****Fittind training data to Model----

  buffaloRSF10.fit.train <- glm(Used ~ veg1m + ndviFire+ slope,
                               data = data.buffaloRSF.train,
                               family = binomial(link = "logit"))

  coef(buffaloRSF10.fit.train)%>%exp()

  vif(buffaloRSF10.fit.train)

  hab9_0722<- stack(ndvi_20140722, slope, veg1m)
  names(hab9_0722) <- c("ndviFire", "slope", "veg1m")
  buffaloRSF10_0722.predict.train <- terra::predict(object = hab9_0722, model = buffaloRSF10.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, slope, veg1m)
  names(hab9_0823) <- c("ndviFire", "slope", "veg1m")
  buffaloRSF10_0823.predict.train <- terra::predict(object = hab9_0823, model = buffaloRSF10.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- NA

  for( i in 1:length(data.buffaloRSF.test$X)){
    if (data.buffaloRSF.test$ndviDate[i] == "2014-07-22"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF10_0722.predict.train, data.buffaloRSF.test[i,1:2])
    } else if(data.buffaloRSF.test$ndviDate[i] == "2014-08-23"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF10_0823.predict.train, data.buffaloRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+70] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+70] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

  roc <- performance(pred, "tpr", "fpr")
  TPR <- roc@y.values[[1]]
  FPR <- roc@x.values[[1]]
  for(j in 1:length(TPR)){
    rsf10_roc_raw <- rsf10_roc_raw%>%
      add_row(tibble_row(Model = "RSF10",
                         fold = toString(f),
                         tpr = TPR[j],
                         fpr = FPR[j]))
  }

  #*****AUC----
  auc <- performance(pred, measure = "auc")
  auc_val <- round(auc@y.values[[1]],3)
  aucs <- append(aucs,auc_val)

  buffalo_cv_results$AUC[f+70] <- auc_val
}

#Deleting place-holder row
rsf10_roc_raw <- rsf10_roc_raw[-1,]


#RSF11----
print("Executing RSF11 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+80] <- "RSF11"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1m <- raster::extract(veg1m, data.buffaloRSF.train[,1:2])

  #Pre post fire ndvi

  data.buffaloRSF.train$ndviFire <- NA

  for ( i in 1:length(data.buffaloRSF.train$X)){
    date <- data.buffaloRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.buffaloRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.buffaloRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  #*****Fittind training data to Model----

  buffaloRSF11.fit.train <- glm(Used ~ veg1m + ndviFire,
                                data = data.buffaloRSF.train,
                                family = binomial(link = "logit"))

  coef(buffaloRSF11.fit.train)%>%exp()

  vif(buffaloRSF11.fit.train)

  hab9_0722<- stack(ndvi_20140722, veg1m)
  names(hab9_0722) <- c("ndviFire", "veg1m")
  buffaloRSF11_0722.predict.train <- terra::predict(object = hab9_0722, model = buffaloRSF11.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, veg1m)
  names(hab9_0823) <- c("ndviFire", "veg1m")
  buffaloRSF11_0823.predict.train <- terra::predict(object = hab9_0823, model = buffaloRSF11.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- NA

  for( i in 1:length(data.buffaloRSF.test$X)){
    if (data.buffaloRSF.test$ndviDate[i] == "2014-07-22"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF11_0722.predict.train, data.buffaloRSF.test[i,1:2])
    } else if(data.buffaloRSF.test$ndviDate[i] == "2014-08-23"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF11_0823.predict.train, data.buffaloRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+80] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+80] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

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

  buffalo_cv_results$AUC[f+80] <- auc_val
}

#Deleting place-holder row
rsf11_roc_raw <- rsf11_roc_raw[-1,]

#RSF12----
print("Executing RSF10 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  buffalo_cv_results$Model[f+90] <- "RSF12"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.buffaloRSF.train <- data.buffaloRSF %>%
    filter(fold != f)

  data.buffaloRSF.test <- data.buffaloRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.buffaloRSF.train$veg1.5m <- raster::extract(veg1.5m, data.buffaloRSF.train[,1:2])

  #Pre post fire ndvi

  data.buffaloRSF.train$ndviFire <- NA

  for ( i in 1:length(data.buffaloRSF.train$X)){
    date <- data.buffaloRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.buffaloRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.buffaloRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.buffaloRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.train <- na.omit(data.buffaloRSF.train)

  #*****Fittind training data to Model----

  buffaloRSF12.fit.train <- glm(Used ~ veg1.5m + ndviFire,
                              data = data.buffaloRSF.train,
                              family = binomial(link = "logit"))

  coef(buffaloRSF12.fit.train)%>%exp()

  vif(buffaloRSF12.fit.train)

  hab9_0722<- stack(ndvi_20140722, veg1.5m)
  names(hab9_0722) <- c("ndviFire", "veg1.5m")
  buffaloRSF12_0722.predict.train <- terra::predict(object = hab9_0722, model = buffaloRSF12.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, veg1.5m)
  names(hab9_0823) <- c("ndviFire", "veg1.5m")
  buffaloRSF12_0823.predict.train <- terra::predict(object = hab9_0823, model = buffaloRSF12.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.buffaloRSF.test$RSF <- NA

  for( i in 1:length(data.buffaloRSF.test$X)){
    if (data.buffaloRSF.test$ndviDate[i] == "2014-07-22"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF12_0722.predict.train, data.buffaloRSF.test[i,1:2])
    } else if(data.buffaloRSF.test$ndviDate[i] == "2014-08-23"){
      data.buffaloRSF.test$RSF[i] <- raster::extract(buffaloRSF12_0823.predict.train, data.buffaloRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.buffaloRSF.test <- na.omit(data.buffaloRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.buffaloRSF.test$Used)
  sse <- sum((obs_data-data.buffaloRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  buffalo_cv_results$R2[f+90] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.buffaloRSF.test$Used)-data.buffaloRSF.test$RSF)^2 ))

  buffalo_cv_results$RMSE[f+90] <- rmse

  #*****ROC----
  pred <- prediction(data.buffaloRSF.test$RSF, data.buffaloRSF.test$Used)

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

  buffalo_cv_results$AUC[f+90] <- auc_val
}

#Deleting place-holder row
rsf12_roc_raw <- rsf12_roc_raw[-1,]

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
rsf1_roc <- average_roc(fprs = rsf1_roc_raw$fpr, tprs_data = rsf1_roc_raw, m = "RSF1")
rsf2_roc <- average_roc(fprs = rsf2_roc_raw$fpr, tprs_data = rsf2_roc_raw, m = "RSF2")
rsf3_roc <- average_roc(fprs = rsf3_roc_raw$fpr, tprs_data = rsf3_roc_raw, m = "RSF3")
rsf4_roc <- average_roc(fprs = rsf4_roc_raw$fpr, tprs_data = rsf4_roc_raw, m = "RSF4")
rsf5_roc <- average_roc(fprs = rsf5_roc_raw$fpr, tprs_data = rsf5_roc_raw, m = "RSF5")
rsf8_roc <- average_roc(fprs = rsf8_roc_raw$fpr, tprs_data = rsf8_roc_raw, m = "RSF8")
rsf9_roc <- average_roc(fprs = rsf9_roc_raw$fpr, tprs_data = rsf9_roc_raw, m = "RSF9")
rsf10_roc <- average_roc(fprs = rsf10_roc_raw$fpr, tprs_data = rsf10_roc_raw, m = "RSF10")
rsf11_roc <- average_roc(fprs = rsf11_roc_raw$fpr, tprs_data = rsf11_roc_raw, m = "RSF11")
rsf12_roc <- average_roc(fprs = rsf12_roc_raw$fpr, tprs_data = rsf12_roc_raw, m = "RSF12")

buffalo_rocs <- bind_rows(rsf1_roc,
                          rsf2_roc,
                          rsf3_roc,
                          rsf4_roc,
                          rsf5_roc,
                          rsf8_roc,
                          rsf9_roc,
                          rsf10_roc,
                          rsf11_roc,
                          rsf12_roc)

buffalo_cv_sum <- buffalo_cv_results %>%
  group_by(Model) %>%
  summarise(mRMSE = mean(RMSE),
            mR2 = mean(R2),
            mAUC = mean(AUC))%>%
  merge(aics, by = "Model")

write.csv(buffalo_cv_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/buffalo_RSF_Model_Selection.csv")

buffalo_rocs <- merge(x = buffalo_rocs, y = buffalo_cv_sum,
                     by = "Model")

buffalo_rocs$legend <- NA

for(i in 1:length(buffalo_rocs$Model)){
  buffalo_rocs$legend[i] <- paste(buffalo_rocs$Model[i]," ","(",buffalo_rocs$mAUC[i],")",sep="")
}

buffalo_roc_plot <- ggplot(buffalo_rocs, aes(x = fpr, y = tpr))+
  geom_line(aes(color = legend))+
  labs(x = "False Positive Rate", y = "True Positive Rate")+
  ggtitle("Model ROC Curves (buffalo Data Only)")+
  scale_color_discrete(name = "Model (AUC)")


buffalo_roc_plot

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/ROC/buffaloROC.png",
       plot = buffalo_roc_plot,
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

