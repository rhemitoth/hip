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
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/Impala_RSF.R")

#ROC Plot Information
palette <- c("#264653", "#2a9d8f","#e9c46a", "#f4a261", "#e76f51")
Models <- c("RSF9", "RSF1", "RSF10", "RSF3", "RSF2")
#Initializing RMSE results tables----

impala_cv_results <- tibble(Model = "",
                            fold = rep(seq(1,10,1),times = 5),
                            RMSE = 0.001,
                            R2 = 0.001,
                            AUC = 0.001)

aucs <- list()

#Initializing ROC tables----
rsf9_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf1_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf10_roc_raw <- tibble(Model = "",
                        fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                        tpr = 0.001,
                fpr = 0.001)

rsf3_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)

rsf2_roc_raw <- tibble(Model = "",
                       fold = factor("1", levels = c("1","2","3",'4','5','6','7','8','9','10')),
                       tpr = 0.001,
                   fpr = 0.001)


#RSF9----
print("Executing RSF9 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  impala_cv_results$Model[f] <- "RSF9"
  print(paste(toString(f/10*100), " %"))

  #******Partitioning data into training vs test using 90% 10% split----

  data.impalaRSF.train <- data.impalaRSF %>%
    filter(fold != f)

  data.impalaRSF.test <- data.impalaRSF %>%
    filter(fold == f)

  #******Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.impalaRSF.train$veg1m <- raster::extract(veg1m, data.impalaRSF.train[,1:2])

  # Slope
  data.impalaRSF.train$slope <- raster::extract(slope,data.impalaRSF.train[,1:2])

  #Distance to Major Rivers
  data.impalaRSF.train$rivers<- raster::extract(rivers, data.impalaRSF.train[,1:2])

  #Pre post fire ndvi

  data.impalaRSF.train$ndviFire <- NA

  for ( i in 1:length(data.impalaRSF.train$X)){
    date <- data.impalaRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.impalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.impalaRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.impalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.impalaRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.impalaRSF.train <- na.omit(data.impalaRSF.train)

  #*****Fittind training data to Model----

  impalaRSF9.fit.train <- glm(Used ~ veg1m + ndviFire+slope+rivers,
                         data = data.impalaRSF.train,
                         family = binomial(link = "logit"))

  coef(impalaRSF9.fit.train)%>%exp()

  vif(impalaRSF9.fit.train)

  hab9_0722<- stack(ndvi_20140722, slope, veg1m,rivers)
  names(hab9_0722) <- c("ndviFire", "slope", "veg1m", "rivers")
  impalaRSF9_0722.predict.train <- terra::predict(object = hab9_0722, model = impalaRSF9.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, slope, veg1m,rivers)
  names(hab9_0823) <- c("ndviFire", "slope", "veg1m", "rivers")
  impalaRSF9_0823.predict.train <- terra::predict(object = hab9_0823, model = impalaRSF9.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.impalaRSF.test$RSF <- NA

  for( i in 1:length(data.impalaRSF.test$X)){
    if (data.impalaRSF.test$ndviDate[i] == "2014-07-22"){
      data.impalaRSF.test$RSF[i] <- raster::extract(impalaRSF9_0722.predict.train, data.impalaRSF.test[i,1:2])
    } else if(data.impalaRSF.test$ndviDate[i] == "2014-08-23"){
      data.impalaRSF.test$RSF[i] <- raster::extract(impalaRSF9_0823.predict.train, data.impalaRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.impalaRSF.test <- na.omit(data.impalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.impalaRSF.test$Used)
  sse <- sum((obs_data-data.impalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  impala_cv_results$R2[f] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.impalaRSF.test$Used)-data.impalaRSF.test$RSF)^2 ))

  impala_cv_results$RMSE[f] <- rmse

  #*****ROC----
  pred <- prediction(data.impalaRSF.test$RSF, data.impalaRSF.test$Used)

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
  impala_cv_results$AUC[f] <- auc_val

}

#Deleting place-holder row
rsf9_roc_raw <- rsf9_roc_raw[-1,]

#RSF1----
print("Executing RSF1 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  impala_cv_results$Model[f+10] <- "RSF1"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.impalaRSF.train <- data.impalaRSF %>%
    filter(fold != f)

  data.impalaRSF.test <- data.impalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.impalaRSF.train$veg1m <- raster::extract(veg1m, data.impalaRSF.train[,1:2])

  # Slope
  data.impalaRSF.train$slope <- raster::extract(slope,data.impalaRSF.train[,1:2])

  #Distance to Major Rivers
  data.impalaRSF.train$rivers<- raster::extract(rivers, data.impalaRSF.train[,1:2])

  #Avg NDVI
  data.impalaRSF.train$ndvi <- raster::extract(ndvi, data.impalaRSF.train[,1:2])

  #Remove rows containing NA

  data.impalaRSF.train <- na.omit(data.impalaRSF.train)

  impalaRSF1.fit.train <- glm(Used ~ veg1m + ndvi +slope + rivers,
                              data = data.impalaRSF.train,
                              family = binomial(link = "logit"))

  coef(impalaRSF1.fit.train)%>%exp()

  vif(impalaRSF1.fit.train)

  hab1 <- stack(ndvi, slope, veg1m, rivers)
  names(hab1) <- c("ndvi", "slope", "veg1m", "rivers")
  impalaRSF1.predict.train <- terra::predict(object = hab1, model = impalaRSF1.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.impalaRSF.test$RSF <- raster::extract(impalaRSF1.predict.train, data.impalaRSF.test[,1:2])

  #Remove rows containing NA

  data.impalaRSF.test <- na.omit(data.impalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.impalaRSF.test$Used)
  sse <- sum((obs_data-data.impalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  impala_cv_results$R2[f+10] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.impalaRSF.test$Used)-data.impalaRSF.test$RSF)^2 ))

  impala_cv_results$RMSE[f+10] <- rmse

  #*****ROC----
  pred <- prediction(data.impalaRSF.test$RSF, data.impalaRSF.test$Used)

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

  impala_cv_results$AUC[f+10] <- auc_val
}

#Deleting place-holder row
rsf1_roc_raw <- rsf1_roc_raw[-1,]


#RSF10----
print("Executing RSF10 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  impala_cv_results$Model[f+20] <- "RSF10"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.impalaRSF.train <- data.impalaRSF %>%
    filter(fold != f)

  data.impalaRSF.test <- data.impalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.impalaRSF.train$veg1m <- raster::extract(veg1m, data.impalaRSF.train[,1:2])

  # Slope
  data.impalaRSF.train$slope <- raster::extract(slope,data.impalaRSF.train[,1:2])

  #Pre post fire ndvi

  data.impalaRSF.train$ndviFire <- NA

  for ( i in 1:length(data.impalaRSF.train$X)){
    date <- data.impalaRSF.train$ndviDate[i]
    if (date == "2014-07-22"){
      data.impalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140722, data.impalaRSF.train[i,1:2])
    } else if (date == "2014-08-23"){
      data.impalaRSF.train$ndviFire[i] <- raster::extract(ndvi_20140823, data.impalaRSF.train[i,1:2])
    }
  }

  #Remove rows containing NA

  data.impalaRSF.train <- na.omit(data.impalaRSF.train)

  #*****Fittind training data to Model----

  impalaRSF10.fit.train <- glm(Used ~ veg1m + ndviFire+ slope,
                              data = data.impalaRSF.train,
                              family = binomial(link = "logit"))

  coef(impalaRSF10.fit.train)%>%exp()

  vif(impalaRSF10.fit.train)

  hab9_0722<- stack(ndvi_20140722, slope, veg1m)
  names(hab9_0722) <- c("ndviFire", "slope", "veg1m")
  impalaRSF10_0722.predict.train <- terra::predict(object = hab9_0722, model = impalaRSF10.fit.train, type = "response")

  hab9_0823 <- stack(ndvi_20140823, slope, veg1m)
  names(hab9_0823) <- c("ndviFire", "slope", "veg1m")
  impalaRSF10_0823.predict.train <- terra::predict(object = hab9_0823, model = impalaRSF10.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.impalaRSF.test$RSF <- NA

  for( i in 1:length(data.impalaRSF.test$X)){
    if (data.impalaRSF.test$ndviDate[i] == "2014-07-22"){
      data.impalaRSF.test$RSF[i] <- raster::extract(impalaRSF10_0722.predict.train, data.impalaRSF.test[i,1:2])
    } else if(data.impalaRSF.test$ndviDate[i] == "2014-08-23"){
      data.impalaRSF.test$RSF[i] <- raster::extract(impalaRSF10_0823.predict.train, data.impalaRSF.test[i,1:2])
    }
  }

  #Remove rows containing NA

  data.impalaRSF.test <- na.omit(data.impalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.impalaRSF.test$Used)
  sse <- sum((obs_data-data.impalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  impala_cv_results$R2[f+20] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.impalaRSF.test$Used)-data.impalaRSF.test$RSF)^2 ))

  impala_cv_results$RMSE[f+20] <- rmse

  #*****ROC----
  pred <- prediction(data.impalaRSF.test$RSF, data.impalaRSF.test$Used)

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

  impala_cv_results$AUC[f+20] <- auc_val
}

#Deleting place-holder row
rsf10_roc_raw <- rsf10_roc_raw[-1,]

#RSF3----
print("Executing RSF3 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  impala_cv_results$Model[f+30] <- "RSF3"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.impalaRSF.train <- data.impalaRSF %>%
    filter(fold != f)

  data.impalaRSF.test <- data.impalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.impalaRSF.train$veg1m <- raster::extract(veg1m, data.impalaRSF.train[,1:2])

  # Slope
  data.impalaRSF.train$slope <- raster::extract(slope,data.impalaRSF.train[,1:2])

  #Avg NDVI
  data.impalaRSF.train$ndvi <- raster::extract(ndvi, data.impalaRSF.train[,1:2])

  #Remove rows containing NA

  data.impalaRSF.train <- na.omit(data.impalaRSF.train)

  impalaRSF3.fit.train <- glm(Used ~ veg1m + ndvi +slope,
                              data = data.impalaRSF.train,
                              family = binomial(link = "logit"))

  coef(impalaRSF3.fit.train)%>%exp()

  vif(impalaRSF3.fit.train)

  hab1 <- stack(ndvi, slope, veg1m)
  names(hab1) <- c("ndvi", "slope", "veg1m")
  impalaRSF3.predict.train <- terra::predict(object = hab1, model = impalaRSF3.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.impalaRSF.test$RSF <- raster::extract(impalaRSF3.predict.train, data.impalaRSF.test[,1:2])

  #Remove rows containing NA

  data.impalaRSF.test <- na.omit(data.impalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.impalaRSF.test$Used)
  sse <- sum((obs_data-data.impalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  impala_cv_results$R2[f+30] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.impalaRSF.test$Used)-data.impalaRSF.test$RSF)^2 ))

  impala_cv_results$RMSE[f+30] <- rmse

  #*****ROC----
  pred <- prediction(data.impalaRSF.test$RSF, data.impalaRSF.test$Used)

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

  impala_cv_results$AUC[f+30] <- auc_val
}

#Deleting place-holder row
rsf3_roc_raw <- rsf3_roc_raw[-1,]

#RSF2----
print("Executing RSF2 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  impala_cv_results$Model[f+40] <- "RSF2"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.impalaRSF.train <- data.impalaRSF %>%
    filter(fold != f)

  data.impalaRSF.test <- data.impalaRSF %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1.5m
  data.impalaRSF.train$veg1.5m <- raster::extract(veg1.5m, data.impalaRSF.train[,1:2])

  # Slope
  data.impalaRSF.train$slope <- raster::extract(slope,data.impalaRSF.train[,1:2])

  #Distance to Major Rivers
  data.impalaRSF.train$rivers<- raster::extract(rivers, data.impalaRSF.train[,1:2])

  #Avg NDVI
  data.impalaRSF.train$ndvi <- raster::extract(ndvi, data.impalaRSF.train[,1:2])

  #Remove rows containing NA

  data.impalaRSF.train <- na.omit(data.impalaRSF.train)

  impalaRSF2.fit.train <- glm(Used ~ veg1.5m + ndvi +slope + rivers,
                              data = data.impalaRSF.train,
                              family = binomial(link = "logit"))

  coef(impalaRSF2.fit.train)%>%exp()

  vif(impalaRSF2.fit.train)

  hab1 <- stack(ndvi, slope, veg1.5m, rivers)
  names(hab1) <- c("ndvi", "slope", "veg1.5m", "rivers")
  impalaRSF2.predict.train <- terra::predict(object = hab1, model = impalaRSF2.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.impalaRSF.test$RSF <- raster::extract(impalaRSF2.predict.train, data.impalaRSF.test[,1:2])

  #Remove rows containing NA

  data.impalaRSF.test <- na.omit(data.impalaRSF.test)

  #*****R2----
  obs_data <- as.numeric(data.impalaRSF.test$Used)
  sse <- sum((obs_data-data.impalaRSF.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  impala_cv_results$R2[f+40] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.impalaRSF.test$Used)-data.impalaRSF.test$RSF)^2 ))

  impala_cv_results$RMSE[f+40] <- rmse

  #*****ROC----
  pred <- prediction(data.impalaRSF.test$RSF, data.impalaRSF.test$Used)

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

  impala_cv_results$AUC[f+40] <- auc_val
}

#Deleting place-holder row
rsf2_roc_raw <- rsf2_roc_raw[-1,]

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

rsf9_roc <- average_roc(fprs = rsf9_roc_raw$fpr, tprs_data = rsf9_roc_raw, m = "RSF9")
rsf1_roc <- average_roc(fprs = rsf1_roc_raw$fpr, tprs_data = rsf1_roc_raw, m = "RSF1")
rsf10_roc <- average_roc(fprs = rsf10_roc_raw$fpr, tprs_data = rsf10_roc_raw, m = "RSF10")
rsf3_roc <- average_roc(fprs = rsf3_roc_raw$fpr, tprs_data = rsf3_roc_raw, m = "RSF3")
rsf2_roc <- average_roc(fprs = rsf2_roc_raw$fpr, tprs_data = rsf2_roc_raw, m = "RSF2")

impala_rocs <- bind_rows(rsf9_roc,
                         rsf1_roc,
                         rsf10_roc,
                         rsf3_roc,
                         rsf2_roc)

impala_cv_sum <- impala_cv_results %>%
  group_by(Model) %>%
  summarise(mRMSE = mean(RMSE),
            mR2 = mean(R2),
            mAUC = mean(AUC))%>%
  merge(aics, by = "Model")

write.csv(impala_cv_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/impala_RSF_Model_Selection.csv")

impala_rocs <- merge(x = impala_rocs, y = impala_cv_sum,
                     by = "Model")

impala_rocs$legend <- NA

for(i in 1:length(impala_rocs$Model)){
  impala_rocs$legend[i] <- paste(impala_rocs$Model[i]," ","(",impala_rocs$mAUC[i],")",sep="")
}

impala_roc_plot <- ggplot(impala_rocs, aes(x = fpr, y = tpr))+
  geom_line(aes(color = legend))+
  labs(x = "False Positive Rate", y = "True Positive Rate")+
  ggtitle("Model ROC Curves (Impala Data Only)")+
  scale_color_discrete(name = "Model (AUC)")


ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/ROC/ImpalaROC.png",
       plot = impala_roc_plot,
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

impala_rocs
