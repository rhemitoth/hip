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
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/Manuscript/PreyCatchability.R")

#All Herbivore RSF----
#Initializing RMSE results tables----
cv_results <- tibble(Model = "",
                             fold = rep(seq(1,10,1), times = 4),
                             RMSE = 0.001,
                             R2 = 0.001,
                             AUC = 0.001,
                     AIC = 0.001)

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


#RSF1----
print("Executing RSF1 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  cv_results$Model[f] <- "RSF1"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.rsf.train <- data.rsf %>%
    filter(fold != f)

  data.rsf.test <- data.rsf %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.rsf.train$veg1m <- raster::extract(veg1m, data.rsf.train[,1:2])

  # Slope
  data.rsf.train$slope <- raster::extract(slope,data.rsf.train[,1:2])

  #Remove rows containing NA

  data.rsf.train <- na.omit(data.rsf.train)

  RSF1.fit.train <- glm(Used ~ veg1m +slope,
                               data = data.rsf.train,
                               family = binomial(link = "logit"))

  coef(RSF1.fit.train)%>%exp()

  vif(RSF1.fit.train)

  hab1 <- stack(slope, veg1m)
  names(hab1) <- c("slope", "veg1m")
  RSF1.predict.train <- terra::predict(object = hab1, model = RSF1.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.rsf.test$RSF <- raster::extract(RSF1.predict.train, data.rsf.test[,1:2])

  #Remove rows containing NA

  data.rsf.test <- na.omit(data.rsf.test)

  #*****R2----
  obs_data <- as.numeric(data.rsf.test$Used)
  sse <- sum((obs_data-data.rsf.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  cv_results$R2[f] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.rsf.test$Used)-data.rsf.test$RSF)^2 ))

  cv_results$RMSE[f] <- rmse

  #*****ROC----
  pred <- prediction(data.rsf.test$RSF, data.rsf.test$Used)

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

  cv_results$AUC[f] <- auc_val

}

#Deleting place-holder row
rsf1_roc_raw <- rsf1_roc_raw[-1,]

#RSF3----
print("Executing RSF3 Cross Validation")
for(i in 1:10){
  f <- fold_sum$fold[i]
  cv_results$Model[f+10] <- "RSF3"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.rsf.train <- data.rsf %>%
    filter(fold != f)

  data.rsf.test <- data.rsf %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.rsf.train$veg1m <- raster::extract(veg1m, data.rsf.train[,1:2])

  #Remove rows containing NA

  data.rsf.train <- na.omit(data.rsf.train)

  RSF3.fit.train <- glm(Used ~ veg1m,
                               data = data.rsf.train,
                               family = binomial(link = "logit"))

  coef(RSF3.fit.train)%>%exp()

  hab1 <- stack(veg1m)
  names(hab1) <- c("veg1m")
  RSF3.predict.train <- terra::predict(object = hab1, model = RSF3.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.rsf.test$RSF <- raster::extract(RSF3.predict.train, data.rsf.test[,1:2])

  #Remove rows containing NA

  data.rsf.test <- na.omit(data.rsf.test)

  #*****R2----
  obs_data <- as.numeric(data.rsf.test$Used)
  sse <- sum((obs_data-data.rsf.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  cv_results$R2[f+10] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.rsf.test$Used)-data.rsf.test$RSF)^2 ))

  cv_results$RMSE[f+10] <- rmse

  #*****ROC----
  pred <- prediction(data.rsf.test$RSF, data.rsf.test$Used)

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

  cv_results$AUC[f+10] <- auc_val

}

#Deleting place-holder row
rsf3_roc_raw <- rsf3_roc_raw[-1,]


#RSF2----
print("Executing RSF2 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  cv_results$Model[f+20] <- "RSF2"
  print(paste(toString(f/10*100), " %"))

  #*****Partitioning data into training vs test using 90% 10% split----

  data.rsf.train <- data.rsf %>%
    filter(fold != f)

  data.rsf.test <- data.rsf %>%
    filter(fold == f)

  #*****Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.rsf.train$slope <- raster::extract(slope, data.rsf.train[,1:2])

  #Remove rows containing NA

  data.rsf.train <- na.omit(data.rsf.train)

  RSF2.fit.train <- glm(Used ~ slope,
                               data = data.rsf.train,
                               family = binomial(link = "logit"))

  hab1 <- stack(slope)
  names(hab1) <- c("slope")
  RSF2.predict.train <- terra::predict(object = hab1, model = RSF2.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.rsf.test$RSF <- raster::extract(RSF2.predict.train, data.rsf.test[,1:2])

  #Remove rows containing NA

  data.rsf.test <- na.omit(data.rsf.test)

  #*****R2----
  obs_data <- as.numeric(data.rsf.test$Used)
  sse <- sum((obs_data-data.rsf.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  cv_results$R2[f+20] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.rsf.test$Used)-data.rsf.test$RSF)^2 ))

  cv_results$RMSE[f+20] <- rmse

  #*****ROC----
  pred <- prediction(data.rsf.test$RSF, data.rsf.test$Used)

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

  cv_results$AUC[f+20] <- auc_val
}

#Deleting place-holder row
rsf2_roc_raw <- rsf2_roc_raw[-1,]

#RSF4----
print("Executing RSF4 Cross Validation...")
for(i in 1:10){
  f <- fold_sum$fold[i]
  cv_results$Model[f+30] <- "RSF4"
  print(paste(toString(f/10*100), " %"))

  #******Partitioning data into training vs test using 90% 10% split----

  data.rsf.train <- data.rsf %>%
    filter(fold != f)

  data.rsf.test <- data.rsf %>%
    filter(fold == f)

  #******Extract Covariates from Training Data----

  # Vegetation Openness 1m
  data.rsf.train$veg1m <- raster::extract(veg1m, data.rsf.train[,1:2])

  # Slope
  data.rsf.train$slope <- raster::extract(slope,data.rsf.train[,1:2])

  #Distance to Major Rivers
  data.rsf.train$rivers<- raster::extract(rivers, data.rsf.train[,1:2])

  #Remove rows containing NA

  data.rsf.train <- na.omit(data.rsf.train)

  #*****Fittind training data to Model----

  RSF4.fit.train <- glm(Used ~ veg1m +slope+rivers,
                               data = data.rsf.train,
                               family = binomial(link = "logit"))

  coef(RSF4.fit.train)%>%exp()

  vif(RSF4.fit.train)

  hab <- stack(slope, veg1m,rivers)
  names(hab) <- c("slope", "veg1m", "rivers")
  RSF4.predict.train <- terra::predict(object = hab, model = RSF4.fit.train, type = "response")

  #*****Extracting RSF score for test data----

  data.rsf.test$RSF <- raster::extract(RSF4.predict.train,data.rsf.test[,1:2])

  #Remove rows containing NA

  data.rsf.test <- na.omit(data.rsf.test)

  #*****R2----
  obs_data <- as.numeric(data.rsf.test$Used)
  sse <- sum((obs_data-data.rsf.test$RSF)^2)
  obs_mean <- mean(obs_data)
  sst <- sum((obs_data - obs_mean)^2)
  r2 <- 1 - (sse/sst)
  cv_results$R2[f+30] <- r2

  #*****RMSE----
  rmse <- sqrt(mean((as.numeric(data.rsf.test$Used)-data.rsf.test$RSF)^2 ))

  cv_results$RMSE[f+30] <- rmse

  #*****ROC----
  pred <- prediction(data.rsf.test$RSF, data.rsf.test$Used)

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
  cv_results$AUC[f+30] <- auc_val

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
rsf1_roc <- average_roc(fprs = rsf1_roc_raw$fpr, tprs_data = rsf1_roc_raw, m = "RSF1")
rsf2_roc <- average_roc(fprs = rsf2_roc_raw$fpr, tprs_data = rsf2_roc_raw, m = "RSF2")
rsf3_roc <- average_roc(fprs = rsf3_roc_raw$fpr, tprs_data = rsf3_roc_raw, m = "RSF3")
rsf4_roc <- average_roc(fprs = rsf4_roc_raw$fpr, tprs_data = rsf4_roc_raw, m = "RSF4")

rocs <- bind_rows(rsf1_roc,
                  rsf2_roc,
                          rsf3_roc,
                          rsf4_roc)

cv_sum <- cv_results %>%
  group_by(Model) %>%
  summarise(mRMSE = mean(RMSE),
            mR2 = mean(R2),
            mAUC = mean(AUC)) %>%
  merge(aics, by = "Model")

write.csv(cv_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/cv_results/Prey_Catchability_Cross_Validation.csv" )

write.csv(cv_sum,
          "/Users/rhemitoth/Documents/Lion_Movement/Manuscript/PreyCatchability/Prey_Catchability_RSF_CV.csv" )

rocs <- merge(x = rocs, y = cv_sum,
                      by = "Model")

rocs$legend <- NA

for(i in 1:length(rocs$Model)){
  rocs$legend[i] <- paste(rocs$Model[i]," ","(",rocs$mAUC[i],")",sep="")
}

roc_plot <- ggplot(rocs, aes(x = fpr, y = tpr))+
  geom_line(aes(color = legend))+
  labs(x = "False Positive Rate", y = "True Positive Rate")+
  ggtitle("Prey Catchability Model ROC Curves")+
  scale_color_discrete(name = "Model (AUC)")


roc_plot

ggsave(filename = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/plots/ROC/Prey_Catchability_ROC.png",
       plot = roc_plot,
       units = c("in"),
       width = 6,
       height = 5,
       dpi = 350)
