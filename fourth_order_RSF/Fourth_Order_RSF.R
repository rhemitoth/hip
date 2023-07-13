
#source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/fourth_order_RSF/fourth_order_RSF_data.R")

# Fitting RSF ----

#**** RSF1 Prey Abundance----

#Fitting RSF
RSF1.fit <- glm(Kill ~ PreyAbundance,
                data = data.rsf,
                family = binomial(link = "logit"))


coef(RSF1.fit)%>%exp()

#**** RSF2 Prey Catchability----

RSF2.fit <- glm(Kill ~ PreyCatchability,
                data = data.rsf,
                family = binomial(link = "logit"))

#**** RSF3 Slope, Veg, NDVI----

RSF3.fit <- glm(Kill ~ slope + veg1m + ndvi,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF3.fit)%>%exp()

#**** RSF4 Slope, Veg ----

#Fitting RSF
RSF4.fit <- glm(Kill ~ veg1m + slope ,
                data = data.rsf,
                family = binomial(link = "logit"))

coef(RSF4.fit)%>%exp()

#AIC ----
models <- c("RSF1", "RSF2", "RSF3", "RSF4")

AICtab(RSF1.fit,
       RSF2.fit,
       RSF3.fit,
       RSF4.fit)

aic_scores <- AIC(RSF1.fit,
                  RSF2.fit,
                  RSF3.fit,
                  RSF4.fit)

aics <- tibble(Model = models,
               AIC = aic_scores$AIC)
