## ****************************************************
## Title: Home Range Estiamtes Script
## Author: Rhemi Toth
## Date Created: 10/17/2022
## Date Last Updated: 10/18/2022
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

##
## ****************************************************

#Loading Data----
source("~/Documents/Lion_Movement/R/hip_lion_issa/scripts/HR_Data.R")


#Babe----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Babe is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Babe_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#****Obtaining starting values for model parameters----

vg_babe <- variogram(Babe_telem)
variogram.fit(vg_babe) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

babe.fitted.mods <- ctmm.select(Babe_telem,
                           CTMM = GUESS,
                           verbose = TRUE,
                           cores = 8,
                           trace = TRUE)

summary(babe.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

babe.iid <- babe.fitted.mods[[6]]
babe.ou <- babe.fitted.mods[[1]]
babe.ouf <- babe.fitted.mods[[5]]


par(mfrow=c(3,2))
plot(vg_babe, CTMM=babe.iid, col.CTMM="darkolivegreen4")
plot(vg_babe, CTMM=babe.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_babe, CTMM=babe.ou, col.CTMM="salmon")
plot(vg_babe, CTMM=babe.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_babe, CTMM=babe.ouf, col.CTMM="cornflowerblue")
plot(vg_babe, CTMM=babe.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----

akde.babe <- akde(Babe_telem, CTMM = babe.ou)
par(mfrow=c(1,1))
plot(Babe_telem, UD = akde.babe)
#95% akde
writeShapefile(object = akde.babe,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Babe/",
               file = "Babe_AKDE_95.shp",
               level.UD = 0.95)

babe_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Babe/Babe_AKDE_95.shp")
plot(babe_akde_95)
#50%akde
writeShapefile(object = akde.babe,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Babe/",
               file = "Babe_AKDE_50",
               level.UD = 0.5)
babe_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Babe/Babe_AKDE_50.shp")
plot(babe_akde_50)

#Fanbelt----

#****Variograms----


level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Fanbelt_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)


#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_fanbelt <- variogram(Fanbelt_telem)
variogram.fit(vg_fanbelt, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

fanbelt.fitted.mods <- ctmm.select(Fanbelt_telem,
                                CTMM = GUESS,
                                verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(fanbelt.fitted.mods)#OUF anisotropic for fanbelt

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

fanbelt.ou <- fanbelt.fitted.mods[[3]]
fanbelt.ouf <- fanbelt.fitted.mods[[1]]


par(mfrow=c(2,2))
plot(vg_fanbelt, CTMM=fanbelt.ou, col.CTMM="salmon")
plot(vg_fanbelt, CTMM=fanbelt.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_fanbelt, CTMM=fanbelt.ouf, col.CTMM="cornflowerblue")
plot(vg_fanbelt, CTMM=fanbelt.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.fanbelt <- akde(Fanbelt_telem, CTMM = fanbelt.ouf)
plot(Fanbelt_telem, UD = akde.fanbelt)
#95% akde
writeShapefile(object = akde.fanbelt,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Fanbelt/",
               file = "Fanbelt_AKDE_95",
               level.UD = 0.95)

fanbelt_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Fanbelt/Fanbelt_AKDE_95.shp")
plot(fanbelt_akde_95)
#50%akde
writeShapefile(object = akde.fanbelt,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Fanbelt/",
               file = "Fanbelt_AKDE_50",
               level.UD = 0.5)
fanbelt_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Fanbelt/Fanbelt_AKDE_50.shp")
plot(fanbelt_akde_50)

#Fluffy----

#****Variograms----

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Fluffy_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Variogram doesnt reach asymptote so fluffly is not a range resident

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_fluffy <- variogram(Fluffy_telem)
variogram.fit(vg_fluffy, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

fluffy.fitted.mods <- ctmm.select(Fluffy_telem,
                                   CTMM = GUESS,
                                   verbose = TRUE,
                                  cores = 0,
                                  trace = TRUE)

summary(fluffy.fitted.mods)

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

fluffy.iid <- fluffy.fitted.mods[[6]]
fluffy.ou <- fluffy.fitted.mods[[1]]
fluffy.ouf <- fluffy.fitted.mods[[2]]


par(mfrow=c(3,2))
plot(vg_fluffy, CTMM=fluffy.iid, col.CTMM="darkolivegreen4")
plot(vg_fluffy, CTMM=fluffy.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_fluffy, CTMM=fluffy.ou, col.CTMM="salmon")
plot(vg_fluffy, CTMM=fluffy.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_fluffy, CTMM=fluffy.ouf, col.CTMM="cornflowerblue")
plot(vg_fluffy, CTMM=fluffy.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----

akde.fluffy <- akde(Fluffy_telem, CTMM = fluffy.ou)
plot(fluffy_telem, UD = akde.fluffy)
akde.fluffy.sf <- as.sf(akde.fluffy)
writeShapefile(object = akde.fluffy,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/exports/Lion_Home_Ranges/",
               file = "Lion_Home_Ranges/Fluffy_HR.shp")

#iHlane----

#****Variograms----

#Asymptote of variogram at longer lags suggests that iHlane is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(iHlane_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_ihlane <- variogram(iHlane_telem)
variogram.fit(vg_ihlane, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

ihlane.fitted.mods <- ctmm.select(iHlane_telem,
                                  CTMM = GUESS,
                                  verbose = TRUE,
                                  cores = 0,
                                  trace = TRUE)

summary(ihlane.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

ihlane.iid <- ihlane.fitted.mods[[6]]
ihlane.ou <- ihlane.fitted.mods[[1]]
ihlane.ouf <- ihlane.fitted.mods[[2]]


par(mfrow=c(3,2))
plot(vg_ihlane, CTMM=ihlane.iid, col.CTMM="darkolivegreen4")
plot(vg_ihlane, CTMM=ihlane.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_ihlane, CTMM=ihlane.ou, col.CTMM="salmon")
plot(vg_ihlane, CTMM=ihlane.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_ihlane, CTMM=ihlane.ouf, col.CTMM="cornflowerblue")
plot(vg_ihlane, CTMM=ihlane.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.ihlane <- akde(iHlane_telem, CTMM = ihlane.ou)
plot(iHlane_telem, UD = akde.ihlane)
akde.ihlane.sf <- as.sf(akde.ihlane)
#95% akde
writeShapefile(object = akde.ihlane,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/iHlane/",
               file = "iHlane_AKDE_95",
               level.UD = 0.95)

iHlane_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/iHlane/iHlane_AKDE_95.shp")
plot(iHlane_akde_95)
#50%akde
writeShapefile(object = akde.ihlane,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/iHlane/",
               file = "iHlane_AKDE_50",
               level.UD = 0.5)
iHlane_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/iHlane/iHlane_AKDE_50.shp")
plot(iHlane_akde_50)

#Koku----

#****Variograms----

#Asymptote of variogram at longer lags suggests that koku is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Koku_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_koku <- variogram(Koku_telem)
variogram.fit(vg_koku, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

koku.fitted.mods <- ctmm.select(Koku_telem,
                                  CTMM = GUESS,
                                  verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(koku.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

koku.iid <- koku.fitted.mods[[6]]
koku.ou <- koku.fitted.mods[[1]]
koku.ouf <- koku.fitted.mods[[2]]


par(mfrow=c(3,2))
plot(vg_koku, CTMM=koku.iid, col.CTMM="darkolivegreen4")
plot(vg_koku, CTMM=koku.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_koku, CTMM=koku.ou, col.CTMM="salmon")
plot(vg_koku, CTMM=koku.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_koku, CTMM=koku.ouf, col.CTMM="cornflowerblue")
plot(vg_koku, CTMM=koku.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.koku <- akde(Koku_telem, CTMM = koku.ou)
plot(Koku_telem, UD = akde.koku)
#95% akde
writeShapefile(object = akde.koku,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Koku/",
               file = "Koku_AKDE_95",
               level.UD = 0.95)

koku_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Koku/Koku_AKDE_95.shp")
plot(iHlane_akde_95)
#50%akde
writeShapefile(object = akde.koku,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Koku/",
               file = "Koku_AKDE_50",
               level.UD = 0.5)
koku_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Koku/Koku_AKDE_50.shp")
plot(koku_akde_50)
#Madonna----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Madonna is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Madonna_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_madonna <- variogram(Madonna_telem)
variogram.fit(vg_madonna, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

madonna.fitted.mods <- ctmm.select(Madonna_telem,
                                CTMM = GUESS,
                                verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(madonna.fitted.mods) #OUF anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

madonna.ou <- madonna.fitted.mods[[3]]
madonna.ouf <- madonna.fitted.mods[[1]]


par(mfrow=c(2,2))
plot(vg_madonna, CTMM=madonna.ou, col.CTMM="salmon")
plot(vg_madonna, CTMM=madonna.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_madonna, CTMM=madonna.ouf, col.CTMM="cornflowerblue")
plot(vg_madonna, CTMM=madonna.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.madonna <- akde(Madonna_telem, CTMM = madonna.ouf)
plot(Madonna_telem, UD = akde.madonna)
#95% akde
writeShapefile(object = akde.madonna,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Madonna/",
               file = "Madonna_AKDE_95",
               level.UD = 0.95)

madonna_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Madonna/Madonna_AKDE_95.shp")
plot(madonna_akde_95)
#50%akde
writeShapefile(object = akde.madonna,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Madonna/",
               file = "Madonna_AKDE_50",
               level.UD = 0.5)
madonna_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Madonna/Madonna_AKDE_50.shp")
plot(madonna_akde_50)

#Mellon----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Fanbelt is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Mellon_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.01,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_mellon <- variogram(Mellon_telem)
variogram.fit(vg_mellon, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

mellon.fitted.mods <- ctmm.select(Mellon_telem,
                                   CTMM = GUESS,
                                   verbose = TRUE,
                                  cores = 0,
                                  trace = TRUE)

summary(mellon.fitted.mods) #OUF anisotropic is best

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----


mellon.ou <- mellon.fitted.mods[[2]]
mellon.ouf <- mellon.fitted.mods[[1]]


par(mfrow=c(2,2))

plot(vg_mellon, CTMM=mellon.ou, col.CTMM="salmon")
plot(vg_mellon, CTMM=mellon.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_mellon, CTMM=mellon.ouf, col.CTMM="cornflowerblue")
plot(vg_mellon, CTMM=mellon.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.mellon <- akde(Mellon_telem, CTMM = mellon.ouf)
plot(Mellon_telem, UD = akde.mellon)
#95% akde
writeShapefile(object = akde.mellon,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Mellon/",
               file = "Mellon_AKDE_95",
               level.UD = 0.95)

mellon_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Mellon/Mellon_AKDE_95.shp")
plot(mellon_akde_95)
#50%akde
writeShapefile(object = akde.mellon,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Mellon/",
               file = "Mellon_AKDE_50",
               level.UD = 0.5)
mellon_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Mellon/Mellon_AKDE_50.shp")
plot(mellon_akde_50)

#Murph----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Murph is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Murph_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_murph <- variogram(Murph_telem)
variogram.fit(vg_murph, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

murph.fitted.mods <- ctmm.select(Murph_telem,
                                  CTMM = GUESS,
                                  verbose = TRUE,
                                 cores = 0,
                                 trace = TRUE)

summary(murph.fitted.mods) #OUF anisotropic is best

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

murph.ou <- murph.fitted.mods[[3]]
murph.ouf <- murph.fitted.mods[[1]]


par(mfrow=c(2,2))

plot(vg_murph, CTMM=murph.ou, col.CTMM="salmon")
plot(vg_murph, CTMM=murph.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_murph, CTMM=murph.ouf, col.CTMM="cornflowerblue")
plot(vg_murph, CTMM=murph.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.murph <- akde(Murph_telem, CTMM = murph.ouf)
plot(Murph_telem, UD = akde.murph)
#95% akde
writeShapefile(object = akde.murph,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Murph/",
               file = "Murph_AKDE_95",
               level.UD = 0.95)

murph_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Murph/Murph_AKDE_95.shp")
plot(murph_akde_95)
#50%akde
writeShapefile(object = akde.murph,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Murph/",
               file = "Murph_AKDE_50",
               level.UD = 0.5)
murph_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Murph/Murph_AKDE_50.shp")
plot(murph_akde_50)

#Ntombi----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Ntombi is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Ntombi_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_ntombi <- variogram(Ntombi_telem)
variogram.fit(vg_ntombi, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

ntombi.fitted.mods <- ctmm.select(Ntombi_telem,
                                 CTMM = GUESS,
                                 verbose = TRUE,
                                 cores = 0,
                                 trace = TRUE)

summary(ntombi.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

ntombi.ou <- ntombi.fitted.mods[[2]]
ntombi.ouf <- ntombi.fitted.mods[[1]]


par(mfrow=c(2,2))
plot(vg_ntombi, CTMM=ntombi.ou, col.CTMM="salmon")
plot(vg_ntombi, CTMM=ntombi.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_ntombi, CTMM=ntombi.ouf, col.CTMM="cornflowerblue")
plot(vg_ntombi, CTMM=ntombi.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.ntombi <- akde(Ntombi_telem, CTMM = ntombi.ouf)
plot(Ntombi_telem, UD = akde.ntombi)
#95% akde
writeShapefile(object = akde.ntombi,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Ntombi/",
               file = "Ntombi_AKDE_95",
               level.UD = 0.95)

ntombi_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Ntombi/Ntombi_AKDE_95.shp")
plot(ntombi_akde_95)
#50%akde
writeShapefile(object = akde.ntombi,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Ntombi/",
               file = "Ntombi_AKDE_50",
               level.UD = 0.5)
ntombi_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Ntombi/Ntombi_AKDE_50.shp")
plot(ntombi_akde_50)


#PokeJR----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Fanbelt is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(PokeJr_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_poke <- variogram(PokeJr_telem)
variogram.fit(vg_poke, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

poke.fitted.mods <- ctmm.select(PokeJr_telem,
                                  CTMM = GUESS,
                                  verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(poke.fitted.mods) #OU anisotropic is best

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

poke.iid <- poke.fitted.mods[[6]]
poke.ou <- poke.fitted.mods[[1]]
poke.ouf <- poke.fitted.mods[[2]]


par(mfrow=c(3,2))
plot(vg_poke, CTMM=poke.iid, col.CTMM="darkolivegreen4")
plot(vg_poke, CTMM=poke.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_poke, CTMM=poke.ou, col.CTMM="salmon")
plot(vg_poke, CTMM=poke.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_poke, CTMM=poke.ouf, col.CTMM="cornflowerblue")
plot(vg_poke, CTMM=poke.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.poke <- akde(PokeJr_telem, CTMM = poke.ou)
plot(PokeJr_telem, UD = akde.poke)
#95% akde
writeShapefile(object = akde.poke,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/PokeJr/",
               file = "PokeJr_AKDE_95",
               level.UD = 0.95)

poke_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/PokeJr/PokeJr_AKDE_95.shp")
plot(poke_akde_95)
#50%akde
writeShapefile(object = akde.poke,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/PokeJr/",
               file = "PokeJr_AKDE_50",
               level.UD = 0.5)
poke_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/PokeJr/PokeJr_AKDE_50.shp")
plot(poke_akde_50)

#Roxy----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Fanbelt is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Roxy_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_roxy <- variogram(Roxy_telem)
variogram.fit(vg_roxy, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

roxy.fitted.mods <- ctmm.select(Roxy_telem,
                                CTMM = GUESS,
                                verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(roxy.fitted.mods) #OUF anisotropic is best

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

roxy.ou <- roxy.fitted.mods[[3]]
roxy.ouf <- roxy.fitted.mods[[1]]


par(mfrow=c(2,2))

plot(vg_roxy, CTMM=roxy.ou, col.CTMM="salmon")
plot(vg_roxy, CTMM=roxy.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_roxy, CTMM=roxy.ouf, col.CTMM="cornflowerblue")
plot(vg_roxy, CTMM=roxy.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.roxy <- akde(Roxy_telem, CTMM = roxy.ouf)
plot(Roxy_telem, UD = akde.roxy)
#95% akde
writeShapefile(object = akde.roxy,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Roxy/",
               file = "Roxy_AKDE_95",
               level.UD = 0.95)

roxy_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Roxy/Roxy_AKDE_95.shp")
plot(roxy_akde_95)
#50%akde
writeShapefile(object = akde.roxy,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Roxy/",
               file = "Roxy_AKDE_50",
               level.UD = 0.5)
roxy_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Roxy/Roxy_AKDE_50.shp")
plot(roxy_akde_50)


#Stud----

#****Variograms----

#Asymptote of variogram at longer lags suggests that stud is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Stud_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_stud <- variogram(Stud_telem)
variogram.fit(vg_stud, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

stud.fitted.mods <- ctmm.select(Stud_telem,
                                CTMM = GUESS,
                                verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(stud.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

stud.iid <- stud.fitted.mods[[6]]
stud.ou <- stud.fitted.mods[[1]]
stud.ouf <- stud.fitted.mods[[2]]


par(mfrow=c(3,2))
plot(vg_stud, CTMM=stud.iid, col.CTMM="darkolivegreen4")
plot(vg_stud, CTMM=stud.iid, col.CTMM="darkolivegreen4", fraction=0.005)

plot(vg_stud, CTMM=stud.ou, col.CTMM="salmon")
plot(vg_stud, CTMM=stud.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_stud, CTMM=stud.ouf, col.CTMM="cornflowerblue")
plot(vg_stud, CTMM=stud.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.stud <- akde(Stud_telem, CTMM = stud.ou)
plot(Stud_telem, UD = akde.stud)
#95% akde
writeShapefile(object = akde.stud,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Stud/",
               file = "Stud_AKDE_95",
               level.UD = 0.95)

stud_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Stud/Stud_AKDE_95.shp")
plot(stud_akde_95)
#50%akde
writeShapefile(object = akde.stud,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Stud/",
               file = "Stud_AKDE_50",
               level.UD = 0.5)
stud_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Stud/Stud_AKDE_50.shp")
plot(stud_akde_50)


#Thembi----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Fanbelt is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Thembi_telem)
par(mfrow=c(1,3))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_thembi <- variogram(Thembi_telem)
variogram.fit(vg_thembi, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

thembi.fitted.mods <- ctmm.select(Thembi_telem,
                                CTMM = GUESS,
                                verbose = TRUE,
                                cores = 0,
                                trace = TRUE)

summary(thembi.fitted.mods) #OU anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

thembi.ou <- thembi.fitted.mods[[3]]
thembi.ouf <- thembi.fitted.mods[[1]]


par(mfrow=c(2,2))

plot(vg_thembi, CTMM=thembi.ou, col.CTMM="salmon")
plot(vg_thembi, CTMM=thembi.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_thembi, CTMM=thembi.ouf, col.CTMM="cornflowerblue")
plot(vg_thembi, CTMM=thembi.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----
par(mfrow=c(1,1))
akde.thembi <- akde(Thembi_telem, CTMM = thembi.ouf)
plot(Thembi_telem, UD = akde.thembi)
#95% akde
writeShapefile(object = akde.thembi,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Thembi/",
               file = "Thembi_AKDE_95",
               level.UD = 0.95)

thembi_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Thembi/Thembi_AKDE_95.shp")
plot(thembi_akde_95)
#50%akde
writeShapefile(object = akde.thembi,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Thembi/",
               file = "Thembi_AKDE_50",
               level.UD = 0.5)
thembi_akde_50 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Thembi/Thembi_AKDE_50.shp")
plot(thembi_akde_50)


#Zulu----

#****Variograms----

#Asymptote of variogram at longer lags suggests that Fanbelt is a range resident
#Slight upward curve at short lags provides evidence of directional persistence
#Can consider IID, OU, and OUF models

level<-0.95#wewanttodisplay95%confidenceintervals

SVF<-variogram(Zulu_telem)
par(mfrow=c(1,1))
plot(SVF,fraction = 1,level=level)
abline(v=1,col="red",lty=2)#adding a line at 1month
plot(SVF,fraction = 0.5,level=level)
abline(v=1,col="red",lty=2) #adding a line at 1 week
plot(SVF,fraction = 0.001,level=level) #adding a line at 1 hr
abline(v=1,col="red",lty=2)

#Should we exclude past ~10 months for home range analysis?
#There's a plateau which indicates home range formation, but then variance increases past 10 months

#****Obtaining starting values for model parameters----
par(mfrow=c(1,1))
vg_zulu <- variogram(Zulu_telem)
variogram.fit(vg_zulu, interactive = TRUE) #Run and click "save to GUESS"

#****Fitting Range Resident Models Via ML----
#Using params obtained from variogram.fit()

zulu.fitted.mods <- ctmm.select(Zulu_telem,
                                  CTMM = GUESS,
                                  verbose = TRUE,
                                cores = 8,
                                trace = TRUE)

summary(zulu.fitted.mods) #OUF anisotropic is best, anisotropic better than isotropic counterparts

#****Visually examining fits of anisotropic versions of IID, OU, and OUF----

zulu.ou <- zulu.fitted.mods[[3]]
zulu.ouf <- zulu.fitted.mods[[1]]


par(mfrow=c(2,2))

plot(vg_zulu, CTMM=zulu.ou, col.CTMM="salmon")
plot(vg_zulu, CTMM=zulu.ou, col.CTMM="salmon", fraction=0.005)

plot(vg_zulu, CTMM=zulu.ouf, col.CTMM="cornflowerblue")
plot(vg_zulu, CTMM=zulu.ouf, col.CTMM="cornflowerblue", fraction=0.005)

#****AKDE----

akde.zulu <- akde(Zulu_telem, CTMM = zulu.ouf)
plot(Zulu_telem, UD = akde.zulu)
akde.zulu.sf <- as.sf(akde.zulu)
#95% akde
writeShapefile(object = akde.zulu,
               folder = "/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Zulu/",
               file = "Zulu_AKDE_95",
               level.UD = 0.95)

zulu_akde_95 <- shapefile("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/data_processed/akde/Zulu/Zulu_AKDE_95.shp")
plot(zulu_akde_95)


