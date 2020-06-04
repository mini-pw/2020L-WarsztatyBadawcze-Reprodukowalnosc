##################################################
## R code to reproduce results from 
## "meta4diag: Bayesian Bivariate Meta-analysis of 
##  Diagnostic Test Studies for Routine Practice"
## by Jingyi Guo and Andrea Riebler
##################################################

###################################################
## 3. Using package meta4diag
###################################################
## 3.1. Package overview
###################################################
## start: to test the code in paper, please first install the package
if (!require("meta4diag")) {
    install.packages("meta4diag")
}
if (!require("INLA")) {
    install.packages("INLA", repos = "https://inla.R-INLA-download.org/R/testing",
                     dependencies = TRUE)
}

###################################################
## 3.2. General data structure required
###################################################
studynames <- c("Ito_1998", "Rahat_1998", "Kavaler_1998", "Yoshida_1997", 
                "Ramakumar_1999", "Landman_1998", "Kinoshita_1997", 
                "Gelmini_2000", "Cheng_2000", "Cassel_2001")
TP <- c(25, 17, 88, 16, 40, 38, 23, 27, 14, 37)
FP <- c(1, 3, 16, 3, 1, 6, 0, 2, 3, 22)
TN <- c(25, 11, 31, 80, 137, 24, 12, 18, 29, 7)
FN <- c(8, 4, 16, 10, 17, 9, 19, 6, 3, 7)

Telomerase <- data.frame(studynames = studynames,
                         TP = TP, FP = FP, TN = TN, FN = FN)
head(Telomerase)

###################################################
## 3.3. Analyzing a standard meta-analysis without covariate
## information
###################################################
library("INLA")
library("meta4diag")

## test Telomerase data
set.seed(18674)
res <- meta4diag(data = Telomerase, model.type = 1, 
                 var.prior = "PC", var2.prior = "PC", cor.prior = "Normal", 
                 var.par = c(3, 0.05), cor.par = c(0, 5), 
                 link = "logit", nsample = 10000, seed = 1672)
## print the result from above model
summary(res)

## code for plot posterior marginals
par(mfrow = c(1, 3))
plot(res, var.type = "var1", overlay.prior = TRUE, lwd = 2, save = FALSE)
plot(res, var.type = "var2", overlay.prior = TRUE, lwd = 2, save = FALSE)
plot(res, var.type = "rho", overlay.prior = TRUE, lwd = 2, save = FALSE)

## print the fitted value of accuracy
fitted(res, accuracy.type = "TPR")
fitted(res, accuracy.type = "DOR")

## code for forest plot
forest(res, accuracy.type = "sens", est.type = "mean", cut = c(0.4, 1),
       nameShow = TRUE, estShow = TRUE, dataShow = "center",
       text.cex = 1.5, arrow.lwd = 1.5)

## code for crosshair plot
crosshair(res, est.type = "mean", col = 1:10)

## code for SROC plot
SROC(res, est.type = "mean", sroc.type = 1, dataShow = "o",
     crShow = TRUE, prShow = TRUE, pr.lwd = 2)

###################################################
## 3.4. Incorporating additional sub-data stratification
###################################################
## load Scheidler data
data("Scheidler", package = "meta4diag")
head(Scheidler)

## run subdata separately
res.CT <- meta4diag(data = Scheidler[Scheidler$modality == "CT", ])
res.LAG <- meta4diag(data = Scheidler[Scheidler$modality == "LAG", ])
res.MRI <- meta4diag(data = Scheidler[Scheidler$modality == "MRI", ])

## code for SROC plot
SROC(res.CT, dataShow = "o", lineShow = TRUE, prShow = FALSE,
     data.cex = "scaled", data.col = "red", cr.col = "red",
     sp.col = "red", line.col = "red", sp.cex = 2.5)
SROC(res.LAG, dataShow = "o", lineShow = TRUE, prShow = FALSE,
     data.cex = "scaled", data.col = "blue", cr.col = "blue", 
     sp.col = "blue", line.col = "blue", add = TRUE, sp.cex = 2.5)
SROC(res.MRI, dataShow = "o", lineShow = TRUE, prShow = FALSE,
     data.cex = "scaled", data.col = "green", cr.col = "green",
     sp.col = "green", line.col = "green", add = TRUE, sp.cex = 2.5)

## run the model for the whole data
res <- meta4diag(data = Scheidler, modality = "modality")
res
## print the result
summary(res)

## code for SROC plot
SROC(res, dataShow = "o", lineShow = TRUE, prShow = FALSE, data.cex = "scaled",
     cr.col = c("red", "blue", "green"), sp.col = c("red", "blue", "green"),
     line.col = c("red", "blue", "green"), sp.cex = 2.5)

## make forest plot for the whole data model
forest(res, accuracy.type = "sens")

###################################################
## 3.5. Use of continuous covariate information
###################################################
## load Catheter data
data("Catheter", package = "meta4diag")
head(Catheter)

## run model for Catheter data
set.seed(19876)
res <- meta4diag(data = Catheter, model.type = 2, var.prior = "PC", 
                 var2.prior = "PC", cor.prior = "PC", var.par = c(3, 0.05), 
                 cor.par = c(1, -0.1, 0.5, -0.95, 0.05, NA, NA),
                 modality = "type", covariates = "prevalence", 
                 quantiles = c(0.125, 0.875), nsample = 10000, seed = 1352)
## print the result
summary(res)

## make forest plot
forest(res, accuracy.type = "LDOR", est.type = "median", nameShow = TRUE, 
       estShow = "left", dataShow = "center", text.cex = 1.5, arrow.lwd = 1.5, 
       cut = c(0, 10), intervals = c(0.125, 0.875))

###################################################
## 3.6. Graphical user interface
###################################################
library("meta4diag")
library("INLA")
meta4diagGUI()


