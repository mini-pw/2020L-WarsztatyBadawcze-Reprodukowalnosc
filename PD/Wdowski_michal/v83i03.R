#################
# preliminaries #
#################

library("EMMIXcskew")
library("EMMIXskew")
set.seed(1)

###############################################
# Fitting a CFUST distribution - section 4.2  #
###############################################

data("iris", package = "datasets")
iris.versicolor <- subset(iris, Species == "versicolor",
  c(Sepal.Width, Petal.Length))
system.time(Fit.versicolor <- fmcfust(1, iris.versicolor))
summary(Fit.versicolor)
Fit.versicolor

###########################################
# Fitting a FM-CFUST model - section 4.3  #
###########################################

fit.unrestricted <- fmmst(3, iris[, -5])
fit.iris <- fmcfust(3, iris[, -5], initial = fit.unrestricted, method = "EMMIXuskew")
fit.iris
summary(fit.iris)

############################################################
# Nested cases of the FM-CFUST distribution - section 4.4  #
############################################################

fit.restricted <- fmcfust(3, iris[, -5], 1)
table(iris$Species, fit.iris$clust)
table(iris$Species, fit.restricted$clust)
table(iris$Species, fit.unrestricted$clust)
error.rate(unclass(iris$Species), fit.iris$clust)
error.rate(unclass(iris$Species), fit.restricted$clust)
error.rate(unclass(iris$Species), fit.unrestricted$clust)
panel1 <- function(x, y, ...) {
    points(x, y, col = c("red", "green3", "blue")[fit.iris$clust], pch = 20)
}
panel2 <- function(x, y, ...) {
    points(x, y, col = c("red", "green3", "blue")[fit.unrestricted$clust], pch = 20)
}
panel3 <- function(x, y, ...) {
    points(x, y, col = c("red", "green3", "blue")[fit.restricted$clust], pch = 20)
}
pairs(iris[1:4], main = "Iris Data", pch = 20, col = c("red","green3","blue")[unclass(iris$Species)], lower.panel = panel1)
pairs(iris[1:4], main = "Iris Data", upper.panel = panel2, lower.panel = panel3)

######################################################
# Random sample from a FM-CFUST model - section 5.1  #
######################################################

RNGversion("3.1.1"); set.seed(1)
rcfust(10, c(1, 2), diag(2), matrix(c(2, 1, 1, 2), 2, 2), 4)
obj <- list()
obj$mu <- list(c(17, 19), c(5, 22), c(6, 10))
obj$sigma <- list(diag(2), matrix(c(2, 0, 0, 1), 2), matrix(c(3, 7, 7, 24), 2))
obj$delta <- list(matrix(c(3, 0, 2, 1.5), 2, 2), matrix(c(5, 0, 0, 10), 2, 2), matrix(c(2, 0, 5, 0), 2, 2))
obj$dof <- c(1, 2, 3)
obj$pro <- c(0.25, 0.25, 0.5)
rfmcfust(3, 100, known = obj)                                 
                         
#######################################################
# Starting values for a FM-CFUST model - section 5.2  #
#######################################################

data("geyser", package = "MASS")
plot(geyser, pch = 20)                        
initial.default <- init.fmcfust(3, geyser)
initial.transformation <- init.fmcfust(3, geyser, method = "transformation")
fit.geyser.restricted <- EmSkew(geyser, 3, "mst", debug = FALSE)
initial.restricted <- init.fmcfust(3, geyser, initial = fit.geyser.restricted, method = "EMMIXskew")
fit.geyser.unrestricted <- fmmst(3, geyser)
initial.unrestricted <- init.fmcfust(3, geyser, initial = fit.geyser.unrestricted, method = "EMMIXuskew")
fit.geyser.t <- EmSkew(geyser, 3, "mvt", debug = FALSE)
initial.t <- init.fmcfust(3, geyser, initial  = fit.geyser.t, method = "EMMIXskew")                        
initial.default$loglik
initial.transformation$loglik
initial.restricted$loglik
initial.unrestricted$loglik
initial.t$loglik                        
fit.geyser1 <- fmcfust(3, geyser, initial = initial.default)
fit.geyser2 <- fmcfust(3, geyser, initial = initial.t)
fit.geyser3 <- fmcfust(3, geyser, initial = initial.restricted)
fit.geyser1$loglik
fit.geyser2$loglik
fit.geyser3$loglik                         
plot(geyser, pch = 20, col = c("red", "blue", "green")[fit.geyser1$clust])
plot(geyser, pch = 20, col = c("red", "blue", "green")[fit.geyser2$clust])
plot(geyser, pch = 20, col = c("red", "blue", "green")[fit.geyser3$clust])

#######################################################
# Stopping criteria - section 5.3                     #
#######################################################

fit.geyser4 <- fmcfust(3, geyser, initial = initial.restricted, convergence = "likelihood")
fit.geyser5 <- fmcfust(3, geyser, initial = initial.restricted, convergence = "parameters")

#######################################################
# Selecting g - section 5.4                           #
#######################################################

fit.geyser.g1 <- fmcfust(1, geyser)
fit.geyser.g2 <- fmcfust(2, geyser) 
fit.geyser.g3 <- fit.geyser1   
fit.geyser.g4 <- fmcfust(4, geyser)
fit.geyser.g1$bic       
fit.geyser.g2$bic
fit.geyser.g3$bic 
fit.geyser.g4$bic
 
###################################################
# Visualization of fitted contours - section 5.5  #
###################################################

fmcfust.contour.2d(iris.versicolor, Fit.versicolor, drawpoints = TRUE,
                   main = "versicolor", lwd = 2, xlim = c(1.5, 4),
                   ylim = c(2.5, 5.5), pcol = "black", ccol = "blue")
obj <- list()
obj$mu <- list(matrix(c(0, 0, 0), 3), matrix(c(5, 5, 5), 3))
obj$sigma <- list(matrix(c(5, 2, 1, 2, 5, 1, 1, 1, 1), 3, 3), 2 * diag(3))
obj$delta <- list(matrix(c(1, 0, 0, 1, 0, 0, 1, 0, 0), 3, 3),
                  matrix(c(5, 0, 0, 0, 10, 0, 0, 0, 15), 3, 3))                     
obj$dof <- c(3, 3)
obj$pro <- c(0.2, 0.8)
fmcfust.contour.3d(model = obj, level = 0.98, drawpoints = FALSE, xlab = "X",
                   ylab = "Y", zlab = "Z", xlim = c(-20, 50), ylim = c(-20, 50),
                   zlim = c(-20, 80), ccol = "green")
RNGversion("3.1.1"); set.seed(1)
X <- rfmcfust(g = 2, n = 500, known = obj)
fmcfust.contour.3d(dat = X[, -4], model = obj, level = c(0.99, 0.92),
                   drawpoints = TRUE, clust = X[, 4], xlab = "X", ylab = "Y",
                   zlab = "Z", xlim = c(-20, 50), ylim = c(-20, 50),
                   zlim = c(-20, 80), component = 1:2, pcol = c("red", "blue"),
                   ccol = c("red", "blue"))
