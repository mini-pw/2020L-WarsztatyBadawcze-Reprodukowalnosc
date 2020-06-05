#
# Comparing pairs(), gpairs(), and ggairs()
#

library(gpairs)
library(GGally)
withcat <- TRUE  # Do you want a mix with categorical variables?

pdfname <- "BenchmarksAllQuant.pdf"
if (witcat) pdfname <- "BenchmarksMixed.pdf"

ans <- NULL
for (N in c(100, 200, 500, 1000, 5000, 10000)) {

  K <- 8

  x <- as.data.frame(matrix(rnorm(N*K), N, K))
  if (withcat) {
    x[,3] <- as.factor(round(x[,3]))
    x[,4] <- as.factor(round(x[,4]))
    x[,5] <- as.factor(round(x[,5]))
    x[,6] <- as.factor(round(x[,6]))
  }

  b <- system.time({
    print(pairs(x))
  })
  g <- system.time({
    print(gpairs(x))
  })
  gg <- system.time({
    print(ggpairs(x))
  })

  ans <- rbind(ans, c(N, b[3], g[3], gg[3]))

}
ans2 <- ans
ans <- as.data.frame(ans)
colnames(ans) <- c("N", "pairs", "gpairs", "ggpairs")
rownames(ans) <- NULL

if (!is.null(pdfname)) pdf(pdfname)
  plot(ans$N, ans$pairs, type="l", ylim=range(as.matrix(ans[,-1])),
       xlab="Number of Observations",
       ylab="Time (seconds)", lwd=2)
  if (withcat) {
    title("Timings: 4 Quantitative, 4 Categorical Variables")
  } else title("Timings: 8 Quantitative Variables")
  lines(ans$N, ans$gpairs, col="red", lwd=2)
  lines(ans$N, ans$ggpairs, col="green", lwd=2)
  legend(100, max(as.matrix(ans[,-1]))*0.98,
         c("pairs()", "gpairs()", "ggpairs()"),
         col=c(1,2,3), lwd=2)
if (!is.null(pdfname)) dev.off()




