################################################################################
### 4. Application to real data
################################################################################

library("TP.idm")
data("colonTP", package = "TP.idm")
colonTP[1:6, 1:5]

nm01 <- TPidm(colonTP, s = 0, t = 365)
nm01

plot(nm01)

nm01$all.probs[seq(1, 194, length.out = 5), 1, ]
nm01$all.probs[nm01$times == 122, 1:4, ]

nm02 <- TPidm(colonTP, s = 0, t = 730)
nm12 <- TPidm(colonTP, s = 365, t = 730)
nm02
nm12

plot(nm12, chosen.tr = c("1 3", "2 3"))

nm0t_rx <- TPidm(colonTP, s = 0, cov = "rx")
nm0t_rx

plot(nm0t_rx, chosen.tr = c("1 1"), col = 1:3)
legend(0, 0.2, legend = c("Obs", "Lev", "Lev+5FU"), lty = 1, col = 1:3)

plot(nm0t_rx, chosen.tr = c("1 3"), col = 1:3)
legend(0, 1, legend = c("Obs", "Lev", "Lev+5FU"), lty = 1, col = 1:3)

test.nm(colonTP[colonTP$rx == "Obs", ], s = 365)

library("survival")

colonTP$entrytime <- colonTP$time1
coxph(Surv(time1, Stime, event) ~ entrytime,
      data = colonTP[colonTP$time1 < colonTP$Stime & colonTP$rx == "Obs", ])

plot(TPidm(colonTP[colonTP$rx == "Obs", ], s = 365), chosen.tr = c("2 2"))
aj1t.Obs <- TPidm(colonTP[colonTP$rx=="Obs", ], s = 365, method = "AJ")
lines(aj1t.Obs$times, aj1t.Obs$all.probs[, 1, 4], type = "s", col = 2)

################################################################################
### 5. Discussion
################################################################################
### Compare computation times, TPidm() vs. etmprep() + etm().
### - colonTP data
### - continuous data
### - grouped or rounded data
################################################################################

## Allowed transitions
tra <- matrix(FALSE, 3, 3)
tra[1, 2:3] <- TRUE
tra[2, 3] <- TRUE

## Event times
timeG <- c(NA, "time1", "Stime")

## Event indicators
statusG <- c(NA, "intermediate_event", "event")

## Naming the states
state.namesG <- c("state0", "state1", "EXITUS")

AddIntermediateEvent <- function(data) {
    intermediate_event <- data$event1
    intermediate_event[data$event1 == 1 & data$event == 1 & data$time1 == data$Stime] <- 1
    intermediate_event[data$event1 == 0 | data$time1 == data$Stime] <- 0
    data$intermediate_event <- intermediate_event
    data
}

################################################################################
## colonTP data
################################################################################
data("colonTP", package = "TP.idm")
data <- colonTP

library("TP.idm")
print(system.time(TPidm(data, s = 365, method = "AJ", CI = TRUE)))

data <- AddIntermediateEvent(data)
keepG <- c("event", "Stime", "rx")

library("etm")
print(system.time({
    dG <- etmprep(timeG, statusG, data = data, tra, state.namesG, start = NULL, id = NULL, cens.name = "cens", keepG)
    etm(dG, state.namesG, tra, cens.name = "cens", s = 365, t = "last", covariance = TRUE)
}))

################################################################################
## continuous data
################################################################################
for (n in c(929, 465, 233)) {
    cat("n = ", n, "\n")
    set.seed(31032017)
    T12 <- rexp(n, rate = 1)
    T13 <- rexp(n, rate = 1)
    time1.0 <- pmin(T12, T13)
    T23 <- rexp(n, rate = 2)
    Stime.0 <- time1.0 + T23 * as.numeric(T12 <= T13)
    C <- runif(n, 0, 2)
    event1 <- as.numeric(time1.0 <= C)
    time1 <- pmin(time1.0, C)
    event <- as.numeric(Stime.0 <= C)
    Stime <- pmin(Stime.0, C)

    data <- data.frame(time1, event1, Stime, event)

    library("TP.idm")
    print(system.time(TPidm(data, s = quantile(time1, probs = .15),
                            method = "AJ", CI = TRUE)))

    data <- AddIntermediateEvent(data)
    keepG <- c("event", "Stime")
    
    library("etm")
    print(system.time({
        dG <- etmprep(timeG, statusG, data = data, tra, state.namesG,
                      start = NULL, id = NULL, cens.name = "cens", keepG)
        etm(dG, state.namesG, tra, cens.name = "cens",
            s = quantile(time1, probs = .15), t = "last", covariance = TRUE)
    }))
}

################################################################################
## grouped or rounded data
################################################################################
for (n in c(929, 465, 233)) {
    cat("n = ", n, "\n")
    set.seed(31032017)
    T12 <- round(rexp(n, rate = 1), 2)
    T13 <- round(rexp(n, rate = 1), 2)
    time1.0 <- pmin(T12, T13)
    T23 <- round(rexp(n, rate = 2), 2)

    ## To ensure entry time < exit time (so the warning of etm package
    ## is avoided).
    v <- min(time1.0[time1.0 > 0])
    u <- min(T23[T23 > 0])
    x <- min(u, v)
    time1.0 <- time1.0 + x/2
    T23 <- T23 + x/2

    Stime.0 <- time1.0 + T23 * as.numeric(T12 <= T13)

    C <- round(runif(n, 0, 2), 2)
    C <- C + x/2

    event1 <- as.numeric(time1.0 <= C)
    time1 <- pmin(time1.0, C)

    event <- as.numeric(Stime.0 <= C)
    Stime <- pmin(Stime.0, C)

    data <- as.data.frame(cbind(time1, event1, Stime, event))

    library("TP.idm")
    print(system.time(TPidm(data, s = quantile(time1, probs = .15),
                            method = "AJ", CI = TRUE)))

    data <- AddIntermediateEvent(data)
    keepG <- c("event", "Stime")

    library("etm")
    print(system.time({
        dG <- etmprep(timeG, statusG, data = data, tra, state.namesG,
                      start = NULL, id = NULL, cens.name = "cens", keepG)
        etm(dG, state.namesG, tra, cens.name = "cens",
            s = quantile(time1, probs = .15), t = "last", covariance = TRUE)
    }))
}
