# Robustness of fixed point algorithms
## Functions with restricted input spaces
library(FixedPoint)
SimpleVectorFunction = function(x){c(0.5*sqrt(x[1] + x[2]), abs(1.5*x[1] + 0.5*x[2]))}
FPSolution = FixedPoint(Function = SimpleVectorFunction, Inputs = c(0.3,900),
                        Method = "Anderson")


FPSolution = FixedPoint(Function = SimpleVectorFunction, Inputs = FPSolution$Inputs, 
                        Outputs = FPSolution$Outputs, Method = "Simple", MaxIter = 5)
# Now we switch to the Anderson Method again. No error results because we are
# close to fixed point.
FPSolution = FixedPoint(Function = SimpleVectorFunction, Inputs = FPSolution$Inputs,
                        Outputs = FPSolution$Outputs, Method = "Anderson")

#  Applications of fixed point acceleration with the FixedPoint package
## Simple examples with analytical functions
library(FixedPoint)
SequenceFunction = function(tn){0.5*(tn + 100/tn)}
FP = FixedPoint(Function = SequenceFunction, Inputs = 6, Method = "SEA")

Vec_Function = function(x){c(0.5*sqrt(abs(x[1] + x[2])), 1.5*x[1] + 0.5*x[2])}
FP_Simple   = FixedPoint(Function = Vec_Function, Inputs = c(0.3,900),
                         Method = "Simple")
FP_Anderson = FixedPoint(Function = Vec_Function, Inputs = c(0.3,900),
                         Method = "Anderson")


## Gas diffusion
phi = 10
Numbering = matrix(seq(1,phi^2,1), phi)           # Numbering scheme for squares

NeighbourSquares = function(n,phi){
  SurroundingIndexes = c(n)
  if (n %% phi != 1){SurroundingIndexes = c(SurroundingIndexes, n-1)}    # above
  if (n %% phi != 0){SurroundingIndexes = c(SurroundingIndexes, n+1)}    # below
  if (n > phi){SurroundingIndexes = c(SurroundingIndexes, n-phi)}        # right
  if (n <= phi^2-phi){SurroundingIndexes = c(SurroundingIndexes, n+phi)} # left
  return(SurroundingIndexes)
}

TwoDimensionalDiffusionIteration = function(x, phi){
  xnew = x
  for (i in 1:(phi^2)){
    Subset = NeighbourSquares(i, phi)
    xnew[i] = mean(x[Subset])
  }
  xnew[1] = 0
  xnew[phi^2] = 1
  return(xnew)
}

FP = FixedPoint(Function = function(x) TwoDimensionalDiffusionIteration(x,phi),
                Inputs =  c(rep(0,50), rep(1,50)), Method = "RRE")

## Finding equilibrium prices in a pure exchange economy
# Generating data
set.seed(3112)
N = 8
G = 10
Endowments = matrix(rlnorm(N*G), nrow = G)
Gamma      = matrix(runif(N*G), nrow = G)
# Every column here represents a household and every row is a good. 
# So Endowments[1,2] is the second household's endowment of good 1.

# We now start solving for equilibrium prices:
TotalEndowmentsPerGood  = apply(Endowments, 1, sum)
TotalGammasPerHousehold = apply(Gamma, 2, sum)
LambdasGivenPriceVector = function(Price){
  ValueOfEndowmentsPerHousehold = Price * Endowments
  TotalValueOfEndowmentsPerHousehold = apply(ValueOfEndowmentsPerHousehold, 2, sum)
  return(TotalGammasPerHousehold /TotalValueOfEndowmentsPerHousehold)
}

IterateOnce = function(Price){
  Lambdas = LambdasGivenPriceVector(Price)                      # eqn 16
  GammaOverLambdas = t(apply(Gamma, 1, function(x) x / Lambdas))
  SumGammaOverLambdas = apply(GammaOverLambdas,1,sum)
  NewPrices = SumGammaOverLambdas/ TotalEndowmentsPerGood       # eqn 18
  NewPrices = NewPrices/NewPrices[1]        # normalising with numeraire
  return(NewPrices)
}

InitialGuess = rep(1,10)
FP = FixedPoint(Function = IterateOnce, Inputs =  InitialGuess, Method = "VEA")

## The training of a perceptron classifier
# Generating linearly separable data
set.seed(10)
data = data.frame(x1 = rnorm(100,4,2), x2 = rnorm(100,8,2), y = -1)
data = rbind(data,data.frame(x1 = rnorm(100,-4,2), x2 = rnorm(100,12), y = 1))

# Iterating training of Perceptron
IteratePerceptronWeights = function(w, LearningRate = 1){
  intSeq = 1:length(data[,"y"])
  for (i in intSeq){
    target = data[i,c("y")]
    score  =  w[1] + (w[2]*data[i, "x1"]) + (w[3]*data[i, "x2"])
    ypred  = 2*(as.numeric( score > 0 )-0.5)
    update = LearningRate * 0.5*(target-ypred)
    w[1]   = w[1] + update
    w[2]   = w[2] + update*data[i, "x1"]
    w[3]   = w[3] + update*data[i, "x2"]
  }
  return(w)
}

InitialGuess = c(1,1,1)
FP = FixedPoint(Function = IteratePerceptronWeights, Inputs =  InitialGuess,
                Method = "Simple", MaxIter = 1200)

IteratePerceptronWeights = function(w, LearningRate = 1){
  intSeq = 1:length(data[,"y"])
  for (i in intSeq){
    target = data[i,c("y")]
    score  =  w[1] + (w[2]*data[i, "x1"]) + (w[3]*data[i, "x2"])
    ypred  = 2*(as.numeric( score > 0 )-0.5)
    if ((target-ypred) != 0){
      update = LearningRate * -sign(score) * sqrt(abs(score))
      w[1] = w[1] + update
      w[2] = w[2] + update*data[i, "x1"]
      w[3] = w[3] + update*data[i, "x2"]
    }
  }
  return(w)
}
FP = FixedPoint(Function = IteratePerceptronWeights, Inputs =  InitialGuess,
                Method = "MPE")


## Valuation of a perpetual American put option
d = 0.05
sigma = 0.1
alpha = 2
S = 10
chi = 0
p = (exp(d) - exp(-sigma) ) / (exp(sigma) - exp(-sigma))

# Initially we guess that the option value decreases linearly from S 
# (when the spot price is 0) to 0 (when the spot price is \alpha S).
UnderlyingPrices = seq(0,alpha*S, length.out = 100)
OptionPrice = seq(S,chi, length.out = 100)

ValueOfExercise = function(spot){S-spot}
ValueOfHolding = function(spot, EstimatedValueOfOption){
  if (spot > alpha*S-1e-10){return(chi)}
  IncreasePrice = exp(sigma)*spot
  DecreasePrice = exp(-sigma)*spot
  return((p*EstimatedValueOfOption(IncreasePrice) +
            (1-p)*EstimatedValueOfOption(DecreasePrice)))
}
ValueOfOption = function(spot, EstimatedValueOfOption){
  Holding = ValueOfHolding(spot, EstimatedValueOfOption)*exp(-d)
  Exercise = ValueOfExercise(spot)
  return(max(Holding, Exercise))
}
IterateOnce = function(OptionPrice){
  EstimatedValueOfOption = approxfun(UnderlyingPrices, OptionPrice, rule = 2)
  for (i in 1:length(OptionPrice)){
    OptionPrice[i] = ValueOfOption(UnderlyingPrices[i], EstimatedValueOfOption)
  }
  return(OptionPrice)
}

library(SQUAREM)
FP = squarem(par=OptionPrice, IterateOnce)

plot(UnderlyingPrices,FP$par, type = "l",
     xlab = "Price of Underlying", ylab = "Price of Option")

## A consumption smoothing problem
library(FixedPoint)
library(schumaker)
library(cubature)
delta = 0.2
beta = 0.95
BudgetStateSpace = c(seq(0,1, 0.015), seq(1.05,3,0.05))
InitialGuess = sqrt(BudgetStateSpace)

ValueGivenShock = function(Budget, epsilon, NextValueFunction){
  optimize(f = function(x) epsilon*(x^delta) + beta*NextValueFunction(Budget - x + 1), 
           lower = 0, upper = Budget, maximum = TRUE)
}

ExpectedUtility = function(Budget, NextValueFunction){
  if (Budget > 0.001){
    adaptIntegrate(f = function(epsilon) ValueGivenShock(Budget,
                                                         epsilon,NextValueFunction)$objective * dlnorm(epsilon),
                   lowerLimit = qlnorm(0.0001), upperLimit = qlnorm(0.9999))$integral
  } else {
    beta*NextValueFunction(1)
  }
}

IterateOnce = function(BudgetValues){
  NextValueFunction = schumaker::Schumaker(BudgetStateSpace, BudgetValues,
                                           Extrapolation = "Linear")$Spline
  for (i in 1:length(BudgetStateSpace)){   # This is often a good loop to parallelise 
    BudgetValues[i] = ExpectedUtility(BudgetStateSpace[i], NextValueFunction)
  }
  return(BudgetValues)
}
FP = FixedPoint(Function = IterateOnce, Inputs = InitialGuess,
                Method = "Anderson")



# For the Speed of convergence comparison please see the test files for the package



# Appendix

library(foreach)
library(doParallel)
cores = 6

TaskAssigner = function(Inputs, Outputs, i, Function){
  library(FixedPoint)
  library(schumaker)
  library(cubature)
  Iterates = dim(Inputs)[2]
  if (i > 1.5) {IterateToDrop = Iterates-i+1} else {IterateToDrop = 0}
  IteratesToUse = (1:Iterates)[ 1:Iterates != IterateToDrop]
  Inputs = matrix(Inputs[,IteratesToUse], ncol = length(IteratesToUse), byrow = FALSE)
  Outputs = matrix(Outputs[,IteratesToUse], ncol = length(IteratesToUse), byrow = FALSE)
  Guess = FixedPointNewInput(Inputs = Inputs, Outputs = Outputs, Method = "Anderson")
  Outputs = matrix(Function(Guess), ncol = 1, byrow = FALSE)
  Inputs = matrix(Guess, ncol = 1, byrow = FALSE)

  return(list(Inputs = Inputs, Outputs = Outputs))
}

CombineLists = function(List1, List2){
  width = dim(List1$Inputs)[2] + dim(List2$Inputs)[2]
  C = list()
  C$Inputs  = matrix(c(List1$Inputs , List2$Inputs ), ncol = width, byrow = FALSE)
  C$Outputs = matrix(c(List1$Outputs, List2$Outputs), ncol = width, byrow = FALSE)
  return(C)
}

# ReSortIterations
# This function takes the previous inputs and outputs from the function and removes
# duplicates and then sorts them into a different order.
ReSortIterations = function(PreviousIterates,
                            ConvergenceMetric = function(Resids){max(abs(Resids))})
{
  # Removing any duplicates
  NotDuplicated   = (!(duplicated.matrix(PreviousIterates$Inputs, MARGIN = 2)))
  PreviousIterates$Inputs  = PreviousIterates$Inputs[,NotDuplicated]
  PreviousIterates$Outputs = PreviousIterates$Outputs[,NotDuplicated]
  # Resorting
  Resid       = PreviousIterates$Outputs - PreviousIterates$Inputs
  Convergence =   ConvergenceVector = sapply(1:(dim(Resid)[2]), function(x)
    ConvergenceMetric(Resid[,x]) )
  Reordering  =   order(Convergence, decreasing = TRUE)
  PreviousIterates$Inputs  = PreviousIterates$Inputs[,Reordering]
  PreviousIterates$Outputs = PreviousIterates$Outputs[,Reordering]
  return(PreviousIterates)
}


ConvergenceMetric = function(Resid){max(abs(Resid))}

# Preparing for clustering and getting a few runs to input to later functions:
PreviousRuns = FixedPoint(Function = IterateOnce, Inputs = InitialGuess,
                          Method = "Anderson", MaxIter = cores)
PreviousRuns$Residuals = PreviousRuns$Outputs - PreviousRuns$Inputs
PreviousRuns$Convergence = apply(PreviousRuns$Residuals , 2, ConvergenceMetric )
ConvergenceVal = min(PreviousRuns$Convergence)

registerDoParallel(cores=cores)

iter = cores
while (iter < 100 & ConvergenceVal > 1e-10){
  NewRuns = foreach(i = 1:cores, .combine=CombineLists) %dopar% {
    TaskAssigner(PreviousRuns$Inputs, PreviousRuns$Outputs, i, IterateOnce)
  }
  # Appending to previous runs
  PreviousRuns$Inputs = matrix(c(PreviousRuns$Inputs , NewRuns$Inputs ),
                               ncol = dim(PreviousRuns$Inputs)[2] + cores, byrow = FALSE)
  PreviousRuns$Outputs = matrix(c(PreviousRuns$Outputs , NewRuns$Outputs ),
                                ncol = dim(PreviousRuns$Outputs)[2] + cores, byrow = FALSE)
  PreviousRuns = ReSortIterations(PreviousRuns)
  PreviousRuns$Residuals = PreviousRuns$Outputs - PreviousRuns$Inputs
  PreviousRuns$Convergence = apply(PreviousRuns$Residuals , 2, ConvergenceMetric )
  # Finding Convergence
  ConvergenceVal = min(PreviousRuns$Convergence)
  iter = iter+cores
}

stopImplicitCluster()

PreviousRuns$FixedPoint = PreviousRuns$Outputs[, dim(PreviousRuns$Outputs)[2]]

