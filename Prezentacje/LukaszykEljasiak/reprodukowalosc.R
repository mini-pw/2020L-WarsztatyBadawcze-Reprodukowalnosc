library(neuralnet)
set.seed(123)



## Przykład 1

nn <- neuralnet(
    case~age+parity+induced+spontaneous,
    data=infert,
    hidden=2,
    err.fct="ce",
    linear.output=FALSE)
print(nn)

## Ich wyniki
# Error 125.21
# Threshold 0.008779
# Steps 5254

## Zreprodukowane (seed 123)
# Error 119.72
# Threshold 0.009744
# Steps 27947

## Przykład 2

out <- cbind(nn$covariate,
             nn$net.result[[1]])
dimnames(out) <- list(NULL,
                      c("age","parity","induced",
                        "spontaneous","nn-output"))
head(out)



## Różnice w nn-output
## Lecz czym jest tak naprawdę nn-output? 



nn.bp <- neuralnet(
    case~age+parity+induced+spontaneous,
    data=infert, hidden=2, err.fct="ce",
    linear.output=FALSE,
    algorithm="backprop",
    learningrate=0.01)
nn.bp$result.matrix

## Ich wyniki
# Error 158.08
# Threshold 0.008087
# Steps 4

## Zreprodukowane (seed 123)
# Error 119.72
# Threshold 0.009744
# Steps 27947

#install.packages('nnet')
library(nnet)

nn.nnet <- nnet(
    case~age+parity+induced+spontaneous,
    data=infert, size=2, entropy=T,
    abstol=0.01)


## Przykład 4

head(nn$generalized.weights[[1]])


## Jak widać występują różnice, czasami znaczne

## Visualizing the results
 plot(nn)

 
 ## Przykład 5
 
 new.output <- compute(nn,
                       covariate=matrix(c(22,1,0,0,
                                          22,1,1,0,
                                          22,1,0,1,
                                          22,1,1,1),
                                        byrow=TRUE, ncol=4))
 new.output$net.result

 # Znów wyniki różnią się od podanych w artykule 
 
 ## Przykłąd 6
 
 par(mfrow=c(2,2))
 
 gwplot(nn,selected.covariate="age",min=-2.5, max=5)
 gwplot(nn,selected.covariate="parity",min=-2.5, max=5)
 gwplot(nn,selected.covariate="induced",min=-2.5, max=5)
 gwplot(nn,selected.covariate="spontaneous",min=-2.5, max=5)
 
 
 
 # Przykład 7
 
 ci <- confidence.interval(nn.new, alpha=0.05)
 ci$lower.ci
 
 ## Tylko nie ma czegoś takiego jak nn.new...
 
 # ok spróbujmy zrobić sami według instrukcji
 
 nn.new <- neuralnet(
     case~parity+induced+spontaneous,
     data=infert, hidden=2, err.fct="ce",
     linear.output=FALSE,
     algorithm="backprop",
     learningrate=0.01)
 
 ci <- confidence.interval(nn.new, alpha=0.05)
 ci$lower.ci
 
 ## nie działa z jakiegoś powodu?
 
 
 
 
 
 
 ### Wizualizacja 
 
 
 # Przykład 1
 nn <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
 print(nn)
 plot(nn)
 
 
 # Przykład 2
 nn <- neuralnet(Species ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
 print(nn)
 plot(nn)
 
 #Przykład 3
 softplus <- function(x) log(1 + exp(x))
 nn <- neuralnet((Species == "setosa") ~ Petal.Length + Petal.Width, iris, 
                 linear.output = FALSE, hidden = c(3,2, 2), act.fct = softplus)
 print(nn)
 plot(nn)
 
 
 
 
 
 