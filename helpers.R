# Classification accuracy
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}


# The Brier score
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}


# The sensitivity of a model
Sensitivity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  t[pos.class, pos.class] / sum(t[pos.class,])
}


# The specificity of a model
Specificity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  # identify the negative class name
  neg.class <- which(row.names(t) != pos.class)
  
  t[neg.class, neg.class] / sum(t[neg.class,])
}


mypredict.generic <- function(object, newdata)
{
  predict(object, newdata, type = "class")
}

mymodel.coremodel <- function(formula, data, target.model)
{
  CoreModel(formula, data, model=target.model)
}

mypredict.coremodel <- function(object, newdata) 
{
  pred <- predict(object, newdata)$class; 
  destroyModels(object); 
  pred
}

mypredict.ksvm <- function(object, newdata)
{
  predict(object, newdata, type = "response")
}

mypredict.nnet <- function(object, newdata)
{
  as.factor(predict(object, newdata, type = "class"))
}

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}



### statistical tests
prepare.ranked.treatments <- function(treatments)
{
  # Calculate ranks for each row        
  ranks <- apply(treatments, 1, rank, ties.method = "average")
  
  # transpose the matrix  
  t(ranks)
}

prepare.binary.treatments <- function(predictions, observed)
{
  dim <- ncol(predictions)
  
  for (i in 1:dim)
  {
    correct <- predictions[,i] == observed
    predictions[,i] <- vector()
    predictions[correct,i] = 1
    predictions[!correct,i] = 0
  }
  
  predictions
}

compare.mcnemar <- function(binary.treatments)
{
  dim <- ncol(binary.treatments)		
  
  m <- matrix(nrow = dim, ncol = dim)
  
  for (i in 1:(dim-1))
    for (j in (i+1):dim)
    {
      t <- table(binary.treatments[,i], binary.treatments[,j])
      n01 <- t[1,2]
      n10 <- t[2,1]
      sm <- (abs(n01-n10)-1)^2/(n01+n10)
      
      m[i,j] <- pchisq(sm, df=1, lower.tail = F)
      m[j,i] <- m[i,j]
    }
  m 
}

compare.cochranQ <- function(binary.treatments)
{     
  k <- ncol(binary.treatments)
  b <- nrow(binary.treatments)
  xc <- apply(binary.treatments, 2, sum)
  xr <- apply(binary.treatments, 1, sum)
  N <- sum(binary.treatments)
  
  T <- k*(k-1)*sum((xc-N/k)^2)/sum(xr*(k-xr))
  
  pchisq(T, k-1, lower.tail = F)
}

corrected.t.test <- function(treatmentsA, treatmentsB)
{
  k <- length(treatmentsA)
  
  diff <- treatmentsA - treatmentsB
  m <- mean(diff)
  s <- sd(diff)
  
  t.stat <- m/(s*sqrt(1/k + 1/(k-1)))
  
  pt(t.stat, k-1, lower.tail=F)
}




