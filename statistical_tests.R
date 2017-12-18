##### STATISTICAL TESTS

source("classifications.R");

# fit two classification models
dt <- rpart(readmitted ~ ., learn);
pred.dt <- predict(dt, test, type = "class");
ca_dt <- CA(true_class, pred.dt);

nb <- CoreModel(readmitted ~ ., learn, model = "bayes");
pred.nb <- predict(nb, test, type="class");
ca_nb <- CA(true_class, pred.nb);

# Which model is better?
# Tests for statistical significance are used to estimate the probability that a performance difference between models occurred only by chance

# McNemar's test is used to compare binary outcome between correlated groups
# (a test example is either correctly or incorrectly classified)

# form a matrix using the prediction vectors as columns
matPredictions <- data.frame(pred.dt, pred.nb);
binPredictions <- prepare.binary.treatments(matPredictions, true_class);

# McNemar's test
mcnemar <- compare.mcnemar(binPredictions);

# there is also a built-in implementation of McNemar's test
mcnemar_built <- mcnemar.test(binPredictions[, 1], binPredictions[, 2]);


# the calculated p-value represents probability of concluding (incorrectly) that there is a difference
# in samples when no true difference exists.

# The p value is only a measure of how compatible the sample is with the
# null hypothesis.

# The p-value is large, hence we can not reject the null hypothesis that
# there is no difference between the performance of the two learning algorithms



# What if we want to compare multiple models (three or more)?
rf <- randomForest(readmitted ~ ., learn);
pred.rf <- predict(rf, test, type = "class");
ca_rf <- CA(true_class, pred.rf);

svm <- ksvm(readmitted ~ ., learn, kernel="rbfdot");
pred.svm <- predict(svm, test, type="response");
ca_svm <- CA(true_class, pred.svm);

matPredictions <- data.frame(pred.dt, pred.nb, pred.rf, pred.svm);

binPredictions <- prepare.binary.treatments(matPredictions, true_class);

# when comparing multiple classifiers, we first have to test the null hypothesis that those classifiers perform equally
# Cochran's Q test is used for binary outcomes (each test example is either correctly or incorrectly classified)
cochranQ <- compare.cochranQ(binPredictions);

# According to the calculated p-value which is less than 0.05 (the selected significance level) we reject the null hypothesis
# and proceed with post-hoc pairwise comparisons using McNemear's test
mcnemar <- compare.mcnemar(binPredictions);




# A comparison between two models using K-fold cross-validation
n <- nrow(learn);
k <- 20;
bucket.id <- rep(1:k, length.out=n);
s <- sample(1:n, n, FALSE);
bucket.id <- bucket.id[s];

cv.dt <- vector();
cv.nb <- vector();
for (i in 1:k)
{	
  sel <- bucket.id == i
  
  model.rpart <- rpart(readmitted ~ ., learn[!sel,])
  predicted <- predict(model.rpart, learn[sel,], type= "class")
  cv.dt[i] <- CA(learn[sel,]$readmitted, predicted)
  
  model.nb <- naiveBayes(readmitted ~ ., learn[!sel,])
  predicted <- predict(model.nb, learn[sel,], type= "class")
  cv.nb[i] <- CA(learn[sel,]$readmitted, predicted)
};


# Before carrying out a t-test we should check whether the two samples are roughly normally distributed
#
# We can use the Shapiro-Wilks test to check data normality.
# The p-value represents the chance that the sample comes from a normal distribution. The lower this value, the smaller the chance.
shapiro <- shapiro.test(cv.dt - cv.nb);

paired_t_test <- t.test(cv.nb, cv.dt, paired = TRUE, alternative = "two.sided");
corrected_t_test <- corrected.t.test(cv.dt, cv.nb);

