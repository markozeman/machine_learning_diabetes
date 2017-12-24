source("helpers.R");
source("preprocessing.R");

diabetes <- preprocess();

### Majority classifier
majority.class <- names(which.max(table(diabetes$readmitted)));
default.accuracy <- sum(diabetes$readmitted == majority.class) / length(diabetes$readmitted);

# install.packages("pROC");
# install.packages(c("pROC", "adabag", ipred", "prodlim", "CORElearn", "e1071", "randomForest", "kernlab", "nnet"));

library(pROC);
library(rpart);
library(CORElearn);
library(e1071);
library(adabag);
library(ipred);
library(randomForest);
library(kernlab);
library(nnet);


learn <- diabetes[1:16000, ];
test <- diabetes[16001:20000, ];
true_class <- test$readmitted;

diabetes_sample <- diabetes[sample(1:nrow(diabetes), 5000, replace=FALSE), ];

obsMat <- model.matrix(~readmitted-1, test);


##### DECISION TREE - rpart

# #dt <- rpart(readmitted ~ ., data = learn, method="class", control=rpart.control(minsplit=2, minbucket=1, cp=0.001));
# dt <- rpart(readmitted ~ ., data = learn, method="class");
# 
# ######### Zakaj ne naredi drevesa, ampak samo koren ???
# 
# predicted <- predict(dt, test, type = "class");
# 
# confusion_matrix <- table(observed, predicted);
# 
# ca <- CA(observed, predicted);



##### DECISION TREE - CoreModel
# cm.dt <- CoreModel(readmitted ~ ., data = learn, model="tree");
# # plot(cm.dt, learn);
# 
# predicted <- predict(cm.dt, test, type="class");
# 
# confusion_matrix <- table(true_class, predicted);
# 
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# err <- errorest(readmitted ~ ., data = diabetes, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree");
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "DT.txt", sep = "\n");


##### ROC curve
# predicted_prob <- predict(cm.dt, test, type = "prob");
# 
# rocobj <- roc(true_class, predicted_prob[, "YES"]);
# # plot(rocobj);
# 
# ### make specificity and sensitivity more equal
# cutoffs <- rocobj$thresholds;
# tp = rocobj$sensitivities;
# fp = 1 - rocobj$specificities;
# 
# dist <- (1-tp)^2 + fp^2;
# best.cutoff <- cutoffs[which.min(dist)];
# 
# predicted.label <- factor(ifelse(predicted_prob[,"YES"] >= best.cutoff, "YES", "NO"));
# 
# confusion_matrix_2 <- table(true_class, predicted.label);
# ca_2 <- CA(true_class, predicted.label);
# sens_2 <- Sensitivity (true_class, predicted.label, "YES");
# spec_2 <- Specificity (true_class, predicted.label, "YES");




##### NAIVE BAYES
### No.1 
# nb <- naiveBayes(readmitted ~ ., data = learn);
# predicted <- predict(nb, test, type="class");
# 
# confusion_matrix <- table(true_class, predicted);
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# predMat <- predict(nb, test, type = "raw");
# bs <- brier.score(obsMat, predMat);
# 
# err <- errorest(readmitted ~ ., data = diabetes, model = naiveBayes, predict = mypredict.generic);
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "NB.txt", sep = "\n");


### No.2
# cm.nb <- CoreModel(readmitted ~ ., data = learn, model="bayes");
# predicted <- predict(cm.nb, test, type="class");
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# predMat <- predict(cm.nb, test, type = "probability");
# bs <- brier.score(obsMat, predMat);
# 
# err <- errorest(readmitted ~ ., data = diabetes, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="bayes");
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "NB_core.txt", sep = "\n");



##### KNN
# knn <- CoreModel(readmitted ~ ., data = learn, model="knn", kInNN = 5);
# predicted <- predict(knn, test, type="class");
# 
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# predMat <- predict(knn, test, type = "probability");
# bs <- brier.score(obsMat, predMat);
# 
# err <- errorest(readmitted ~ ., data = diabetes_sample, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn");
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "kNN.txt", sep = "\n");

### POŽENI IN SI SHRANI !!!



##### RANDOM FOREST
### No.1
# rf <- randomForest(readmitted ~ ., data = learn);
# predicted <- predict(rf, test, type="class");
# 
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# predMat <- predict(rf, test, type = "prob");
# bs <- brier.score(obsMat, predMat);
#
# err <- errorest(readmitted ~ ., data = diabetes_sample, model = randomForest, predict = mypredict.generic);
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "RF_20000_samples.txt", sep = "\n");

### POŽENI IN SI SHRANI !!!


### No.2
# cm.rf <- CoreModel(readmitted ~ ., data = learn, model="rf");
# predicted <- predict(cm.rf, test, type="class");
# 
# ca <- CA(true_class, predicted);
# sens <- Sensitivity (true_class, predicted, "YES");
# spec <- Specificity (true_class, predicted, "YES");
# 
# predMat <- predict(cm.rf, test, type = "probability");
# bs <- brier.score(obsMat, predMat);
# 
# err <- errorest(readmitted ~ ., data = diabetes_sample, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="rf");
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "RF_core_20000_samples.txt", sep = "\n");

### POŽENI IN SI SHRANI !!!




##### SVM
### No.1
# sm <- svm(readmitted ~ ., data = learn);
# predicted <- predict(sm, test, type="class");
# ca <- CA(true_class, predicted);
# 
# sm <- svm(readmitted ~ ., learn, probability = T);
# pred <- predict(sm, test, probability = T);
# predMat <- attr(pred, "probabilities");
# # in this particular case, the columns of predMat are in reverse order, so we need to invert them
# bs <- brier.score(obsMat, predMat[, c(2,1)]);
# 
# err <- errorest(readmitted ~ ., data = diabetes_sample, model = svm, predict = mypredict.generic);
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "SVM_20000_samples.txt", sep = "\n");


### No.2
# model.svm <- ksvm(readmitted ~ ., data = learn, kernel = "rbfdot");
# predicted <- predict(model.svm, test, type = "response");
# ca <- CA(true_class, predicted);
# 
# model.svm <- ksvm(readmitted ~ ., data = learn, kernel = "rbfdot", prob.model = T);
# predMat <- predict(model.svm, test, type = "prob");
# bs <- brier.score(obsMat, predMat);
#
# err <- errorest(readmitted ~ ., data = diabetes_sample, model = ksvm, predict = mypredict.ksvm);
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "kSVM_20000_samples.txt", sep = "\n");



##### NEURAL NETWORKS
# # the algorithm is more robust when normed data is used
# norm.data <- scale.data(rbind(learn, test));
# norm.learn <- norm.data[1:nrow(learn), ];
# norm.test <- norm.data[-(1:nrow(learn)), ];
# norm.diabetes <- scale.data(diabetes);
# norm.diabetes_sample = scale.data(diabetes_sample);

# nn <- nnet(readmitted ~ ., data = norm.learn, size = 5, decay = 0.0001, maxit = 10000);
# predicted <- predict(nn, norm.test, type = "class");
# ca <- CA(true_class, predicted);
# 
# # in the case of a binary classification task the method returns probabilities just for one class so we have to reconstruct the complete matrix on our own
# pm <- predict(nn, norm.test, type = "raw");
# predMat <- cbind(1-pm, pm);
# bs <- brier.score(obsMat, predMat);

# err <- errorest(readmitted ~ ., data = norm.diabetes, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000);
# CA <- 1 - err$error;
# write(c("CA: ", CA), file = "NN.txt", sep = "\n");



##### LOGISTIC REGRESSION
### not working!
# log_reg <- glm(readmitted ~ . , family = binomial(link='logit'), data = learn);
# predicted <- predict(log_reg, test, type="response");
# ca <- CA(true_class, predicted);




##### Combining machine learning algorithms
# modelDT <- CoreModel(readmitted ~ ., learn, model="tree");
# modelNB <- CoreModel(readmitted ~ ., learn, model="bayes");
# modelKNN <- CoreModel(readmitted ~ ., learn, model="knn", kInNN = 5);
# 
# predDT <- predict(modelDT, test, type="class");
# caDT <- CA(true_class, predDT);
# 
# predNB <- predict(modelNB, test, type="class");
# caNB <- CA(true_class, predNB);
# 
# predKNN <- predict(modelKNN, test, type="class");
# caKNN <- CA(true_class, predKNN);


### Voting

# # combine predictions into a data frame
# pred <- data.frame(predDT, predNB, predKNN);
# 
# predicted <- voting(pred);
# ca_voting <- CA(true_class, predicted);



### Weighted voting
# predDT.prob <- predict(modelDT, test, type="probability");
# predNB.prob <- predict(modelNB, test, type="probability");
# predKNN.prob <- predict(modelKNN, test, type="probability");
# 
# # combine predictions into a data frame
# pred.prob <- caDT * predDT.prob + caNB * predNB.prob + caKNN * predKNN.prob;
# 
# # pick the class with the highest score
# highest <- apply(pred.prob, 1, which.max);
# classes <- levels(learn$readmitted);
# predicted <- classes[highest];
# 
# ca_weighted_voting <- CA(true_class, predicted);



### Stacking

# # divide the learning set into two sets
# sel <- sample(1:nrow(learn), size=1000, replace=F);
# base.train <- learn[-sel, ];
# base.valid <- learn[sel, ];
# 
# # get predictions from the base models
# predM1 <- predict(modelDT, base.valid, type="class");
# predM2 <- predict(modelNB, base.valid, type="class");
# predM3 <- predict(modelKNN, base.valid, type="class");
# 
# # combine predictions into a data frame
# combiner.train <- data.frame(M1=predM1, M2=predM2, M3=predM3, readmitted=base.valid$readmitted);
# 
# # train a combiner model
# combiner.M <- multinom(readmitted ~ ., combiner.train, maxit=1000);
# 
# 
# ## testing the stacked model
# 
# # get predictions from the base models
# test.M1 <- predict(modelDT, test, type="class");
# test.M2 <- predict(modelNB, test, type="class");
# test.M3 <- predict(modelKNN, test, type="class");
# 
# # combine predictions into a data frame
# combiner.test <- data.frame(M1=test.M1, M2=test.M2, M3=test.M3);
# 
# # get the final predictions from the combiner model
# predicted <- predict(combiner.M, combiner.test, type="class");
# 
# ca_stacking <- CA(true_class, predicted);




### Bagging

# n <- nrow(learn);
# m <- 15;
# 
# models <- list();
# for (i in 1:m)
# {
#   sel <- sample(1:n, n, T);
#   train <- learn[sel, ];
#   models[[i]] <- CoreModel(readmitted ~ ., train, model="tree", minNodeWeightTree=2);
# }
# 
# tmp <- NULL;
# for (i in 1:m)
#   tmp <- cbind(tmp, as.character(predict(models[[i]], test, type="class")));
# 
# highest <- apply(tmp, 1, function(x){which.max(table(factor(x, levels=classes)))});
# classes <- levels(learn$readmitted);
# predicted <- classes[highest];
# ca_our_bagging <- CA(true_class, predicted);
# 
# 
# # bagging is implemented in the package "ipred"
# bag <- bagging(readmitted ~ ., learn, nbagg=15);
# bag.pred <- predict(bag, test, type="class");
# ca_bagging <- CA(true_class, bag.pred);



### Boosting

# bm <- boosting(readmitted ~ ., learn);
# predictions <- predict(bm, test);
# 
# predicted <- predictions$class;
# ca_boosting <- CA(true_class, predicted);



