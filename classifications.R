source("helpers.R");
source("preprocessing.R");

diabetes <- preprocess();

### Majority classifier
majority.class <- names(which.max(table(diabetes$readmitted)));
default.accuracy <- sum(diabetes$readmitted == majority.class) / length(diabetes$readmitted);

# install.packages("pROC");
# install.packages(c("pROC", "ipred", "prodlim", "CORElearn", "e1071", "randomForest", "kernlab", "nnet"));

library(pROC);
library(rpart);
library(CORElearn);
library(e1071);
library(ipred);
library(randomForest);
library(kernlab);
library(nnet);


learn <- diabetes[1:8000, ];
test <- diabetes[8001:10000, ];
true_class <- test$readmitted;

diabetes_sample <- diabetes[sample(1:nrow(diabetes), 1000, replace=FALSE), ];

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
# # cm.dt <- CoreModel(readmitted ~ . - number_inpatient, data = learn, model="tree");   ### Zakaj je to enako ???
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
# 
# 
# ##### ROC curve
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
# err <- errorest(readmitted ~ ., data=learn, model = naiveBayes, predict = mypredict.generic);
# CA <- 1 - err$error;


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




##### NEURAL NETWORKS
# # the algorithm is more robust when normed data is used
# norm.data <- scale.data(rbind(learn, test));
# norm.learn <- norm.data[1:nrow(learn), ];
# norm.test <- norm.data[-(1:nrow(learn)), ];
# norm.diabetes_sample = scale.data(diabetes_sample);

# nn <- nnet(readmitted ~ ., data = norm.learn, size = 5, decay = 0.0001, maxit = 10000);
# predicted <- predict(nn, norm.test, type = "class");
# ca <- CA(true_class, predicted);
# 
# # in the case of a binary classification task the method returns probabilities just for one class so we have to reconstruct the complete matrix on our own
# pm <- predict(nn, norm.test, type = "raw");
# predMat <- cbind(1-pm, pm);
# bs <- brier.score(obsMat, predMat);

# err <- errorest(readmitted ~ num_medications + number_inpatient + number_outpatient, data = norm.diabetes_sample, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000);
# CA <- 1 - err$error;












