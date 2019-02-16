library(dplyr)

data = read.csv("train_50k.csv")

# Since we have time-window variables that count occurance in the last 60 seconds,
# observations at the beginning of the 1st 60 seconds should be excluded
data_filter = data %>%
  filter(click_time == "2017-11-07 01:43:37")
# record prior to 2000 should be excluded


## PCA - data preparation

# remove the dependent and categorical variables
PCA_data = data %>%
  select(-c(record, ip, app, device, os, channel, click_time, is_attributed, hour))

# Devide data into training and testing (record prior to 2000 should be excluded)
train = PCA_data[2001:35000,]
test = PCA_data[35001:50000,]


## PCA

# perform principal components analysis
pr.out = prcomp(train, scale = TRUE)

# calculate proportion of variance explained by each PC
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)

# plot the PVE explained by each component
plot(pve, 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1),
     type = 'b')

# plot the cunulative PVE explained by each component
plot(cumsum(pve), 
xlab = "Principal Component", 
ylab = "Cumulative Proportion of Variance Explained", 
ylim = c(0, 1),
type = 'b')

# create a data frame that contains PCAs
PCA_train = data.frame(data$record[2001:35000], 
                       data$is_attributed[2001:35000], 
                       pr.out$x[,c(1:67)])
colnames(PCA_train)[1:2] = c("record", "is_attributed")

# save a table of PVE and cumulative PVE for future reference
PCA_varaiance = cbind.data.frame(pve, cumsum(pve))
colnames(PCA_varaiance) = c("% of variance", "cumulative %")
rownames(PCA_varaiance) = colnames(PCA_train[,3:69])
write.csv(PCA_varaiance, "PCA_varaiance.csv")


# transform test data set into PCA
PCA_test = data.frame(predict(pr.out, newdata = test))

PCA_test = data.frame(data$record[35001:50000],
                      data$is_attributed[35001:50000],
                      PCA_test[,c(1:67)])
colnames(PCA_test)[1:2] = c("record", "is_attributed")


## LDA - PCA

# use LOOCV to tune # of PCs to put into model (highest AUC)
library (MASS)
library(ROCR)

set.seed (1)
cv.auc = rep (0, 67)
for (i in 1:67) {
  # run a LDA
  lda.fit = lda(is_attributed~. ,data = PCA_train[,2:(2+i)], CV = TRUE)
  
  # calculate AUC of the result
  pred = prediction(lda.fit$posterior[,2], PCA_train$is_attributed) 
  perf = performance(pred, measure = "auc")
  cv.auc[i] = perf@y.values[[1]]
  }
cv.auc

which.max(cv.auc) # 10 is the max, use 10 PCs to put into the model
cv.auc[10] # AUC = 0.9378823

# make prediction on test data
lda.fit = lda(is_attributed~. ,data = PCA_train[,2:(2+10)])
lda.pred = predict(lda.fit, PCA_test)

# plot ROC of the model
pred = prediction(lda.pred$posterior[,2], PCA_test$is_attributed) 
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc,colorize=TRUE)

# calculate AUC of the model
perf_auc = performance(pred, measure = "auc")
perf_auc@y.values[[1]] # 0.9745857

# Confusion matrix
lda.class = lda.pred$class
table(PCA_test$is_attributed, lda.class)

# Accuracy
mean(lda.class == PCA_test$is_attributed) # 0.9942667


## QDA - PCA

# use LOOCV to tune # of PCs to put into model (highest AUC)

set.seed (1)
cv.auc = rep (0, 67)
for (i in 1:67) {
  # run a QDA
  qda.fit = qda(is_attributed~. ,data = PCA_train[,2:(2+i)], CV = TRUE)
  
  # calculate AUC of the result
  pred = prediction(qda.fit$posterior[,2], PCA_train$is_attributed) 
  perf = performance(pred, measure = "auc")
  cv.auc[i] = perf@y.values[[1]]
}
cv.auc

which.max(cv.auc) # 26 is the max, use 26 PCs to put into the model
cv.auc[26] # AUC = 0.9512229

# make prediction on test data
qda.fit = qda(is_attributed~. ,data = PCA_train[,2:(2+26)])
qda.pred = predict(qda.fit, PCA_test)

# plot ROC of the model
pred = prediction(qda.pred$posterior[,2], PCA_test$is_attributed) 
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc,colorize=TRUE)

# calculate AUC of the model
perf_auc = performance(pred, measure = "auc")
perf_auc@y.values[[1]] # 0.9513211

# Confusion matrix
qda.class = qda.pred$class
table(PCA_test$is_attributed, qda.class)

# Accuracy
mean(qda.class == PCA_test$is_attributed) # 0.9874667


## KS
ks = data.frame(t(read.csv("ks_variables.csv"))[-1,])
colnames(ks) = read.csv("ks_variables.csv")[,1]

# remove the dependent and categorical variables
ks = ks %>%
  select(-c(ip, app, device, os, channel, click_time, is_attributed, hour))

# reorder the train and test data set to be consistent with the order of ks score
ks_train = train[,colnames(ks)]
ks_test = test[,colnames(ks)]

ks_train = data.frame(data$record[2001:35000], 
                      data$is_attributed[2001:35000], 
                      ks_train)
colnames(ks_train)[1:2] = c("record", "is_attributed")

ks_test = data.frame(data$record[35001:50000],
                     data$is_attributed[35001:50000],
                     ks_test)
colnames(ks_test)[1:2] = c("record", "is_attributed")


## LDA - KS
# use LOOCV to tune # of variables to put into model (highest AUC)
library (MASS)
library(ROCR)
set.seed (1)
cv.auc = rep (0, 67)
for (i in 1:67) {
  # run a LDA
  lda.fit = lda(is_attributed~. ,data = ks_train[,2:(2+i)], CV = TRUE)
  
  # calculate AUC of the result
  pred = prediction(lda.fit$posterior[,2], ks_train$is_attributed) 
  perf = performance(pred, measure = "auc")
  cv.auc[i] = perf@y.values[[1]]
}
cv.auc

which.max(cv.auc) # 19 is the max, use 19 PCs to put into the model
cv.auc[19] # AUC = 0.9292373

# make prediction on test data
lda.fit = lda(is_attributed~. ,data = ks_train[,2:(2+19)])
lda.pred = predict(lda.fit, ks_test)

# plot ROC of the model
pred = prediction(lda.pred$posterior[,2], ks_test$is_attributed) 
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc,colorize=TRUE)

# calculate AUC of the model
perf_auc = performance(pred, measure = "auc")
perf_auc@y.values[[1]] #0.9777049

# Confusion matrix
lda.class = lda.pred$class
table(ks_test$is_attributed, lda.class)

# Accuracy
mean(lda.class == ks_test$is_attributed) # 0.9872


## QDA - KS
# use LOOCV to tune # of variables to put into model (highest AUC)
set.seed (1)
cv.auc = rep (0, 67)
for (i in 1:67) {
  # run a QDA
  qda.fit = qda(is_attributed~. ,data = ks_train[,2:(2+i)], CV = TRUE)
  
  # calculate AUC of the result
  pred = prediction(qda.fit$posterior[,2], ks_train$is_attributed) 
  perf = performance(pred, measure = "auc")
  cv.auc[i] = perf@y.values[[1]]
}
cv.auc

which.max(cv.auc) # 40 is the max, use 40 PCs to put into the model
cv.auc[40] # 0.949446

# make prediction on test data
qda.fit = qda(is_attributed~. ,data = ks_train[,2:(2+40)])
qda.pred = predict(qda.fit, ks_test)

# plot ROC of the model
pred = prediction(qda.pred$posterior[,2], ks_test$is_attributed) 
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc,colorize=TRUE)

# calculate AUC of the model
perf_auc = performance(pred, measure = "auc")
perf_auc@y.values[[1]] # 0.9512315

# Confusion matrix
qda.class = qda.pred$class
table(ks_test$is_attributed, qda.class)

# Accuracy
mean(qda.class == ks_test$is_attributed) # 0.9786
