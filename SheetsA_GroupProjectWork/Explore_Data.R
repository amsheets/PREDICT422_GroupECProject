# 1) Select a real-world data set of interest (from national database, from work, from Kaggle, etc.).
# 
# 2) Conduct exploratory data analysis.
# 
# 3) Perform supervised and/or unsupervised learning.
# 
# 4) Give a presentation during the Wednesday sync session and submit the slides in PDF format on Canvas.

setwd("/Users/asheets/Documents/Work_Computer/Grad_School/PREDICT_422/PREDICT422_GroupECProject/SheetsA_GroupProjectWork")
data <- read.csv(file='Speed_Dating_Data.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)

##Look at data
summary(data)

##Are there any complete observations?
table(complete.cases(data))

# 
# vars <- data.frame(varname = colnames(data),pct_miss = rep(0,length(colnames(data))))
# 
# for (i in 1:length(colnames(data))) {
#   na = sum(is.na(data[,c(vars$varname[i])]))
#   total = length(data[,c(vars$varname[i])])
#   vars$pct_miss[i] <- round(na / total,digits = 3)
# }
# 
# use_vars <- vars[which(vars$pct_miss <= 0.20),]

##Only use waves where metrics are on the same scale
data2 <- data[which(data$wave %in% c(1:5,10:21)),]

##Try subset variables due to missing data
data2 <- data2[,c(3,7:8,10,13:17,34,40:42,46:48,51:68,70:75,82:92)]
data2 <- data2[complete.cases(data2),]

plots <- vector("list", 51)
names <- colnames(data)

#Visualize the variables
plot_vars = function (data2, column)
  ggplot(data = data, aes_string(x = column)) +
  geom_histogram(color =I('black'),fill = I('#099009'))+
  xlab(column)

plots <- lapply(colnames(data2)[1:51], plot_vars, data = data2)

n <- length(plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plots))

##Look into which variables are numeric and which are character
nums <- data.frame(sapply(data2, is.numeric))
char <- data.frame(sapply(data2, is.character))

nums$variable <- rownames(nums)
char$variable <- rownames(char)

nums <- rownames(nums[which(nums$sapply.data2..is.numeric. == TRUE),])
char <- rownames(char[which(char$sapply.data2..is.character. == TRUE),])

#Examine Correlations

correlations <- data.frame(cor(data2[,c(nums)], use="complete.obs", method="pearson") )
M <- cor(data2[,c(nums)], use="complete.obs", method="pearson")
#
significant.correlations <- data.frame(
  var1 = character(),
  var2 = character(),
  corr = numeric())
#
for (i in 1:nrow(correlations)){
  for (j in 1:ncol(correlations)){
    tmp <- data.frame(
      var1 = as.character(colnames(correlations)[j]),
      var2 = as.character(rownames(correlations)[i]),
      corr = correlations[i,j])
    #
    if (!is.na(correlations[i,j])) {
      if (correlations[i,j] > 0.5 & as.character(tmp$var1) != as.character(tmp$var2)
          | correlations[i,j] < -0.5 & as.character(tmp$var1) != as.character(tmp$var2) ) {
        significant.correlations <- rbind(significant.correlations,tmp) }
    }
  }
}

significant.correlations <- significant.correlations[order(abs(significant.correlations$corr),decreasing=TRUE),] 
significant.correlations <- significant.correlations[which(!duplicated(significant.correlations$corr)),]

# var1     var2       corr
# 1       art  museums  0.8464676
# 9     music concerts  0.6602164
# 11  sinc2_1  attr2_1 -0.6204424
# 12 intel2_1  attr2_1 -0.5786796
# 2   theater  museums  0.5141893
# 4   theater      art  0.5060697
# 7    movies  theater  0.5047329
# 13   amb2_1  attr2_1 -0.5029098

##some multicollinearity

corrgram(data2[,c(nums)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")

corrplot(M, method = "circle") #plot matrix



## split data into train and test
smp_size <- floor(0.75 * nrow(data2))
set.seed(22)
train_ind <- sample(seq_len(nrow(data2)), size = smp_size)

train <- data2[train_ind, ]
test <- data2[-train_ind, ]

y.train <- data.frame(match = train$match)
y.test <- data.frame(match = test$match)

x.train <- train[,c(-5)]
x.test <- test[,c(-5)]

train.mean <- apply(x.train, 2, mean)
train.sd <- apply(x.train, 2, sd)
train.std <- t((t(x.train)-train.mean)/train.sd) # standardize to have zero mean and unit sd
train.std <- cbind(y.train,train.std)

test.mean <- apply(x.test, 2, mean)
test.sd <- apply(x.test, 2, sd)
test.std <- t((t(x.test)-test.mean)/test.sd) # standardize to have zero mean and unit sd
test.std <- cbind(y.test,test.std)

##Try some models
set.seed(1)
library(doParallel)
cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

#Logistic Reg
glm.fit=glm(match~.,
            data=train.std,family = binomial)
# AIC: 4330.9
step.glm <- step(glm.fit)
# AIC: 4297
# vif(step.glm)
# gender     order  int_corr  samerace     age_o       age   imprace      date    go_out    dining   museums 
# 2.140385  1.011482  1.090534  1.040206  1.045940  1.162631  1.105335  1.292842  1.227202  1.384723  4.389361 
# art       clubbing   reading    movies  concerts   attr1_1   sinc1_1   shar1_1   attr2_1   sinc2_1  intel2_1 
# 4.097239  1.124215  1.235381  1.371002  1.424144  2.502898  1.569047  1.595997 84.721625 17.696489 15.015277 
# fun2_1    amb2_1   shar2_1   attr3_1    amb3_1 
# 14.962845 14.890252 12.787696  1.310381  1.231207 
glm.final=glm(match~gender+order+int_corr+samerace+age_o+age+imprace+date+go_out+dining+museums+art+clubbing+reading+movies+concerts+
              attr1_1 + sinc1_1 + shar1_1 + attr3_1 + amb3_1,
            data=train.std,family = binomial)
# AIC: 4297.5
glm.probs <- predict(glm.final,test.std,type="response")

my_roc <- function(predicted.probs=glm.probs,actual=test.std$match) {
  
  threshold_sensitivity <- function(predicted.probs,actual,threshold) {
    confusion <- table(factor(predicted.probs >= threshold,levels=c("FALSE","TRUE")),factor(actual))
    x <- (confusion[4]) / (confusion[3] + confusion[4])
    return(x)
  }
  
  threshold_specificity <- function(predicted.probs,actual,threshold) {
    confusion <- table(factor(predicted.probs >= threshold,levels=c("FALSE","TRUE")),factor(actual))
    x <- (confusion[1]) / (confusion[1] + confusion[2])
    return(x)
  }
  
  d <- data.frame(threshold = seq(0,1,by=0.01), sensitivity=NA, specificity=NA)
  d$sensitivity <- sapply(d$threshold, function(x) threshold_sensitivity(predicted.probs,actual,x))
  d$specificity <- sapply(d$threshold, function(x) threshold_specificity(predicted.probs,actual,x))
  d$spec1 <- 1- d$specificity
  
  p <- ggplot(d, aes(spec1,sensitivity)) +
    geom_line(color="blue") +
    coord_fixed() + 
    geom_line(aes(threshold,threshold)) +
    labs(title = sprintf("ROC")) + xlab("1-Specificity") + ylab("Sensitivity")
  
  height <- (d$sensitivity[-1]+d$sensitivity[-length(d$sensitivity)])/2
  width <- -diff(d$spec1,na.rm=TRUE) 
  auc <- sum(height*width,na.rm=TRUE)
  
  return(list(p,auc))
}

my_roc_stuff <- my_roc(data$scored.probability,data$class)

glm.pred <- ifelse(glm.probs >= 0.35,1,0)
confusionMatrix(glm.pred,test.std$match,positive='1')

##Other modeling techniques

##KNN
set.seed(1)
knn.pred=knn(train.std,test.std,train.std$match,k=7)
table(knn.pred ,test.std$match)
confusionMatrix(knn.pred,test.std$match,positive='1')

##Tree
set.seed(1)
model.tree1 <- rpart(as.factor(match) ~ gender+order+int_corr+samerace+age_o+age+imprace+date+go_out+dining+museums+art+clubbing+reading+movies+concerts+
                       attr1_1 + sinc1_1 + shar1_1 + attr3_1 + amb3_1,data=train.std,method="class")
prp(model.tree1)

tree.pred<- predict(model.tree1,test.std)

##Not good... 

##Random Forest
set.seed(1)
model.RF1 <- randomForest(as.factor(match)~.,data=train.std,
                          mtry=13, ntree =501)

pred.RF1 = predict(model.RF1,newdata = test.std)

varImpPlot(model.RF1)
importance(model.RF1)

confusionMatrix(pred.RF1,test.std$match,positive='1')

##SVM
library(e1071)

##Tune the model
set.seed(1)
svm.tune=tune(svm,match~.,data=train.std ,kernel ="radial",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(svm.tune)

bestmod =svm.tune$best.model
summary(bestmod)

pred.svm =data.frame(pred = predict(bestmod,test.std))
pred.svm$flag <-ifelse(post.valid.svm$pred >= 0.15,1,0)
table(pred.svm$flag,test.std$match)
confusionMatrix(pred.svm$flag,test.std$match,positive='1')
