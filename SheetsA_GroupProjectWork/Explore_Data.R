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
data2 <- data2[,c(2:3,7:8,10,13:17,34,40:42,46:48,51:68,70:75,82:92)]
data2 <- data2[complete.cases(data2),]

##Look into which variables are numeric and which are character
nums <- data.frame(sapply(data2, is.numeric))
char <- data.frame(sapply(data2, is.character))

nums$variable <- rownames(nums)
char$variable <- rownames(char)

nums <- rownames(nums[which(nums$sapply.data2..is.numeric. == TRUE),])
char <- rownames(char[which(char$sapply.data2..is.character. == TRUE),])

#Examine Correlations

correlations <- data.frame(cor(data2[,c(nums)], use="complete.obs", method="pearson") )

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
      if (correlations[i,j] > .5 & as.character(tmp$var1) != as.character(tmp$var2)
          | correlations[i,j] < -.5 & as.character(tmp$var1) != as.character(tmp$var2) ) {
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

corrplot(correlations, method = "circle") #plot matrix



## split data into train and test
smp_size <- floor(0.75 * nrow(data2))
set.seed(22)
train_ind <- sample(seq_len(nrow(data2)), size = smp_size)

train <- data2[train_ind, ]
test <- data2[-train_ind, ]

##Try some models

#Logistic Reg
glm.fit=glm(match~.,
            data=train,family = binomial)
step.glm <- step(glm.fit)
glm.probs <- predict(step.glm,test,type="response")
glm.pred <- ifelse(glm.probs >= 0.35,1,0)
confusionMatrix(glm.pred,test$match,positive='1')

##Other modeling techniques

##KNN
set.seed(1)
knn.pred=knn(train,test,train$match,k=7)
table(knn.pred ,test$match)
confusionMatrix(knn.pred,test$match,positive='1')

##Tree
model.tree1 <- tree(as.factor(match) ~ .,data=train)
plot(model.tree1)
text(model.tree1)

tree.pred<- predict(model.tree1,test)

##SVM
library(e1071)
library(doParallel)
cl <- makeCluster(detectCores()) 
registerDoParallel(cl)

##Tune the model
set.seed(1)
svm.tune=tune(svm,match~.,data=train ,kernel ="radial",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(svm.tune)

bestmod =svm.tune$best.model
summary(bestmod)

post.valid.svm =data.frame(pred = predict(bestmod,train))
post.valid.svm$flag <-ifelse(post.valid.svm$pred >= 0.1,1,0)
table(post.valid.svm$flag,train$match)
confusionMatrix(post.valid.svm$flag,train$match,positive='1')
