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

vars <- data.frame(varname = colnames(data),pct_miss = rep(0,length(colnames(data))))

for (i in 1:length(colnames(data))) {
  na = sum(is.na(data[,c(vars$varname[i])]))
  total = length(data[,c(vars$varname[i])])
  vars$pct_miss[i] <- round(na / total,digits = 3)
}

use_vars <- vars[which(vars$pct_miss <= 0.20),]

data2 <- data[,c(use_vars$varname)]

##Look into which variables are numeric and which are character
nums <- data.frame(sapply(data2, is.numeric))
char <- data.frame(sapply(data2, is.character))

nums$variable <- rownames(nums)
char$variable <- rownames(char)

nums <- nums[which(nums$sapply.data2..is.numeric. == TRUE),]
char <- char[which(char$sapply.data2..is.character. == TRUE),]

#Examine Correlations

correlations <- data.frame(cor(data2[,c(nums$variable)], use="complete.obs", method="pearson") )

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

##Lots of significant correlations and multi-collinearity

corrgram(data2[,c(nums$variable)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")

corrplot(correlations, method = "circle") #plot matrix

##Try out modeling "decision" using variables from the SCORECARD in the data dictionary
testing <- data[,c(2:3,7:8,10,13:17,34,40:42,46:48,51:68,70:75,82:92)]
testing <- testing[complete.cases(testing),]

## split data into train and test
smp_size <- floor(0.75 * nrow(testing))
set.seed(22)
train_ind <- sample(seq_len(nrow(testing)), size = smp_size)

train <- testing[train_ind, ]
test <- testing[-train_ind, ]

##Try some models

#Linear Reg
lm.fit <- lm(match~.,data=train)
step.lm <- step(lm.fit)

#Logistic Reg
glm.fit=glm(match~.,
            data=testing,family = binomial)
step.glm <- step(glm.fit)

##Other modeling techniques

##KNN
set.seed(1)
knn.pred=knn(train,test,train$dec,k=1)
table(knn.pred ,test$dec)
