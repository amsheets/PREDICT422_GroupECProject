# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Any results you write to the current directory are saved as output.

data <- read.csv("../input/Speed Dating Data.csv")
data$income2 <- sub(",", "", data$income)
data$income2 <- as.numeric(data$income2)

method1 <- data[data$wave>5 & data$wave<10,]
method2 <- data[data$wave<6 | data$wave>9,]
In [225]:
  # does income influence what motives people had to participate in the study?
  
  #ggplot(data[!duplicated(data$iid),], aes(factor(goal),income2)) + geom_boxplot() + geom_jitter()
  
  par(mar=c(12,5,3,3))
boxplot(data$income2~data$goal, ylab="Income", xlab="", xaxt="n")
axis(1, at=c(1,2,3,4,5,6), labels=c("Fun", "Meet people", "Date", "Serious relationship", "Experience", "Other"), las=2)
mtext("Reason for participating", side=1, line=10)
In [226]:
  # does income relate to how frequently you date/go out?
  
  data$incomesplit <- cut(data$income2, c(0,30000,50000,70000,90000,110000))
data$date <- as.numeric(data$date)

#par(fig=c(0,0.5,0,1))
#boxplot(data$income2~data$date, xlab="Dates a lot    ---    Seldom dates", ylab="Income")
lm(data$income2~data$date)
p1 <- ggplot(na.omit(data[,c("incomesplit", "date")])[!duplicated(data$iid),], aes(incomesplit, date)) + geom_boxplot() + geom_jitter(alpha=1/2)


#par(fig=c(0.5,1,0,1), new=T)
#boxplot(data$income2~data$go_out, xlab="Goes out a lot  ---  Seldom goes out", ylab="Income")
lm(data$income2~data$go_out)
p2 <- ggplot(na.omit(data[,c("incomesplit", "go_out")])[!duplicated(data$iid),], aes(incomesplit, go_out)) + geom_boxplot() + geom_jitter(alpha=1/2)



multiplot(p1,p2, ncol=2)
In [227]:
  # what do the top earners do?
  
  topearn <- unique(data[data$income2>quantile(data$income2, 0.95, na.rm=T) & !is.na(data$income2),c(49,196)])
topearn[order(topearn$income2, decreasing=T),]
In [228]:
  # what are the exercise habits of the wealthy? - slight trend for high value of exercise in the ultra-wealthy
  
  ggplot(na.omit(data[,c("incomesplit", "exercise")])[!duplicated(data$iid),], aes(exercise, x=incomesplit)) +  geom_boxplot() + geom_jitter()


#plot(data$income2, data$exercise)
#boxplot(data$income2~data$exercise, ylab="Income", xlab="Exercise interest")
In [229]:
  # how about reading?
  
  boxplot(data$income2~data$reading, ylab="Income", xlab="Reading interest")
In [230]:
  summary(lm(data$income2~data$reading))
In [231]:
  # do the wealthy value shopping more?
  
  boxplot(data$income2~data$shopping, ylab="Income", xlab="Shopping interest")
In [232]:
  summary(lm(data$income2~data$shopping))
In [233]:
  # the study group think they're smarter than the average person, and the average person they dated
  
  hist(data$intel3_1, xlab="Percieved intelligence", breaks=6, xlim=c(0,10))
hist(data$intel, xlab="Dates' intelligence", breaks=10, xlim=c(0,10))
In [234]:
  # how did people rate their own intelligence in comparison to their date's?
  
  ggplot(data, aes(y=intel, x=intel3_1)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank()) + ylab("Date's intelligence") + xlab("Own intelligence")

summary(lm(data$intel~data$intel3_1))
In [235]:
  # how do peoples percieved qualities affect their confidence of getting a date?
  
  cor(data$intel3_1,data$prob, use="complete.obs")
cor(data$attr3_1,data$prob, use="complete.obs")
cor(data$fun3_1,data$prob, use="complete.obs")
cor(data$sinc3_1,data$prob, use="complete.obs")
cor(data$amb3_1,data$prob, use="complete.obs")
In [236]:
  # how does this compare to shared interests?
  cor(data$shar,data$prob, use="complete.obs")
In [237]:
  # did people's perception of themselves change over the course of the experiment?
  
  cor(data$intel3_1, data$intel3_3, use="complete.obs")
cor(data$attr3_1, data$attr3_3, use="complete.obs")
cor(data$fun3_1, data$fun3_3, use="complete.obs")
cor(data$sinc3_1, data$sinc3_3, use="complete.obs")
cor(data$amb3_1, data$amb3_3, use="complete.obs")
In [238]:
  ggplot(data, aes(x=intel3_1, y=intel3_3)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
ggplot(data, aes(x=attr3_1, y=attr3_3)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
ggplot(data, aes(x=fun3_1, y=fun3_3)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
ggplot(data, aes(x=sinc3_1, y=sinc3_3)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
ggplot(data, aes(x=amb3_1, y=amb3_3)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
In [239]:
  # how do mate quality preferences vary across career choices?
  
  # attractiveness
  par(mar=c(10,5,3,3))
#ggplot(data, aes(factor(career_c), attr1_1)) + geom_boxplot()

boxplot(method2$attr1_1~method2$career_c, xaxt="n")
abline(h=median(method2$attr1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [240]:
  # sincerity - genuinely interesting
  par(mar=c(10,5,3,3))
boxplot(method2$sinc1_1~method2$career_c, xaxt="n", ylab="Rating /100")
abline(h=median(method2$sinc1_1, na.rm=T))
axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [241]:
  # intelligence also interesting, see zero values
  par(mar=c(10,5,3,3))
boxplot(method2$intel1_1~method2$career_c, xaxt="n", ylab="Rating /100")
abline(h=median(method2$intel1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [242]:
  # fun
  par(mar=c(10,5,3,3))
boxplot(method2$fun1_1~method2$career_c, xaxt="n", ylab="Rating /100")
abline(h=median(method2$fun1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [243]:
  # ambition
  par(mar=c(10,5,3,3))
boxplot(method2$amb1_1~method2$career_c, xaxt="n", ylab="Rating /100")
abline(h=median(method2$amb1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [244]:
  # shared interests
  par(mar=c(10,5,3,3))
boxplot(method2$shar1_1~method2$career_c, xaxt="n", ylab="Rating /100")
abline(h=median(method2$shar1_1, na.rm=T))

axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)
In [245]:
  # are these choices re-evaluated after the experiment?
  
  par(fig=c(0,0.33,0,0.5), mar=c(4,2,2,2))
boxplot(method2$attr1_1, method2$attr1_3, main="Attractiveness", names=c("Before", "After"), las=2)
par(fig=c(0.33,0.66,0,0.5), new=T)
boxplot(method2$sinc1_1, method2$sinc1_3, main="Sincerity", names=c("Before", "After"), las=2)
par(fig=c(0.66,1,0,0.5), new=T)
boxplot(method2$intel1_1, method2$intel1_3, main="Intelligence", names=c("Before", "After"), las=2)
par(fig=c(0,0.33,0.5,1), new=T)
boxplot(method2$fun1_1, method2$fun1_3, main="Fun", names=c("Before", "After"), las=2)
par(fig=c(0.33,0.66,0.5,1), new=T)
boxplot(method2$amb1_1, method2$amb1_3, main="Ambition", names=c("Before", "After"), las=2)
par(fig=c(0.66,1,0.5,1), new=T)
boxplot(method2$shar1_1, method2$shar1_3, main="Shared interests", names=c("Before", "After"), las=2)
In [246]:
  # what matters most in mate choice?
  
  par(mar=c(10,5,3,3))
boxplot(method2$attr1_1, method2$sinc1_1,method2$intel1_1,method2$fun1_1,method2$amb1_1,method2$shar1_1, names=c("Attractiveness", "Sincerity", "Intelligence", "Fun", "Ambition", "Shared Interests"), las=2, ylab="Importance /100")
# who are the people who only want attractiveness? - CEOs, presidents
# who are the people who don't care? - Professor/want to be professor
In [247]:
  # how do people's ratings of their own attractiveness determine their preferences for partners attractiveness
  # more attractive people have a stronger preference for attractiveness in a partner
  
  summary(lm(method2$attr3_1~method2$attr1_1))
#plot(method2$attr3_1, method2$attr1_1)
ggplot(method2, aes(factor(attr3_1), attr1_1)) + geom_boxplot()
In [248]:
  # what's the strongest correlation between self and date preference? sincerity
  
  cor(method2$attr1_1, method2$attr3_1, use="complete.obs")
cor(method2$sinc1_1, method2$sinc3_1, use="complete.obs")
cor(method2$intel1_1, method2$intel3_1, use="complete.obs")
cor(method2$fun1_1, method2$fun3_1, use="complete.obs")
cor(method2$amb1_1, method2$amb3_1, use="complete.obs")

ggplot(data, aes(x=sinc1_1, y=sinc3_1)) + geom_point(shape=1, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10)
In [249]:
  # how does hobby preference affect date preference
  
  summary(lm(method2$attr1_1~method2$gaming))
summary(lm(method2$intel1_1~method2$reading))
summary(lm(method2$attr1_1~method2$exercise))
summary(lm(method2$attr1_1~method2$clubbing))
summary(lm(method2$intel1_1~method2$tv))

#par(fig=c(0,0.33,0,0.5), mar=c(3,3,5,3))
#boxplot(method2$attr1_1~method2$gaming, main="Gaming ~ attr") # positive correlation
#par(fig=c(0.33,0.66,0,0.5), new=T)
#boxplot(method2$intel1_1~method2$reading, main="Reading ~ intell") # surprisingly low correlation
#par(fig=c(0.66,1,0,0.5), new=T)
#boxplot(method2$attr1_1~method2$exercise, main="Exercise ~ attr") # not a strong correlation
#par(fig=c(0,0.5,0.5,1), new=T)
#boxplot(method2$attr1_1~method2$clubbing, main="Clubbing ~ attractiveness") # not a strong correlation
#par(fig=c(0.5,1,0.5,1), new=T)
#boxplot(method2$intel1_1~method2$tv, main="TV ~ intelligence") # slight negative correlation
In [250]:
  # what do speed daters like to do the rest of the time?
  hobbies <- data[,51:67]

boxplot(hobbies, las=3)
In [251]:
  # sports players were much more optimistic than bodybuilders
  
  cor(data$sports, data$exphappy, use="complete.obs")
cor(data$exercise, data$exphappy, use="complete.obs")
In [252]:
  # how do people compare their self-perceptions to how they think others see them?
  # one person rates themselves as a 9 on attractiveness but thinks other people think they're a 4
  
  
  p1 <- ggplot(data, aes(attr3_1, attr5_1)) + geom_point(shape=1, size=0.2, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank())
p2 <- ggplot(data, aes(sinc3_1, sinc5_1))+ geom_point(shape=1, size=0.2, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank())
p3 <- ggplot(data, aes(intel3_1, intel5_1)) + geom_point(shape=1, size=0.2, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank())
p4 <- ggplot(data, aes(fun3_1, fun5_1)) + geom_point(shape=1, size=0.2, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank())
p5 <- ggplot(data, aes(amb3_1, amb5_1)) + geom_point(shape=1, size=0.2, position=position_jitter(width=1,height=1)) + xlim(0,10) + ylim(0,10) + theme(panel.background = element_blank())

multiplot(p1,p2,p3,p4,p5, cols=2)
In [253]:
  # how long do people like to talk for?
  
  howlong <- data$length
howlong <- cbind(howlong, c(rep("NA", length(howlong))))
howlong[howlong[,1]=="1",2] <- 1
howlong[howlong[,1]=="3",2] <- 2
howlong[howlong[,1]=="2",2] <- 3
par(fig=c(0,0.5,0,1), mar=c(10,4,1,1))
boxplot(as.numeric(howlong[,2])~data$career_c, xaxt="n", ylab="Too little time -- Too much time")
axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)

howmany <- data$numdat_2
howmany <- cbind(howmany, c(rep("NA", length(howmany))))
howmany[howmany[,1]=="1",2] <- 1
howmany[howmany[,1]=="3",2] <- 2
howmany[howmany[,1]=="2",2] <- 3

par(fig=c(0.5,1,0,1), new=T)
boxplot(as.numeric(howmany[,2])~data$career_c, xaxt="n", ylab="Too few dates -- Too many dates")
axis(1, at=seq(1,17,1), labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture"), las=2)

#howlong <- cbind.data.frame(length=as.numeric(howlong[,2]), career=data$career_c)
#p <- ggplot(howlong, aes(x=factor(career), y=length)) + geom_point(shape=19, alpha=1/8, position=position_jitter(width=0.4,height=0.4)) + ylim(1,3) + scale_x_discrete(labels=c("Lawyer", "Academic", "Psychologist", "Doctor", "Engineer", "Entertainment", "Finance/Business", "Real Estate", "International Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athlete", "Other", "Journalism", "Architecture", "NA"))
#print(p)
In [254]:
  # people think their preferences become more like everyone else's as the experiment goes on e.g. attractiveness
  
  # what they looked for at the start vs what they thought influenced their decision later
  cor(data$attr1_1, data$attr7_2, use="complete.obs")
# what they looked for at the start vs what they thought others looked for
cor(data$attr1_1, data$attr4_1, use="complete.obs")
# what they think influenced their choce vs what they think other people look for
cor(data$attr7_2, data$attr4_1, use="complete.obs")
# what they think influenced their choce vs what they think other people look for
cor(data$attr7_2, data$attr4_2, use="complete.obs")
# what they think influenced their choce vs what they think other people look for
cor(data$attr7_3, data$attr4_3, use="complete.obs")

# how their views on other people's preferences changed
cor(data$attr4_1, data$attr4_3, use="complete.obs")
# how their views on their preferences changed
cor(data$attr1_1, data$attr1_3, use="complete.obs")
In [255]:
  # people generally realise they care about attractiveness more than they thought they did
  
  plot(method2$attr1_1, method2$attr1_3, xlab="Initial value on atractiveness", ylab="Value on attractiveness after experiment", xlim=c(0,100), ylim=c(0,100))
lines(seq(0,100,10), seq(0,100,10))