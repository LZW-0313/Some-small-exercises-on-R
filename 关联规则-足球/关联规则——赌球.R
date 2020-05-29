library(arules)
library(arulesViz)
da<-read.csv("football.csv",header=FALSE)
getwd()
setwd("C:/Users/Eos/Desktop")
da[da=="NULL"]<-NA
da<-na.omit(da)
fix(da)
rules=apriori(da,parameter=list(support=0.01,confidence=0.7,minlen=2))      #计算所有规则
rules<-sort(rules,by='confidence')
rules<-subset(rules,subset=!(lhs%pin%c("盘路")))
rules<-subset(rules,subset=!(lhs%pin%c("赛果")))
rules<-subset(rules,subset=!(lhs%pin%c("大小")))
rules<-subset(rules,subset=!(rhs%pin%c("赛事")))
rules<-rules[!is.redundant(rules)]
inspect(rules)
write(rules,file="RULES_football.csv",sep=",",quote=TRUE,row.names=FALSE)    #导出所有规则
rules_RESULT<-subset(rules,rhs%pin%c("赛果"))
inspect(rules_RESULT)
write(rules_RESULT,file="rules_RESULT.csv",sep=",",quote=TRUE,row.names=FALSE) #导出与赛果有关的规则
rules_zhudui<-subset(rules,lhs%pin%c("主队"))
inspect(rules_zhudui)
