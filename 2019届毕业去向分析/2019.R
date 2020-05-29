library(plotrix)
data<-read.csv("data.csv")
data1<-data[data$学历名称=="本科生毕业",]
data2<-data[data$学历名称=="硕士生毕业",]
data3<-data[data$学历名称=="博士生毕业",]
#本科#
all<-table(data1$毕业去向名称)
pie(all)
title("2019届本科生就业去向情况")
all_school<-data1[data1$毕业去向名称=="升学",]#本科升学情况
school<-all_school$单位名称
school<-table(school)
school<-data.frame(school)
school<-school[-which(school$Freq==0),]
pie(school$Freq,labels = school$school)
title("2019届本科生升学情况")
all_job1<-data1[data1$毕业去向名称=="签就业协议形式就业",]#本科就业情况
all_job2<-data1[data1$毕业去向名称=="签劳动合同形势就业",]
all_job3<-data1[data1$毕业去向名称=="其他录用形式就业",]
all_job<-rbind(all_job1,all_job2,all_job3)
job<-all_job$单位名称
job<-table(job)
job<-data.frame(job)
job<-job[-which(job$Freq==0),]
pie(job$Freq,labels = job$job,title("2019届本科生就业企业分布"))
abroad<-data1[data1$毕业去向名称=="出国、出境",]#本科出国情况
abroad<-table(abroad$单位名称)
abroad<-data.frame(abroad)
abroad<-abroad[-which(abroad$Freq==0),]
pie(abroad$Freq,labels = abroad$Var1,title("2019届本科生出国留学分布"))
#硕士#
all<-table(data2$毕业去向名称)
pie(all)
title("2019届硕士生就业去向情况")
all_school<-data2[data2$毕业去向名称=="升学",]#硕士升学情况
school<-all_school$单位名称
school<-table(school)
school<-data.frame(school)
school<-school[-which(school$Freq==0),]
pie(school$Freq,labels = school$school)
title("2019届硕士生升学情况")
all_job1<-data2[data2$毕业去向名称=="签就业协议形式就业",]#硕士就业情况
all_job2<-data2[data2$毕业去向名称=="签劳动合同形势就业",]
all_job3<-data2[data2$毕业去向名称=="其他录用形式就业",]
all_job<-rbind(all_job1,all_job2,all_job3)
job<-all_job$单位名称
job<-table(job)
job<-data.frame(job)
job<-job[-which(job$Freq==0),]
pie(job$Freq,labels = job$job,title("2019届硕士生就业企业分布"))
#本科专业就业、升学情况对比#
data_jin<-data[data$专业名称=="金融数学",]#本科各专业升学情况对比
data_tong<-data[data$专业名称=="应用统计学",]
data_shu<-data[data$专业名称=="数学与应用数学",]
data_suan<-data[data$专业名称=="信息与计算科学",]
schoolrate_jin<-dim(data_jin[data_jin$毕业去向名称=="升学",])[1]/dim(data_jin)[1]
schoolrate_tong<-dim(data_tong[data_tong$毕业去向名称=="升学",])[1]/dim(data_tong)[1]
schoolrate_shu<-dim(data_shu[data_shu$毕业去向名称=="升学",])[1]/dim(data_shu)[1]
schoolrate_suan<-dim(data_suan[data_suan$毕业去向名称=="升学",])[1]/dim(data_suan)[1]
zy<-c("金融数学","应用统计学","数学与应用数学","信息与计算科学")
dim(zy)<-c(4,1)
schoolrate<-rbind(schoolrate_jin,schoolrate_tong,schoolrate_shu,schoolrate_suan)
par(mfrow=c(1,2))
barplot(schoolrate[,1],names.arg = zy,col=c("yellow","blue","red","green"))
title("本科各专业升学率对比")
rate_jin<-(dim(data_jin[data_jin$毕业去向名称=="签就业协议形式就业",])[1]+dim(data_jin[data_jin$毕业去向名称=="签劳动合同形势就业",])[1]+dim(data_jin[data_jin$毕业去向名称=="其他录用形式就业",])[1])/dim(data_jin)[1]
rate_tong<-(dim(data_tong[data_tong$毕业去向名称=="签就业协议形式就业",])[1]+dim(data_tong[data_tong$毕业去向名称=="签劳动合同形势就业",])[1]+dim(data_tong[data_tong$毕业去向名称=="其他录用形式就业",])[1])/dim(data_tong)[1]
rate_shu<-(dim(data_shu[data_shu$毕业去向名称=="签就业协议形式就业",])[1]+dim(data_shu[data_shu$毕业去向名称=="签劳动合同形势就业",])[1]+dim(data_shu[data_shu$毕业去向名称=="其他录用形式就业",])[1])/dim(data_shu)[1]
rate_suan<-(dim(data_suan[data_suan$毕业去向名称=="签就业协议形式就业",])[1]+dim(data_suan[data_suan$毕业去向名称=="签劳动合同形势就业",])[1]+dim(data_suan[data_suan$毕业去向名称=="其他录用形式就业",])[1])/dim(data_suan)[1]
jobrate<-rbind(rate_jin,rate_tong,rate_shu,rate_suan)
barplot(jobrate[,1],names.arg = zy,col=c("yellow","blue","red","green"))
title("本科各专业就业率对比")
