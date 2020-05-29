library(jiebaR)                                     #方法一
library(wordcloud2)
a<-scan('C:/Users/Eos/Desktop/zihan.txt',sep='\n',what='')
seg<-qseg[a]
seg<-seg[nchar(seg)>1]
seg<-table(seg)
seg<-seg[!grepl('[0-9]+',names(seg))]
seg
seg<-sort(seg,decreasing = TRUE)[1:50]
wordcloud2(seg)

wk<-worker(stop_word = "中文停用词.txt")            #方法二
wk[file.choose()]
f<-scan(file.choose(),sep="\n",what="")
df<-freq(wk[f])
wordcloud2(df)

#实例：人民的名义关系网#
library(tidyverse)               #使用此包中的ggplot2包（数据可视化）,stringr包（字符串处理）,readr包（读取数据）
library(jiebaR)                  #支持分词
library(network)                 #以下R包支持网络分析及可视化
library(sna)    
library(ggnetwork)  
library(igraph)   
library(intergraph)
text <- readLines("rmdmy.txt")         #导入小说文本
text <- text[nchar(text)!=0]               #去掉空行
role = readLines("role.txt")               #以下为导入人物数据
role_all = readLines("role-fate.txt")
role_all2 = readLines("name.txt")
#人物关系图#
subtext=text
n=length(subtext)                               #n记为总段落数
data=vector('list',n)                           #创建列表
cutter = worker('mix')                          #设置分词环境为mix
for(i in 1:n) data[[i]] = cutter[subtext[i]][which(cutter[subtext[i]] %in% role_all2)] #分词，若在role_all2中出现则提取
weidata=data.frame(t(combn(role_all2,2)))       #给出所有人名可能的组合，行列互换后保存为数据框
names(weidata)=c('name1','name2')               #修改列名
weiname=rep(0,120)                              #weiname为210个元素的零数组
for(i in 1:120) weiname[i]=paste(weidata$name1[i],weidata$name2[i],sep = '--') #将weidata中的两个人名用“-—”连接并储存
weidata$weiname=weiname                         #在weidata中添加weiname列
weidata$weight = rep(0,120)                     #添加weight列
for(i in 1:n){                                  #对于i从1到n
  if(length(data[[i]]) != 0){                   #如果data[i]的长度不是0
    test=as.data.frame(table(paste(expand.grid(data[[i]],data[[i]])$Var1,
                                   expand.grid(data[[i]],data[[i]])$Var2,sep = '--'))) #将data[i]中的字符组合，将得到的两列用“-—”连接,并统计频数
    test$Var1=as.vector(test$Var1)               #将test第一列转化为向量
    test$Freq=test$Freq/max(test$Freq)           #将频数除以此时频数的最大值
    id1=which(test$Var1 %in% weidata$weiname)    #id1为weidata$weiname出现在test$Var1中的行的序号
    id2=which( weidata$weiname %in% test$Var1)   #id2为test$Var1出现在weidata$weiname中的行的序号
    weidata$weight[id2]=weidata$weight[id2]+test$Freq[id1] #权重相应增加
  }
}

#绘制人物关系网络图#
weidata1 = weidata[which(weidata$weight !=0),]   #取weidata中权重不为0的行

g1=graph.data.frame(weidata1[,-3], directed = F) #把数据变成图像数据矩阵
n=fortify(g1)                                    #转换为ggplot2可识别的类型
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(linetype = 2, color = "grey50",curvature = 0.1) +
  geom_nodes(aes(color =  vertex.names, size =  weight)) +
  geom_nodelabel_repel(aes(color = vertex.names, label = vertex.names),
                       fontface = "bold", box.padding = unit(1, "lines")) +
  theme(legend.position='none',
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank()
  )                                               #绘制网状图
