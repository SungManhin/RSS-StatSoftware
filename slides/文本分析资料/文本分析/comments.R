rm(list=ls()) 
setwd("F:\\360云盘文件\\工作\\教学课程\\统计分析与R软件\\文本分析课程资料\\class\\")                                                                                                            
library(jiebaRD)
library(jiebaR) 
library(ggplot2)
                                                      


##读入comments数据
com_data=read.csv("Apple7_comments.csv", header = T, stringsAsFactors = F)
dim(com_data)
head(com_data)
comments=com_data[,6]
ratings=as.factor(com_data[,2])



##进行简单的统计
####统计字数
words=nchar(comments)
####看下第一个comment
comments[1:2]
words[1:2]
####统计句子数，我们按照“，。！？”来表明一个句子
########首先按照标点符号进行分句处理
sententce=strsplit(comments,split="，|。|！|？")
########统计各个comment中的句子数
Lsen=lapply(sententce,length)
########Lsen是一个list，我们去掉它的list形式
Lsen=unlist(Lsen)
ggplot(com_data, aes(x=ratings, y=words),fill=ratings) + geom_boxplot()+xlab("评分")+ylab("字数")
ggplot(com_data, aes(x=ratings, y=Lsen),fill=ratings) + geom_boxplot()+xlab("评分")+ylab("句子数")
####更多的字符处理函数
#########paste:字符拼接函数
example="你我他"
paste(example,"是熊孩子",sep="")
example=c("你","我","他")
paste(example,"是熊孩子",sep="")
#########grep:字符查找函数
grep("不错",comments[1:6])
#########gsub:字符替代函数
gsub(pattern = "不错", replacement = "非常棒", comments[1:2])
########substr：字符串提取函数
substr(comments[1:2], start=2, stop=5)



##进行分词
####初始化分词器，即能分词，又可以进行词性标注，并且在分词的时候去停用词
cutter = worker(bylines = TRUE) 
####进行分词                                         
res = cutter[comments] 


##优化词库1：去掉停用词
stoppath="./stopwords.dat"
cutter = worker(bylines = TRUE,stop_word=stoppath) 
res_stop = cutter[comments] 



##优化词库2：自定义字典
####显示默认词典路径
syspath=show_dictpath()  
#jieba.dict.utf8, 默认的系统字典，第一列为词项，第二列为词频，第三列为词性标记
#user.dict.utf8, 默认的用户自定义词典，第一列为词项，第二列为词性标记
sys_user_path=file.path(syspath,"user.dict.utf8")
res[3] "吐槽"
res[4] "双11"
res[5] "给力"
res[6] "靠谱"
res[13] "双十一"
res[35] "挺好"
res[47] "打心里"
res[49] "亮黑色"
res[82] "很给力"
res[85] "亮黑"，"电流声" 
res[96] "学生机"，“老人机”
res[97] "双十一"， "人手一个"
####user.dict.utf8文件（这个是默认载入的用户词典文件）
########方法1：把新词直接copy paste到默认的词典中
########方法2：使用R语句添加特定的词
sys_user_dict=load_user_dict(filePath = sys_user_path)
add_user_words(dict = sys_user_dict, words = c("老人机","双十一"),tags=c("n","n"))
########方法3：直接建立一个自己的词典
dictpath="./add_dict.txt"
cutter = worker(bylines = TRUE,user = dictpath) 
res_user = cutter[comments] 


##优化词库3：添加搜狗细胞词库
install.packages("devtools")
install.packages("stringi")
install.packages("pbapply")
install.packages("Rcpp")
install.packages("RcppProgress")
library(devtools)
install_github("qinwf/cidian")
library(cidian)
####进行转换
decode_scel(scel = "phone_words.scel",output = "phone_words_sogou.txt")


##标注词性
cutter = worker(bylines = TRUE,"tag") 
res_tag = cutter[comments] 
####提取每个词的词性
tag = lapply(res_tag,names)



##按照词性进行筛选
####将res_tag从list转换为matrix格式
text = lapply(res_tag,as.matrix)
####将tag从list转换为matrix
tag = lapply(tag,as.matrix) 
####将每行文本的分词结果逐一排列起来
text_all = as.data.frame(do.call(rbind, text), stringsAsFactors = F) 
####将标注的词性结果逐一排列起来
tag_all = as.data.frame(do.call(rbind, tag), stringsAsFactors = F) 


##对分词结果按照词性进行筛选
####读入选取的词性
choose_tags=read.csv("tags.csv", header = F, stringsAsFactors = F) 
####找到词性所在列
choose_tags=as.matrix(choose_tags[,1])
####找出tag中属于choose_tags中任意一个词性的小标
tnum=apply(choose_tags,1,function(x){return(which(tag_all[,1]==x))})
####将tnum从list变为vector
tnum=unlist(tnum)
####得到筛选后的tag和text
tag_all2=tag_all[tnum,1]
text_all2=text_all[tnum,1]



##关键词提取1：对选出的词进行第二次筛选
####找出每个词的长度
Len=nchar(text_all2)
####选出长度大于1的词
########找出长度大于1的词的下标
num=which(Len>1)
########得到筛选后的tag和text
tag_all3=tag_all2[num]
text_all3=text_all2[num]
########观察选出的词一共有多少个
unique_phrases=unique(text_all3)
length(unique_phrases)



##关键词提取2：提取高频词作为关键词
####进行词频统计
freq = as.data.frame(table(text_all3),stringsAsFactors = F) 
####按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
####挑选前20个高频词                                
top20 = freq[1:20,]


##关键词提取3：用TF-IDF提取关键词
####初始化
cutter=worker("keywords",bylines = TRUE,topn = 20)
####评论必须单独保存为一个文件
write.table(comments, file = "comments.txt", quote = F, row.names = F, col.names = F)
key=keywords(code="comments.txt",cutter)



##制作词云
####安装并加装词云包
install.packages("RColorBrewer")
install.packages("wordcloud")
library("RColorBrewer")
library("wordcloud") 
####挑选前100个高频词用于词云制作                                                     
top100 = freq[1:100,] 
colnames(top100)=c("phrases","freq")
####制作词云
#########设置颜色系(调用调色板Dark2中的8个颜色)
mycolors=brewer.pal(8,"Dark2")
##########设置字体
windowsFonts(myFont=windowsFont("华文彩云"))
##########画图
wordcloud(top100$phrases,top100$freq,random.order=F,random.color=F,colors=mycolors,family="myFont")               

##如何画出更漂亮的词云图
####输出高频词数据用于制作tagxedo词云标签                                                   
write.table(top100, file = "Top100.txt", quote = F, row.names = F, col.names = F)


