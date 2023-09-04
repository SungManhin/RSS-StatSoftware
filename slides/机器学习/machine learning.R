library(caret)
library(lattice)
library(ggplot2)
library(randomForest)
library(fastAdaboost)

##加载机器学习包

## 加载数据
dat = read.csv('相亲数据2.csv')
dim(dat)
## [1] 15 12
head(dat)

###设置随机种子
set.seed(1234)
###将数据集的80%划分为训练集，20%划分为测试集
trainIndex = createDataPartition(dat$决定, p = .8, list = FALSE, times = 1)
datTrain = dat[ trainIndex,]        #训练集
datTest  = dat[-trainIndex,]        #测试集
table(dat$决定)/nrow(dat)           #全集上因变量各个水平的比例
table(datTrain$决定)/nrow(datTrain) #训练集上因变量各个水平的比例
table(datTest$决定)/nrow(datTest)   #测试集上因变量各个水平的比例


set.seed(1234)
index = createFolds(dat$决定, k = 3, list = FALSE, returnTrain = TRUE)
index 

testIndex = which(index == 1)
datTraincv = dat[-testIndex , ]     #训练集
datTestcv  = dat[testIndex , ]      #测试集

createResample(dat$决定, times = 3, list = F)


## 加载数据
growdata = read.csv('time.csv')
head(growdata)
#参数定下来有多少测试集和训练集就确定了
timeSlices = createTimeSlices(1:nrow(growdata), initialWindow = 5, horizon = 2, fixedWindow = TRUE)
timeSlices


imputation_k = preProcess(datTrain,method = 'medianImpute')
datTrain1 =  predict(imputation_k, datTrain)
datTest1 =  predict(imputation_k, datTest)
datTest1
median(datTrain$智力,na.rm = T)


library(RANN)
imputation_k = preProcess(datTrain,method = 'knnImpute')
## Warning in preProcess.default(datTrain, method = "knnImpute"): These variables
## have zero variances: 是否喜欢矮矬穷, 对方是否喜欢矮矬穷
datTrain1 =  predict(imputation_k, datTrain)
datTest1 =  predict(imputation_k, datTest)
datTrain$智力 = datTrain1$智力 * sd(datTrain$智力,na.rm = T) + mean(datTrain$智力,na.rm = T)
datTest$智力 = datTest1$智力 * sd(datTrain$智力,na.rm = T) + mean(datTrain$智力,na.rm = T)
datTest


#label为数字
dim(datTrain)
## [1] 12 12
nzv = nearZeroVar(datTrain)
nzv
## [1]  4 10
datTrain = datTrain[,-nzv] #直接删掉矮矬穷


# 处理高度线性相关变量
# “性别”和“对方性别”
# findCorrelation() 函数：找到高度共线性的变量，并给出建议删除的变量
# 注：使用函数时，数据是不能有缺失值、只能包含数值型变量的dataframe或者matrix


#数据中不能有NA
datTrain1 = datTrain[,-c(1,6)] #去掉人名
descrCor = cor(datTrain1)
descrCor

highlyCorDescr = findCorrelation(descrCor, cutoff = .75,names = F,verbose = T)
## Compare row 2  and column  6 with corr  0.837 
##   Means:  0.366 vs 0.265 so flagging column 2 
## All correlations <= 0.75
highlyCorDescr
## [1] 2
filteredTrain = datTrain1[,-highlyCorDescr[2]]
dat.train <- filteredTrain

preProcValues = preProcess(datTrain, method = c("center", "scale"))
trainTransformed = predict(preProcValues, datTrain)
testTransformed = predict(preProcValues, datTest)


# dat.train <- createResample(dat$决定, times = 3, list = F)

dat.train$决定 <- as.factor(dat.train$决定)
dat.train$对方决定 <- as.factor(dat.train$对方决定)
dat.train$对方性别 <- as.factor(dat.train$对方性别)
set.seed(825)
fitControl = trainControl(method = "cv", number = 10,classProbs = TRUE,summaryFunction = twoClassSummary)
gbmGrid =  expand.grid(interaction.depth = c(1, 5, 9), n.trees = (1:20)*20, shrinkage = 0.1, n.minobsinnode = 20)
nrow(gbmGrid)
gbmFit2 = train(决定 ~ ., data = dat.train, method = "gbm", trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid,metric = "ROC",classProbs=F)
gbmFit2

##画图
trellis.par.set(caretTheme())
plot(gbmFit2)



set.seed(825)
fitControl = trainControl(method = "cv",number = 10,classProbs = TRUE,summaryFunction = twoClassSummary,search = "random")
gbmFit3 = train(决定 ~ ., data = dat.train, method = "gbm", trControl = fitControl, verbose = FALSE, metric = "ROC",tuneLength = 10)
gbmFit3



##网格搜索
data.predict = predict(gbmFit2, newdata = dat.test)
confusionMatrix(data.predict,dat.test$决定)
##随机搜索
data.predict = predict(gbmFit3, newdata = dat.test)
confusionMatrix(data.predict,dat.test$决定)


fit1 = train(决定 ~ ., data = dat.train, method = "glm",family="binomial")  #训练模型
pstate1 = predict(fit1, newdata = dat.test)  #在测试集上预测
confusionMatrix(pstate1, dat.test$决定)      #利用混淆矩阵评估模型


fit2 = train(决定 ~ ., data = dat.train, method = "rpart")  #训练模型
pstate2 = predict(fit2, newdata = dat.test) #在测试集上预测
confusionMatrix(pstate2, dat.test$决定) #利用混淆矩阵评估模型


set.seed(1234)
fit3 = train(决定 ~ ., data = dat.train, method = "rf")  #训练模型
pstate3 = predict(fit3, newdata = dat.test) #在测试集上预测
confusionMatrix(pstate3, dat.test$决定) #利用混淆矩阵评估模型


results = data.frame(pstate1, pstate2,  pstate3)
results = apply(results,2,as.character)
major_results = apply(results, 1, function(x){
  tb = sort(table(x),decreasing = T)
  if(tb[1] %in% tb[2]){
    return(sample(c(names(tb)[1],names(tb)[2]),1))
  }else{ return(names(tb)[1]) }
})
major_results = factor(major_results,levels = c("拒绝","接收"))
confusionMatrix(major_results, dat.test$决定)


set.seed(1234)
combPre = data.frame(pstate1, pstate2, pstate3,决定 = dat.test$决定)
combfit = train(决定~., method = "rf", data = combPre)
## note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
combpstate = predict(combfit, newdata = dat.test)
confusionMatrix(combpstate, dat.test$决定)


set.seed(1234)
fit4 = train(决定 ~ ., data = dat.train, method = "adaboost")  #训练模型
pstate4 = predict(fit4, newdata = dat.test) #在测试集上预测
confusionMatrix(pstate4, dat.test$决定) #利用混淆矩阵评估模型



#画图

library(jpeg) 
library(png)
data1 <- readJPEG("bear_1.jpg") # 读入jpg格式的图像
writeJPEG(data1, target = "bear_jpg_saved.jpg", quality = 1) # 存储图像
library(EBImage)
X1 <- readImage("bear_1.jpg") # 读入图像
n.size <- 300 
X1 <- resize(X1, n.size, n.size) # 修整图像尺寸，为下面的处理做准备
X11 <- X1; X11[,,1] <- 0 * X11[,,1]; X11[,,2] <- 0 * X11[,,2]; display(X11) # 提取蓝色通道的图像
library(RgoogleMaps) 
gray_image <- RGB2GRAY(data1)

# 画图展示图像分割
plot_jpeg <- function(M) # 画图函数
{
  res <- dim(M)[1:2] # get the resolution
  plot(1, 1, xlim = c(1, res[1]), ylim = c(1, res[2]), type = "n", asp = 1, xlab = '', ylab = '', bty = 'n', xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n')
  rasterImage(M, 1, 1, res[1], res[2])
  axis(1, at = (1:9) * 30, tick = T);
  axis(2, at = (1:9) * 30, tick = T);
  abline(h = (1:9) * 30, v = (1:9) * 30, col = "yellow", lty = 2)
}
plot_jpeg(gray_image)

# 将每个分割的小块像素归一
index_group <- cut(1:300, seq(0, 300, 30))
lev <- levels(index_group)
M1 <- matrix(0, nrow = length(lev), ncol = length(lev))
for (i in 1:length(lev))
{
  for (j in 1:length(lev))
  {
    M1[i, j] <- mean(gray_image[index_group == lev[i], index_group == lev[j]]) # M1 即为 10 * 10的图像
  }
} 
plot_jpeg(M1)


display(X1 + 0.5) # 熊大 + 0.5
display(X1 - 0.5) # 熊大 - 0.5
display(X1 * 2) # 熊大 * 2
display(X1 / 2) # 熊大 / 2
display(1 - X1) # 1 - 熊大
display(X1 ^ 10)# 熊大 ^ 10

library(magick) 
data1 <- image_read("bear_1.jpg") # 读入图像
image_crop(data1, "100 x 150 + 50") # 剪切图像
image_border(data1, "black", "10 x 10") # 加边框
image_rotate(data1, 45) # 旋转 45 度
image_flip(data1) # 上下翻转
image_flop(data1) # 左右翻转
image_negate(data1) # 反色
# 加标签
image_annotate(data1, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",degrees = 60, location = "+ 50 + 100") 
image_noise(data1) # 噪点
image_charcoal(data1) # 素描风格