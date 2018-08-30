##########################################################################
# 朴素贝叶斯
##########################################################################

rm(list=ls())

# 读取数据，原数据在github中https://github.com/starsfell
setwd('C:/Users/xintong.yan/Desktop')
df <- read.table('sample_data.txt',header = T)

# 上述案例
test <- list('否','否','OK','多云')


library(e1071)
# 建模
naive_bay <- naiveBayes(Result~SXR+JB+NPY+TQ, df, laplace = 0)
# 预测
test_result <- predict(naive_bay, test, type="class")



##########################################################################
# 贝叶斯网络
##########################################################################
# 朴素贝叶斯分类对于缺失值并不敏感。R语言中的e1071包中就有可以实施朴素贝叶斯分类的函数
# 但在本例我们使用klaR包中的NaiveBayes函数，因为该函数较之前者增加了两个功能，一个是可以输入先验概率，另一个是在正态分布基础上增加了核平滑密度函数[4]。

# R语言中可以使用bnlearn包来对贝叶斯网络进行建模。
# 但要注意的是，bnlearn包不能处理混合数据，所以先将连续数据进行离散化（因子型），再进行建模训练。

# bnlearn包包含贝叶斯网络的结构学习、参数学习和推理三个方面的功能，其中结构学习包含基于约束的算法、 基于得分的算法和混合算法， 参数学习包括最大似然估计和贝叶斯估计两种方法。
# 此外还有自助法（bootstrap），交叉验证（cross-validation）和随机模拟（stochastic simulation）等功能，附加的绘图功能需要调用前述的 Rgraphviz and lattice 包。

library(bnlearn)

################## 数据准备 ################## 

# 加载扩展包和数据  
library(caret)  
data(PimaIndiansDiabetes2,package='mlbench')  

# 对缺失值使用装袋方法进行插补  
preproc <- preProcess(PimaIndiansDiabetes2[-9],method="bagImpute")  
data <- predict(preproc,PimaIndiansDiabetes2[-9])  
data$Class <- PimaIndiansDiabetes2[,9]  

################# 建模 #######################
# 贝叶斯网络

library(bnlearn)  

# 数据离散化  
data2 <- discretize(data[-9],method='quantile')  
data2$class <- data[,9]  

# 使用爬山算法进行结构学习  
bayesnet <- hc(data2)  

# 显示网络图  
plot(bayesnet)  

# 修改网络图中的箭头指向  
bayesnet<- set.arc(bayesnet,'age','pregnant')  

# 参数学习  
fitted <- bn.fit(bayesnet, data2,method='mle')  

# 训练样本预测并提取混淆矩阵  
pre <- predict(fitted,data=data2,node='class')  
confusionMatrix(pre,data2$class)  

# 进行条件推理  
cpquery(fitted,(class=='pos'),(age=='(36,81]'&mass=='(34.8,67.1]'))  







