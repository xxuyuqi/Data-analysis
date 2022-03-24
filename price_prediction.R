setwd("C:/Users/uiaiu/Desktop/R")
library(ggplot2)
data <- read.csv("project_residential_price_data.csv")
# print(ncol((data)))
# print(nrow(data))
# print("'''''最大值，最小值，均值，四分位数'''''")
# print(summary(data))
print("'''''数量，平均值，标准差，中值，截尾均值，绝对中位差，最大值，最小值，全距，偏度，峰度，标准误差'''''")
print(psych::describe(data))

plot(density(data$V.9),col='blue',lty=2) # 价格分布核密度函数
ggplot(data=data,aes(x=V.9,fill=as.factor(V.1)))+geom_density(alpha=.3) # 查看两地住房价格的分布
hist(data$V.9) # 查看价格分布的频率图
pairs(data[,c(2:8)]) # 变量间的散点图
plot(data$V.2, data$V.9) # 住房面积和价格
# cor(data[,c[2:8]]) # 变量之间的相关系数


price=lm(V.9~V.1+V.2+V.3+V.4+V.5+V.6+V.7+V.8, data=data) #房价与V.1,v.2,v.3,v.4,v.5,v.6,v.7,v.8的关系
summary(price)
# 假设总体房价均值H0:u=1390
t.test(data$V.9,alternative = 'two.sided',mu=1390)
shapiro.test(scale(data$V.9)) # 验证房子的单价是否呈正态分布 

cor(data) # 相关性分析，v.5，v.8，v.15，v.25，v.26，v.10相关性最高
md = data[,c(5,8,13,23,24,28,29)]
summary(md)
attach(md)
reg = lm(md$V.9~md$V.5+md$V.8+md$V.15+md$V.25+md$V.26+md$V.10)
summary(reg)
library(rpart) # 使用决策树预测
n=0.7*nrow(md) #作为训练集的样本量
sub_train=sample(nrow(md),n)#随机抽取n个样本
data_train=md[sub_train,-1]#训练集
data_test=md[-sub_train,-1]#测试集
fit=rpart(V.9~.,data_train,method="class",maxdepth=2,minsplit=10)#建立决策树
summary(fit)
print(fit)
plot(fit)
text(fit, use.n = T)
#对测试集预测目标变量
pre=predict(fit,data_test,type="class")
#计算错误率
error_rp=sum(pre!=data_test$V.9)/nrow(data_test)

