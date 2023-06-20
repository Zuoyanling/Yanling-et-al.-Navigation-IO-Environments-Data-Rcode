## =============================数据准备=======================================
setwd("E:/我的工作学习/_（正在）_我的研究/第1个实验/4数据分析/3-Rcode-合并数据-考虑控制变量")
# 打开数据
library(openxlsx)
data = read.xlsx("合并的实验数据.xlsx", "data")
# 查看数据
head(data)
str(data)
View(data)
names(data)

## ============================相关性分析======================================

# 描述变量之间的皮尔逊相关系数及相应p值（不包含人口特征变量）
library(psych)
data_corr = corr.test(data[,c(2, 4:10, 12:13, 15:20)])
data_corr = corr.test(data[data$agegroup == "1", c(2, 4:10, 12:13, 15:20)], 
                      use = "pairwise", method = "pearson")
data_corr = corr.test(data[data$agegroup == "0", c(2, 4:10, 12:13, 15:20)], 
                      use = "pairwise.complete.obs", method = "pearson")
#发现有变量标准差为0
sapply(data[data$agegroup == "0", c(2, 4:10, 12:13, 15:20)], sd) #找出该变量
data_corr = corr.test(data[data$agegroup == "0", c(2, 4:5, 7:10, 12:13, 15:20)], 
                      use = "pairwise", method = "pearson") #去掉该变量
# 输出相关系数矩阵的下三角部分，保留两位小数
lowerMat(data_corr$r, digits = 2)
# 输出相关系数显著性检验的p值
lowerMat(data_corr$p, digits = 4)

## ==========================全组内分析准备=====================================

data$gender <-factor(data$gender,labels = c("male","female"))
data$agegroup <-factor(data$agegroup,labels = c("younger","older"))
data$education <-factor(data$education,labels = c("小学及以下","初中","高中","本科或专科"))
data$disease <-factor(data$disease,labels = c("未患病","冠心病","其他"))
data$strategy <-factor(data$strategy,labels = c("egocentric","allocentric","both"))
data$alignment <-factor(data$alignment,labels = c("0 degree","45 degree"))
data$visual <-factor(data$visual,labels = c("low","high"))
data$grouporder = factor(data$grouporder)
data$groups = factor(data$groups)
data$participants = factor(data$participants)

## ==========================检验顺序效应=====================================

## 检验顺序效应
library(car)
model <- manova(cbind(wayfindingtime, wayfindingdistance, hesitation) 
                ~ alignment*visual 
                + Error(grouporder/(alignment*visual)), 
                data=subset(data, agegroup == "older"))
summary(model) #p值不显著，表明不存在顺序效应
model <- manova(cbind(turnwrong, maptime,mapcorrect) 
                ~ alignment*visual 
                + Error(grouporder/(alignment*visual)), 
                data=subset(data, agegroup == "older"))
summary(model) #p值不显著，表明不存在顺序效应

## =======================分析1：wayfindingtime=================================

# 老年人组
## 控制rotation之后的wayfindingtime
### 训练线性回归模型
model <- lm(mapcorrect ~ rotation, data = subset(data, agegroup == "older"))
### 获取残差
residuals <- residuals(model)
### 将残差存储到数据框中的新列
data$residuals <- residuals
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$residuals)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$residuals)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$residuals)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$residuals)
m = aov(residuals ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(residuals ~ alignment, 
           data=subset(data, agegroup == "older"), center=median) # Brown-Forsythe test
leveneTest(residuals ~ visual, 
           data=subset(data, agegroup == "older"), center=median) 
leveneTest(residuals ~ alignment:visual, 
           data=subset(data, agegroup == "older"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "older"), dv = .(residuals), 
             wid = .(participants), within = .(alignment, visual), 
             type = 3, detailed = T)
m$ANOVA ##主要效应不显著，交互效应也不显著

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$residuals)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$residuals)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$residuals)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$residuals)
m = aov(residuals ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(residuals ~ alignment, 
           data=subset(data, agegroup == "younger"), center=median) # Brown-Forsythe test
leveneTest(residuals ~ visual, 
           data=subset(data, agegroup == "younger"), center=median) 
leveneTest(residuals ~ alignment:visual, 
           data=subset(data, agegroup == "younger"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "younger"), dv = .(residuals), 
             wid = .(participants), within = .(alignment, visual), 
             type = 3, detailed = T)
m$ANOVA ##主要效应不显著，交互效应也不显著

# 年龄差异
## 由于满足正态假设，因此采用独立样本T-test检验
t.test(residuals ~ agegroup, data=data, var.equal=TRUE) ##有显著差异

# 绘制箱型图
library(ggplot2)
ggplot(data, aes(x=alignment, y=residuals, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="wayfindingtime", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("1-wayfindingtime.png", width=10, height=5, dpi=300)
