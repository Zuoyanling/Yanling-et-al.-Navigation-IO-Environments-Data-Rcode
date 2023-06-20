## =============================数据准备=======================================
setwd("E:/我的工作学习/_（正在）_我的研究/第1个实验/4数据分析/2-Rcode-合并数据-未考虑控制变量")
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
data_corr = corr.test(data[,c(2, 4:10, 12:13, 15:20)], method = "spearman")
data_corr = corr.test(data[data$agegroup == "1", c(2, 4:10, 12:13, 15:20)], 
                      use = "pairwise", method = "spearman")
data_corr = corr.test(data[data$agegroup == "0", c(2, 4:10, 12:13, 15:20)], 
                      use = "pairwise.complete.obs", method = "spearman")
#发现有变量标准差为0
sapply(data[data$agegroup == "0", c(2, 4:10, 12:13, 15:20)], sd) #找出该变量
data_corr = corr.test(data[data$agegroup == "0", c(2, 4:5, 7:10, 12:13, 15:20)], 
                      use = "pairwise", method = "spearman") #去掉该变量
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

# 描述性统计
library(pastecs)
by(data$wayfindingtime, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$wayfindingtime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$wayfindingtime)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$wayfindingtime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$wayfindingtime)
m = aov(wayfindingtime ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(wayfindingtime ~ alignment, 
           data=subset(data, agegroup == "older"), center=median) # Brown-Forsythe test
leveneTest(wayfindingtime ~ visual, 
           data=subset(data, agegroup == "older"), center=median) 
leveneTest(wayfindingtime ~ alignment:visual, 
           data=subset(data, agegroup == "older"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "older"), dv = .(wayfindingtime), 
             wid = .(participants), within = .(alignment, visual), 
             between_covariates = .(rotation, gender),
             type = 3, detailed = T)
m$ANOVA ##主要效应不显著，交互效应也不显著

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$wayfindingtime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$wayfindingtime)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$wayfindingtime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$wayfindingtime)
m = aov(wayfindingtime ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "younger")
model <- art(wayfindingtime ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ##主要效应不显著，交互效应也不显著

# 年龄差异
## 由于不满足正态假设，因此采用Mann-Whitney U test
library(coin)
wilcox_test(wayfindingtime ~ agegroup, data=data, distribution="exact") ##有显著差异

by(data$wayfindingtime, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
library(ggplot2)
ggplot(data, aes(x=alignment, y=wayfindingtime, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="wayfindingtime", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("1-wayfindingtime.png", width=10, height=5, dpi=300)

## =======================分析2：wayfindingdistance=================================

# 描述性统计
library(pastecs)
by(data$wayfindingdistance, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$wayfindingdistance)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$wayfindingdistance)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$wayfindingdistance)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$wayfindingdistance)
m = aov(wayfindingdistance ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "older")
model <- art(wayfindingdistance ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ##主要效应不显著，交互效应也不显著

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$wayfindingdistance)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$wayfindingdistance)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$wayfindingdistance)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$wayfindingdistance)
m = aov(wayfindingdistance ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "younger")
model <- art(wayfindingdistance ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ##主要效应不显著，交互效应也不显著

# 年龄差异
## 由于不满足正态假设，因此采用Mann-Whitney U test
library(coin)
wilcox_test(wayfindingdistance ~ agegroup, data=data, distribution="exact") ##有显著差异
data_subset <- subset(data, data$alignment == "45 degree" & data$agegroup == "older")
wilcox_test(wayfindingdistance ~ visual, 
            data=data_subset, 
            distribution="exact") ##有显著差异

by(data$wayfindingdistance, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
library(ggplot2)
ggplot(data, aes(x=alignment, y=wayfindingdistance, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="wayfindingdistance", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("2-wayfindingdistance.png", width=10, height=5, dpi=300)

## =======================分析3：hesitation=================================

# 描述性统计
library(pastecs)
by(data$hesitation, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$hesitation)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$hesitation)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$hesitation)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$hesitation)
m = aov(hesitation ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "older")
model <- art(hesitation ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ## 无显著差异

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$hesitation)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$hesitation)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$hesitation)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$hesitation)
m = aov(hesitation ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "younger")
model <- art(hesitation ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ## 无显著差异

# 年龄差异
## 由于不满足正态假设，因此采用Mann-Whitney U test
library(coin)
wilcox_test(hesitation ~ agegroup, data=data, distribution="exact") ##有显著差异

by(data$hesitation, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
ggplot(data, aes(x=alignment, y=hesitation, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="hesitation", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("3-hesitation.png", width=10, height=5, dpi=300)

## =======================分析4：turnwrong=================================

# 描述性统计
library(pastecs)
by(data$turnwrong, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$turnwrong)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$turnwrong)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$turnwrong)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$turnwrong)
m = aov(turnwrong ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(turnwrong ~ alignment, 
           data=subset(data, agegroup == "older"), center=median) # Brown-Forsythe test
leveneTest(turnwrong ~ visual, 
           data=subset(data, agegroup == "older"), center=median) 
leveneTest(turnwrong ~ alignment:visual, 
           data=subset(data, agegroup == "older"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "older"), dv = .(turnwrong), 
             wid = .(participants), within = .(alignment, visual), 
             between_covariates = .(rotation, gender),
             type = 3, detailed = T)
m$ANOVA ##主要效应不显著，交互效应也不显著

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$turnwrong)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$turnwrong)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$turnwrong)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$turnwrong)
m = aov(turnwrong ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "younger")
model <- art(turnwrong ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ## 无显著差异

# 年龄差异
## 由于不满足正态假设，因此采用Mann-Whitney U test
library(coin)
wilcox_test(turnwrong ~ agegroup, data=data, distribution="exact") ##有显著差异

by(data$turnwrong, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
ggplot(data, aes(x=alignment, y=turnwrong, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="turnwrong", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("4-turnwrong.png", width=10, height=5, dpi=300)

## =======================分析5：maptime=================================

# 描述性统计
library(pastecs)
by(data$maptime, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$maptime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$maptime)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$maptime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$maptime)
m = aov(maptime ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "older")
model <- art(maptime ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ## 无显著差异

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$maptime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$maptime)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$maptime)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$maptime)
m = aov(maptime ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Aligned Rank Transform分析方法
library(ARTool)
data_subset <- subset(data, agegroup == "younger")
model <- art(maptime ~ alignment * visual+ (1|participants), data = data_subset)
anova(model) ## 有显著差异

by(data$maptime, list(data$alignment, data$agegroup), 
   stat.desc, basic = FALSE)

# 年龄差异
## 由于不满足正态假设，因此采用Mann-Whitney U test
library(coin)
wilcox_test(maptime ~ agegroup, data=data, distribution="exact") ##有显著差异

by(data$maptime, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
library(ggplot2)
ggplot(data, aes(x=alignment, y=maptime, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="maptime", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("5-maptime.png", width=10, height=5, dpi=300)

## =======================分析6：mapcorrect=================================

# 描述性统计
library(pastecs)
by(data$mapcorrect, list(data$alignment, data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

# 老年人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "older",]$mapcorrect)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "older",]$mapcorrect)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "older",]$mapcorrect)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "older",]$mapcorrect)
m = aov(mapcorrect ~ alignment + visual, data=subset(data, agegroup == "older")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(mapcorrect ~ alignment, 
           data=subset(data, agegroup == "older"), center=median) # Brown-Forsythe test
leveneTest(mapcorrect ~ visual, 
           data=subset(data, agegroup == "older"), center=median) 
leveneTest(mapcorrect ~ alignment:visual, 
           data=subset(data, agegroup == "older"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "older"), dv = .(mapcorrect), 
             wid = .(participants), within = .(alignment, visual), 
             between_covariates = .(rotation, gender),
             type = 3, detailed = T)
m$ANOVA ##主要效应显著，交互效应显著

by(data$mapcorrect, list(data$alignment, data$agegroup), 
   stat.desc, basic = FALSE)
by(data$mapcorrect, list(data$visual, data$agegroup), 
   stat.desc, basic = FALSE)

### 现在针对交互效应进行分析
#### 首先绘制交互效应折线图
summarySE <- function(data=NULL, outcome, factor=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) { #定义一个计算标准误的函数
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, factor, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 outcome
  )
  
  datac <- rename(datac, c("mean" = outcome))
  
  datac$se <- datac$sd / sqrt(datac$N) 
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
#### 绘制老年人组的交互效应折线图
older_data <- subset(data, agegroup == "older")
older_data_se <- summarySE(data=older_data, outcome = "mapcorrect",
                        factor = c("alignment","visual")) # 计算每个组合条件下的均值和标准误
head(older_data_se)
library(ggplot2)
pd <- position_dodge(0.1) # move them .05 to the left and right
ggplot(older_data_se, aes(x = alignment, y = mapcorrect, colour = visual, group = visual)) + 
  geom_errorbar(aes(ymin = mapcorrect - se, ymax = mapcorrect + se), colour = "black", width = 0.2, position = pd) +
  geom_line(position = pd, size = 1, aes(linetype = visual)) +
  geom_point(position = pd, size = 3) +
  scale_color_manual(values = c("low" = "steelblue", "high" = "#FF6B6B")) +
  scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"))
ggsave("6-mapcorrect-older.png", width=5, height=4, dpi=300)
#### 进行交叉事后检验
head(older_data)
pairwise.t.test(older_data$mapcorrect, older_data$groups, paired = TRUE, p.adjust.method = "bonferroni")

# 年轻人组
## 检验正态
shapiro.test(data[data$alignment == "0 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$mapcorrect)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "low"
                  & data$agegroup == "younger",]$mapcorrect)
shapiro.test(data[data$alignment == "0 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$mapcorrect)
shapiro.test(data[data$alignment == "45 degree" & data$visual == "high"
                  & data$agegroup == "younger",]$mapcorrect)
m = aov(mapcorrect ~ alignment + visual, data=subset(data, agegroup == "younger")) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验factorial repeated measures ANOVA分析方法的其他假设
## 因为自变量都只有两个水平，因此无需进行球形检验（自变量水平≥3）
## 检验方差齐性
library(car)
leveneTest(mapcorrect ~ alignment, 
           data=subset(data, agegroup == "younger"), center=median) # Brown-Forsythe test
leveneTest(mapcorrect ~ visual, 
           data=subset(data, agegroup == "younger"), center=median) 
leveneTest(mapcorrect ~ alignment:visual, 
           data=subset(data, agegroup == "younger"), center=median)
## 由于满足方差齐性，因此采用factorial repeated measures ANOVA方法
library(ez)
m <- ezANOVA(data = subset(data, agegroup == "younger"), dv = .(mapcorrect), 
             wid = .(participants), within = .(alignment, visual), 
             between_covariates = .(rotation, gender),
             type = 3, detailed = T)
m$ANOVA ##alignment的主要效应显著，交互效应显著
### 现在针对交互效应进行分析
#### 首先绘制交互效应折线图
younger_data <- subset(data, agegroup == "younger")
younger_data_se <- summarySE(data=younger_data, outcome = "mapcorrect",
                           factor = c("alignment","visual")) # 计算每个组合条件下的均值和标准误
head(younger_data_se)
library(ggplot2)
pd <- position_dodge(0.1) # move them .05 to the left and right
ggplot(younger_data_se, aes(x = alignment, y = mapcorrect, colour = visual, group = visual)) + 
  geom_errorbar(aes(ymin = mapcorrect - se, ymax = mapcorrect + se), colour = "black", width = 0.2, position = pd) +
  geom_line(position = pd, size = 1, aes(linetype = visual)) +
  geom_point(position = pd, size = 3) +
  scale_color_manual(values = c("low" = "steelblue", "high" = "#FF6B6B")) +
  scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"))
ggsave("6-mapcorrect-younger.png", width=5, height=4, dpi=300)
#### 进行交叉事后检验
head(younger_data)
pairwise.t.test(younger_data$mapcorrect, younger_data$groups, paired = TRUE, p.adjust.method = "bonferroni")

# 年龄差异
## 由于满足正态假设，因此采用独立样本T-test检验
t.test(mapcorrect ~ agegroup, data=data, var.equal=TRUE) ##有显著差异

by(data$mapcorrect, list(data$agegroup), 
   stat.desc, basic = FALSE)

# 绘制箱型图
library(ggplot2)
ggplot(data, aes(x=alignment, y=mapcorrect, fill=agegroup)) +
  geom_boxplot(alpha=0.5, position=position_dodge(width=0.85)) +
  facet_wrap(~visual) +
  labs(x="alignment", y="mapcorrect", fill="age group") +
  scale_fill_manual(values=c("steelblue", "#FF6B6B"), 
                    guide_legend(title="age group", override.aes=list(alpha=0.3)))
ggsave("6-mapcorrect.png", width=10, height=5, dpi=300)

## =======================分析7：rotation的中介效应=================================

# 1. process方法

## 学习资源
### [在RStudio应用process for R检验简单的中介效应_哔哩哔哩_bilibili]
### (https://www.bilibili.com/video/BV1P34y1a7hR/?vd_source=d79a42e048af63c723cf71703d2df174)

## 激活process for R
source("C:/Users/So/Documents/R/win-library/4.1/PROCESS v4.3 for R/process.R")

## 用模型4执行统计分析
process(data = data, y = "mapcorrect", x = "age", m = "rotation", model = 4,
        effsize = 1, total = 1, stand = 1, modelbt = 1, boot = 10000, seed = 1234)
### 中介效应不显著，置信区间未穿过0

# 2. 三步回归方法

## 学习资源
### [在Rstudio执行三步回归检验简单的中介效应_哔哩哔哩_bilibili]
### (https://www.bilibili.com/video/BV1554y1Z79s/?spm_id_from=333.999.0.0&vd_source=d79a42e048af63c723cf71703d2df174)

med.lm = lm(mapcorrect ~ age, data = data)
summary(med.lm)
med.lm2 = lm(rotation ~ age, data = data)
summary(med.lm2)
med.lm3 = lm(mapcorrect ~ age + rotation, data = data)
summary(med.lm3)
### 中介效应不显著

## =======================分析8：gender的调节效应=================================

## 用process模型1执行统计分析
process(data = data, y = "mapcorrect", x = "alignment", w = "gender", model = 1,
        center = 2, moments = 1, jn = 1, covcoeff = 1, modelbt = 1, 
        boot = 10000, seed = 1234, plot = 1)
### 调节效应不显著





























