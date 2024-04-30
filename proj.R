library(haven)
#选取变量
data<-read_dta("C://data//cda//CEPS基线调查学生数据.dta")
data2<-read_dta("C://data//cda//CEPS基线调查家长数据.dta")
#被解释变量
Ex<-cbind.data.frame(data$cog3pl,data$a1801,data$a1802,
                     data$a1803,data$a1804,data$a1805,data$a1201,
                     data$a1202,data$a1203,data$c25)
colnames(Ex)<-c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
Ex$X1<- (Ex$V2+Ex$V3+Ex$V4+Ex$V5+Ex$V6)
Ex$X1<-scale(Ex$X1)
#Ex$X1<-cut(Ex$X1,breaks=c(5,10,15,20,25),include.lowest=TRUE,right=FALSE,labels=c(1,2,3,4))
Ex$X2<- (Ex$V7+Ex$V8+Ex$V9)
Ex$X2<-scale(Ex$X2)
#Ex$X2<-cut(Ex$X2,breaks=c(3,6,9,12,15),include.lowest=TRUE,right=FALSE,labels=c(1,2,3,4))
Ex$X1<-as.numeric(as.character(Ex$X1))
Ex$X2<-as.numeric(as.character(Ex$X2))
Ex$V1<-scale(Ex$V1)
Ex$V10<-scale(Ex$V10)
Ex$X3<-Ex$V1+Ex$V10+Ex$X1+Ex$X2
Ex$X3<-scale(Ex$X3)

Ex$X3<-cut(Ex$X3,breaks=c(-4.7,-2.5,-0.3,1.9,4),include.lowest=TRUE,right=FALSE,labels=c(1,2,3,4))
summary(Ex$X3)
#解释变量
Stu<-cbind.data.frame(data$ids,data$grade9,data$a01,data$a17,data$b01,
                      data$b06,data$b07,data$b14a1,data$b14a2,data$b2000,
                      data$c1307,data$c1706,data$b2501,data$b2502)
Par<-cbind.data.frame(data2$ba0801,data2$ba09,data2$bd03,data2$be19)
Time<-data$b14a1+data$b14a2/60
data0<-cbind(Stu,Par,Time,Ex$X3);colnames(data0)<-c("ids","V1","V2","V3","V4","V5","V6",
                                                    "V7","V8","V9","V10","V11","V12","V13",
                                                    "V14","V15","V16","V17","V18","V19")
#清除缺失值
data0<-na.omit(data0)
summary(data0$V19)

#描述性统计
#饼图
library(RColorBrewer)
freq<-table(data0$V19)
win.graph(height=15,width=18)
per <- paste(round(100 * freq / sum(freq),2),"%");color <- colorRampPalette(brewer.pal(4,"Blues"))(4)
names<-c('较差','一般','较好','很好')
pie(freq,labels=per,col= color)
legend("topright",names,cex=0.7,fill=color)

#柱状图
#年级
freq<-xtabs(~V1+V19,data = data0)
prop<-prop.table(xtabs(~V1+V19,data = data0),margin = 1)
win.graph(height=5,width=6)
barplot(height = prop,names.arg = c('较差','一般','较好','很好'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '心理健康水平',  # X轴名称
        ylab = '人数占比',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 0.7), # Y轴取值范围
        legend.text = c('七年级','九年级'),  # 图例文本
        beside = TRUE  # 是否平行排列
)

#性别
freq<-xtabs(~V2+V19,data = data0)
prop<-prop.table(xtabs(~V2+V19,data = data0),margin = 1)
win.graph(height=5,width=6)
barplot(height = prop,names.arg = c('较差','一般','较好','很好'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '心理健康水平',  # X轴名称
        ylab = '人数占比',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 0.7), # Y轴取值范围
        legend.text = c('男','女'),  # 图例文本
        beside = TRUE  # 是否平行排列
)

#户口
freq<-xtabs(~V16+V19,data = data0)
prop<-prop.table(xtabs(~V16+V19,data = data0),margin = 1)
win.graph(height=5,width=6)
barplot(height = prop,names.arg = c('较差','一般','较好','很好'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '心理健康水平',  # X轴名称
        ylab = '人数占比',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 0.7), # Y轴取值范围
        legend.text = c('本地','外地'),  # 图例文本
        beside = TRUE  # 是否平行排列
)
#独生子女
freq<-xtabs(~V4+V19,data = data0)
prop<-prop.table(xtabs(~V4+V19,data = data0),margin = 1)
win.graph(height=5,width=6)
barplot(height = prop,names.arg = c('较差','一般','较好','很好'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '心理健康水平',  # X轴名称
        ylab = '人数占比',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 0.7), # Y轴取值范围
        legend.text = c('独生','非独生'),  # 图例文本
        beside = TRUE  # 是否平行排列
)
#父母受教育程度
table(data0$V5);table(data0$V6)
freq<-cbind(table(data0$V5),table(data0$V6))
win.graph(height=5,width=6)
barplot(height = freq,  # 柱子名称
        col = rainbow(9),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab='(a)母亲受教育程度                          (b)父亲受教育程度',
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 7000), # Y轴取值范围
        legend.text = c('没受过任何教育','小学','初中','中专/技校','职业高中','普通高中','大学专科','大学本科','研究生及以上'),  # 图例文本
        args.legend = list(x = "topright", cex=0.7),
        beside = TRUE  # 是否平行排列
)
#亲子关系
table(data0$V12);table(data0$V13)
freq<-cbind(table(data0$V12),table(data0$V13))
win.graph(height=5,width=6)
barplot(height = freq,  # 柱子名称
        col = rainbow(3),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab=' (a)和母亲关系                           (b)和父亲关系',
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 12000), # Y轴取值范围
        legend.text = c('不亲近','一般','很亲近'),  # 图例文本
        args.legend = list(x = "topleft", cex=0.7),
        beside = TRUE  # 是否平行排列
)
#学业管教
freq<-table(data0$V14)
color <- colorRampPalette(brewer.pal(3,"Blues"))(3)
win.graph(height=5,width=6)
barplot(height = freq,  # 柱子名称
        col = color,  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab='(a)学业管教',
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 12000), # Y轴取值范围
        legend.text = c('不管','管，但不严','很严'),  # 图例文本
        args.legend = list(x = "topleft", cex=0.7),
        beside = TRUE  # 是否平行排列
)
freq<-table(data0$V15)
color <- colorRampPalette(brewer.pal(6,"Blues"))(6)
win.graph(height=5,width=6)
barplot(height = freq,  # 柱子名称
        col = color,  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab='(b)意见不一致时解决办法',
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 10000), # Y轴取值范围
        legend.text = c('顺着孩子','说服孩子','强迫孩子接受','民主讨论','不了了之','其他'),  # 图例文本
        args.legend = list(x = "topleft", cex=0.7),
        beside = TRUE  # 是否平行排列
)
#家庭经济条件
freq<-table(data0$V17)
win.graph(height=7,width=7)
per <- paste(round(100 * freq / sum(freq),2),"%");color<-color<- colorRampPalette(brewer.pal(5,"Blues"))(5)
names<-c('非常困难','比较困难','中等','比较富裕','富裕')
pie(freq,labels=per,col= color)
legend("topright",names,cex=0.7,fill=color)
#学习压力
Time<-data0$V7+data0$V8/60
level <- ifelse(
 Time<=1, "0~1小时",ifelse(
  Time<=2, "1~2小时",ifelse(
   Time<=3, "2~3小时",ifelse(
    Time<=4, "3~4小时","4小时以上"
   )
  )
 )
)

freq<-table(level)
win.graph(height=5,width=5)
barplot(height = freq,  # 柱子名称
        col = 'steelblue',  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab='每天做作业时长',
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 5000), # Y轴取值范围
        
)
#师生关系
freq<-table(data0$V10)
win.graph(height=5,width=4)
barplot(height = freq,  # 柱子名称
        col = 'steelblue',  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '(a)师生关系',  # X轴名称
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 7000), # Y轴取值范围
        
)
#同伴关系
freq<-table(data0$V11)
barplot(height = freq,  # 柱子名称
        col = 'steelblue',  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        xlab = '(b)同伴关系',  # X轴名称
        ylab = '人数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        ylim = c(0, 7500), # Y轴取值范围
        
)
#列联表分析
library(cdabookfunc)
library(cdabookdb)
#性别差异
table(data0$V2,data0$V19)
freq<-xtabs(~V2+V19,data = data0)
chisq.test(freq) #X2
#年级差异
table(data0$V1,data0$V19)
freq<-xtabs(~V1+V19,data = data0)
independent_test_of_table(freq,"all",0:1,1:4)
#独生差异
table(data0$V4,data0$V19)
freq<-xtabs(~V4+V19,data = data0)
independent_test_of_table(freq,"all",0:1,1:4)
#户口差异
table(data0$V16,data0$V19)
freq<-xtabs(~V16+V19,data = data0)
independent_test_of_table(freq,"all",0:1,1:4)

#逻辑回归
library(VGAM)
mydata<-data0[,-1]
mydata<-mydata[,-7]
mydata<-mydata[,-7]
mydata<-mydata[,-14]
colnames(mydata)<-c('年级','性别','健康状态','独生子女','母亲教育水平','父亲教育水平',
                    '兴趣爱好','师生关系','同伴关系','亲子关系-母亲','亲子关系-父亲',
                    '学业管教','矛盾解决方式','家庭经济','学习压力','心理健康')
#mydata$'心理健康'<-factor(mydata$'心理健康')#数据处理
sub<-sample(1:15160,12160)#12160个样例作为训练集，其余作为测试集

train<-mydata[sub,]
test<-mydata[-sub,]

m<-vglm(心理健康~.,
            family=cumulative(parallel=TRUE),data=train)
summary(m)
library(MASS) 
model <- polr(心理健康~.,data = train,
                  Hess = TRUE, model = TRUE,
                  method = "logistic") 
summary(model)

#模型检验
fit01a<-polr(factor(V19)~1,data=data0)
fit01b<-polr(factor(V19)~ V1+V2+V3+V4+V5+V6+V9+V10+V11+V12+V13+V14+V15+V17+V18,data=data0)
anova(fit01a,fit01b)
#对测试集进行预测
pre_ran <- predict(model,newdata=test)
#将真实值和预测值整合到一起
obs_p_ran = data.frame(prob=pre_ran,obs=test$心理健康)
#输出混淆矩阵
table(test$心理健康,pre_ran,dnn=c("真实值","预测值"))



#XGboost回归
library(xgboost)
df <- data0[,-c(1,8,9,15,20)]
colnames(df)<-c('年级','性别','健康状态','独生子女','母亲教育水平','父亲教育水平',
                '兴趣爱好','师生关系','同伴关系','亲子关系-母亲','亲子关系-父亲',
                '学业管教','矛盾解决方式','家庭经济','学习压力')
y <- data0$V19
dtrain <- xgb.DMatrix(data = as.matrix(df),label=y) #构造数据
#设置参数
paras <- list(eta=0.5,
              max_depth=2,
              nthread=2,
              eval_metric="auc")
#模型拟合              
xgb <- xgb.train(dtrain,params=paras,nrounds = 10)
importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix = importance_matrix)

imtable<-head(importance_matrix,12)
imtable<-imtable[,1:2]
imtable<-data.frame(imtable)
imtable<-imtable[order(imtable$Gain),]
win.graph(height=18,width=8)
barplot(imtable$Gain,names.arg=imtable$Feature,
        horiz = TRUE,
        xlab="重要性",
        ylab="因素",
        xlim=c(0,0.35),
        col="#6666ff",
        border = '#ffffff',
        cex.axis =0.8,cex.lab = 0.8,cex.names=1)
