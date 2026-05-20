#设置工作目录
setwd("~/Desktop/学术/噬菌体防治水稻白叶枯病害/噬菌体和水稻百叶库病害/1a实验设计/毕业论文/毕业论文/温州宁波台州水稻产区的宏基因组学研究/文章思路+结果/宏基因组+多样性（真菌细菌）结果/多样性/细菌结果/alpha多样性")
#导入数据
df<-read.table("bacalpha.txt",sep="\t",header = T)
df
head(df)
#加载ggplot2
library(ggplot2)
####################################################################
####  load global packages    ####
library("tidyr")
library("Rmisc")
library("Hmisc")
library("ggthemes")
library("ggsci")
library("ggplot2")
library("vegan")

theme_ykm<- function(..., bg='white'){
     theme_few() +
          theme(aspect.ratio =1,
                plot.title = element_text(hjust = 0.5,size = 10,face = "bold"),
                axis.text.x=element_text(size=8,color="black"),
                axis.text.y=element_text(size=8,color="black"),
                axis.title.y=element_text(colour="black",size=10),
                axis.title.x=element_text(colour="black",size=10),
                legend.text=element_text(size=8))
     
}

dataframe2matrix  <- function(input){
     dataframe = input
     rownames(dataframe) = dataframe[,1]
     dataframe = dataframe[,-1]
     return(dataframe)
}
############################################################################################
##chao1指数

#导入数据
df<-read.table("Pantoea.txt",sep="\t",header = T)
df
head(df)
#df1<-df[,c(1,8,10,11)]

#以 group 指数为例，同时将分组列转换为因子变量
#df<-df1[,c('Sample','Evenness',"group1","group2")]
head(df)

#df$group<-factor(df$group)
#alpha多样性绘图
library(ggplot2)
library(patchwork)
mycolors1<-c('#55aeda','#fa6464')
mycolors1<-c('#84D2E5','#F5958A')
#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
#mycolors1<-c("#0055AA","#C40003","#4AA329","#894FC6","#00C19B","#3CB371")

color=colorRampPalette(c("#55aeda","white","#fa6464"))(100))


color=colorRampPalette(c("#84D2E5","white","#F5958A"))(100))

mycolors1<-c('#55aeda','#fa6464','#FDDB88')

#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
#mycolors1<-c("#0055AA","#C40003","#4AA329","#894FC6","#00C19B","#3CB371")
df<-read.table("Stenotrophomonas.txt",sep="\t",header = T)
df
head(df)
#scale_color_manual(values=c("day-7" = "#0055AA","day-14" = "#C40003","day-21" = "#894FC6","day-28" = "#00C19B"))
#ggsave("finalchao1.pdf",width = 10,height=9)
#df$group3 <- factor(df$group3,levels = c("day-7","day-14","day-21","day-28"))
p<-ggplot(df,aes(x=Group,y=Num,fill=Group),outline=F)+
     geom_boxplot(alpha=0.7,size=0.7,width=0.6,outlier.colour = NA)+
     #geom_point(alpha=0.6,size=4.5, aes(fill=group))+
     #scale_color_manual(values = c("SF" = "#0055AA","CK" = "#C40003"))+
     scale_fill_manual(values=mycolors1)+
     labs(x = '', y = 'Relative Abundance(%)') +
        theme(axis.text=element_text(colour='black',size=9))+
        theme_base()+theme(legend.title=element_text(size=1) , legend.text=element_text(size=5))+
        theme(text=element_text(size=10),axis.text.x = element_text(size = 20,face="bold",hjust = 1, colour = "black"),axis.text.y = element_text(size = 20,face="bold",colour = "black"))+
        theme(legend.title=element_text(size=0,face="bold") , legend.text=element_text(size=12,face="bold"))+
        theme(axis.title.x = element_text(size=22,face="bold"),axis.title.y = element_text(size = 22,face="bold"))+
        theme(legend.position = "none")#+
       # theme(legend.position = c(0.85, 0.92))

p

ggsave('窄Stenotrophomonas.pdf',p,width = 8.1 ,height =13,unit='cm')







# 定义自定义颜色向量
mycolors <- c("#55aeda", "#FDDB88")
"#55aeda", "white","#FDDB88"
# 定义一个颜色向量，用于后续图形中分组的颜色映射

# 绘图：利用 stat_poly_eq 自动添加回归方程及 R²、p值注释
p <- ggplot(data, aes(x = index, y = diseaseindex, color = group1)) +
        # 初始化ggplot图形，指定 x 轴、y 轴和分组变量 Group 用于颜色映射
        
        geom_point(aes(color = group1), alpha = 0.92, size = 7.5) +
        geom_point(
                shape = 21,  
                # fill = "#fa6464",
                size=6.5,# 圆形散点（shape 21 是一个可填充颜色且有边框的圆形）
                color = "black",   # 边框颜色设为黑色
                stroke = 0.7,      # 边框线的粗细
                alpha = 1        # 点的透明度（0 = 完全透明，1 = 不透明）
        ) +
        labs(title = "", x = "Relative abundance", y = "Disease index") +
        theme_base() + 
        geom_smooth(method=lm,level=0.65,size=0.6,linetype="dashed",aes(fill = group1))+
        #stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', 
        # label.x.npc = 'left', label.y.npc = 'top', size = 15.5, show.legend = FALSE)+
        scale_color_manual(values = mycolors) +
        scale_fill_manual(values = c("#55aeda", "#FDDB88")) +
        # stat_poly_eq(formula = y ~ x,
        # aes(label = paste(..eq.label.., ..rr.label..,
        #  ..p.value.label.., sep = "~~~")),
        #    parse = TRUE,
        #   label.x = "left",
        # label.y = "top",
        #  size = 6) +
        
        theme(axis.text=element_text(colour='black',size=9))+
        theme_base()+theme(legend.title=element_text(size=1) , legend.text=element_text(size=5))+
        theme(text=element_text(size=10),axis.text.x = element_text(size = 20,face="bold",hjust = 1, colour = "black"),axis.text.y = element_text(size = 20,face="bold",colour = "black"))+
        theme(legend.title=element_text(size=0,face="bold") , legend.text=element_text(size=12,face="bold"))+
        theme(axis.title.x = element_text(size=22,face="bold"),axis.title.y = element_text(size = 22,face="bold"))+
        theme(legend.position = "none")+
        theme(legend.position = c(0.85, 0.92))

p
ggsave('不带任何标志最终相关性图.pdf', p, width = 16, height = 16,units = "cm")

#group2$group2 <- factor(group2$group2)
#levels(group2$group2)
#names(ann_colors$group2)
ann_colors = list(group2 = c(Stenotrophomonas = "#0d898a", 
                             Pantoea = "#ADDB88",
                             Agrobacterium = "#f9cc52",
                             Enterobacter = "#894FC6"))

color=colorRampPalette(c("#55aeda","white","#FDDB88"))(100))

color=colorRampPalette(c("#9AB9DA","white","#FDDB88"))(100))


















library(ggpubr)
df<-read.table("YJ.txt",sep="\t",header = T)
df
head(df)
ggboxplot(data = df, x = 'group1', y = 'Chao1', color = 'group1') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('Disease', 'Healthy')))

ggboxplot(data = df, x = 'group1', y = 'Shannon', color = 'group1') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('Disease', 'Healthy')))

ggboxplot(data = df, x = 'group1', y = 'Richness', color = 'group1') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('Disease', 'Healthy')))

ggboxplot(data = df, x = 'group1', y = 'Evenness', color = 'group1') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('Disease', 'Healthy')))
#scale_color_manual(values = c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B',"#3399CC","#CCCCFF"))
#ggplot(df,aes(x=group2,y=Chao1,fill=group1),outline=F)+




df<-read.table("YJ真菌细菌alpha.txt",sep="\t",header = T)
df
head(df)

#以 group 指数为例，同时将分组列转换为因子变量
df<-df1[,c('Sample','Evenness',"group1","group2")]
head(df)

#df$group<-factor(df$group)
#alpha多样性绘图
library(ggplot2)
library(patchwork)

mycolors1<-c('#0055AA','#C40003')

#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
mycolors1<-c("#0055AA","#C40003","#4AA329","#894FC6","#00C19B","#3CB371")

#scale_color_manual(values=c("day-7" = "#0055AA","day-14" = "#C40003","day-21" = "#894FC6","day-28" = "#00C19B"))
#ggsave("finalchao1.pdf",width = 10,height=9)
#df$group3 <- factor(df$group3,levels = c("day-7","day-14","day-21","day-28"))
p<-ggplot(df,aes(x=group1,y=Chao1,fill=group2),outline=F)+
     geom_boxplot(alpha=0.7,size=0.7,width=0.7,outlier.colour = NA)+
     #geom_point(alpha=0.6,size=4.5,mapping = aes(x=group1,y=Chao1,fill=group2))+
     #scale_color_manual(values = c("SF" = "#0055AA","CK" = "#C40003"))+
     scale_fill_manual(values=mycolors1)+
     labs(title= "",x="",y="Chao1 Index",size=19)+
     theme_base()+theme(legend.title=element_text(size=10) , legend.text=element_text(size=10))+
     theme(text=element_text(size=35),axis.text.x = element_text(size = 22,face="bold",colour = "black"),
           axis.text.y = element_text(size = 20,face="bold",colour = "black"))+
     theme(legend.title=element_text(size=24,face="bold") , legend.text=element_text(size=23,face="bold"))+
     theme(axis.title.x = element_text(size=2,face="bold"),axis.title.y = element_text(size = 22,face="bold"))
p

ggsave('YJ真菌细菌alpha.pdf',p,width = 14 ,height =14,unit='cm')


library(ggpubr)
df<-read.table("yejixijun.txt",sep="\t",header = T)
df
head(df)
ggboxplot(data = df, x = 'group2', y = 'Chao1', color = 'group2') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('SF', 'CK')))

# geom_boxplot(alpha=0.4,size=0.7,width=0.8,outlier.colour = NA,aes(color = group1))+
# geom_jitter(alpha=0.6,size=4.5,aes(colour = group2))+theme_ykm()+scale_color_manual(values = c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B',"#3399CC","#CCCCFF"))+
# labs(title= "leaf Richness index",x="Treatment",y="Richness Index")+
# theme_ykm()+scale_fill_manual(values=mycolors1)+theme(legend.title=element_text(size=10) , legend.text=element_text(size=10))+
#theme(text=element_text(size=30),axis.text.x = element_text(size = 26, angle = 45,face="bold",hjust = 1,colour = "black"),axis.text.y = element_text(size = 16,face="bold",colour = "black"))+
#theme(legend.title=element_text(size=24,face="bold") , legend.text=element_text(size=23,face="bold"))+
#theme(axis.title.x = element_text(size=24,face="bold"),axis.title.y = element_text(size = 24,face="bold"))
#+
annotate("text",x=1,y=0.97,label="b",size=10,color="#0055AA")+
     annotate("text",x=2,y=1.25,label="a",size=10,color="#C40003")+
     annotate("text",x=3,y=1.35,label="a",size=10,color="#4AA329")+
     annotate("text",x=4,y=1.32,label="a",size=10,color="#894FC6")

p
ggsave('Richnessindex.pdf',p,width = 30 ,height =24,unit='cm')
############################################################################33
不按照健康和发病组进行区分

df1<-height[,c(1,2,9,10,11)]
df1
head(df1)
#以 group 指数为例，同时将分组列转换为因子变量
df<-df1[,c('Sample','Chao1','treat',"group2","group3")]
head(df)

df$treat<-factor(df$treat)
#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
mycolors<-c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6')
#mycolors<-c("#0055AA","#C40003","#894FC6","#00C19B")
#mycolors<-c("Red","Navy","Yellow","Blue")
#df$Group1<-factor(df$Group1,levels = c("CK2","CuO2","ZnO2","FeO2","CK4","CuO4","ZnO4","FeO4"),ordered=TRUE)
#df$group2 <- factor(df$group2,levels = c("CK","CuO","ZnO","FeO"))
df$treat<- factor(df$treat,levels = c("CK_2","CK_3","CK_4","CK_5","C2_2","C2_3","C2_4","C2_5"))
##########

p1 <- ggplot(df,aes(x=treat,y=Chao1,fill=treat),outline=F)+
     geom_boxplot(alpha=0.6,size=0.7,width=0.8,outlier.colour = NA,aes(color = treat))+
     geom_point(alpha=0.5,size=9.5,aes(colour = treat))+#+facet_wrap(~date)
     labs(title= "leaf Chao1 index",x="",y="Chao1 Index",size=30)+
     scale_color_manual(values = c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6'))+
     theme_ykm()+scale_fill_manual(values=mycolors)+theme(legend.title=element_text(size=6) , legend.text=element_text(size=5))+
     theme(text=element_text(size=30),axis.text.x = element_text(size = 25, face="bold",hjust = 1,angle = 45,colour = "black"),
           axis.text.y = element_text(size = 25,face="bold",colour = "black"))+
     theme(legend.title=element_text(size=20,face="bold") , legend.text=element_text(size=20,face="bold"))+
     theme(axis.title.x = element_text(size=20,face="bold"),axis.title.y = element_text(size = 20,face="bold"))+
     annotate("text",x=1,y=9830,label="a",size=10,color="#0055AA")+
     annotate("text",x=2,y=9850,label="c",size=10,color="#C40003")+
     annotate("text",x=3,y=8300,label="d",size=10,color="#4AA329")+
     annotate("text",x=4,y=8550,label="bc",size=10,color="#894FC6")+
     annotate("text",x=5,y=8200,label="c",size=10,color="#00C19B")+
     annotate("text",x=6,y=7300,label="d",size=10,color="#7FD2FF")+
     annotate("text",x=7,y=9600,label="bc",size=10,color="#FF9D1E")+
     annotate("text",x=8,y=8760,label="b",size=10,color="#CAB2D6")

p1
ggsave('chao1index.pdf',p1,width = 40 ,height =30,unit='cm')
############################################################################33
#Shannon指数

df1<-height[,c(1,7,9,10,11)]
df1
head(df1)
#以 group 指数为例，同时将分组列转换为因子变量
df<-df1[,c('Sample','Shannon','treat',"group2","group3")]
head(df)

df$treat<-factor(df$treat)
#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
mycolors<-c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6')
#mycolors<-c("#0055AA","#C40003","#894FC6","#00C19B")
#mycolors<-c("Red","Navy","Yellow","Blue")
#df$Group1<-factor(df$Group1,levels = c("CK2","CuO2","ZnO2","FeO2","CK4","CuO4","ZnO4","FeO4"),ordered=TRUE)
#df$group2 <- factor(df$group2,levels = c("CK","CuO","ZnO","FeO"))
df$treat<- factor(df$treat,levels = c("CK_2","CK_3","CK_4","CK_5","C2_2","C2_3","C2_4","C2_5"))

#scale_color_manual(values=c("day-7" = "#0055AA","day-14" = "#C40003","day-21" = "#894FC6","day-28" = "#00C19B"))
#ggsave("finalchao1.pdf",width = 10,height=9)
p1 <- ggplot(df,aes(x=treat,y=Shannon,fill=treat),outline=F)+
     geom_boxplot(alpha=0.6,size=0.7,width=0.8,outlier.colour = NA,aes(color = treat))+
     geom_point(alpha=0.5,size=9.5,aes(colour = treat))+#+facet_wrap(~date)
     labs(title= "leaf Shannon index",x="",y="Shannon Index",size=30)+
     scale_color_manual(values = c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6'))+
     theme_ykm()+scale_fill_manual(values=mycolors)+theme(legend.title=element_text(size=6) , legend.text=element_text(size=5))+
     theme(text=element_text(size=30),axis.text.x = element_text(size = 25, face="bold",hjust = 1,angle = 45,colour = "black"),
           axis.text.y = element_text(size = 25,face="bold",colour = "black"))+
     theme(legend.title=element_text(size=20,face="bold") , legend.text=element_text(size=20,face="bold"))+
     theme(axis.title.x = element_text(size=20,face="bold"),axis.title.y = element_text(size = 20,face="bold"))+
     annotate("text",x=1,y=11.61,label="a",size=10,color="#0055AA")+
     annotate("text",x=2,y=11.72,label="a",size=10,color="#C40003")+
     annotate("text",x=3,y=11.42,label="bc",size=10,color="#4AA329")+
     annotate("text",x=4,y=11.485,label="ab",size=10,color="#894FC6")+
     annotate("text",x=5,y=11.01,label="c",size=10,color="#00C19B")+
     annotate("text",x=6,y=11.16,label="b",size=10,color="#7FD2FF")+
     annotate("text",x=7,y=11.794,label="a",size=10,color="#FF9D1E")+
     annotate("text",x=8,y=11.64,label="a",size=10,color="#CAB2D6")
p1
ggsave('Shannonindex.pdf',p1,width = 40 ,height =30,unit='cm')

###############################################################################

df1<-height[,c(1,6,9,10,11)]
df1
head(df1)
#以 group 指数为例，同时将分组列转换为因子变量
df<-df1[,c('Sample','Pielou_e','treat',"group2","group3")]
head(df)

df$treat<-factor(df$treat)
#alpha多样性绘图
library(ggplot2)
library(patchwork)

#mycolors1<-c("#990000","#33CCFF","#000099","#009900","#3399CC","#CCCCFF","#FF6600","#3CB371")

#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
mycolors<-c("#0055AA","#C40003","#4AA329","#894FC6","#00C19B")

scale_color_manual(values=c("day-7" = "#0055AA","day-14" = "#C40003","day-21" = "#894FC6","day-28" = "#00C19B"))
#ggsave("finalchao1.pdf",width = 10,height=9)
df$group3 <- factor(df$group3,levels = c("day-7","day-14","day-21","day-28"))

df$treat<-factor(df$treat)
#mycolors2<-c("#33CCFF","#A9A9A9","#CCCCFF","#3CB371")
mycolors<-c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6')
#mycolors<-c("#0055AA","#C40003","#894FC6","#00C19B")
#mycolors<-c("Red","Navy","Yellow","Blue")
#df$Group1<-factor(df$Group1,levels = c("CK2","CuO2","ZnO2","FeO2","CK4","CuO4","ZnO4","FeO4"),ordered=TRUE)
#df$group2 <- factor(df$group2,levels = c("CK","CuO","ZnO","FeO"))
df$treat<- factor(df$treat,levels = c("CK_2","CK_3","CK_4","CK_5","C2_2","C2_3","C2_4","C2_5"))

#scale_color_manual(values=c("day-7" = "#0055AA","day-14" = "#C40003","day-21" = "#894FC6","day-28" = "#00C19B"))
#ggsave("finalchao1.pdf",width = 10,height=9)
p1 <- ggplot(df,aes(x=treat,y=Pielou_e,fill=treat),outline=F)+
     geom_boxplot(alpha=0.6,size=0.7,width=0.8,outlier.colour = NA,aes(color = treat))+
     geom_point(alpha=0.5,size=9.5,aes(colour = treat))+#+facet_wrap(~date)
     labs(title= "Pielou_e index",x="",y="Pielou_e Index",size=30)+
     scale_color_manual(values = c('#0055AA','#C40003','#4AA329','#894FC6','#00C19B','#7FD2FF','#FF9D1E','#CAB2D6'))+
     theme_ykm()+scale_fill_manual(values=mycolors)+theme(legend.title=element_text(size=6) , legend.text=element_text(size=5))+
     theme(text=element_text(size=30),axis.text.x = element_text(size = 25, face="bold",hjust = 1,angle = 45,colour = "black"),
           axis.text.y = element_text(size = 25,face="bold",colour = "black"))+
     theme(legend.title=element_text(size=20,face="bold") , legend.text=element_text(size=20,face="bold"))+
     theme(axis.title.x = element_text(size=20,face="bold"),axis.title.y = element_text(size = 20,face="bold"))+
     annotate("text",x=1,y=0.890,label="c",size=10,color="#0055AA")+
     annotate("text",x=2,y=0.899,label="b",size=10,color="#C40003")+
     annotate("text",x=3,y=0.892,label="a",size=10,color="#4AA329")+
     annotate("text",x=4,y=0.894,label="a",size=10,color="#894FC6")+
     annotate("text",x=5,y=0.854,label="b",size=10,color="#00C19B")+
     annotate("text",x=6,y=0.890,label="b",size=10,color="#7FD2FF")+
     annotate("text",x=7,y=0.9065,label="b",size=10,color="#FF9D1E")+
     annotate("text",x=8,y=0.9016,label="b",size=10,color="#CAB2D6")

p1
ggsave('Pielou_eindex.pdf',p1,width = 40 ,height =30,unit='cm')

alpha多样性指数绘图（2021年2月14日周二上午）
###############################################################################
Chao1指数显著性差异分析
####叶表差异分
df <- read.delim('alpha.txt', sep = '\t',header = T,check.names = F)
head(df)
#以 chao1 指数为例，同时将分组列转换为因子变量
df <- df[ ,c('Chao1', 'treat')]
df$treat <- factor(df$treat)
str(df)
head(df)
#QQ-plot 检查数据是否符合正态分布（所有的点都离直线很近，落在置信区间内说明正态性良好）
library(car)
qqPlot(lm(Chao1~treat, data = df), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
#使用 Bartlett 检验进行方差齐性检验（p 值大于 0.05 说明方差齐整）
bartlett.test(Chao1~treat, data = df)
#满足假设，单因素方差分析，详情使用?aov查看帮助，
fit <- aov(Chao1~treat, data = df)
summary(fit)

#若想查看各组均值及标准差，可使用 aggregate()
#chao1_mean <- aggregate(chao1$chao1, by = list(chao1$site), FUN = mean)
#chao1_sd <- aggregate(chao1$chao1, by = list(chao1$site), FUN = sd)

##方差分析后，多重比较，继续探寻两两分组间的差异
#Tukey HSD 检验
tuk <- TukeyHSD(fit, conf.level = 0.95)
plot(tuk)
######################################################
library(multcomp)
tuk <- glht(fit, alternative = 'two.sided', linfct = mcp(treat = 'Tukey'))
plot(cld(tuk, level = 0.05, decreasing = TRUE))

#####如果数据不符合正态性可以进行如下的非参数的Kruskal-Wallis检验
#Kruskal-Wallis Test，可使用 ?kruskal.test 查看帮助
kruskal.test(Chao1~treat, data = df)
事后两两比较
##此时若想继续探寻两两分组间的差异，可使用 Wilcoxon 秩和检验，如果分组较多，可以使用循环来完成
#继续在上述 Kruskal-Wallis 检验的基础上，探究两两分组间差异，一个示例如下
df <- read.delim('alpha.txt', sep = '\t',header = T,check.names = F)
head(df)
df1<-df[,c(1,2,9,10,11)]
df1
head(df1)
#以 chao1 指数为例，同时将分组列转换为因子变量
df <- df1[ ,c('Sample', 'treat', 'Chao1')]
df$treat <- factor(df$treat)
str(df)
head(df)
group <- levels(df$treat)
group
group1 <- NULL
group2 <- NULL
median1 <- NULL
median2 <- NULL
p <- NULL

for (i in 1:(
     length(group) - 1)) {
     for (j in (i + 1):length(group)) {
          group1 <- c(group1, group[i])
          group2 <- c(group2, group[j])
          group_ij <- subset(df, treat %in% c(group[i], group[j]))
          group_ij$treat <- factor(group_ij$treat, levels = c(group[i], group[j]))
          
          wilcox_test <- wilcox.test(Chao1~treat, data = group_ij, alternative = 'two.sided', conf.level = 0.95)
          p <- c(p, wilcox_test$p.value)
          median1 <- c(median1, median(subset(group_ij, treat == group[i])$Chao1))
          median2 <- c(median2, median(subset(group_ij, treat == group[j])$Chao1))
     }
}

result <- data.frame(group1, group2, median1, median2, p)
result$padj <- p.adjust(result$p, method = 'BH')   #推荐加上 p 值校正，这里使用 Benjamini 方法校正 p 值
result
#write.table(result, 'Wilcoxon.txt', sep = '\t', row.names = FALSE, quote = FALSE)
###############################################################################
#使用 ggpubr（ggplot2 的扩展包），作图展示 wilcox 比较的结果
library(ggpubr)

ggboxplot(data = df, x = 'treat', y = 'Chao1', color = 'treat') +
     stat_compare_means(method = 'wilcox.test', comparisons = list(c('C2_2', 'CK_2'), c('C2_2', 'C2_3'), c('C2_2', 'CK_3'),c('C2_2','C2_4'),c('C2_2','CK_4'),c('C2_2','C2_5'),c('C2_2','CK_5'),
                                                                   c('CK_2','C2_3'),c('CK_2','CK_3'),c('CK_2','C2_4'),c('CK_2','CK_4'),c('CK_2','C2_5'),c('CK_2','CK_5'),
                                                                   c('C2_3','CK_3'),c('C2_3','C2_4'),c('C2_3','CK_4'),c('C2_3','C2_5'),c('C2_3','CK_5')
                                                                   ,c('CK_3','C2_4'),c('CK_3','CK_4'),c('CK_3','C2_5'),c('CK_3','CK_5')
                                                                   ,c('C2_4','CK_4'),c('C2_4','C2_5'),c('C2_4','CK_5')
                                                                   ,c('CK_4','C2_5'),c('CK_4','CK_5')
                                                                   ,c('C2_5','CK_5')))


