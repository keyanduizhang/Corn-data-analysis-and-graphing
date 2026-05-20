


library(ggplot2)

deseq_res <- read.delim('差异代谢物.txt', sep = '\t')
head(deseq_res)

dim(deseq_res)
#例如这里根据 |log2FC| >= 1 & FDR p-value < 0.05 定义“差异”
deseq_res[which(deseq_res$pvalue %in% NA),'sig'] <- 'no diff'
deseq_res[which(deseq_res$log2FoldChange >= 0 & deseq_res$pvalue < 0.05),'sig'] <- 'rich (pvalue < 0.05, log2FC >= 0)'
deseq_res[which(deseq_res$log2FoldChange <= 0 & deseq_res$pvalue < 0.05),'sig'] <- 'down (pvalue < 0.05, log2FC <= 0)'
deseq_res[which(abs(deseq_res$log2FoldChange) < 0 | deseq_res$pvalue >= 0.05),'sig'] <- 'no diff'
mycolors1<-c('#55aeda','#fa6464')
mycolors1<-c('#84D2E5','#F5958A')

#纵轴为显著性 p 值
volcano_p <- ggplot(deseq_res, aes(log2FoldChange, -log(pvalue, 10))) +
     geom_point(aes(color = sig), alpha = 0.8, size = 2) +
     scale_color_manual(values = c('#55aeda', 'gray30', '#fa6464')) +theme_base()+
     theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), 
           legend.position = c(0.26, 0.92)) +
     theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'), 
           legend.background = element_rect(fill = 'transparent')) +
     geom_vline(xintercept = c(-1, 1), color = 'gray', size = 0.25) +
     geom_hline(yintercept = -log(0.05, 10), color = 'gray', size = 0.25) +
     labs(x = 'log2 Fold Change', y = '-log10 p-value', color = NA) +
     theme(axis.text.x = element_text(size=36,face="bold"),
           axis.text.y = element_text(size = 36,face="bold"))+
     theme(axis.title.x = element_text(size=36,face="bold"),
           axis.title.y = element_text(size = 36,face="bold"))+
     theme(legend.title=element_text(size=38,face="bold") ,
           legend.text=element_text(size=36,face="bold"))+xlim(-5, 5)

volcano_p
#ggsave('volcano_p.pdf', volcano_p, width = 5, height = 6)
ggsave('带图例.pdf', volcano_p, width = 20, height = 20)




volcano_p <- ggplot(deseq_res, aes(log2FoldChange, -log(pvalue, 10))) +
     geom_point(aes(color = sig), alpha = 0.6, size = 2.0) +
     scale_color_manual(values = c('#55aeda', 'gray30', '#fa6464')) +theme(legend.position = "none")+ xlim(-8, 7.5)+
     theme(legend.title=element_text(size=10) , legend.text=element_text(size=10))+
     labs(x = 'log2(Fold Change)', y = '-log10(p-value)', color = NA)+theme_base()+
     theme(text=element_text(size=30),axis.text.x = element_text(size = 26,face="bold",hjust = 1,colour = "black"),axis.text.y = element_text(size = 16,face="bold",colour = "black"))+
     theme(legend.title=element_text(size=24,face="bold") , legend.text=element_text(size=23,face="bold"))+
     theme(axis.title.x = element_text(size=24,face="bold"),
           axis.title.y = element_text(size = 24,face="bold"))+xlim(-5, 5)+
     theme(legend.position = "none")
volcano_p
ggsave('差异代谢物.pdf', volcano_p, width = 6, height = 7)






library(pheatmap)
#读取数据
da <- read.csv("PAL/4CL.csv", sep=",", header=T, check.names=F,row.names = 1)
#求物种相对丰度
da<- read.delim('molisuan.txt', sep = '\t', row.names = 1)
group<- read.delim('group.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""
head(da)
head(group)
group<-as.data.frame(group)


group<- read.delim('group.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""
head(da)
head(group)
group<-as.data.frame(group)
#colnames(group)<-c("GROUP")
#da <- apply(data,2,function(x)x/sum(x))
#head(da)
#绘图
annotation_col1 = data.frame(
        Group = c('B73', 'B73', 'B73', 'OE40', 'OE40', 'OE40'),
        row.names = colnames(da)
)

head(annotation_col1)
pheatmap(da)
#不聚类
pheatmap(da,cluster_rows=F,cluster_cols=F,)
#修改颜色
pheatmap(da,color=colorRampPalette(c("navy","white","firebrick3"))(100))
#修改每个格子的大小
pheatmap(da,cellwidth=12,cellheight=12,color=colorRampPalette(c("navy","white","firebrick3"))(100))


#保存图片cluster_rows=F,cluster_cols=F,


da<- read.delim('RETU.txt', sep = '\t', row.names = 1)
head(da)
group1<- read.delim('group1.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""

head(group1)
group1<-as.data.frame(group1)

group2<- read.delim('group2.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""
head(da)
head(group2)
group2<-as.data.frame(group2)
head(group2)
#colnames(group)<-c("GROUP")
#da <- apply(data,2,function(x)x/sum(x))
#head(da)
#绘图
#annotation_col1 = data.frame(
#  Group = c('B73', 'B73', 'B73', 'OE40', 'OE40', 'OE40'),
# row.names = colnames(da)
#)

#annotation_col1 = data.frame(
#  Group = c('B73', 'B73', 'B73', 'OE40', 'OE40', 'OE40'),
#  row.names = colnames(da)
#)
#
#group2 = c(EDS1 = "#0055AA", NPR1 = "#ADDB88", TGA = "#FAC7B3",
#PR1 = "#894FC6", CTR1 = "#e18283", EIN2 = "#0d898a",
#EIN3 = "#C40003",ERF1 = "#55aeda"))

ann_colors = list(group1= c(KB="#8ccdbf",GB="#ef8476"),
                  group2 = c(Enterobacter = "#8ccdbf", Pantoea = "#ef8476"))
ann_colors = list(group1= c(KB="#8ccdbf",GB="#ef8476"),
                  group2 = c(Enterobacter = "#8ccdbf", Pantoea = "#ADDB88",  Stenotrophomonas = "#FAC7B3",
                             Agrobacterium = "#894FC6"))

head(da)
head(da)

da
da1<-log2((da))
head(da1)
da1
c('#0055AA','#C40003','#4AA329','#894FC6','#e18283','#f9cc52','#0d898a'))
#连续数值型分组可设置成渐变
pheatmap(da1,cluster_rows=F,cluster_cols=F,
         cellwidth=20,cellheight=20,border_color = "white",
         annotation_col = group1,annotation_row = group2,
         annotation_colors = ann_colors,,fontsize_col = 16,  fontsize_row= 16, face="bold",
         color=colorRampPalette(c("navy","white","#fa6464"))(500))


pheatmap(da,scale = "row"cluster_rows=F,cluster_cols=F)
#"#0055AA","#C40003","#4AA329","#894FC6","#00C19B"

#row.names(annotation_row) <- rownames(A)

da<- read.delim('PINGJUN.txt', sep = '\t', row.names = 1)
head(da)
head(da)

da
da1<-log(da)
head(da1)
da1
#group1<- read.delim('group1.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""

#head(group1)
#group1<-as.data.frame(group1)

group2<- read.delim('group2.txt', sep = '\t',row.names = 1)#,row.names = 1,check.names = F,quote = ""
head(da)
head(group2)
group2<-as.data.frame(group2)
head(group2)

#group2$group2 <- factor(group2$group2)
#levels(group2$group2)
#names(ann_colors$group2)
ann_colors = list(group2 = c(Stenotrophomonas = "#0d898a", 
                             Pantoea = "#ADDB88",
                             Agrobacterium = "#f9cc52",
                             Enterobacter = "#894FC6"))
ann_colors
#c('#0055AA','#C40003','#4AA329','#894FC6','#e18283','#f9cc52','#0d898a'))
pheatmap(da,cluster_rows=F,cluster_cols=F,scale = "row",
         cellwidth=30,cellheight=30,border_color = "white",
         annotation_row = group2, #annotation_colors = ann_colors,
         fontsize_col = 18,  fontsize_row= 18, face="bold",
        # display_numbers=TRUE,
         color=colorRampPalette(c("#55aeda","white","#fa6464"))(50))






#mycolors1<-c('#55aeda','#fa6464')
mycolors1<-c('#84D2E5','#F5958A')

'#D93F49','#fa6464','#55aeda','#fa6464''#55aeda','#fa6464'

ann_colors = list(group1= c(RS="#55aeda",Control="#F5958A"))#,MYC2="#25B7BE"
#连续数值型分组可设置成渐变



