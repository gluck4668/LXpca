

LXpca <- function(data_file){

# list all the packages that have been installed
all_packages <- data.frame(installed.packages())

# To judge whether a package was installed. If not, it will be installed.
pack <- data.frame(c("vegan","dplyr","ggplot2", "openxlsx","factoextra","tidyfst","ggalt","proj4") )

pack$type <- pack[,1] %in% all_packages$Package

for (i in 1:nrow(pack)){
  if(pack[i,2]==FALSE)
    install.packages(pack[i,1])
}
rm(i)

# 批量library
packages <- as.character(pack[,1])

for(i in packages){
  library(i, character.only = T)
}
rm(i)


#读取数据，并对行和列进行转置（即行、列位置对掉???

if(dir.exists("analysis result")==FALSE)
  dir.create("analysis result")

df <- read.xlsx(data_file)
colnames(df)[1] <- "samples"
df <- distinct(df,samples, .keep_all = T)
df$sum <- rowSums(df[,2:10])
df <- dplyr::filter(df,sum>0)
df <- df[,-11]
rownames(df) <- df[,1]
df <- df[,-1] %>% t() %>% data.frame()

col_n <-ncol(df)#excell表中数据有多少列

#抽提出行???
g <- rownames(df) %>% data.frame()
# 行名中的数字"\\d"用空格“”替换，即去掉数???
g$name <- gsub("\\d","",g[,1])
groups <- g$name

#数据标准???
#center (中心???)：将数据减去均???
#scale (标准???)：在中心化后的数据基础上再除以数据的标准差
data <- scale(df[,1:col_n], center=T, scale=T)

# scale函数进行数据标准???
# 这只能看到两个维???
plot(data, main="scaled data")

# PCA分析
pca <- prcomp(data,center=F,scale=F) # PCA分析函数 prcomp()
pca

# 查看PCA后的结果
summ <- summary(pca) #查看prcomp()分析汇总结???
summ                   #主要看Proportion of Variance（方差占比）
pc <- summ$importance  #导出prcomp()分析汇总结???
#pc
pc[2,1] #抽提出PC1结果
pc[2,2] #抽提出PC2结果

##分别把PC1、PC2转换???%
pc1 <- paste('PC1:', round(pc[2,1]*100, 2), '%')
pc1
pc2 <- paste('PC2:',round(pc[2,2]*100, 2), '%')
pc2

#用fviz_eig()验证上述PC结果
fviz_eig(pca, addlabels = T)  # 以图表形式来展现选择PC1/PC1,2/PC1,2,3???..N （选择标准???>80%???

#用ggplot绘制比较好看的图
pca_df <- data.frame(pca$x) #ggplot要求数据为data.frame


my_theme <- theme_bw()+
            theme(panel.grid = element_blank(),
                  text = element_text(colour = "black", size = 14), #titile等轴字体颜色及大???
                  axis.text = element_text(colour = "black", size = 12),#x、y轴字体颜色及大小
                  panel.background = element_rect(color = 'black', linewidth=1,linetype = 1), #大方框颜色、粗细、类型（"dotdash"或NULL???
                  legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'))+
           theme(plot.title = element_text(hjust = 0.5)) +#标题居中
           theme(plot.margin = unit(c(t=0.5, r=0.5, b=0.5, l=0.5),"cm")) #边距


point_size <- case_when( sum(table(groups))>30 ~2,
                         sum(table(groups))>20 ~3,
                         sum(table(groups))>10 ~3.5,
                         TRUE ~4 )

#ggplot绘图
p1 <- ggplot(pca_df,aes(PC1, PC2,shape=groups,group=groups))+ #数据pca_df；x轴PC1，y轴PC2，形状以group来区???
      geom_point(aes(color = groups),size=point_size)+ #颜色以groups为区分；圆点大小size=1.6
      #scale_color_manual(values = c('darkgreen', 'red', 'blue')) + #圆点颜色
      stat_ellipse(aes(color = groups),type = "t",linetype = 2,linewidth=1,level = 0.95, alpha = 0.3,show.legend = F)+ #虚线椭圆
      labs(x = pc1, y = pc2, title = 'PCA graphics') + #村题名称???
      geom_vline(xintercept = 0, color = 'gray', linewidth = 0.5) + #坚线
      geom_hline(yintercept = 0, color = 'gray', linewidth = 0.5)+ #横线
      my_theme+coord_fixed(1)
p1

ggsave("analysis result/PCA graphics01.png",p1,width=800, height =600, dpi=150,units = "px")


p2 <- ggplot(pca_df,aes(PC1, PC2,shape=groups,group=groups))+ #数据pca_df；x轴PC1，y轴PC2，形状以group来区???
  geom_point(aes(color = groups),size=point_size)+ #颜色以groups为区分；圆点大小size=1.6
  #scale_color_manual(values = c('darkgreen', 'red', 'blue')) + #圆点颜色
  geom_encircle(aes(fill=groups),alpha = 0.1,show.legend = F)+ #虚线椭圆
  labs(x = pc1, y = pc2, title = 'PCA graphics') + #村题名称???
  geom_vline(xintercept = 0, color = 'gray', linewidth = 0.5) + #坚线
  geom_hline(yintercept = 0, color = 'gray', linewidth = 0.5)+ #横线
  my_theme+coord_fixed(1)

p2

ggsave("analysis result/PCA graphics02.png",p2,width=800, height =600, dpi=150,units = "px")


#颜色填充的椭???
p3 <- p1+ stat_ellipse(aes(fill = groups), type = "t",geom = 'polygon',
                       level = 0.95, alpha = 0.3,show.legend = T) #geom ='polygon'显示颜色，path为线条；

ggsave("analysis result/PCA graphics03.png",p3,width=800, height =600, dpi=150,units = "px")

print("--------------------------------------------------------------------")
print("The PCA graphics can be found in the folder of <analysis result>")

p3

}
