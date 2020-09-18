require(mlbench) #package to get glass dataset
library(tidyverse) #To install and load multiple tidyverse packages
require(rgl) #3D visualization device package
require(ggplot2)
require(MASS) #TO load LDA
require(Rtsne) #FOR TSNE PACKAGE
require(umap) #FOR UMAP PACKAGE
require(factoextra) #To draw bar plot of variable contribution

#Problem 1) Glass Data
glassdata<-data("Glass")
unique_rows<-data.frame(Glass)
unique_rows<-unique_rows[!duplicated(unique_rows),] #find and remove the duplicate row
view(unique_rows) 

#a)Mathematics of PCA
#I)CORRELATION MATRIX
my_num_data <- unique_rows[, sapply(unique_rows, is.numeric)] #remove non-numeric
corMat<-cor(my_num_data,use = "complete.obs") #correlational matrix
corMat

#II)Eigen vectors and eigen values
eigen_vector<-eigen(corMat)
eigen_vector
eigen_values<-eigen_vector$values
eigen_values

#III)prcomp
?prcomp
comp<-prcomp(Glass[,c(1:9)],scale. = TRUE,
  center = TRUE) #since pca works better with numerical value we avoid Type attribute
comp
summary(comp)

#IV)compare results

#V)Proove orthogonal
Z<-cor(comp$rotation)
Z<-data.frame(Z) #convert to dataframe to perform calculations
w<-Z[(c("PC1"))] #to store PC1
x<-Z[c("PC2")] #to store PC2
sum((w)*x) #dot product


#b)Application of PCA
#I)BIPLOT
plot((comp),type='l')
biplot(comp,group_by="Type",color="Type",scale = 0)

#II)Interpretation of PC1 and PC2
str(comp) #to check all the objects stored in PCA
glass_pca<-cbind(Glass,comp$x[,1:2]) #for the  rows add PC1 and PC2
head(glass_pca)

ggplot(glass_pca,aes(PC1,PC2,col=Type,fill=Type))+ 
  stat_ellipse(geom='polygon',col="black",alpha=0.5)+
  geom_point(shape=18,col="black")
correlation<-cor(Glass[,-10],glass_pca[,11:12])
correlation


#III)Degree of agreement
fviz_contrib(comp, choice = "var", axes = 1:2) #bar plot
fviz_eig(comp, addlabels = TRUE, ylim = c(0, 50)) #scree plot

#c)Application of LDA
#I)LDA METHOD
glass_lda<-lda(Type~., data = Glass)
plot(glass_lda)

#II)Interpretation of LD1


#III)LDA Hist
predict_values<-predict(glass_lda) #Make prediction values
ldahist(predict_values$x[,1],g=Glass$Type) #stacked histogram for LD1
ldahist(predict_values$x[,2],g=Glass$Type) #stacked histogram for LD2
par(mar=c(2,2,2,2))
dev.off()
plot(predict_values$x[,1],predict_values$x[,2]) # make a scatterplot
text(predict_values$x[,1],predict_values$x[,2],Glass$Type,cex=0.7,pos=4,col="red") # add labels

#Problem2) Facebook metrics
#a)PCA
FB_data<-read.csv("FB-metrics.csv")
FB_data
FB_comp<-prcomp(FB_data[,c(8:18)],scale. = TRUE,
             center = TRUE) #on last 11 features with scaling
FB_comp
summary(FB_comp)
plot((FB_comp),type='l')
biplot(FB_comp,group_by=FB_data$Type,color=FB_data$Type,scale = 0)

#II)Interpretation of PC1 and PC2
str(FB_comp) #to check all the objects stored in PCA
FB_pca<-cbind(FB_data,FB_comp$x[,1:2]) #for the  rows add PC1 and PC2
head(FB_pca)

ggplot(FB_pca,aes(PC1,PC2,col=Type,fill=Type))+ 
  stat_ellipse(geom='polygon',col="black",alpha=0.5)+
  geom_point(shape=18,col="black") #graphical representation

correlation<-cor(FB_data[,8:18],FB_pca[,19:20])
correlation #to determine if there is positive or negative correlation

fviz_contrib(FB_comp, choice = "var", axes = 1:2) #bar plot
fviz_eig(FB_comp, addlabels = TRUE, ylim = c(0, 50)) #scree plot


#B)t-SNE
tsne_out <- Rtsne(FB_data[,8:18],
                  perplexity=10,
                  theta=0.0, 
                  max_iter = 3000) # Run TSNE
tsne_out
summary(tsne_out)
# Show the objects in the 2D tsne representation
df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], type=FB_data$Type)
ggplot(data=df,aes(x=x,y=y,group=type,color=type))+geom_point()
plot(tsne_out$Y,asp = 1,col=FB_data$Category)
#using a dist obj
tsne_out1 <- Rtsne(dist(FB_data),
                  perplexity=30,
                  theta=0.0, 
                  max_iter = 3000) # Run TSNE
head(tsne_out$Y)
df1<-data.frame(x=tsne_out1$Y[,1],y=tsne_out1$Y[,2], type=FB_data$Category)
ggplot(data=df1,aes(x=x,y=y,group=FB_data$Category,color=FB_data$Category))+geom_point()



df2<-data.frame(x=tsne_out1$Y[,1],y=tsne_out1$Y[,2], type=FB_data$Type)
ggplot(data=df2,aes(x=x,y=y,group=FB_data$Type,color=FB_data$Type))+geom_point()

df3<-data.frame(x=tsne_out1$Y[,1],y=tsne_out1$Y[,2], type=FB_data$Paid)
ggplot(data=df3,aes(x=x,y=y,group=FB_data$Paid,color=FB_data$Paid))+geom_point()

df4<-data.frame(x=tsne_out1$Y[,1],y=tsne_out1$Y[,2], type=FB_data$Post.Weekday)
ggplot(data=df4,aes(x=x,y=y,group=FB_data$Post.Weekday,color=FB_data$Post.Weekday))+geom_point()

df5<-data.frame(x=tsne_out1$Y[,1],y=tsne_out1$Y[,2], type=FB_data$Post.Hour)
ggplot(data=df5,aes(x=x,y=y,group=FB_data$Post.Hour,color=FB_data$Post.Hour))+geom_point()


#Problem 3) UMAP
fb_data.frame<-FB_data
fbdata_umap<-umap(fb_data.frame)
fbdata_umap #minimal summary
head(fbdata_umap$layout)
umap.defaults
plot(fbdata_umap$layout,col=fb_data.frame$Category)
df10<-data.frame(x=fbdata_umap$layout[,1],y=fbdata_umap$layout[,2], type=FB_data$Type)
ggplot(data=df10,aes(x=x,y=y,group=FB_data$Type,color=FB_data$Type))+geom_point()

