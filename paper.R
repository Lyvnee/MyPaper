####数据处理
cw<-read.csv("shizi.csv")
head(cw)

a0=cw$A
a1=(cw$B+cw$G+cw$L)/3
a2=(cw$C+cw$R+cw$M+cw$H)/4
a3=(cw$D+cw$S+cw$N+cw$I)/4
a4=(cw$E+cw$T+cw$O+cw$J)/4
a5=(cw$F+cw$U+cw$P+cw$K)/4

cwa<-cbind(a0,a1,a2,a3,a4,a5)
write.csv(cwa,"cwa.csv")
head(cwa)
cwa=as.data.frame(cwa)

####聚类分析
##系统聚类(层次聚类)

d<-dist(scale(cwa))

hc1<-hclust(d, "mcquitty");hc2<-hclust(d, "average") #mcquitty相似法，类平均法
hc3<-hclust(d, "centroid");hc4<-hclust(d,"median")   #重心法，中间距离
hc5<-hclust(d, "complete");hc6<-hclust(d,"ward.D")   #最长距离法，力差平方和法

result1=cutree(hc1,k=4);result2=cutree(hc2,k=4);
result3=cutree(hc3,k=4);result4=cutree(hc4,k=4);
result5=cutree(hc5,k=4);result6=cutree(hc6,k=4);

table(result1)

##绘制树形图
opar <- par(mfrow = c(2, 3))
plot(hc1,hang=-1); re1=rect.hclust(hc1,k=4);
plot(hc2,hang=-1); re2=rect.hclust(hc2,k=4);
plot(hc3,hang=-1); re3=rect.hclust(hc3,k=4);
plot(hc4,hang=-1); re4=rect.hclust(hc4,k=4);
plot(hc5,hang=-1); re5=rect.hclust(hc5,k=4);
plot(hc6,hang=-1); re6=rect.hclust(hc6,k=4);
par(opar)

##效果图
mds=cmdscale(d,k=2)   #降维
opar <- par(mfrow = c(2, 3))
plot(mds, col = result1, pch = result1, main ="mcquitty");
plot(mds, col = result2, pch = result2, main ="average");
plot(mds, col = result3, pch = result3, main ="centroid");
plot(mds, col = result4, pch = result4, main ="median");
plot(mds, col = result5, pch = result5, main ="complete");
plot(mds, col = result6, pch = result6, main ="ward.D");
par(opar)

opar <- par(mfrow = c(1, 3))
plot(mds, col = result2, pch = result2, main ="average");
plot(mds, col = result4, pch = result4, main ="median");
plot(mds, col = result6, pch = result6, main ="ward.D");
par(opar)


#动态聚类 K-均值

km3 <- kmeans(scale(cwa), 3,iter.max = 300,nstart = 50);      #中心化处理并聚类
#sort(km3$cluster);km3$size
sc=scale(cwa);
km4 <- kmeans(sc, 4,iter.max = 300,nstart = 50); 
km4$size;km4$center
km5 <- kmeans(scale(cwa), 5,iter.max = 300,nstart = 50);

mds=cmdscale(d,k=2)   #降维
opar3 <- par(mfrow = c(1, 3))
plot(mds,col = km3$cluster, pch = km3$cluster, main="K-means, k=3");
plot(mds,col = km4$cluster, pch = km4$cluster, main="K-means, k=4");
plot(mds,col = km5$cluster, pch = km5$cluster, main="K-means, k=5");
par(opar3)

#取出每种分类的下标
cwa1=which(km4$cluster==1);write.csv(cwa[cwa1,],"cwa1.csv")
cwa2=which(km4$cluster==2);write.csv(cwa[cwa2,],"cwa2.csv")
cwa3=which(km4$cluster==3);write.csv(cwa[cwa3,],"cwa3.csv")
cwa4=which(km4$cluster==4);write.csv(cwa[cwa4,],"cwa4.csv")

opar4 <- par(mfrow = c(2, 2))
plot(km4$center[1,],type='l');plot(km4$center[2,],type='l')
plot(km4$center[3,],type='l');plot(km4$center[4,],type='l')
par(opar4)


opar <- par(mfrow = c(1, 2))
plot(mds, col = result2, pch = result2, main ="average");
plot(mds,col = km4$cluster, pch = km4$cluster, main="K-means, k=4");
par(opar)

#还原标准化数据
kmc=matrix(0,4,6)
for (i in 1:6)
kmc[,i]=km4$center[,i]*sd(cwa[[i]]) + mean(cwa[[i]]);

opar5 <- par(mfrow = c(2, 2))
plot(kmc[1,],type='l',sub="(a)");plot(kmc[2,],type='l',sub="(b)")
plot(kmc[3,],type='l',sub="(c)");plot(kmc[4,],type='l',sub="(d)")
par(opar5)


opar5 <- par(mfrow = c(2, 2))
plot(kmc[1,-c(5,6)],type='l',ylab="℃",sub="(a)");plot(kmc[4,-c(5,6)],ylab="℃",type='l',sub="(b)")
plot(kmc[3,-c(5,6)],type='l',ylab="℃",sub="(c)");plot(kmc[2,-c(5,6)],ylab="℃",type='l',sub="(d)")
par(opar5)



#取出每种分类的下标
cwa1=which(result6==1);write.csv(cwa[cwa1,],"cwa11.csv")
cwa2=which(result6==2);write.csv(cwa[cwa2,],"cwa21.csv")
cwa3=which(result6==3);write.csv(cwa[cwa3,],"cwa31.csv")
cwa4=which(result6==4);write.csv(cwa[cwa4,],"cwa41.csv")

#类内总方差、类间方差
ss1=sum(scale(scale(cwa)[cwa1,],scale=FALSE)^2)
ss2=sum(scale(scale(cwa)[cwa2,],scale=FALSE)^2)
ss3=sum(scale(scale(cwa)[cwa3,],scale=FALSE)^2)
ss4=sum(scale(scale(cwa)[cwa4,],scale=FALSE)^2)
(withinss=c(ss1,ss2,ss3,ss4))
(tot.withinss = sum(withinss))
(totss=sum(scale(scale(cwa),scale=FALSE)^2))
(betweenss = totss - tot.withinss)


cwa1=which(result4==1);write.csv(cwa[cwa1,],"cwa11.csv")
cwa2=which(result4==2);write.csv(cwa[cwa2,],"cwa21.csv")
cwa3=which(result4==3);write.csv(cwa[cwa3,],"cwa31.csv")
cwa4=which(result4==4);write.csv(cwa[cwa4,],"cwa41.csv")

ss1=sum(scale(scale(cwa)[cwa1,],scale=FALSE)^2)
ss2=sum(scale(scale(cwa)[cwa2,],scale=FALSE)^2)
ss3=sum(scale(scale(cwa)[cwa3,],scale=FALSE)^2)
ss4=sum(scale(scale(cwa)[cwa4,],scale=FALSE)^2)
(withinss=c(ss1,ss2,ss3,ss4))
(tot.withinss = sum(withinss))
(totss=sum(scale(scale(cwa),scale=FALSE)^2))
(betweenss = totss - tot.withinss)
km4$withinss;km4$tot.withinss;km4$betweenss


cwa1=which(result2==1);write.csv(cwa[cwa1,],"cwa11.csv")
cwa2=which(result2==2);write.csv(cwa[cwa2,],"cwa21.csv")
cwa3=which(result2==3);write.csv(cwa[cwa3,],"cwa31.csv")
cwa4=which(result2==4);write.csv(cwa[cwa4,],"cwa41.csv")

ss1=sum(scale(scale(cwa)[cwa1,],scale=FALSE)^2)
ss2=sum(scale(scale(cwa)[cwa2,],scale=FALSE)^2)
ss3=sum(scale(scale(cwa)[cwa3,],scale=FALSE)^2)
ss4=sum(scale(scale(cwa)[cwa4,],scale=FALSE)^2)
(withinss=c(ss1,ss2,ss3,ss4))
(tot.withinss = sum(withinss))
(totss=sum(scale(scale(cwa),scale=FALSE)^2))
(betweenss = totss - tot.withinss)
km4$withinss;km4$tot.withinss;km4$betweenss

km4 <- kmeans(sc, 4,iter.max = 300,nstart = 50); 
d<-dist(scale(cwa))
mds=cmdscale(d,k=2)   #降维
plot(mds,col = km4$cluster,main="K-means k=4");
