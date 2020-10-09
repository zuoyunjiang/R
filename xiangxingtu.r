setwd("D:/F盘（2020）/第一个实验/")

data <-read.csv(file = "./PE-YD.csv",header = TRUE,sep = ',') 
head(data)
attach(data)
### yinzi shuju  fenzu 
data$YD <- factor(data$YD, labels = c("BL",'MH','MWS','SM'))
data$CL <- factor(data$CL,labels = c('MG','YG'))
detach(data)
attach(data)
library(ggplot2)
# grouped boxplot
ggplot(data, aes(x=YD, y=PE, fill=CL)) + 
  geom_boxplot()+
  theme_classic()

# 第二种方法
# Then I make the boxplot, asking to use the 2 factors : variety (in the good order) AND treatment :
opar <- par(mar=c(3,4,3,1))
myplot <- boxplot(PE ~ YD*CL , data=data  , 
                  boxwex=0.4 , ylab="PE",
                  main="the PE of different cl in each YD" , 
                  col=c("slateblue1" , "tomato") ,  
                  xaxt="n")## main 是图形主标题

# To add the label of x axis
my_names <- sapply(strsplit(myplot$names , '\\.') , function(x) x[[1]] )
my_names <- my_names[seq(1 , length(my_names)/2)]
axis(1, 
     at = seq(1.5 , 9 , 2), 
     labels = my_names , 
     tick=FALSE , cex=0.3)

# Add the grey vertical lines
for(i in seq(0.5 , 20 , 2)){ 
  abline(v=i,lty=1, col="grey")
}

# Add a legend
legend("topright", legend = c("MG", "YG"), 
       col=c("slateblue1" , "tomato"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F, inset = c(0.1, 0.1))
par(opar)
