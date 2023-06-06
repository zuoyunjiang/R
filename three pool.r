# 加载数据
# CO2 <- read.csv("CO2_flux_data.csv", header=TRUE)
CO2 <- read_excel('D:/AAAA-资料E盘/data/培养实验/csoct.xlsx')
Cpool_proportions <- c(0.05, 0.15, 0.8) # 定义三个池的初始比例

# 定义模型参数和初始值
K1 <- 0.01
K2 <- 0.0009
K3 <- 0.000002
Cpool1 <- sum(CO2$`co2-cumulative emissions`) * Cpool_proportions[1]
Cpool2 <- sum(CO2$`co2-cumulative emissions`) * Cpool_proportions[2]
Cpool3 <- sum(CO2$`co2-cumulative emissions`) * Cpool_proportions[3]

# 设置时间步长和模拟期
dt <- 1 # 时间步长

t <- CO2$day
# 创建结果存储向量
C1 <- rep(0, length(t))
C2 <- rep(0, length(t))
C3 <- rep(0, length(t))
CO2_model <- rep(0, length(t))

# 进行模拟
for(i in 1:length(t)){
  # 计算每个池子的C分解
  dCpool1 <- -K1 * Cpool1
  dCpool2 <- K1 * Cpool1 - K2 * Cpool2
  dCpool3 <- K2 * Cpool2 - K3 * Cpool3
  
  # 计算每个池子的新C值
  Cpool1 <- Cpool1 + (dCpool1 + CO2$`co2-cumulative emissions`[i] * Cpool_proportions[1]) * dt
  Cpool2 <- Cpool2 + (dCpool2 + CO2$`co2-cumulative emissions`[i] * Cpool_proportions[2]) * dt
  Cpool3 <- Cpool3 + (dCpool3 + CO2$`co2-cumulative emissions`[i] * Cpool_proportions[3]) * dt
  
  # 存储结果
  C1[i] <- Cpool1
  C2[i] <- Cpool2
  C3[i] <- Cpool3
  CO2_model[i] <- dCpool1 + dCpool2 + dCpool3 + CO2$`co2-cumulative emissions`[i] # 计算模拟的CO2通量
}

# 绘制结果
plot(t, CO2$`co2-cumulative emissions`, type="l", col="black", xlab="Time (days)", ylab="CO2 flux")
lines(t, CO2_model, col="red",lwd =2)
legend("topright", c("Observed CO2 flux", "Modelled CO2 flux"), col=c("black", "red"), lty=1)


library(readxl)
library(zoo)
CO25 <- read_excel('D:/AAAA-资料E盘/data/培养实验/5°CO2.xlsx')
CO2FLUX <- as.data.frame(CO25[2:32,c(1,2,5,8,11,14,17,20,23)])
CO2FLUX[,2] <- as.double(CO2FLUX[,2])
# filter(CO2FLUX$...2/2, rep(1, 2))*24/1000
rollmean(CO2FLUX$...2,2)*24/1000
