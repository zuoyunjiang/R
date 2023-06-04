# 准备观测数据和卷积核
n <- 100
m <- 10
y <- rnorm(n)
h <- rep(1/m, m)
# 生成观测数据的卷积结果
y_conv <- convolve(y, h, type = "open")
# # 进行逆卷积运算
library(Rcpp)
library(Tikhonov)
# y_deconv <- tikhonov(y_conv, h, method = "tikhonov", alpha = 0.01)$estimate
# 进行逆卷积运算（不使用Tikhonov包）
H <- toeplitz(c(h, rep(0, n-m)))
alpha <- 0.01
I <- diag(n)
y_deconv <- solve(t(H) %*% H + alpha * I, t(H) %*% y_conv)

# 分析结果
plot(y_deconv, type = "l", col = "red", lwd = 2, xlab = "Time", ylab = "Carbon flux")

# 安装和加载signal包
install.packages("signal")
library(signal)

# 观测数据
x <- c(1, 3, 6, 13, 20, 27, 34, 41, 48, 55, 69, 83, 97, 111, 125, 155, 184, 216, 260, 275, 308, 345, 373, 398, 470, 540, 610, 700, 780, 850, 938)
y <- c(293.4325, 293.4134, 293.3746, 293.3154, 293.2932, 293.2660, 293.2392, 293.2106, 293.1867, 293.1590, 293.0763, 293.0125, 292.9278, 292.8595, 292.7839, 292.6555, 292.5202, 292.4325, 292.3473, 292.2850, 292.2097, 292.0962, 291.9906, 291.9315, 291.7756, 291.6205, 291.4530, 291.0440, 290.8931, 290.7498, 290.5896)

# 估计卷积核
kernel <- deconvolve(y, x, method = "LAPACK")

# 将卷积核长度扩展到与观测数据相同
kernel$inverseFFT <- fft(kernel$inverseFilter, length(y))

# 计算逆卷积结果
result <- Re(fft(y) / kernel$inverseFFT)

# 绘制结果
plot(x, y, type = "l", col = "blue", xlab = "Days", ylab = "CO2 Flux")
lines(x, result, type = "l", col = "red")
legend("topright", legend = c("Observed", "Deconvolved"), col = c("blue", "red"), lty = 1)

install.packages("ReFit")
library(ReFit)
# 创建观测数据
x <- c(1, 3, 6, 13, 20, 27, 34, 41, 48, 55, 69, 83, 97, 111, 125, 155, 184, 216, 260, 275, 308, 345, 373, 398, 470, 540, 610, 700, 780, 850, 938)
y <- c(293.4325, 293.4134, 293.3746, 293.3154, 293.2932, 293.2660, 293.2392, 293.2106, 293.1867, 293.1590, 293.0763, 293.0125, 292.9278, 292.8595, 292.7839, 292.6555, 292.5202, 292.4325, 292.3473, 292.2850, 292.2097, 292.0962, 291.9906, 291.9315, 291.7756, 291.6205, 291.4530, 291.0440, 290.8931, 290.7498, 290.5896)

# 设置参数
h <- median(diff(x)) # 带宽
sigma <- 2 # 正则化参数

# 执行逆卷积分析
result <- deconvolve(y, kernel = "gaussian", bw = h, method = "Tikhonov", lambda = sigma)

# 打印结果
plot(result$recovered, type = "l", ylim = c(290, 295))
