data <- read_excel('D:/AAAA-资料E盘/data/培养实验/csoct.xlsx')
x <- data$day
y <- data$csoct

if(length(x) != length(y)){
  n <- min(length(x), length(y))
  x <- x[1:n]
  y <- y[1:n]
}

# 用平滑曲线拟合数据
fit <- smooth.spline(x, y)

# 添加平滑曲线
lines(fit, col = "red")

# 从拟合曲线中获取初始值
a <- fit$y[which.max(fit$y)]
b <- 1/mean(diff(fit$x))
c <- b/10
# 定义模型
model <- function(x, a, b,c) a * exp(-b * x)+(293.45-a)*exp(-c*x)
model <- function(x, a, b,c,d) a * exp(-b * x)+(293.45-a-d)*exp(-c*x) + d*exp(-0.000019*x)
# 拟合模型
fit <- nls(y ~ model(x, a, b, c), data = data.frame(x, y), start = list(a = a, b = b,c= c))
fit <- nls(y ~ model(x, a, b, c,d), data = data.frame(x, y), start = list(a = 0.43, b = 0.009, c = 0.0004, d = 274),control = )
# 输出结果
summary(fit)





x <- c(1, 3, 6, 13, 20, 27, 34, 41, 48, 55, 69, 83, 97, 111, 125, 155, 184, 216, 260, 275, 308, 345, 373, 398, 470, 540, 610, 700, 780, 850, 938)
y <- c(293.4325, 293.4134, 293.3746, 293.3154, 293.2932, 293.2660, 293.2392, 293.2106, 293.1867, 293.1590, 293.0763, 293.0125, 292.9278, 292.8595, 292.7839, 292.6555, 292.5202, 292.4325, 292.3473, 292.2850, 292.2097, 292.0962, 291.9906, 291.9315, 291.7756, 291.6205, 291.4530, 291.0440, 290.8931, 290.7498, 290.5896)

# 定义模型
model <- function(x, a, b, c, d) a * exp(-b * x) + (293.45 - a - d) * exp(-c * x) + d * exp(-0.000019 * x)

# 拟合模型
fit <- nls(y ~ model(x, a, b, c, d),
           data = data.frame(x, y),
           start = list(a = 200, b = 0.1, c = 0.01, d = 10),
           minFactor = 0.0001)
fit <- nls(y ~ model(x, a, b, c,d), data = data.frame(x, y), start = list(a = 200, b = 0.1, c = 0.01, d = 10),
           minFactor=1e-6)


# 打印拟合结果
summary(fit)

