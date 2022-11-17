#########################################
#######Chapter 31 R初等统计分析##########
##概率分布
##最大似然估计
##一元正态分布参数最大似然估计
objf.norm1 <- function(theta, x){  ##目标函数
  mu <- theta[1]
  s2 <- exp(theta[2])
  n <- length(x)
  res <- n*log(s2) + 1/s2*sum((x - mu)^2)
  res
}
##用 optim 函数来求极小值点
norm1d.mledemo1 <- function(n=30){
  mu0 <- 20
  sigma0 <- 2
  set.seed(1)
  x <- rnorm(n, mu0, sigma0)
  theta0 <- c(0,0)##初始值设置
  ores <- optim(theta0, objf.norm1, x=x)##求最小值
  print(ores)
  theta <- ores$par
  mu <- theta[1]##均值估计
  sigma <- exp(0.5*theta[2])##标准差估计
  cat('真实 mu=', mu0, ' 公式估计 mu=', mean(x),
      ' 数值优化估计 mu=', mu, '\n')
  cat('真实 sigma=', sigma0,
      '公式估计 sigma=', sqrt(var(x)*(n-1)/n),
      ' 数值优化估计 sigma=', sigma, '\n')
}
norm1d.mledemo1()
##用 nlm() 函数求最小值点（eg求正态分布最大似然估计）
norm1d.mledemo2 <- function(){
  set.seed(1)
  n <- 30
  mu <- 20
  sig <- 2
  z <- rnorm(n, mean=mu, sd=sig)
  neglogL <- function(parm) {
    -sum( dnorm(z, mean=parm[1],
                sd=exp(parm[2]), log=TRUE) )
  }####没看懂这块计算???????????????
  res <- nlm(neglogL, c(10, log(10)))
  print(res)
  sig2 <- exp(res$estimate[2])
  cat('真实 mu=', mu, ' 公式估计 mu=', mean(z),
      ' 数值优化估计 mu=', res$estimate[1], '\n')
  cat('真实 sigma=', sig,
      '公式估计 sigma=', sqrt(var(z)*(n-1)/n),
      ' 数值优化估计 sigma=', sig2, '\n')
  invisible()
}
norm1d.mledemo2()

##函数 optim() 指定 BFGS 拟牛顿法
norm1d.mledemo1b <- function(n=30){
  mu0 <- 20
  sigma0 <- 2
  set.seed(1)
  x <- rnorm(n, mu0, sigma0)
  theta0 <- c(1,1)
  ores <- optim(theta0, objf.norm1, x=x, method="BFGS")##指定 BFGS 拟牛顿法
  print(ores)
  theta <- ores$par
  mu <- theta[1]
  sigma <- exp(0.5*theta[2])
  cat('真实 mu=', mu0, ' 公式估计 mu=', mean(x),
      ' 数值优化估计 mu=', mu, '\n')
  cat('真实 sigma=', sigma0,
      '公式估计 sigma=', sqrt(var(x)*(n-1)/n),
      ' 数值优化估计 sigma=', sigma, '\n')
}
norm1d.mledemo1b()
##optimize()进行单参数估计,对数似然函数中带入已知的mu,估计sigma方
norm1d.mledemo3 <- function(n=30){
  mu0 <- 20
  sigma0 <- 2
  set.seed(1)
  x <- rnorm(n, mu0, sigma0)
  mu <- mean(x)
  ss <- sum((x - mu)^2)/length(x)
  objf <- function(delta, ss) log(delta) + 1/delta*ss
  ores <- optimize(objf, lower=0.0001,
                   upper=1000, ss=ss)
  delta <- ores$minimum
  sigma <- sqrt(delta)
  print(ores)
  cat('真实 sigma=', sigma0,
      '公式估计 sigma=', sqrt(var(x)*(n-1)/n),
      ' 数值优化估计 sigma=', sigma, '\n')
}
norm1d.mledemo3()

##假设检验和置信区间
##均值的假设检验和置信区间
##单样本均值
###大样本情形
##计算均值的近似置信区间(总体方差未知)
BSDA::z.test(x, sigma.x=sd(x), conf.level=1-alpha)
##进行双侧Z检验
##加选项alternative="greater"作右侧检验，加选项alternative="less"作左侧检验。
BSDA::z.test(x, mu=mu0, sigma.x=sd(x))
##计算近似置信区间(总体方差已知)
BSDA::z.test(x, sigma.x=sigma0)
##进行Z检验,仍可用选项 alternative= 指定双侧、右侧、左侧
BSDA::z.test(x, mu=mu0, sigma.x=sigma0)
###小样本正态情形,样本量较小（小于 30）
##总体方差已知，使用Z检验
##计算均值的置信区间程序为 
BSDA::z.test(x, sigma.x=sigma0)
##计算Z检验的程序为 
BSDA::z.test(x,mu=mu0, sigma.x=sigma0)
##可以自己写一个计算 Z 检验的较通用的 R 函数：                                                                  mu=mu0, sigma.x=sigma0)。
z.test.1s <- function(
    x, n=length(x), mu=0, sigma=sd(x), alternative="two.sided"){
  z <- (mean(x) - mu) / (sigma / sqrt(n))
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(z)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(z)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(z)
  } else {
    stop("alternative unknown!")
  }
  c(stat=z, pvalue=pvalue)
}
##总体方差未知，使用T检验
##计算均值的置信区间程序为
t.test(x, conf.level=1-alpha)
##单样本 t 检验
t.test(x, mu=mu0, alternative="two.side") 
##如果已知的样本均值和样本标准差而非具体样本数据，可以自己写一个 R 函数计算 t 检验：
t.test.1s <- function(
    x, n=length(x), sigma=sd(x), mu=0,
    alternative="two.sided"){
  tstat <- (mean(x) - mu) / (sigma / sqrt(n))
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pt(abs(tstat), n-1))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pt(tstat, n-1)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pt(tstat, n-1)
  } else {
    stop("alternative unknown!")
  }
  c(stat=tstat, pvalue=pvalue)
}

###单样本均值比较例子
load(file="hyptest-data.RData")######没有数据???????????????
##咖啡标重的单侧检验
z.test.1s(x=2.92, mu=3, n=36, sigma=0.18, alternative="less")
z.test.1s(Coffee[["Weight"]], mu=3, sigma=0.18, alternative="less")
BSDA::z.test(Coffee[["Weight"]], mu=3, sigma.x=0.18, alternative="less")
##高尔夫球性能的双侧检验 
z.test.1s(GolfTest[['Yards']], mu=295, sigma=12, alternative="two.sided")
BSDA::z.test(GolfTest[['Yards']], mu=295, sigma.x=12, alternative="two.sided")
##Heathrow 机场打分的检验
t.test(AirRating[["Rating"]], mu=7, alternative="greater")
t.test.1s(x=7.25, sigma=1.052, n=60, mu=7, alternative="greater")
##玩具厂商订货量的假设检验
t.test(Orders[["Units"]], mu=40, alternative="two.sided")
t.test.1s(x=37.4, sigma=11.79, n=25, mu=40, alternative="two.sided")


##均值比较
###独立两样本Z检验
##一个大样本方差未知或已知,小样本独立两正态总体方差已知情况做Z检验的R函数：
z.test.2s <- function(
    x, y, n1=length(x), n2=length(y), delta=0,
    sigma1=sd(x), sigma2=sd(y), alternative="two.sided"){
  z <- (mean(x) - mean(y) - delta) / sqrt(sigma1^2 / n1 + sigma2^2 / n2)
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(z)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(z)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(z)
  } else {
    stop("alternative unknown!")
  }
  c(stat=z, pvalue=pvalue)
}
##独立两样本 t 检验(小样本、两正态、两方差未知，但已知方差相等)
###计算两样本 t 检验
t.test(x, y, var.equal=TRUE)##用alternative= 选项指定双侧、右侧、左侧检验
##可以自己写一个这样的R函数作两样本t检验，允许只输入统计量而非具体样本值：
t.test.2s <- function(
    x, y, n1=length(x), n2=length(y),
    sigma1=sd(x), sigma2=sd(y), delta=0,
    alternative="two.sided"){
  sp <- sqrt(1/(n1+n2-2) * ((n1-1)*sigma1^2 + (n2-1)*sigma2^2))
  t <- (mean(x) - mean(y) - delta) / (sp * sqrt(1 / n1 + 1 / n2))
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pt(abs(t), n1+n2-2))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pt(t, n1+n2-2)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pt(t, n1+n2-2)
  } else {
    stop("alternative unknown!")
  }
  c(stat=t, pvalue=pvalue)
}
###Welch 两样本 t 检验，或 Satterthwaite 两样本 t 检验
##两个独立正态总体的方差不相等，样本量不够大
t.test(x, y, var.equal=FALSE)##校正了自由度
##成对均值 t 检验
t.test(x, y, paired=TRUE)

###均值比较的例子
##顾客平均年龄差别比较
z.test.2s(n1=36, x=40, sigma1=9,
          n2=49, y=35, sigma2=10,
          alternative="two.sided")
##两个银行营业所顾客平均存款比较(缺数据?????)
t.test(CheckAcct[[1]], CheckAcct[[2]], var.equal = FALSE, alternative = "two.sided")
##两种工具软件的比较
t.test(SoftwareTest[['Current']], SoftwareTest[['New']],
       var.equal = FALSE, alternative = 'greater')
##两种工艺所需时间的比较 
t.test(Matched[["Method 1"]], Matched[["Method 2"]],
       paired=TRUE, alternative = "two.sided")



##比例的假设检验和置信区间
##单个比例的问题
prop.test.1s <- function(x, n, p=0.5, alternative="two.sided"){
  phat <- x/n
  zstat <- (phat - p)/sqrt(p*(1-p)/n)
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(zstat)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(zstat)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(zstat)
  } else {
    stop("alternative unknown!")
  }
  c(stat=zstat, pvalue=pvalue)
}
##两个比例的比较
prop.test.2s <- function(x, n, delta=0.0, alternative="two.sided"){
  phat <- sum(x)/sum(n)
  p <- x / n
  if(delta==0.0){
    zstat <- (p[1] - p[2])/sqrt(phat*(1-phat)*(1/n[1] + 1/n[2]))
  } else {
    zstat <- (p[1] - p[2] - delta)/sqrt(p[1]*(1-p[1])/n[1] + p[2]*(1-p[2])/n[2])
  }
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(zstat)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(zstat)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(zstat)
  } else {
    stop("alternative unknown!")
  }
  c(stat=zstat, pvalue=pvalue)
}
##独立两总体比例比较(小样本情形),可用Fisher 精确检验
fisher.test()

###比例检验的例子
##高尔夫培训女生比例检验例子
prop.test(100, 400, p=0.20, alternative = "greater")
##用自定义的大样本 Z 检验函数 prop.test.1s():
prop.test.1s(100, 400, p=0.20, alternative = "greater")
##用基于二项分布的 binom.test() 检验:
binom.test(100, 400, p=0.20, alternative = "greater")
##报税代理分理处的错误率比较
##作双侧检验，水平 0.10
prop.test(c(35,27), c(250,300), alternative = "two.sided")
##用自定义的 prop.test.2s():
prop.test.2s(c(35,27), c(250,300), alternative = "two.sided")


##方差的假设检验和置信区间
###单总体方差的假设检验和置信区间
##自定义函数计算单正态总体方差的检验：
var.test.1s <- function(x, n=length(x), var0, alternative="two.sided"){
  if(length(x)==1){ # 输入的是方差
    varx <- x
  } else {
    varx <- var(x)
  }
  xi <- (n-1)*varx/var0
  if(alternative=="less"){
    pvalue <- pchisq(xi, n-1)
  } else if (alternative=="right"){
    pvalue <- pchisq(xi, n-1, lower.tail=FALSE)
  } else if(alternative=="two.sided"){
    pvalue <- 2*min(pchisq(xi, n-1), pchisq(xi, n-1, lower.tail=FALSE))
  }
  c(statistic=xi, pvalue=pvalue)
}
##独立两总体方差的比较
var.test(x, y, alternative=...)

##拟合优度检验
###各类比例相等的检验
chisq.test(x)##作各类的总体比例相等的拟合优度卡方检验
chisq.test(x, p)##作各类的总体比例为指定概率的拟合优度卡方检验
###带有未知参数的单分类变量的拟合优度假设检验
chisq.test(c(300, 200, 500), c(0.25, 0.25, 0.50))##p值所用的自由度错误
##重新计算p值
res <- chisq.test(c(300, 200, 500), c(0.25, 0.25, 0.50))
c(statistic=res$statistic, pvalue=pchisq(res$statistic, res$parameter - 1, lower.tail=FALSE))

##检验分布类型--goodfit 函数
##用来拟合指定的某种理论分布(包括泊松\二项\负二项分布),并检验服从该理论分布的零假设
install.packages('vcd')
library(vcd)
##生成一组速率参数为 2 的泊松随机数，检验其分布是否泊松分布：
set.seed(101)
datax <- rpois(100, 2)
summary(goodfit(datax, "poisson"))
##检验其是否服从二项分布，取二项分布试验数为 10：
summary(goodfit(datax, "binomial", par = list(size = 10)))

##列联表独立性卡方检验
chisq.test(x,y)##x,y分别为两个变量的原始观测值
chisq.test(x)##x为保存了矩阵格式的列联表
##列联表独立性卡方检验例子
##性别与啤酒种类的独立性检验
ctab.beer <- rbind(c(
  20, 40, 20),
  c(30,30,10))
colnames(ctab.beer) <- c("Light", "Regular", "Dark")
rownames(ctab.beer) <- c("Male", "Female")
addmargins(ctab.beer)
chisq.test(ctab.beer)##在 0.05 水平下认为啤酒类型偏好与性别有关
##男性组的偏好分布、女性组的偏好分布、所有人的偏好分布：
tab2 <- round(prop.table(addmargins(ctab.beer, 1), 1), 3)
rownames(tab2)[3] <- "All"
tab2



##############################################
#######Chapter 32 R 相关与回归##########
##相关分析
##线性相关的模拟数据的散点图
set.seed(1)
nsamp <- 30
x <- runif(nsamp, -10, 10)
y <- 20 + 0.5*x + rnorm(nsamp,0,0.5)
plot(x, y)
##二次曲线相关的模拟数据散点图
set.seed(1)
y2 <- 0.5*x^2 + rnorm(nsamp,0,2)
plot(x, y2)
##指数关系的例子
set.seed(1)
y3 <- exp(0.2*(x+10)) + rnorm(nsamp,0,2)
plot(x, y3)
##对数关系的例子
set.seed(1)
y4 <- log(10*(x+12)) + rnorm(nsamp,0,0.1)
plot(x, y4)

##相关系数的性质
##对称轴x=7
set.seed(1)
x <- runif(30, 0, 10);x
xx <- seq(0, 10, length.out = 100)
y <- 40 - (x-7)^2 + rnorm(30)
yy <- 40 - (xx-7)^2
plot(x, y, pch=16)
lines(xx, yy)
cor(x, y)
##对称轴x=5
x <- runif(30, 0, 10)
xx <- seq(0, 10, length.out = 100)
y <- 40 - (x-5)^2 + rnorm(30)
yy <- 40 - (xx-5)^2
plot(x, y, pch=16)
lines(xx, yy)
cor(x, y)
##指数型
x <- runif(30, 0, 10)
xx <- seq(0, 10, length.out = 100)
y <- 40*exp(-x/2) + rnorm(30)
yy <- 40*exp(-xx/2)
plot(x, y, pch=16)
lines(xx, yy)
cor(x, y)

##相关与因果
##相关系数大小

##相关系数的检验cor.test(x,y) 
d.class <- readr::read_csv('E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv')
cor.test(d.class$height, d.class$weight)

set.seed(1)
nsamp <- 30
x <- runif(nsamp, -10, 10)
y <- 0.5*x^2 + rnorm(nsamp,0,2)
plot(x,y,pch=16)
cor.test(x, y)

##相关阵
install.packages('corrgram')
library(corrgram)##绘制相关系数矩阵的图形
corrgram(##选项 order=TRUE 重排各列的次序使得较接近的列排列在相邻位置
  baseball[,c("Assists","Atbat","Errors","Hits","Homer","logSal",
              "Putouts","RBI","Runs","Walks","Years")],
  order=TRUE, main="Baseball data PC2/PC1 order",
  lower.panel=panel.shade, upper.panel=panel.pie)


####一元回归分析
##最小二乘法
plot(x, y)
abline(lm(y ~ x), col="red", lwd=2)
##设数据保存在数据框d中,变量名为y和x,用R的lm()函数计算回归，如:
set.seed(1)
nsamp <- 30
x <- runif(nsamp, -10, 10)
y <- 20 + 0.5*x + rnorm(nsamp,0,0.5)
d <- data.frame(x=x, y=y)
plot(x, y)
abline(lm(y ~ x), col="red", lwd=2)
lm1 <- lm(y ~ x, data=d); lm1
summary(lm1)
##对d.class数据集,建立体重对身高的回归方程:
lm2 <- lm(weight ~ height, data=d.class)
summary(lm2)

##回归诊断--判断误差项是否符合回归模型假定中的独立性、方差齐性、正态性等
##利用回归残差，还可以计算一些异常值、强影响点的诊断
residuals()##从回归结果计算残差
rstandard()##从回归结果计算标准化残差
##第一个图是残差对预测值散点图，散点应该随机在 0 线上下波动，不应该有曲线模式、分散程度增大模式、特别突出的离群点等情况。
##第二个图是残差的正态 QQ 图，散点接近于直线时可以认为模型误差项的正态分布假定是合理的。
##第三个图是误差大小 (标准化残差绝对值的平方根) 对拟合值的图形，可以判断方差齐性假设 (方差不变)。
##第四个图是残差对杠杆量图，并叠加了 Cook 距离等值线。杠杆量代表了回归自变量对结果的影响大小，超过 4/𝑛的值是需要重视的。Cook 距离考察删去第 𝑖 个观测对回归结果的影响。
plot(lm2)
##残差图的几种常见的缺陷：
##非线性:
set.seed(1)
x <- runif(30, 0, 10)
xx <- seq(0, 10, length.out = 100)
y <- 40 - (x-7)^2 + rnorm(30)
yy <- 40 - (xx-7)^2
lms1 <- lm(y ~ x)
opar <- par(mfrow=c(1,2))
plot(x, y, pch=16, main=" 数据和真实模型")
lines(xx, yy)
plot(x, rstandard(lms1), main=" 使用线性回归的残差")
par(opar)
##异方差:
x <- sort(runif(30, 0, 10))
y <- 10 + 2*x + rnorm(30)*(seq(30)/10)
lms2 <- lm(y ~ x)
opar <- par(mfrow=c(1,2))
plot(x, y, pch=16, main=" 数据和真实模型")
abline(a=10, b=2)
plot(x, rstandard(lms2), main=" 线性模型的残差")
par(opar)
##序列自相关：
ar1 <- arima.sim(list(ar=c(0.5)), n=30)
plot(1:30, ar1[], type="l", xlab="time", ylab="residual",
     main=" 有序列自相关的残差图形")

install.packages('car')
library(car)
car::ncvTest(lm2)##检验方差齐性,零假设是方差齐性成立
car::durbinWatsonTest(lm1)##DW检验残差中是否有序列自相关,零假设是没有序列自相关
##对回归残差绘制 ACF 图
##如果除了横坐标 0 之外都落在两条水平界限内则认为没有序列自相关
##如果有明显超出界限的就认为有序列自相关
acf(residuals(lm1), lag.max = 10, main="")

##预测区间
predict(lm2)##得到y的预测值
predict(lm2, interval="prediction",level = 0.95) ##得到预测的置信区间,加入 level= 选项设定置信度。


####多元线性回归
## R 的多元回归程序
lm(y ~ x1 + x2 + x3, data=d) ##做多元回归，数据集为 d，自变量为 x1,x2,x3 三列。
##例：体重对身高和年龄的回归
lm3 <- lm(weight ~ height + age, data=d.class)
summary(lm3)

##回归自变量筛选
##模型中不显著的自变量应该逐一剔除,可以用step函数进行逐步回归变量选择
lm5 <- step(lm(weight ~ height + age + sex, data=d.class))
summary(lm5)


##哑变量与变截距项的模型
lm6 <- lm(weight ~ height + sex, data=d.class)
summary(lm6)##结果中的sexM项表示以女生为基数,男生体重的平均增加量,这一项不显著。

##如果忽略了分类变量，结论可能是错误的。
##考察 iris 数据中花萼长宽之间的关系。
##数据中有三个品种的花，仅考虑其中的 setosa 和 versicolor 两个品种。
d <- iris[iris[["Species"]] %in% c("setosa", "versicolor"),
          c("Sepal.Length", "Sepal.Width", "Species")]
d$Species <- factor(as.character(d$Species))
lm7 <- lm(Sepal.Width ~ Sepal.Length, data=d)
with(d, plot(Sepal.Length, Sepal.Width, col=(2:3)[Species]))
with(d, legend("topleft", pch=1, col=2:3, legend=levels(Species)))
abline(lm7, lwd=2)
summary(lm7)##回归结果花萼长、宽是负相关的，这明显不合理
##加入 Species 分类变量作为回归自变量：
lm8 <- lm(Sepal.Width ~ Species + Sepal.Length, data=d)
summary(lm8)##花萼长度变量的系数为正而且高度显著。
##作两条回归直线的图形：
with(d, plot(Sepal.Length, Sepal.Width, col=(2:3)[Species]))
with(d, legend("topleft", pch=1, col=2:3, legend=levels(Species)[1:2]))
abline(a=coef(lm8)[1], b=coef(lm8)[3], col=2, lwd=2)
abline(a=sum(coef(lm8)[1:2]), b=coef(lm8)[3], col=3, lwd=2)

##残差诊断
residuals(lmres) ##可以求残差
rstandard(lmres) ##可以求标准化残差
rstudent(lmres) ##可以求外部学生化残差
plot()##作4个残差诊断图,可以用which=1~4指定仅作第一~四幅图

##多重共线性
##如果两个自变量之间的相关系数显著地不等于零，这两个自变量就有广义的共线性。
##如果线性关系的 F 检验显著但是单个回归系数都不显著，可能是由于多重共线性。
##如果有单个回归系数显著但是 𝐹 检验不显著，可能是由于多重共线性。
##如果某些回归系数的正负号与通常的认识相反，可能是由于多重共线性。
##car 包的 vif() 函数计算方差膨胀因子。
car::vif(lmrst01)

##强影响点分析
hatvalues(lmres)##可以求杠杆值
rstudent(lmres)##可以求外学生化
cooks.distance(lmres)##可以求 Cook 距离
##R中的强影响点诊断函数还有 dfbetas(),dffits(),covratio()


##过度拟合示例
##R方代表了模型对数据的拟合程度,模型中加入的自变量越多,R方越大。
##模型中的自变量太多可能会发生 “过度拟合”。
set.seed(10)
n <- 20
x <- sample(1:n, size=n, replace=TRUE)
a <- 100
b <- 2
sigma <- 5
y <- a + b*x + rt(n, 4)*sigma
xnew <- c(1.5, 2.5, 3.5)
ynew <- a + b*xnew + rnorm(length(xnew), 0, sigma)
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
legend("topleft", pch=c(16,2), col=c("black", "red"),
       legend=c(" 拟合用", " 测试用"))
##作线性回归：
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
lmof1 <- lm(y ~ x)
abline(lmof1)
##回归系数：
summary(lmof1)
##二次多项式回归：
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
lmof2 <- lm(y ~ x + I(x^2))
xx <- seq(1, n, length=100)
yy <- predict(lmof2, newdata=data.frame(x=xx))
lines(xx, yy)
##回归系数：
summary(lmof2)##回归结果出现多重共线性问题,也已经过度拟合。
##三次多项式回归：
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
lmof3 <- lm(y ~ x + I(x^2)+ I(x^3))
xxx <- seq(1, n, length=100)
yyy <- predict(lmof3, newdata=data.frame(x=xxx))
lines(xxx, yyy)
##回归系数：
summary(lmof3)##回归结果出现多重共线性问题,也已经过度拟合。
##四次多项式回归：
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
lmof4 <- lm(y ~ x + I(x^2)+ I(x^3)+ I(x^4))
xxxx <- seq(1, n, length=100)
yyyy <- predict(lmof4, newdata=data.frame(x=xxxx))
lines(xxxx, yyyy)
##回归系数：
summary(lmof4)
##五次多项式回归：
plot(x, y, pch=16, xlim=c(0, n+1), ylim=c(90,140))
points(xnew, ynew, pch=2, col="red")
lmof5 <- lm(y ~ x + I(x^2)+ I(x^3)+ I(x^4)+ I(x^5))
xxxxx <- seq(1, n, length=100)
yyyyy <- predict(lmof4, newdata=data.frame(x=xxxxx))
lines(xxxxx, yyyyy)
##回归系数：
summary(lmof5)


##嵌套模型的比较(方差分析)--完全模型vs精简模型
anova()##函数比较两个嵌套的线性回归结果可以进行这样的方差分析F检验
##lmrst01是完全模型,包含5个自变量;lmrst02是嵌套的精简模型,包含3个自变量。
##用anova()函数可以检验多出的变量是否有意义
##原假设：多出的自变量对应系数为0
anova(lmrst01, lmrst02)

##对不同模型计算AIC,取AIC较小的模型--非嵌套的模型
AIC(lmrst01, lmrst02)

##拟合与预测
##拟合
predict(lmres)##对原数据框中的观测值做预测
##点预测
predict(lmres, newdata=dp)##对新数据做预测
##例如，利用包含居民数、人均餐费、到市中心距离的模型 lmrst02，求居民数 =50(万居民)，人均餐费 =100(元)，距市中心 10 千米的餐馆的日均营业额：
predict(
  lmrst02,
  newdata=data.frame(
    `居民数`=50, `人均餐费`=100, `距离`=10
  ))

expand.grid() ##可以对若干个变量的指定值,生成包含所有组合的数据框
newd <- expand.grid(
  `居民数`=c(60, 140),
  `人均餐费`=c(50, 130),
  `距离`=c(6, 16))
newd##生成包含所有组合的数据框
predict(lmrst02, newdata=newd)##做预测

##均值的置信区间
##加选项interval="confidence",用level=指定置信度,可以计算均值的置信区间
predict(
  lmrst02, interval="confidence", level=0.95,
  newdata=data.frame(
    `居民数`=50, `人均餐费`=100, `距离`=10
  ))

##个别值的预测区间
##加选项interval="prediction",用level=指定置信度，可以计算预测区间
predict(
  lmrst02, interval="prediction", level=0.95,
  newdata=data.frame(
    `居民数`=50, `人均餐费`=100, `距离`=10
  ))


##利用线性回归模型做曲线拟合
SteelBag <- data.frame(
  x = c(2, 3, 4, 5, 7, 8, 10,
        11, 14, 15, 16, 18, 19),
  y = c(106.42, 108.20, 109.58, 109.50, 110.0,
        109.93, 110.49, 110.59, 110.60, 110.90,
        110.76, 111.00, 111.20)
)
knitr::kable(SteelBag)
with(SteelBag, plot(
  x, y, xlab=" 使用次数", ylab=" 钢包容积"
))##散点图呈现非线性
##用线性回归近似：
lmsb1 <- lm(y ~ x, data=SteelBag)
summary(lmsb1)##结果显著，R方为0.689
##拟合图：
with(SteelBag, plot(
  x, y, xlab=" 使用次数", ylab=" 钢包容积",
  main=" 线性近似"
))
abline(lmsb1, col="red", lwd=2)
##残差诊断：
plot(lmsb1,which=1)##残差图呈现非线性
##用双曲线模型：
with(SteelBag, plot(
  1/x, 1/y, xlab="1/使用次数", ylab="1/钢包容积",
  main="x 和 y 都做倒数变换"
))
lmsb2 <- lm(I(1/y) ~ I(1/x), data=SteelBag)
summary(lmsb2)##结果显著,R方从线性近似的0.69提高到了0.97。
##拟合图：
with(SteelBag, plot(
  x, y, xlab=" 使用次数", ylab=" 钢包容积",
  main=" 线性和非线性回归"
))
abline(lmsb1, col="red", lwd=2)
curve(1/(0.008967 + 0.0008292/x), 1, 20,
      col="green", lwd=2, add=TRUE)
legend("bottomright", lty=1, lwd=2,
       col=c("red", "green"),
       legend=c(" 线性回归", " 曲线回归"))


Reynolds<- data.frame(
  Months = c(41,106,76,104,22,12,85,111,40,51,9,12,6,56,19),
  Sales = c(275,296,317,376,162,150,367,308,189,235,83,112,67,325,189)
)
knitr::kable(Reynolds)
##散点图：
with(Reynolds, plot(Months, Sales))##散点图呈现非线性。
##用线性近似：
lmre1 <- lm(Sales ~ Months, data=Reynolds)
summary(lmre1)##结果显著,R方= 0.78。
##拟合图：
with(Reynolds, plot(Months, Sales, main=" 线性近似"))
abline(lmre1, col="red", lwd=2)
##残差诊断：
plot(lmre1, which=1)##残差图有明显的非线性。
##考虑最简单的非线性模型-二元线性回归模型，作二次多项式回归:
lmre2 <- lm(Sales ~ Months + I(Months^2), data=Reynolds)
summary(lmre2)##模型显著。R方从线性近似的 0.78 提高到 0.90。
##添加中心化的二次项-防止造成x与x方之间的共线性
lmre3 <- lm(Sales ~ Months + I((Months-60)^2), data=Reynolds)
summary(lmre3)
##拟合图：
with(Reynolds, plot(Months, Sales, main=" 线性和非线性回归"))
abline(lmre1, col="red", lwd=2)
curve(196.50 + 2.2065*x - 0.03449*(x-60)^2, 5, 110,
      col="green", lwd=2, add=TRUE)
legend("bottomright", lty=1, lwd=2,
       col=c("red", "green"),
       legend=c(" 线性回归", " 曲线回归"))

##分组建立多个模型
library(readr)
d.cancer <- read_csv(
  "E:\\00study\\MJ\\Rlearning\\R.DATA\\cancer.csv", locale=locale(encoding="GBK"))
print(d.cancer)
##以肺癌病人数据为例，建立 v1 对 v0 和 age 的多元线性回归模型：
lmgr01 <- lm(v1 ~ v0 + age, data = d.cancer)
summary(lmgr01)
library(broom)
tidy(lmgr01)##将系数估计结果转换成合适的 tibble 数据框格式
##用 broom 包的augment()函数可以获得拟合值、残差等每个观测的回归结果：
knitr::kable(augment(lmgr01), digits=2)
##glance() 函数可以将回归的复相关系数平方、F 检验 p 值等整体结果做成一行的数据框
knitr::kable(glance(lmgr01), digits=2)
##将男病人与女病人分别建模，并以表格形式合并分组的建模结果
install.packages('tidyr')
library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(broom)
d.cancer %>%
  group_by(sex) %>%
  nest()
##用 purrr::map() 函数对每一组分别建模，建模结果可以借助 broom 包制作成合适的数据框格式
##用 unnest() 函数将不同组的结果合并成一个大数据框
d.cancer %>%
  group_by(sex) %>%
  nest() %>%
  mutate(model = map(data, function(df) summary(lm(v1 ~ v0 + age, data=df))),
         tidied = map(model, tidy)) %>%
  unnest(tidied, .drop = TRUE)
###p849

##Logistic 回归p859
##进行 logistic 回归的R程序
glm(y ~ x1 + x2, family=binomial, data=d)##y取值0或1，数据集d

d.remiss <- read.csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\remiss.csv", header=TRUE)
glm1 <- glm(
  remiss ~ cell+smear+infil+li+blast+temp,
  family=binomial, data=d.remiss)
summary(glm1)
##以 p 值 0.30 为界限，逐步删去不显著的自变量:
##删去blast
glm1b <- glm(
  remiss ~ cell + smear + infil + li + temp,
  family=binomial, data=d.remiss)
summary(glm1b)
##删去smear
glm1c <- glm(
  remiss ~ cell + infil + li + temp,
  family=binomial, data=d.remiss)
summary(glm1c)
##删去infil
glm1d <- glm(
  remiss ~ cell + li + temp,
  family=binomial, data=d.remiss)
summary(glm1d)

##或可用逐步回归:
glm2 <- step(glm(
  remiss ~ cell + smear + infil + li + blast + temp,
  family=binomial, data=d.remiss))
summary(glm2)


##############################################
#######Chapter 33 R时间序列分析##########
##时间序列数据类型
##ts 类型
##用函数 ts 把一个向量转换为时间序列
yd <- ts(
  c(4, 8, 7, 7, 3, 1, 8, 9, 8, 6, 3, 5,
    5, 8, 2, 5, 9, 2, 5, 2, 3, 2, 2, 4),
  frequency=1, start=2001); yd##yd 是年数据，从 2001 到 2024 年
ym <- ts(
  c(9, 6, 3, 5, 4, 8, 2, 5, 8, 4, 6, 3,
    2, 2, 6, 4, 1, 4, 2, 1, 8, 9, 4, 6),
  frequency=12, start=c(2001,1)); ym##ym 是月度数据从 2001 年 1 月到 2002 年 12 月

##as.vector 把时间序列的数据转换成普通向量
as.vector(ym)
class(as.vector(ym))

data(AirPassengers)
attributes(AirPassengers)
AirPassengers
start(AirPassengers)##求时间序列的开始点
end(AirPassengers)##求时间序列的结束点
frequency(AirPassengers)##求采样频率
##aggregate() 函数可以把月度数据加总成年数据
AP.year <- aggregate(AirPassengers); AP.year
##cycle() 函数对月度数据返回序列每个时间点所在的月份
cy.AP <- cycle(AirPassengers); cy.AP
##window() 函数取出时间序列的一段
##此处仅取出1949年1月份的数据
AP.Jan <- window(AirPassengers, start=c(1949,1),
                 frequency=TRUE); AP.Jan##指定frequency=TRUE可以仅取出某个月（季度）

##xts 类型与常用函数
install.packages('xts')
library(xts)
library(lubridate)
xts.1 <- xts(
  c(5, 5, 4, 6, 4, 3, 3, 3, 4, 5, 5, 4),
  make_date(2018, 1, 1) + ddays(0:11)); xts.1
as.xts(ym)
##xts 类型支持强大的子集提取功能
data(AirPassengers)
xts.ap <- as.xts(AirPassengers)
xts.ap["1949"]
##用"from/to" 的格式指定一个日期时间范围,也不需要开始点和结束点恰好有数据
xts.1["2018-01-10/2018-01-14"]
##取出开头的三个月
first(xts.ap, "3 months")
##字符串中取负值时表示扣除，如扣除开始的 3 个月：
start(xts.ap)
start(first(xts.ap, "-3 months"))

##基本分析与作图
##对 ts 类型，plot() 函数作曲线图
data(AirPassengers)
plot(AirPassengers)
xts.ap <- as.xts(AirPassengers)##ts类型转化为xts类型
plot(xts.ap, main="Air Passengers",
     major.ticks="years", minor.ticks=NULL,
     grid.ticks.on="years",
     col="red")
##计算滞后 1 序列
stats::lag(ts(1:5, start=2001), k=-1)


##ARIMA 建模和模拟
##AR(4) 模拟例子
set.seed(101)
xar4 <- arima.sim(
  model=list(ar=c(-0.9, -1.4, -0.7, -0.6)),
  n = 100,
  sd = sqrt(4) )
##作时间序列图：
plot(xar4)
##作 ACF 图：
acf(xar4)
##作 PACF 图（偏自相关函数图）
pacf(xar4)
##acf(x) 加 type 选项和 plot=FALSE 选项可以返回估计的自协方差函数、自相关函数、偏自相关函数。
##注意自相关函数、自协方差函数都是从滞后0开始,而偏自相关函数从滞后 1 开始
## 前 10 个自相关函数值：
round(c( acf(xar4, lag.max=10, type="correlation", plot=FALSE)$acf ), 4)
## 前 10 个自协方差函数值：
round(c( acf(xar4, lag.max=10, type="covariance", plot=FALSE)$acf ), 4)
## 前 9 个偏自相关函数值：
round(c( acf(xar4, lag.max=9, type="partial", plot=FALSE)$acf ), 4)
##用 polyroot() 计算多项式的所有复根，输入是从零次项到最高次项的系数。
##用 Mod() 或 abs() 求复数模
a <- c(-0.9, -1.4, -0.7, -0.6)
Mod(polyroot(c(1, -a)))##四个根都在单位圆外，满足 AR 模型的最小相位条件。

## MA(2) 模拟例子
##模拟生成长度为 100 的样本，作时间序列图，ACF，PACF 图：
set.seed(101)
xma2 <- arima.sim(
  model=list(ma=c(0.5, -0.4)),
  n = 100,
  sd = sqrt(4) )
plot(xma2)
acf(xma2)
pacf(xma2)

##ARMA(4,2) 模拟例子
##模拟生成长度为 𝑁 = 100 的样本，并作时间序列图、ACF 图、PACF 图：
set.seed(101)
xarma42 <- arima.sim(
  model=list(
    ar=c(-0.9, -1.4, -0.7, -0.6),
    ma=c(0.5, -0.4)),
  n = 100,
  sd = sqrt(4) )
plot(xarma42)
acf(xarma42)
pacf(xarma42)

##ARIMA(4,1,2) 模拟例子
set.seed(101)
xarima412 <- arima.sim(
  model=list(
    order = c(4,1,2),
    ar=c(-0.9, -1.4, -0.7, -0.6),
    ma=c(0.5, -0.4)),
  n = 100,
  sd = sqrt(4) )[-1]##要注意arima.sim() 在有一阶差分时会输出n+1个值。
ts.plot(xarima412)
acf(xarima412)
pacf(xarima412)

## AR 建模
##stats::ar(x, method="mle") 可以用最大似然估计方法估计模型参数并用 AIC 定阶。如：
resar <- ar(xar4, method="mle"); resar

## ARMA 建模
##stats::arima()可以用最大似然方法估计AR、MA、ARMA和ARIMA模型，需要人为指定(p,d,q)值
armares <- arima(
  100 + xarma42, order = c(4,0,2) )
armares


##模型诊断
##结果包括标准化残差、残差的 ACF、残差的 Ljung-Box 白噪声检验 p 值
##检验对多个滞后值计算,p值高于横虚线(0.05 线)表示模型适合
tsdiag(armares)

##白噪声检验--零假设为数据来自白噪声列
Box.test(xarma42, type="Ljung-Box", lag = 10)

##对ARMA或者ARIMA建模的残差进行白噪声检验,需要加fitdf选项,取值为p+q
Box.test(armares$residuals, type="Ljung-Box", lag = 10, fitdf=4+2)

##稀疏系数估计
##考虑稀疏系数的 ARMA(4,2) 模型
##产生模拟数据：
set.seed(101)
xsparma42 <- 100 + arima.sim(
  model=list(
    ar=c(0, 0, 0, 0.7),
    ma=c(0, -0.4)),
  n = 100,
  sd = sqrt(4) )
plot(xsparma42)
acf(xsparma42)
pacf(xsparma42)
##拟合 ARMA(4,2) 模型：
spres1 <- arima(
  xsparma42, order = c(4,0,2) )
spres1
##计算各个系数估计除以标准误差的比值，在系数为 0 的条件下近似服从标准正态分布：
zstat.arima <- function(arimares){
  with(
    arimares, {
      y <- numeric(length(coef))
      names(y) <- names(coef)
      y[mask] <- coef[mask] / sqrt(diag(var.coef))
      y
    })
}
round(zstat.arima(spres1), 2)
##当近似Z统计量值在正负2之间时,可认为相应的系数等于零,用如下程序估计稀疏系数的ARMA模型：
##在arima中用fixed=选项指定某些系数,需要指定的系数就输入指定的值,需要从数据中估计的值就输入NA,次序按arima显示结果时各个系数的次序。
spres2 <- arima(
  xsparma42, order = c(4,0,2),
  fixed = c(0, 0, 0, NA, 0, NA, NA),
  transform.pars = FALSE)
spres2
round(zstat.arima(spres2), 2)


##单位根检验(ADF 检验)--零假设是有单位根，即不平稳
install.packages('fUnitRoots')
##对模拟的 ARMA(4,2) 序列数据做单位根检验
fUnitRoots::adfTest(100 + xarma42, lags=8, type="c")
##对模拟的 ARIMA(4,1,2) 序列数据检验：
fUnitRoots::adfTest(xarima412, lags=8, type="c")
##选项 type 选择基础模型，可以取：
##"nc"，表示没有漂移项或截距项；
##"c"，表示带有一个漂移项或截距项；
##"ct"，表示基础模型中带有 𝑎 + 𝑏𝑡 这样的线性项。
#########p900




##############################################
#######Chapter 34 统计学习介绍##########
install.packages('leaps')
install.packages('ISLR')
install.packages('ISLR')
library(leaps) # 全子集回归
library(ISLR) # 参考书对应的包
library(ISLR) # 岭回归和 lasso
install.packages('tree')
library(tree) # 树回归
library(randomForest) # 随机森林和装袋法
install.packages('gbm')
library(MASS)
library(gbm) # boosting
library(e1071) # svm

##Hitters 数据分析
names(Hitters)##查看数据集包含哪些变量
str(Hitters)##数据集的详细变量信息
##以 Salary 为因变量，查看其缺失值个数
sum( is.na(Hitters$Salary) )
##去掉有缺失值的观测
d <- na.omit(Hitters); dim(d)


##回归自变量选择
##最优子集选择
##先进行一个包含所有自变量的全集回归：
regfit.full <- regsubsets(Salary ~ ., data=d, nvmax=19)
reg.summary <- summary(regfit.full)
reg.summary
##比较这些最优模型的 BIC 值:
reg.summary$bic
plot(reg.summary$bic)
##其中̂p= 6,8的值相近,都很低,取̂p= 6,用coef()加id=6指定第六种子集：
coef(regfit.full, id=6)##选取 BIC 最小的自变量子集

##逐步回归方法
##在用lm()做了全集回归后,把全集回归结果输入到step()函数中可以执行逐步回归。
lm.full <- lm(Salary ~ ., data=d)
print(summary(lm.full))
lm.step <- step(lm.full)##把全集回归结果输入到step()函数中可以执行逐步回归
print(lm.step)


#划分训练集与测试集
set.seed(1)
train <- sample(nrow(d), size = round(nrow(d)/2))
test <- -train
##为了在测试集上用模型进行预报并估计预测均方误差,写一个预测函数:
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
##对每个子集大小,用最优子集在测试集上进行预报,计算均方误差：
regfit.best <- regsubsets( Salary ~ ., data=d[train,], nvmax=19 )
val.errors <- rep(as.numeric(NA), 19)
for(i in 1:19){
  #pred <- predict.regsubsets(regfit.best, newdata=d[test,], id=i)
  pred <- predict(regfit.best, newdata=d[test,], id=i)
  val.errors[i] <- mean( (d[test, 'Salary'] - pred)^2 )
}
print(val.errors)
best.id <- which.min(val.errors); best.id##用测试集得到的最优子集大小为 6
##模型子集和回归系数为:
coef(regfit.best, id=best.id)


##用 10 折交叉验证方法选择最优子集
##对数据中每一行分配一个折号：
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(d), replace=TRUE)
##对 10 折中每一折都分别当作测试集一次，得到不同子集大小的均方误差：
cv.errors <- matrix( as.numeric(NA), k, 19, dimnames=list(NULL, paste(1:19)) )
for(j in 1:k){ # 对
  best.fit <- regsubsets(Salary ~ ., data=d[folds != j,], nvmax=19)
  for(i in 1:19){
    pred <- predict( best.fit, d[folds==j,], id=i)
    cv.errors[j, i] <- mean( (d[folds==j, 'Salary'] - pred)^2 )
  }
}
head(cv.errors)
##对每列的 10 个元素求平均，可以得到每个子集大小的平均均方误差
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
best.id <- which.min(mean.cv.errors)
plot(mean.cv.errors, type='b')##找到的最优子集大小是 10
##对全数据集重新建模但是选择最优子集大小为10
reg.best <- regsubsets(Salary ~ ., data=d, nvmax=19)
coef(reg.best, id=best.id)


##岭回归
install.packages('glmnet')
library(glmnet)
library(Matrix)
x <- model.matrix(Salary ~ ., d)[,-1]
y <- d$Salary
##设定岭回归中lambda参数
grid <- 10^seq(10, -2, length=100)
##用所有数据针对这样的调节参数网格计算岭回归结果
##glmnet() 函数默认对数据进行标准化
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))##coef() 的结果是一个矩阵，每列对应一个调节参数值。


##划分训练集与测试集
##把数据分为一半训练、一半测试：
set.seed(1)
train <- sample(nrow(x), size = nrow(x)/2)
test <- (-train)
y.test <- y[test]
##仅用测试集建立岭回归：
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1E-12)
##用建立的模型对测试集进行预测，并计算调节参数等于 4 时的均方误差：
##𝜆 称为调节参数，𝜆 越大，相当于模型复杂度越低。
ridge.pred <- predict( ridge.mod, s=4, newx=x[test,] )
mean( (ridge.pred - y.test)^2 )
##如果用因变量平均值作预测，这是最差的预测：
mean( (mean(y[train]) - y.test)^2 )


##用 10 折交叉验证选取调节参数
##仍使用训练集，但训练集再进行交叉验证。cv.glmnet() 函数可以执行交叉验证。
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min;bestlam##获得最优调节参数 𝜆 = 326.0827885
##用最优调节参数对测试集作预测，得到预测均方误差：
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean( (ridge.pred - y.test)^2 )##结果比 𝜆 = 4 略有改进。
##用选取的最优调节系数对全数据集建模，得到相应的岭回归系数估计
out <- glmnet(x, y, alpha=0)
predict(out, type='coefficients', s=bestlam)[1:20,]


##Lasso 回归
##使用glmnet()函数计算Lasso回归,指定一个调节参数网格(沿用前面的网格)
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)##对 lasso 结果使用 plot() 函数可以绘制延调节参数网格变化的各回归系数估计，横坐标不是调节参数而是调节参
##数对应的系数绝对值和,可以看出随着系数绝对值和增大，实际是调节参数变小，更多地自变量进入模型。
##用交叉验证估计调节参数
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)##执行交叉验证
plot(cv.out)
bestlam <- cv.out$lambda.min; bestlam
##得到调节参数估计后，对测试集计算预测均方误差：
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean( (lasso.pred - y.test)^2 )
##为了充分利用数据，使用前面获得的最优调节参数，对全数据集建模：
out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type='coefficients', s=bestlam)[1:20,]; lasso.coef
lasso.coef[lasso.coef != 0]##选择的自变量子集有 11 个自变量。

##树回归的简单演示
d <- na.omit(Hitters[,c('Salary', 'Years', 'Hits')])
print(str(d))
##建立完整的树:
tr1 <- tree(log(Salary) ~ Years + Hits, data=d)
tr1b <- prune.tree(tr1, best=3)##剪枝为只有3个叶结点
print(tr1b)##显示树
print(summary(tr1b))##显示概括
##做树图:
plot(tr1b); text(tr1b, pretty=0)
##树回归
d <- na.omit(Hitters)
set.seed(1)
##把数据随机地分成一半训练集，一半测试集
train <- sample(nrow(d), size=round(nrow(d)/2))
test <- (-train)
##对训练集，建立未剪枝的树：
tr1 <- tree(log(Salary) ~ ., data=d, subset=train)
plot(tr1); text(tr1, pretty=0)
##对训练集上的未剪枝树用交叉验证方法寻找最优大小：
cv1 <- cv.tree(tr1)
print(cv1)
plot(cv1$size, cv1$dev, type='b')
best.size <- cv1$size[which.min(cv1$dev)[1]]##最优大小为 7
abline(v=best.size, col='gray')
##获得训练集上构造的树剪枝后的结果：
best.size <- 4
tr1b <- prune.tree(tr1, best=best.size)
##在测试集上计算预测均方误差:
pred.test <- predict(tr1b, newdata=d[test,])
test.mse <- mean( (d[test, 'Salary'] - exp(pred.test))^2 )
test.mse
##如果用训练集的因变量平均值估计测试集的因变量值，均方误差为:
worst.mse <- mean( (d[test, 'Salary'] - mean(d[train, 'Salary']))^2 )
worst.mse
##用所有数据来构造未剪枝树：
tr2 <- tree(log(Salary) ~ ., data=d)
##用训练集上得到的子树大小剪枝：
tr2b <- prune.tree(tr2, best=best.size)
plot(tr2b); text(tr2b, pretty=0)


##装袋法
##randomForest() 函数实际是随机森林法，但是当 mtry 的值取为所有自变量个数时就是装袋法。
##对训练集用装袋法：
bag1 <- randomForest(log(Salary) ~ ., data=d, subset=train, mtry=ncol(d)-1, importance=TRUE)
bag1
##对测试集进行预报:
pred2 <- predict(bag1, newdata=d[test,])
test.mse2 <- mean( (d[test, 'Salary'] - exp(pred2))^2 )
test.mse2##结果与剪枝过的单课树相近。
##在全集上使用装袋法：
bag2 <- randomForest(log(Salary) ~ ., data=d, mtry=ncol(d)-1, importance=TRUE)
bag2
##变量的重要度数值和图形：各变量的重要度数值及其图形：
importance(bag2)
varImpPlot(bag2)


##随机森林
##对训练集用随机森林法：
rf1 <- randomForest(log(Salary) ~ ., data=d, subset=train, importance=TRUE)
rf1
##对测试集进行预报:
pred3 <- predict(rf1, newdata=d[test,])
test.mse3 <- mean( (d[test, 'Salary'] - exp(pred3))^2 )
test.mse3
##在全集上使用随机森林
rf2 <- randomForest(log(Salary) ~ ., data=d, importance=TRUE)
rf2
##各变量的重要度数值及其图形：
importance(rf2)
varImpPlot(rf2)


## Heart 数据分析
##Heart 数据是心脏病诊断的数据，因变量 AHD 为是否有心脏病，试图用各个自变量预测（判别）。
##读入 Heart 数据集，并去掉有缺失值的观测：
Heart <- read.csv(
  'E:\\00study\\MJ\\Rlearning\\R.DATA\\Heart.csv', header=TRUE, row.names=1,
  stringsAsFactors=TRUE,fileEncoding="GBK")
Heart <- na.omit(Heart)
str(Heart)
t(summary(Heart))
##树回归
##划分训练集与测试集
set.seed(1)
train <- sample(nrow(Heart), size=round(nrow(Heart)/2))
test <- (-train)
test.y <- Heart[test, 'AHD']
##在训练集上建立未剪枝的判别树:
tr1 <- tree(AHD ~ ., data=Heart[train,])
plot(tr1); text(tr1, pretty=0)
##适当剪枝
##用交叉验证方法确定剪枝保留的叶子个数，剪枝时按照错判率执行：
cv1 <- cv.tree(tr1, FUN=prune.misclass)
cv1
plot(cv1$size, cv1$dev, type='b', xlab='size', ylab='dev')
best.size <- cv1$size[which.min(cv1$dev)];best.size
##最优的大小是 12。但是从图上看，4 个叶结点已经足够好，所以取为 4
##对训练集生成剪枝结果：
best.size <- 4
tr1b <- prune.misclass(tr1, best=best.size)
plot(tr1b); text(tr1b, pretty=0)
##对测试集计算误判率
pred1 <- predict(tr1b, Heart[test,], type='class')
tab1 <- table(pred1, test.y); tab1
test.err <- (tab1[1,2]+tab1[2,1])/sum(tab1[]); test.err##对测试集的错判率约 26%。
##利用未剪枝的树对测试集进行预测, 一般比剪枝后的结果差:
pred1a <- predict(tr1, Heart[test,], type='class')
tab1a <- table(pred1a, test.y); tab1a
test.err1a <- (tab1a[1,2]+tab1a[2,1])/sum(tab1a[]); test.err1a
##利用全集数据建立剪枝判别树
tr2 <- tree(AHD ~ ., data=Heart)
tr2b <- prune.misclass(tr2, best=best.size)
plot(tr2b); text(tr2b, pretty=0)

##用装袋法
##对训练集用装袋法：
bag1 <- randomForest(AHD ~ ., data=Heart, subset=train, mtry=13, importance=TRUE)
bag1
##对测试集进行预报:
pred2 <- predict(bag1, newdata=Heart[test,])
tab2 <- table(pred2, test.y); tab2
test.err2 <- (tab2[1,2]+tab2[2,1])/sum(tab2[]); test.err2
##对全集用装袋法
bag1b <- randomForest(AHD ~ ., data=Heart, mtry=13, importance=TRUE)
bag1b
##各变量的重要度数值及其图形：
importance(bag1b)
varImpPlot(bag1b)

##用随机森林
##对训练集用随机森林法：
rf1 <- randomForest(AHD ~ ., data=Heart, subset=train, importance=TRUE)
rf1##这里 mtry 取缺省值，对应于随机森林法。
##对测试集进行预报:
pred3 <- predict(rf1, newdata=Heart[test,])
tab3 <- table(pred3, test.y); tab3
test.err3 <- (tab3[1,2]+tab3[2,1])/sum(tab3[]); test.err3
##对全集用随机森林:
rf1b <- randomForest(AHD ~ ., data=Heart, importance=TRUE)
rf1b
##各变量的重要度数值及其图形：
importance(rf1b)
varImpPlot(rf1b)

##汽车销量数据分析
d <- na.omit(Carseats)
d$High <- factor(ifelse(d$Sales > 8, 'Yes', 'No'))
dim(d)
##判别树
##全体数据的判别树
##对全体数据建立未剪枝的判别树:
tr1 <- tree(High ~ . - Sales, data=d)
summary(tr1)
plot(tr1)
text(tr1, pretty=0)
##划分训练集和测试集
##把输入数据集随机地分一半当作训练集，另一半当作测试集：
set.seed(2)
train <- sample(nrow(d), size=round(nrow(d)/2))
test <- (-train)
test.high <- d[test, 'High']
##用训练数据建立未剪枝的判别树:
tr2 <- tree(High ~ . - Sales, data=d, subset=train)
summary(tr2)
plot(tr2)
text(tr2, pretty=0)
##用未剪枝的树对测试集进行预测，并计算误判率：
pred2 <- predict(tr2, d[test,], type='class')
tab <- table(pred2, test.high); tab
test.err2 <- (tab[1,2] + tab[2,1]) / sum(tab[]); test.err2
##用交叉验证确定训练集的剪枝
set.seed(3)
cv1 <- cv.tree(tr2, FUN=prune.misclass)
cv1
plot(cv1$size, cv1$dev, type='b')
best.size <- cv1$size[which.min(cv1$dev)];best.size
##用交叉验证方法自动选择的最佳树大小为 21。
##剪枝:
tr3 <- prune.misclass(tr2, best=best.size)
summary(tr3)
plot(tr3)
text(tr3, pretty=0)
##用剪枝后的树对测试集进行预测，计算误判率：
pred3 <- predict(tr3, d[test,], type='class')
tab <- table(pred3, test.high); tab
test.err3 <- (tab[1,2] + tab[2,1]) / sum(tab[]); test.err3
##随机森林
##对训练集用随机森林法：
rf4 <- randomForest(High ~ . - Sales, data=d, subset=train, importance=TRUE)
rf4##
##对测试集进行预报:
pred4 <- predict(rf4, newdata=d[test,])
tab <- table(pred4, test.high); tab
##对测试集进行预报:
pred4 <- predict(rf4, newdata=d[test,])
tab <- table(pred4, test.high); tab
test.err4 <- (tab[1,2]+tab[2,1])/sum(tab[]); test.err4
##对全集用随机森林:
rf5 <- randomForest(High ~ . - Sales, data=d, importance=TRUE)
rf5
##各变量的重要度数值及其图形：
importance(rf5)
varImpPlot(rf5)

##波士顿郊区房价数据
##把缺失值去掉后存入数据集 d:
d <- na.omit(Boston)
##数据集概况：
str(d)
summary(d)

##回归树
##划分训练集和测试集
set.seed(1)
train <- sample(nrow(d), size=round(nrow(d)/2))
test <- (-train)
##对训练集建立未剪枝的树：
tr1 <- tree(medv ~ ., d, subset=train)
summary(tr1)
plot(tr1)
text(tr1, pretty=0)
##用未剪枝的树对测试集进行预测，计算均方误差：
yhat <-predict(tr1, newdata=d[test,])
mse1 <- mean((yhat - d[test, 'medv'])^2)
mse1
##用交叉验证方法确定剪枝复杂度
cv1 <- cv.tree(tr1)
plot(cv1$size, cv1$dev, type='b')
best.size <- cv1$size[which.min(cv1$dev)]; best.size
##剪枝并对测试集进行预测：
tr2 <- prune.tree(tr1, best=best.size)
plot(tr2)
text(tr2, pretty=0)
yhat <-predict(tr2, newdata=d[test,])
mse2 <- mean((yhat - d[test, 'medv'])^2)
mse2##剪枝后效果没有改善。

##装袋法
set.seed(1)
bag1 <- randomForest(
  medv ~ ., data=d, subset=train,
  mtry=ncol(d)-1, importance=TRUE)
bag1
##在测试集上计算装袋法的均方误差：
yhat <- predict(bag1, newdata=d[test,])
mean( (yhat - d[test, 'medv'])^2 )##比单棵树的结果有明显改善。
##随机森林
##对训练集计算
set.seed(1)
rf1 <- randomForest(
  medv ~ ., data=d, subset=train,
  importance=TRUE)
rf1
##在测试集上计算随机森林法的均方误差：
yhat <- predict(rf1, newdata=d[test,])
mean( (yhat - d[test, 'medv'])^2 )
##比单棵树的结果有明显改善, 比装袋法的结果也好一些。
##各变量的重要度数值及其图形：
importance(rf1)
varImpPlot(rf1)

##提升法
set.seed(1)
bst1 <- gbm(medv ~ ., data=d[train,], distribution='gaussian', n.trees=5000, interaction.depth=4)
summary(bst1)##lstat 和 rm 是最重要的变量。
##在测试集上预报，并计算均方误差：
yhat <- predict(bst1, newdata=d[test,], n.trees=5000)
mean( (yhat - d[test, 'medv'])^2 )
##与随机森林方法结果相近。
##如果提高学习速度：
bst2 <- gbm(medv ~ ., data=d[train,], distribution='gaussian', n.trees=5000, interaction.depth=4, shrinkage=0.2)
yhat <- predict(bst2, newdata=d[test,], n.trees=5000)
mean( (yhat - d[test, 'medv'])^2 )##均方误差有改善。



##支持向量机方法
##共 297 个观测，随机选取其中 207 个作为训练集，90 个作为测试集。
set.seed(1)
Heart<-read.csv(
  'E:\\00study\\MJ\\Rlearning\\R.DATA\\Heart.csv', header=TRUE, row.names=1,
  stringsAsFactors=TRUE,fileEncoding="GBK")
d <- na.omit(Heart)
train <- sample(nrow(d), size=207)
test <- -train
d[["AHD"]] <- factor(d[["AHD"]], levels=c("No", "Yes"))
##定义一个错判率函数:
classifier.error <- function(truth, pred){
  tab1 <- table(truth, pred)
  err <- 1 - sum(diag(tab1))/sum(c(tab1))
  err
}
##线性的 SVM
##先随便取调节参数 cost=1 试验支持向量判别法：
res.svc <- svm(AHD ~ ., data=d[train,], kernel="linear", cost=1, scale=TRUE)
fit.svc <- predict(res.svc)
summary(res.svc)
##计算拟合结果并计算错判率：
tab1 <- table(truth=d[train,"AHD"], fitted=fit.svc); tab1
cat("SVC 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##e1071 函数提供了 tune() 函数，可以在训练集上用十折交叉验证选择较好的调节参数。
set.seed(101)
res.tune <- tune(svm, AHD ~ ., data=d[train,], kernel="linear", scale=TRUE,
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(res.tune)
##找到的最优调节参数为 0.1，可以用 res.tune$best.model 获得对应于最优调节参数的模型：
summary(res.tune$best.model)
##在测试集上测试：
pred.svc <- predict(res.tune$best.model, newdata=d[test,])
tab1 <- table(truth=d[test,"AHD"], predict=pred.svc); tab1
cat("SVC 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")

##多项式核 SVM
res.svm1 <- svm(AHD ~ ., data=d[train,], kernel="polynomial",
                order=2, cost=0.1, scale=TRUE)
fit.svm1 <- predict(res.svm1)
summary(res.svm1)
tab1 <- table(truth=d[train,"AHD"], fitted=fit.svm1); tab1
cat("2 阶多项式核 SVM 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##尝试找到调节参数 cost 的最优值：
set.seed(101)
res.tune2 <- tune(svm, AHD ~ ., data=d[train,], kernel="polynomial",
                  order=2, scale=TRUE,
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(res.tune2)
fit.svm2 <- predict(res.tune2$best.model)
tab1 <- table(truth=d[train,"AHD"], fitted=fit.svm2); tab1
cat("2 阶多项式核最优参数 SVM 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##看这个最优调节参数的模型在测试集上的表现：
pred.svm2 <- predict(res.tune2$best.model, d[test,])
tab1 <- table(truth=d[test,"AHD"], predict=pred.svm2); tab1
cat("2 阶多项式核最优参数 SVM 测试集错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##在测试集上的表现与线性方法相近。

##径向核 SVM
##径向核需要的参数为 𝛾 值。取参数 gamma=0.1。
res.svm3 <- svm(AHD ~ ., data=d[train,], kernel="radial",
                gamma=0.1, cost=0.1, scale=TRUE)
fit.svm3 <- predict(res.svm3)
summary(res.svm3)
tab1 <- table(truth=d[train,"AHD"], fitted=fit.svm3); tab1
cat(" 径向核（gamma=0.1, cost=0.1）SVM 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##选取最优 cost, gamma 调节参数：
set.seed(101)
res.tune4 <- tune(svm, AHD ~ ., data=d[train,], kernel="radial",
                  scale=TRUE,
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                              gamma=c(0.1, 0.01, 0.001)))
summary(res.tune4)
fit.svm4 <- predict(res.tune4$best.model)
tab1 <- table(truth=d[train,"AHD"], fitted=fit.svm4); tab1
cat(" 径向核最优参数 SVM 错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##看这个最优调节参数的模型在测试集上的表现：
pred.svm4 <- predict(res.tune4$best.model, d[test,])
tab1 <- table(truth=d[test,"AHD"], predict=pred.svm2); tab1
cat(" 径向核最优参数 SVM 测试集错判率：", round((tab1[1,2] + tab1[2,1])/ sum(c(tab1)), 2), "\n")
##与线性方法结果相近。



##############################################
#######Chapter 35 随机模拟##########
##随机数
round(runif(5), 2)##产生5个标准均匀分布随机数
round(rnorm(5), 2)##产生5个标准正态分布随机数

##sample()函数--从一个有限集合中无放回或有放回地随机抽取，产生随机结果
##size 指定抽样个数,prob指定每个值的概率,replace=TRUE说明是有放回抽样
sample(c('正面', '反面'), size=10,
       prob=c(0.7, 0.3), replace=TRUE)##产生X的10个随机抽样值
##如果要做无放回等概率的随机抽样，可以不指定prob和replace(缺省是FALSE)
sample(1:10, size=4)
##从1:n中等概率无放回随机抽样直到每一个都被抽过
sample(10)##返回1:10的一个重排

##随机模拟示例
##估计期望值
est_pi <- function(N){
  set.seed(101)
  x1 <- runif(N, 0, 1)
  x2 <- runif(N, 0, 1)
  y <- as.numeric(x1^2 + x2^2 <= 1)
  hat_pi <- 4*mean(y)
  se <- 4 * sd(y) / sqrt(N)
  cat("N = ", N, " pi 估计值 =", hat_pi, " SE =", se, "\n")
  invisible(list(N=N, hat_pi = hat_pi, SE = se))
}
est_pi(1E4)##N=1000估计的标准误差较大，增大样本量
est_pi(1E6)##N=1000000,精度有所提升

##线性回归模拟
n <- 10; a <- 10; b <- 2
x <- sample(1:10, size=n, replace=TRUE)##自变量x用随机数产生
eps <- rnorm(n, 0, 0.5)
y <- a + b * x + eps
lm(y ~ x)
summary(lm(y ~ x))##计算线性回归的多种统计量
summary(lm(y ~ x))$coefficients##返回一个矩阵,包括a,b的估计值、标准误差、t检验统计量、检验p值
c(summary(lm(y ~ x))$coefficients[,1:2])##把上述矩阵的前两列拉直成一个向量返回：
##执行多次模拟
reg.sim <- function(
    a=10, b=2, sigma=0.5,
    n=10, B=1000){
  set.seed(1)
  resm <- replicate(B, {
    x <- sample(1:10, size=n, replace=TRUE)
    eps <- rnorm(n, 0, 0.5)
    y <- a + b * x + eps
    c(summary(lm(y ~ x))$coefficients[,1:2])
  })
  resm <- t(resm)
  colnames(resm) <- c('a', 'b', 'SE.a', 'SE.b')
  cat(B, '次模拟的平均值:\n')
  print( apply(resm, 2, mean) )
  cat(B, '次模拟的标准差:\n')
  print( apply(resm, 2, sd) )
}
set.seed(1)
reg.sim()

##核密度的bootstrap置信区间?????????
x <- faithful$eruptions
est0 <- density(x)
plot(est0)



##############################################
#######Chapter 36 R语言的文本处理##########
##简单的文本处理
##字符型常量与字符型向量
##R的字符型向量每个元素是一个字符串
s <- c("123", "abc", " 张三李四", "@#$%^&")
s
##字符串连接、重复
library(stringr)
str_c(c("x", "y"), c("a", "b"), sep="*")
str_c("data", 1:3, ".txt")
## collapse 选项要求将连接后的字符型向量的所有元素连接在一起
##collapse 的值为将多个元素合并时的分隔符
str_c(c("a", "bc", "def"), collapse="---")
str_c("data", 1:3, ".txt", sep="", collapse=";")
##collapse 默认值是空字符串
str_flatten(c("a", "bc", "def"), collapse="---")
str_flatten(c("a", "bc", "def"))
paste(c("x", "y"), c("a", "b"), sep="*")
paste("data", 1:3, ".txt", sep="")
paste0("data", 1:3, ".txt")
paste(c("a", "bc", "def"), collapse="---")
paste("data", 1:3, ".txt", sep="", collapse=";")


##stringr::str_dup(string, times)可以将字符型向量的元素按照 times 指定的次数在同一字符串内重复
str_dup(c("abc", " 长江"), 3)
##也可以针对每个元素指定不同重复次数，如
str_dup(c("abc", " 长江"), c(3, 2))


##格式化输出
as.character(1.000)
as.character(1.2)
as.character(1.23)
format(c(1.000, 1.2, 1.23))##将一个数值型向量的各个元素按照统一格式转换为字符型
##nsmall 控制非科学记数法显示时小数点后的至少要有的位数
##digits 控制至少要有的有效位数
format(c(pi, pi*10000), digits=8, nsmall=4)
##width 参数指定至少要有的输出宽度，不足时默认在左侧用空格填充
format(1.000, width=6, nsmall=2)

##sprintf() 函数
##把一个元素或一个向量的各个元素按照 C 语言输出格式转换为字符型向量
##%6.2f 表示输出宽度为 6、宽度不足时左填空格、含两位小数的实数
sprintf("%6.2f", pi)
##%03d 表示输出宽度为3、不够左填0的整数
sprintf("tour%03d.jpg", c(1, 5, 10, 15, 100))
##支持多个向量同时转换
sprintf("%1dx%1d=%2d", 1:5, 5:1, (1:5)*(5:1))

##字符串插值函数
##stringr::str_glue() 和 stringr::str_glue_data() 
##只要在字符串内用大括号写变量名，则函数可以将字符串内容中的变量名替换成变量值
name <- " 李明"
tele <- "13512345678"
str_glue(" 姓名: {name}\n电话号码: {tele}\n")
##可以输入多个字符串作为自变量，内容自动连接在一起，可以用参数.sep 指定分隔符
name <- " 李明"
tele <- "13512345678"
str_glue(" 姓名: {name}, ", " 电话号码: {tele}")
str_glue(" 姓名: {name}", " 电话号码: {tele}", .sep="; ")
##可以直接在 str_glue() 中指定变量值
str_glue(" 姓名: {name}", " 电话号码: {tele}", .sep="; ",
         name = " 张三", tele = "13588888888")
##stringr::str_glue_data() 则以一个包含变量定义的对象.x 为第一自变量，类型可以是环境、列表、数据框等。
str_glue_data(list(name = " 王五", tele = "13500000000"),
              " 姓名: {name}", " 电话号码: {tele}", .sep="; ")

##字符串长度
##stringr::str_length(string) 求字符型向量 string 每个元素的长度。一个汉字长度为 1。
str_length(c("a", "bc", "def", "北京"))
##函数 nchar(text) 计算字符串长度，默认按照字符个数计算而不是按字节数计算，如
nchar(c("a", "bc", "def", "北京"))
nchar(c("a", "bc", "def", "北京"), type="bytes")##加选项 type="bytes" 可用按字符串占用的字节数计算

##取子串
str_sub("term2017", 5, 8)
str_sub(c("term2017", "term2018"), 5, 8)
str_sub("term2017", 5)##默认结束位置为最后一个字符
str_sub("term2017", -4, -1)##用负数表示倒数位置
str_sub("term2017", end=4)##默认开始位置为 1
##取子串时，一般按照字符个数计算位置，如
str_sub(" 北京市海淀区颐和园路 5 号", 4, 6)
##当起始位置超过总长度或结束位置超过第一个字符时返回空字符串；当起始位置超过结束位置是返回空字符串。
str_sub("term2017", 9)
str_sub("term2017", 1, -9)
str_sub("term2017", 8, 5)
##可以对 str_sub() 结果赋值，表示修改子串内容，如：
s <- "term2017"
str_sub(s, 5, 8) <- "18"
s
##substring() 对三个参数 text, first, last 都是向量化的，长度不一致时按照一般的不等长向量间运算规则处理
substring(c("term2017", "term2018"), first=c(1, 5), last=c(4, 8))
substring("term2017", first=c(1, 5), last=c(4, 8))
##substring() 也允许修改某个字符串的指定子串的内容
s <- "123456789"
substring(s, 3, 5) <- "abc"
s

##字符串变换
##大小写p1023
##字符变换表
##基本 R 的 chartr(old, new, x) 函数指定一个字符对应关系，旧字符在 old 中，新字符在 new 中，x 是一个要进行替换的字符型向量。
chartr("!;", ".,", c("Hi;boy!","How do you do!"))
chartr("。,; 县", ".,; 区", " 昌平县,大兴县;固安县。")
##空白处理
##stringr::str_trim(string, side) 返回删去字符型向量 string 每个元素的首尾空格的结果
##可以用side指定删除首尾空格（"both"）、开头空格（"left"）、末尾空格（"right"）。
str_trim(c(" 李明", " 李明 ", " 李明 ", " 李 明"))
str_trim(c(" 李明", " 李明 ", " 李明 ", " 李 明"), side="left")
str_trim(c(" 李明", " 李明 ", " 李明 ", " 李 明"), side="right")
##stringr::str_squish(string) 对字符型向量 string 每个元素，删去首尾空格，将重复空格变成单个，返回变换后的结果。
str_squish(c(" 李明", " 李明 ", " 李明 ", " 李 明"))
##基本 R 函数 trimws(x, which) 与 str_trim() 作用类似
##选项 which="left" 可以仅删去开头的空格，选项which="right" 可以仅删去结尾的空格。
trimws(c(" 李明", " 李明 ", " 李明 ", " 李 明"))
trimws(c(" 李明", " 李明 ", " 李明 ", " 李 明"), which="left")
trimws(c(" 李明", " 李明 ", " 李明 ", " 李 明"), which="right")
##为了去掉输入字符串中所有空格，可以用 gsub() 替换功能，如：
gsub(" ", "", c(" 李明", " 李明 ", " 李明 ", " 李 明"), fixed=TRUE)
##stringr::str_pad(string, width) 可以将字符型向量 string 的每个元素加长到 width 个字符
##不足时左补空格，已经达到或超过 width 的则不变，如：
str_pad(c("12", "1234"), 3)

##简单匹配与查找
##开头和结尾匹
##基本 R 的 startsWith(x, prefix) 可以判断字符型向量 x 的每个元素是否以 prefix 开头，结果为一个与 x长度相同的逻辑型向量。
startsWith(c("xyz123", "tu004"), "tu")
##endsWith(x, suffix) 可以判断字符型向量 x 的每个元素是否以 suffix 结尾，如
endsWith(c("xyz123", "tu004"), "123")
##中间匹配
grepl("our", c("flavor", "tournament"), fixed=TRUE)

##字符串替换
##用 gsub(pattern, replacement, x, fixed=TRUE) 把字符型向量 x 中每个元素中出现的子串 pattern 都替换为 replacement
gsub("the", "**",
     c("New theme", "Old times", "In the present theme"),
     fixed=TRUE)
##设有些应用程序的输入要求使用逗号 “,” 分隔，但是用户可能输入了中文逗号 “，”，就可以用 gsub() 来替换：
x <- c("15.34,14.11", "13.25，16.92")
x <- gsub("，", ",", x, fixed=TRUE); x

##字符串拆分
x <- c("11,12", "21,22,23", "31,32,33,34")
res1 <- str_split(x, fixed(","))
res1
##str_split() 可以用选项 n 指定仅拆分出成几项，最后一项合并不拆分，如：
x <- c("11,12", "21,22,23", "31,32,33,34")
res2 <- str_split(x, fixed(","), n=2)
res2
##拆分的结果可以用 lapply(), sapply()，vapply() 等函数处理。例如，将每个元素的拆分结果转换成数值型：
lapply(res1, as.numeric)
##可以用 unlist() 函数将列表中的各个向量连接成一个长向量，如：
unlist(res1)
##注意，即使输入只有一个字符串，str_split() 的结果也是列表，所以输入只有一个字符串时我们应该取出结果列表的第一个元素，如
strsplit("31,32,33,34", split=",", fixed=TRUE)[[1]]
##如果确知每个字符串拆分出来的字符串个数都相同，可以用 stringr::str_split_fixed()
##用参数 n 指定拆出来的项数，这时结果为一个字符型矩阵，原来的每个元素变成结果中的一行：
x <- c("11,12", "21,22", "31,32")
res3 <- str_split_fixed(x, fixed(","), n=2)
res3
##基本 R 的 strsplit(x,split,fixed=TRUE) 可以把字符型向量 x 的每一个元素按分隔符 split 拆分为一个字符型向量
##strsplit 的结果为一个列表，每个列表元素对应于 x 的每个元素。
x <- c("11,12", "21,22,23", "31,32,33,34")
res4 <- strsplit(x, split=",", fixed=TRUE)
res4

##文本文件读写p1029

























##p1076
##数据处理p1137
##小题分题型分数汇总
##读入此数据为 R 数据框，只要用如下程序:
dm <- read.csv('E:\\00study\\\\Rlearning\\R.DATA\\subscore-subtype.csv',header=TRUE,stringsAsFactors=FALSE,fileEncoding="GBK")
##结果显示如下:
knitr::kable(dm)
##用如下 R 程序读入小题分数据为 R 数据框
ds <- read.csv('E:\\00study\\\MJ\Rlearning\\R.DATA\\subscore-subscore.csv', header=TRUE,
               stringsAsFactors=FALSE,fileEncoding="GBK")
##结果显示如下:
knitr::kable(ds)
##汇总计算每个学生的题型分
resm <- data.frame(
  '学号'=ds[,'学号'],
  '选择题'= rowSums(ds[, paste('Y', 1:10, sep='')]),
  '简答题'=rowSums(ds[,paste('Y', c(11,12,14:17), sep='' )]),
  '填空题'=ds[,'Y13'],
  '作文'=ds[,'Y18']
)
knitr::kable(resm[order(resm[,'学号']),], row.names=FALSE)

##类别编号重排
dc0 <- tibble::tibble(
  obs = 1:10,
  g = c(3, 1, 3, 1, 2, 2, 1, 3, 3, 1)
)
knitr::kable(dc0)
##设对每个类进行概括统计得到了有代表性的统计量，结果如下：
dcstat <- tibble::tibble(
  g = 1:3,
  stat=c(2.3, 1.1, 3.0)
)
knitr::kable(dcstat)





















