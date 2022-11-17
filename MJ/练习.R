#######随机数生成
set.seed(1)
x1 <- runif(20,1,2);x1##均匀分布
x2 <- rbinom(20,10,0.9);x2##二项分布
x3 <- rnorm(20,2,1);x3##正态分布
x4 <- rexp(100,1/5);x4##指数分布,第二个参数是均值倒数
x5 <- rchisq(100,2);x5##卡方分布(df)
x6 <- rt(100,2);x6##t分布
x7 <- rf(100,1,2);x7##F分布
x8 <- rgeom(100,0.5);x8##几何分布
x9 <- rhyper(100,30,20,20);x9##超几何分布


#######随机抽样
sample(100)
sample(1:100,50,replace = T)
Y <- sample(c('饼干','曲奇','奶酥'),100,prob = c(0.5,0.4,0.1),replace = T)
table(Y)


#######dplyr包数据整理
library(tidyverse)
d.class <- readr::read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
##随机抽取
set.seed(1)
d.class %>%  
  sample_n(5,replace = T) %>% 
  knitr::kable()
##去重
d.class %>% 
  distinct(sex,age,.keep_all = T) %>% 
  knitr::kable()
##去重
d.class %>% 
  distinct(age,sex) %>% 
  nrow()
##提取列子集
vars <- c("name","age","sex")
d.class %>% 
  select(one_of(vars)) %>% 
  knitr::kable()
##向量元素合并
d.class %>% 
  pull("name") %>% 
  paste(collapse = ':')
##排序(降序)
d.class %>% 
  arrange(desc(weight),sex) %>% 
  knitr::kable()
##排序
d.class %>% 
  arrange(weight,sex) %>% 
  knitr::kable()
##排序
d.class %>% 
  arrange(sex,weight) %>% 
  knitr::kable()
##修改变量名
d.class %>% 
  rename(id=name) %>% 
  knitr::kable()
##提取行子集
d.class %>% 
  filter(sex=="M") %>% 
  knitr::kable()
##提取行子集
d.class %>% 
  head(5) %>% 
  knitr::kable()
##提取行子集
d.class %>% 
  slice(2:7) %>% 
  knitr::kable()
##新增列(含原先列)
d.class %>% 
  mutate(
    iden = ifelse(age<15,'小','大'),
    rwh = weight/height
  ) %>% 
  knitr::kable()
##新增列
d.class %>% 
  mutate(
    sexc={
      x <- rep('男',length(sex))
      x[sex=='F'] <- "女"
      x
    }) %>% 
  head(3) %>% 
  knitr::kable()
##新增列(不含原先列)
d.class %>% 
  transmute(
    rwh=weight/height,
    iden=ifelse(age<15,'小','大')
  ) %>% 
  head(4) %>% 
  knitr::kable()
##符号.
d.class %>% 
  lm(weight~height,data = .) %>% 
  coef()

##宽表变长表
d.dwide1 <- readr::read_csv('E:\\00study\\MJ\\Rlearning\\R.DATA\\dwide1.csv')
d.dwide1 %>% 
  pivot_longer(
    `1`:`4`,
    names_to = 'id',
    values_to = 'value',
    values_drop_na = T
  ) %>% 
  knitr::kable()

with(
  d.cancer,
  tapply(v1, sex, mean)
)

aggregate(d.cancer[,c('age','v0','v1')],
          list(sex=d.cancer$sex),
          mean,na.rm=T)


#######统计汇总
library(tidyverse)
d.cancer <- readr::read_csv('E:\\00study\\MJ\\Rlearning\\R.DATA\\cancer.csv',locale = locale(encoding = 'GBK'))
head(5,data=d.cancer)
d.cancer %>% 
  filter(sex=='F') %>% 
  drop_na() %>% 
  summarise(
    nob=n(),
    meanv0.F=mean(v0),
    sdv1.F=mean(v1)
  ) %>% 
  knitr::kable()
##汇总
d.cancer %>% 
  filter(sex=='F') %>% 
  summarise(
    nob=n(),
    n=sum(!is.na(age)),
    mean.v0=mean(v0,na.rm=T)
  ) %>% 
  knitr::kable()
##汇总
d.cancer %>% 
  summarise(
    mean.age=mean(age,na.rm=T),
    mean.v0=mean(v0,na.rm=T),
    mean.v1=mean(v1,na.rm=T)
  ) %>% 
  knitr::kable()
##多变量汇总
d.cancer %>% 
  drop_na() %>% 
  summarise_at(
    c('age','v0','v1'),
    list(avg=~mean(.),sd=~sd(.)),
    na.rm=TRUE
  ) %>% 
  knitr::kable()
##序号表示变量子集
d.cancer %>% 
  summarise_at(
    5:6,
    list(avg=~mean(.),std=~sd(.),var=~var(.)),
    na.rm=T
  ) %>% 
  knitr::kable()
##vars()表示变量子集
d.cancer %>% 
  summarise_at(
    vars <- c('v0','v1'),
    list(avg=~mean(.),std=~sd(.),var=~var(.),med=~median(.)),
    na.rm=T
  ) %>% 
  knitr::kable()

##
d.cancer %>% 
  summarise_if(
    is.numeric,
    list(avg=~mean(.),sd=~sd(.)),
    na.rm=T
  ) %>% 
  knitr::kable()

d.cancer %>% 
  select(5:6) %>% 
  summarise_all(
    list(avg=~mean(.),sd=~sd(.)),
    na.rm=T
  ) %>% 
  knitr::kable()

##分组函数group_by()
d.cancer %>% 
  drop_na() %>% 
  group_by(sex) %>% 
  summarise_at(
    c('v0','v1'),
    list(avg=~mean(.),std=~sd(.)),
    na.rm=T
  ) %>% 
  knitr::kable()
##分组汇总
d.cancer %>% 
  drop_na() %>%
  group_by(sex) %>% 
  summarise(
    count=n(),
    meanv0=mean(v0),
    sdv0=sd(v0)
  ) %>% 
  knitr::kable()
##分组汇总
d.cancer %>% 
  drop_na() %>% 
  group_by(sex) %>% 
  summarise_at(
    c('v0','v1'),
    list(
      nod=~n(),avg=~mean(.),std=~sd(.)
    )
  ) %>% 
  knitr::kable()
##分组筛选--找出v0排名在前两名的
d.cancer %>% 
  group_by(sex) %>% 
  filter(rank(desc(v0))<=2) %>% 
  arrange(sex,desc(v0)) %>% 
  knitr::kable()
##分组计算新变量
d.cancer %>% 
  group_by(sex) %>% 
  mutate(
    v00=v0/max(v0),
    v11=v1/max(v1)
  ) %>% 
  knitr::kable()
##计算交叉分类频数
d.cancer %>% 
  count(sex,type) %>% 
  knitr::kable()
##交叉分类汇总
d.cancer %>% 
  group_by(sex,type) %>% 
  summarise(
    freq=n(),
    meanv0=mean(v0),
    meanv1=mean(v1)
  ) %>% 
  knitr::kable()
##将 v0_count 和 v1_count 合并
bind_cols(
  d.cancer %>% 
    drop_na() %>% 
    group_by(sex) %>% 
    summarise(
      count=n()
    ),
  d.cancer %>% 
    drop_na() %>% 
    group_by(sex) %>% 
    summarise_at(
      c('v0','v1'),
      list(avg=~mean(.),std=~sd(.))
    ) %>% 
    select(-sex)
)
##计算NHANES表中ID(受访者编码)与SurveyYr(考察年份)每一对组合的出现次数
##筛选出二次及以上者，并降序排列，仅显示前 10 行结果：
library(NHANES)
data("NHANES")
NHANES %>% 
  count(ID,SurveyYr) %>% 
  filter(n>=2) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  knitr::kable()
##按照性别和病理类别交叉分类计算频数后求按性别分类的所有病人的总人数
d.cancer %>% 
  group_by(sex,type) %>% 
  summarise(
    count=n()
  ) %>% 
  summarise(
    N=sum(count)
  ) %>% 
  knitr::kable()
##按性别分类求病人总数
d.cancer %>% 
  group_by(sex) %>% 
  summarise(
    N=sum(n())
  ) %>% 
  knitr::kable()

d.cancer %>% 
  group_by(sex,type) %>% 
  summarise(
    n=n()
  ) %>% 
  ungroup() %>% 
  summarise(
    N=sum(n)
  ) %>% 
  knitr::kable()

#######绘图
##绘制函数曲线curve()函数
curve(x^2,-2,2)
curve(sin(x),0,2*pi)
##添加参考线
abline(h=0)

library(tidyverse)
library(readr)
d.cancer <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\cancer.csv",
                     locale=locale(encoding="GBK"))
d.class <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv",
                    locale=locale(encoding="GBK"))

#####条形图barplot()
##统计男女个数并用条形图表示：
F <- table(d.cancer$sex);F
barplot(F)
barplot(F,width=1.4,xlim=c(0,5),main = '性别分布',
        col=c('antiquewhite1','antiquewhite2'))
##按性别与病理类型交叉分组后统计频数，结果称为列联表：
D <- with(d.cancer,table(sex,type));D
barplot(D,legend=T)
barplot(D,beside = T,legend = T,width = 1.5,xlim = c(0,10),ylim = c(0,20),
        main = '病理分布',col = c("azure","azure1","azure2","azure3"))
head(colors(),20)

#####直方图hist()
x <- rnorm(1000,0,1)
hist(x,col = "light blue",main = '标准正态分布',
     xlab='x',ylab='freq')

#####核密度density()
##为随机分布添加密度曲线
x <- rnorm(1000,0,1)
den <- density(x)
hist(x,freq = FALSE,main = '标准正态分布',ylim = c(0,max(den$y)))
lines(den,lwd=2)

#####盒型图boxplot
##展示变量分布
with(d.cancer,boxplot(v0))
with(d.cancer,boxplot(v0~sex))
with(d.cancer,boxplot(v0,v1))
with(d.cancer,
     boxplot(list('疗前'=v0, '疗后'=v1)))

#####正态QQ图qqnorm()
z <- rnorm(1000,2,3)
qqnorm(z)
qqline(z)

#####散点图plot()
plot(d.cancer$v0,d.cancer$v1)
plot(d.class$height,d.class$weight,pch=8,cex=0.5)
##用气泡大小表现第三维（年龄）
with(d.class,
     plot(height, weight,
          pch=16, col='blue',
          cex=1 + (age - min(age))/(max(age)-min(age))))
##用气泡大小表现年龄，用颜色区分性别：
with(d.class,
     plot(height, weight,
          main='体重与身高关系',
          xlab='身高', ylab='体重',
          pch=16,
          col=ifelse(sex=='M', 'blue', 'red'),
          cex=1 + (age - min(age))
          /(max(age)-min(age))))
plot(d.class$height,d.class$weight,type = 'l')

#####曲线图curve()
curve(sin, -2*pi, 2*pi)
##plot作曲线图
x <- seq(0,2*pi,length=200)
y <- sin(x)
plot(x,y,type='l')
z <- tan(x)
##多条曲线
matplot(x,cbind(y,z),type = 'l',ylim = c(-5,5))
abline(h=0)

