###################################################################
#############Chapter Seven R向量下标和子集##############
##正整数下标
x <- c(1, 4, 6.25)
x[2]##取出第二个元素
x[2] <- 99; x##修改\变更第二个元素
x[c(1,3)]
x[c(1,3)] <- c(11, 13);x
x[c(1,3,1)]
##负整数下标--负下标表示扣除相应的元素后的子集
x <- c(1,4,6.25)
x[-2]
x[-c(1,3)]
##空下标与零下标
##x[]表示取x的全部元素作为子集
x <- c(1,4,6.25)
x[] <- 999##将x全部元素修改为999
x
x <- c(1,4,6.25)
x <- 999##将x定义为999
x
##零下标
x <- c(1,4,6.25)
x[0]##结果返回类型相同、长度为零的向量,相当于空集
class(x[0])
x <- c(1,4,6.25)
x[c(2,0)]##当 0 与正整数下标\负整数下标一起使用时会被忽略

##下标超界
x <- c(1,4,6.25)
x[5]##使用大于n的下标,读取时返回缺失值,不报错
x##下标超界给超出n的下标元素赋值,则向量自动变长,
x[5] <- 9##中间没有赋值的元素为缺失值
x


##逻辑下标—-用来对向量/数据框取子集
##下标可以是与向量等长的逻辑表达式
x <- c(1,4,6.25)
x[x > 3]##取出x的大于3的元素组成的子集
f <- function(x){
  y <- numeric(length(x))##输出0
  y[x >= 0] <- 1
  y[x < 0] <- 0 #此语句多余
  y
}
f(30)

y <- ifelse(x>=0, 1, 0)##向量化的逻辑选择

x <- c(1, 4, 6.25, NA)
x[x > 2]##逻辑下标中有缺失值,对应结果也是缺失值
x[!is.na(x) & x > 2]##考虑到缺失值问题,加上!is.na

x <- c(3, 4, 3, 5, 7, 5, 9)
which(x > 5)##找到满足x>5的条件的下标，x>5即7和9，它俩下标分别为5和7
seq(along=x)[x > 5]##会生成由 x 的下标组成的向量
##求最小值的下标和最大值的下标,不唯一时只取第一个
which.min(x)##最小值的下标
which.max(x)##最大值的下标

##向量可以为每个元素命名
##样例一
ages <- c(" 李明"=30, " 张聪"=25, " 刘颖"=28);ages
##样例二使用函数names()
ages <- c(30, 25, 28)
names(ages) <- c(" 李明", " 张聪", " 刘颖");ages
##样例三使用函数setNames()
ages <- setNames(c(30, 25, 28), c(" 李明", " 张聪", " 刘颖"));ages
##结果输出可以用元素名或元素名向量作为向量的下标
ages[" 张聪"]
ages[c(" 李明", " 刘颖")]
ages[" 张聪"]<-26;ages[" 张聪"]##变更张聪的信息

sex <- c(" 李明"=" 男", " 张聪"=" 男", " 刘颖"=" 女");sex
sex <- c(" 李明"=" 男", " 张聪"=" 男"," 李明"=" ", " 刘颖"=" 女");sex
##去掉元素名
unname(sex)
names(sex)<-NULL;sex

##用 R 向量下标作映射
price.map <- c(68, 88, 168)
items <- c(3,2,1,1,2,2,3)
y <- price.map[items];##向量下标作映射，允许下标重复
print(y)
##用字符型向量作下标，允许重复
sex <- c(" 男", " 男", " 女", " 女", " 男", " 女", " 女", " 女", " 女", " 男")
sex.color <- c(" 男"="blue", " 女"="red")
cols <- sex.color[sex]; print(cols)
unname(cols)##去掉元素名


##集合运算
unique(c(1, 5, 2, 5))##获得 x 的所有不同值
##用 a %in% x 判断 a 的每个元素是否属于向量 x
5 %in% c(1,5,2)
c(5,6) %in% c(1,5,2)

##对向量x的每个元素，从向量table中查找其首次出现位置并返回这些位置
match(5, c(1,5,2))##首次出现位置
match(5, c(1,5,2,5))
match(c(2,5), c(1,5,2,5))
match(c(2,5,0), c(1,5,2,5))

##intersect(x,y)求交集，结果中不含重复元素
intersect(c(5, 7), c(1, 5, 2, 5))
##union(x,y) 求并集，结果中不含重复元素
union(c(5, 7), c(1, 5, 2, 5))
##setdiff(x,y) 求差集，即y的元素中不属于y的元素组成的集合，结果中不含重复元素
setdiff(c(5, 7), c(1, 5, 2, 5))##求差集
##setequal(x,y)判断两个集合是否相等，不受次序与重复元素的影响
setequal(c(1,5,2), c(2,5,1))##不受次序影响
setequal(c(1,5,2), c(2,5,1,5))##不受重复元素影响


###################################################################
#############Chapter Eight R 数据类型的性质##############
####数据类型的性质#####
##用typeof()函数来返回一个变量或表达式的类型
typeof(1:3)##整数
typeof(c(1,2,3))##
typeof(c(1, 2.1, 3))
typeof(c(TRUE, NA, FALSE))
typeof('Abc')
typeof(factor(c('F', 'M', 'M', 'F')))##因子的结果是integer而不是因子

##数值一般看作double,如果需要明确表明某些数值是整数，可以在数值后面附加字母L
is.integer(c(1, -3))##判断整数型
is.integer(c(1L, -3L))##加L表示整数
c(-1, 0, 1)/0##Inf,-Inf,NaN分别表示正无穷、负无穷、not a number
is.na(c(-1, 0, 1)/0)##判断是否 NA 或 NaN
##列表的元素不需要属于相同的基本类型，而且列表的元素可以不是单一基本类型元素
typeof(list("a", 1L, 1.5))
##原子类型的各个元素除了基本类型相同，还不包含任何嵌套结构
c(1, c(2,3, c(4,5)))
typeof(c(1, c(2,3, c(4,5))))

##类型转换与类型升档
##as.xxx() 类的函数在不同类型之间进行强制转换
as.numeric(c(FALSE, TRUE))##逻辑型转化为数值型
as.character(sqrt(1:4))##数值型转化为字符串
##类型升档
##在用c()函数合并若干元素时，如果元素基本类型不同，将统一转换成最复杂的一个，复杂程度从简单到复杂依次为：logical<integer<double<character。这种做法称为类型升档
##logical<integer<double<character
c(FALSE, 1L, 2.5, "3.6")##均转化为字符串
##不同类型参与要求类型相同的运算时，也会统一转换为最复杂的类型，也称为类型升档
TRUE + 10
paste("abc", 1)

##属性
##R对象一般都有length和mode两个属性。
##常用属性有 names,dim,class 等。
##对象 x 的所有属性可以用 attributes() 读取
x <- table(c(1,2,1,35,65,2,1))##计算每个不同值的个数
print(x)##x是个表格
attributes(x)##读取x的所有属性
x[1]
x['35']
attributes(x)<-NULL##用attributes()函数修改属性
x
##用attr(x, " 属性名")的格式读取或定义 x 的属性
x <- c(1,3,5)
attr(x, "theta") <- c(0, 1)##添加属性
print(x)
attributes(x)
##names属性读取
x <- 1:5
y <- x^2
lmr <- lm(y ~ x);lmr
print(names(lmr))
##dim属性的存在表明对象是矩阵或一维、多维数组
x <- matrix(1:12, nrow=3, ncol=4)
attr(x, "dim")##读取dim属性
##修改dim属性--将向量转换成矩阵（数组），修改了矩阵的性质
##元素按列次序重排填入新的矩阵
x <- 1:4
dim(x) <- c(2,2)##修改dim属性、列次序
x
##类属--class
##函数class()可以返回变量类型的类属
typeof(factor(c('F', 'M', 'M', 'F')))##查看数据元素类型，基本等同于mode,B比mode更详细
mode(factor(c('F', 'M', 'M', 'F')))##查看数据元素类型
storage.mode(factor(c('F', 'M', 'M', 'F')))##比mode更为精确
class(factor(c('F', 'M', 'M', 'F')))##查看数据结构:matrix\array\dataframe\list等
class(as.numeric(factor(c('F', 'M', 'M', 'F'))))
class(x)
##用str()函数可以显示对象的类型和主要结构及典型内容
s <- 101:200##显示主要内容
attr(s,'author') <- '李小明'##添加属性
attr(s,'date') <- '2016-09-12'
str(s)
print(s)##用print()函数可以显示对象全部内容


###################################################################
#############Chapter Nine R 日期时间##############
####R 日期时间#########
install.packages('lubridate')
##扩展包提供了多个可以更容易地生成、转换、管理日期型和日期时间型数据的方便函数
library(lubridate)
today()##返回当前日期
now()##返回当前日期时间，CST是时区，在中国代表中国标准时间（北京时间）
##用ymd(),mdy(),dmy()将字符型向量转换为日期型向量
ymd(c("1998-3-10", "2018-01-17", "18-1-17"))
mdy(c("3-10-1998", "01-17-2018"))
dmy(c("10-3-1998", "17-01-2018"))
make_date(1998, 3, 10)##从三个数值构成日期向量
ymd_hms("1998-03-16 13:15:45")##添加后缀将字符串转换成日期时间
make_datetime(1998, 3, 16, 13, 15, 45.2)##从最多六个数值组成日期时间
as_date(as.POSIXct("1998-03-16 13:15:45"))##将日期时间型转换为日期型
as_datetime(as.Date("1998-03-16"))
##日期显示格式
x <- as.POSIXct(c('1998-03-16', '2015-11-22'))
as.character(x)##把日期型数据转换为字符型
as.character(x, format='%m/%d/%Y')##用format选项指定显示格式
##包含时间的转换
x <- as.POSIXct('1998-03-16 13:15:45')
as.character(x)
as.character(x, format='%H:%M:%S')

##访问日期时间的组成值
####一些常用的取成分函数
##year() 取出年
##month() 取出月份数值
##mday() 取出日数值
##yday() 取出日期在一年中的序号，元旦为 1
##wday() 取出日期在一个星期内的序号，但是一个星期从星期天开始，星期天为 1, 星期一为 2，星期六为 7。
##hour() 取出小时
##minute() 取出分钟
##second() 取出秒
month(as.POSIXct("2018-1-17 13:15:40"))
mday(as.POSIXct("2018-1-17 13:15:40"))
wday(as.POSIXct("2018-1-17 13:15:40"))
##成分函数允许被赋值，结果修改为相应元素的值
x <- as.POSIXct("2018-1-17 13:15:40")
year(x) <- 2000
month(x) <- 1
mday(x) <- 1
x
##update()函数
x <- as.POSIXct("2018-1-17 13:15:40")
y <- update(x, year=2000)##对一个日期或一个日期型向量统一修改其组成部分的值
y
##用 lubridate 包的功能计算周岁如下：
age.int <- function(birth, now){
  age <- year(now) - year(birth)
  sele <- (month(now) * 100 + mday(now)
           < month(birth) * 100 + mday(birth))
  ## sele 是那些没有到生日的人
  age[sele] <- age[sele] - 1
  age
}
age.int(birth='1990-10-12',now='2022-10-17')

##日期舍入计算
##对日期可以用 unit= 指定一个时间单位进行舍入,时间单位为字符串
x <- ymd_hms("2018-01-11 08:32:44")
floor_date(x, unit="10 minutes")
ceiling_date(x, unit="10 minutes")
round_date(x, unit="10 minutes")
##日期计算
##R的POSIXct日期时间之间可以相减
d1 <- ymd_hms("2000-01-01 0:0:0")
d2 <- ymd_hms("2000-01-02 12:0:5")
di <- d2 - d1
di##结果显示与日期之间差别大小有关系，结果是类型是 difftime
as.duration(di)##按整秒计算
##直接生成时间长度类型的数据，时间长度类型总是以秒作为单位
dhours(1)
##可以在时间长度之间相加，也可以对时间长度乘以无量纲数，
dhours(1) + dseconds(5)
dhours(1)*10
##可以给一个日期加或者减去一个时间长度，结果严格按推移的秒数计算
d2 <- ymd_hms("2000-01-02 12:0:5")
d2 - dhours(5)
d2 + ddays(10)
##将时间长度数据的类型转换为普通数值
unclass(dhours(1))
##时间周期的结果可以相加、乘以无量纲整数
years(2) + 10*days(1)
##使用时间周期进行日期的前后平移
ymd("2016-01-01") + dyears(1)##2016年是闰年，按秒推移，得到的并不是2017-01-01
ymd("2016-01-01") + years(1)##使用时间周期函数则得到预期结果
##时间区间
##%--% 运算符构造一个时间期间
d1 <- ymd_hms("2000-01-01 0:0:0")
d2 <- ymd_hms("2000-01-02 12:0:5")
din <- (d1 %--% d2); di
##可以用除法对一个时间区间计算其时间长度
din / ddays(1)
din / dseconds(1)
##生成时间区间，也可以用:interval(start, end) 函数
interval(ymd_hms("2000-01-01 0:0:0"), ymd_hms("2000-01-02 12:0:5"))
##指定时间长度和开始日期生成时间区间
d1 <- ymd("2018-01-15")
din <- as.interval(dweeks(1), start=d1); din
##访问时间区间的端点
int_start(din)
int_end(din)
##用int_shift()平移一个时间区间
din2 <- int_shift(din, by=ddays(7)); din2
##用int_overlaps() 判断两个时间区间是否有共同部分
int_overlaps(din, din2)

##########自定义求交集的函数############
int_intersect <- function(int1, int2){
  n <- length(int1)
  int1 <- lubridate::int_standardize(int1)
  int2 <- lubridate::int_standardize(int2)
  sele <- lubridate::int_overlaps(int1, int2)
  inter <- rep(lubridate::interval(NA, NA), n)
  if(any(sele)){
    inter[sele] <-
      lubridate::interval(pmax(lubridate::int_start(int1[sele]),
                               lubridate::int_start(int2[sele])),
                          pmin(lubridate::int_end(int1[sele]),
                               lubridate::int_end(int2[sele])))
  }
  inter
}
##测试
d1 <- ymd(c("2018-01-15", "2018-01-18", "2018-01-25"))
d2 <- ymd(c("2018-01-21", "2018-01-23", "2018-01-30"))
din <- interval(d1, d2); din
int_intersect(rep(din[1], 2), din[2:3])

##基本 R 软件的日期功能
##生成日期和日期时间型数据
x <- as.Date("1970-1-31"); x##转换为 Date 类型
as.numeric(x)##与1970-1-1间隔天数-1
as.Date(c("1970-1-5", "2017-9-12"))##可以将多个日期字符串转换成 Date 类型
as.Date("1/5/1970", format="%m/%d/%Y")##对于非标准的格式，可以增加一个format选项
as.POSIXct(c('1998-03-16'))##把年月日格式的日期转换为 R 的标准日期
as.POSIXct(c('1998/03/16'))
as.POSIXct('1998-03-16 13:15:45')
as.POSIXct(c('1998-03-16 13:15:45', '2015-11-22 9:45:3'))## 可以同时转换多项日期时间
##转换后的日期变量有class属性，取值为POSIXct与POSIXt, 并带有一个tzone（时区）属性
x <- as.POSIXct(c('1998-03-16 13:15:45', '2015-11-22 9:45:3'))
attributes(x)
as.POSIXct('3/13/15', format='%m/%d/%y')##用format参数指定一个日期格式
##日期仅有年和月，必须添加日（添加01为日即可）才能读入
as.POSIXct(paste('1991-12', '-01', sep=''), format='%Y-%m-%d')
old.lctime <- Sys.getlocale('LC_TIME');old.lctime##获取当前系统本地信息
Sys.setlocale('LC_TIME', 'C')##设置当前系统本地信息
as.POSIXct(paste('01', 'DEC91', sep=''), format='%d%b%y')
Sys.setlocale('LC_TIME', old.lctime)
as.POSIXct('1949-10-01', tz='Etc/GMT+8')##tz指定时区

##取出日期时间的组成值
x <- as.POSIXct('1998-03-16 13:15:45')
y <- as.POSIXlt(x)
y
cat(1900+y$year, y$mon+1, y$mday, y$hour, y$min, y$sec, '\n')##year 要加 1900，mon 要加 1
##对多个日期，as.POSIXlt() 会把它们转换成一个列表
x <- as.POSIXct(c('1998-03-16', '2015-11-22'))
as.POSIXlt(x)$year + 1900##取出年份

##日期计算
##Date 类型是用数值保存的，所以可以给日期加减一个整数
x <- as.Date("1970-1-5")
x1 <- x + 10; x1
x2 <- x - 5; x2
##给一个日期加减一定的秒数
as.POSIXct(c('1998-03-16 13:15:45')) - 30
as.POSIXct(c('1998-03-16 13:15:45')) + 10
##给一个日期加减一定天数，可以通过加减秒数实现
as.POSIXct(c('1998-03-16 13:15:45')) + 3600*24*2##把日期推后了两天

##用difftime(time1, time2, units='days')计算time1减去time2的天数
x <- as.POSIXct(c('1998-03-16', '2015-11-22'))
c(difftime(x[2], x[1], units='days'))
##如果前两个自变量中含有时间部分，则间隔天数也会带有小数部分
x <- as.POSIXct(c('1998-03-16 13:15:45', '2015-11-22 9:45:3'))
c(difftime(x[2], x[1], units='days'))


###################################################################
#############Chapter Ten R 因子类型##############
####R因子类型#####
##因子--分类变量--如性别、省份、职业。
##因子--有序量度--如打分结果，疾病严重程度等。
x <- c(" 男", " 女", " 男", " 男", " 女")
sex <- factor(x)##用 factor() 函数把字符型向量转换成因子
sex
attributes(sex)##因子有class属性，取值为"factor"，还有一个levels(水平值)属性
levels(sex)
##把因子转换为纯粹的整数值
as.numeric(sex)
##把因子转换成原来的字符型
as.character(sex)
#########因为一个因子的 levels 属性是该因子独有的，所以合并两个因子有可能造成错误
#########版本问题？########
li1 <- factor(c('男', '女'));li1
li2 <- factor(c('男', '男'));li2
c(li1, li2)
##正确做法:恢复成字符型后合并，然后再转换为因子
factor(c(as.character(li1), as.character(li2)))
##统计因子各水平的出现次数（称为频数或频率）
table(sex)
##按照因子分组然后每组计算另一变量的概括统计
h <- c(165, 170, 168, 172, 159)
tapply(h, sex, mean)##分男女两组计算了身高平均值


##R数据类型--六种：向量、矩阵、数组、数据框、因子、列表



set.seed(1)##产生随机数
fac <- sample(c("red", "green", "blue"), 30, replace=T);fac
fac <- factor(fac, levels=c("red", "green", "blue"));fac
x <- round(100*(10+rt(30,2)));x
res1 <- tapply(x, fac, sd); res1
barplot(res1)
install.packages("forcats")
library(forcats)
fac2 <- fct_reorder(fac, x, sd);fac2##按照统计量次序对因子排序
res2 <- tapply(x, fac2, sd);res2##对因子排序后的结果
barplot(res2)
levels(fac)
##用fct_relevel()函数将特定的一个或几个水平次序放到因子水平最前面
fac3 <- fct_relevel(fac, "blue"); levels(fac3)
##可以有多个字符型参数表示要提前的水平
fac3 <- fct_relevel(fac, "blue","green"); levels(fac3)
##fct_recode() 可以修改每个水平的名称
fac4 <- fct_recode(
  fac,
  " 红"="red", " 绿"="green", " 蓝"="blue")
table(fac4)

#####在修改水平名时允许多个旧水平对应到一个新水平，从而合并原来的水平
####如果合并很多，可以用fct_collapse()函数
compf <- fct_collapse(
  comp,
  " 其它"=c("", " 无名", " 无应答"),
  " 联想"=c(" 联想", " 联想集团"),
  " 百度"=c(" 百度", " 百度集团")
)


###################################################################
#############Chapter Eleven 列表类型##############
####列表类型#######
##列表可以有多个元素，列表的不同元素的类型可以不同
rec <- list(name=" 李明", age=30,
            scores=c(85, 76, 90))
rec
typeof(rec)##判断一个列表
is.list(rec)##判断某个对象是否列表类型
##列表元素访问
##列表的一个元素也可以称为列表的一个 “变量”
rec[[3]]##单个列表元素必须用两重方括号格式访问（访问元素）
rec[[3]][2]
rec[["age"]]
##列表的单个元素也可以用 $ 格式访问
rec$age
rec[3]##使用单重方括号对列表取子集，结果还是列表而不是列表元素（访问子集属于列表）
is.list(rec[3])
names(rec)##用 names() 函数查看和修改元素名
names(rec)[names(rec)=="scores"] <- " 三科分数"
names(rec)
rec[[" 三科分数"]]
##修改列表元素内容
rec[[" 三科分数"]][2] <- 0
print(rec)
##直接给列表不存在的元素名定义元素值就添加了新元素
rec[[" 身高"]] <- 178
print(rec)
##把某个列表元素赋值为NULL就删掉这个元素。如
rec[["age"]] <- NULL
print(rec)
##在 list() 函数中允许定义元素为 NULL，这样的元素是存在的
li <- list(a=120, b="F", c=NULL); li
##要把已经存在的元素修改为NULL值或者给列表增加一个取值为NULL的元素
##需要用单重的方括号取子集，这样的子集会保持其列表类型，给这样的子列表赋值为list(NULL)
li["b"] <- list(NULL)
li["d"] <- list(NULL)
li

##列表类型转换
li1 <- as.list(1:3)
li1
li2 <- list(x=1, y=c(2,3));li2
unlist(li2)

##返回列表的函数示例–strsplit()
##返回一个项数与字符型向量元素个数相同的列表，列表每项对应于字符型向量中一个元素的拆分结果
x <- c("10, 8, 7", "5, 2, 2", "3, 7, 8", "8, 8, 9")
res <- strsplit(x, ","); res##输入一个字符型向量并指定一个分隔符
sapply(res, as.numeric)##把拆分结果进一步转换成一个数值型矩阵
t(sapply(res, as.numeric))



###################################################################
#############Chapter Twelve R 矩阵和数组##############
####R 矩阵和数组#####
A <- matrix(11:16, nrow=3, ncol=2); print(A)##存储次序为按列存储
B <- matrix(c(1,-1, 1,1), nrow=2, ncol=2, byrow=TRUE); print(B)##用 byrow=TRUE 选项可以转换成按行填入
##访问矩阵的行数和列数
nrow(A)
ncol(A)
##矩阵有一个dim属性
attributes(A)##dim属性的两个元素分别为矩阵的行数和列数
dim(A)##dim属性可以用dim()函数访问
t(A)##返回A的转置
##矩阵子集
A
A[1,]##取出 A 的第一行，变成一个普通向量。
A[,1]##取出 A 的第一列，变成一个普通向量。
A[c(1,3), 1:2]##取出指定行、列对应的子矩阵
colnames(A) <- c('X', 'Y')##给矩阵每列命名，也可以访问矩阵列名
rownames(A) <- c('a', 'b', 'c')##给矩阵每行命名，也可以访问矩阵行名
A
attributes(A)##矩阵可以有一个 dimnames 属性
##有了列名、行名后，矩阵下标可以用字符型向量
A[,'Y']
A['b',]
A[c('a', 'c'), 'Y']
##选项 drop=FALSE避免取矩阵单行或单列时变成R向量
A[,1,drop=FALSE]

##矩阵也可以用逻辑下标取子集
A
A[A[,1]>=2,'Y']##行取第一列中大于等于2的所有值所在行（此处取所有行），列取Y列
A[A[,1]>=12,'Y']##行取第一列中大于等于12的所有值所在行（此处取所有行），列取Y列
##矩阵本质是按列次序填入元素的向量，所以可以像对一个向量取子集那样，仅用一个正整数向量的矩阵取子集。
A[c(1,3,5)]##取第1、3、5个元素（列顺序）
##挑选矩阵的任意元素组成的子集而不是子矩阵
##用一个两列的矩阵作为下标，矩阵的每行的两个元素分别指定一个元素的行号和列号
ind <- matrix(c(1,1, 2,2, 3,2), ncol=2, byrow=TRUE)
A;ind
A[ind]##取A中(1,1),(2,2),(3,2)三个位置上的元素
##返回矩阵 A 的所有元素
c(A);A[]
diag(A)
A
##cbind() 和 rbind() 函数
##cbind(x1, x2, x3)把等长列向量并在一起组成一个矩阵
cbind(c(1,2), c(3,4), c(5,6))
cbind(A, c(1,-1,10))##同时包含向量与矩阵，向量的长度必须与矩阵行数相等
cbind(1, c(1,-1,10))##自变量中有标量，此标量被重复使用

##矩阵运算
##四则运算
A
C1 <- A + 2; C1##矩阵与标量作四则运算，结果为每个元素进行相应运算
C2 <- A / 2; C2
##两个同形状的矩阵进行加、减运算，即对应元素相加、相减
C1 + C2;C1 - C2
C1 * C2##对两个同形状的矩阵，用 * 表示两个矩阵对应元素相乘
C1 / C2##用/表示两个矩阵对应元素相除
##矩阵乘法
##用%*% 表示矩阵乘法而不是用 * 表示
##注意矩阵乘法要求左边的矩阵的列数等于右边的矩阵的行数
A;B
C3 <- A %*% B; C3
##向量与矩阵相乘
##矩阵与向量进行乘法运算时，向量按需要解释成列向量或行向量。
##当向量左乘矩阵时，看成行向量；当向量右乘矩阵时，看成列向量
B
c(1,1) %*% B
B %*% c(1,1)
c(1,1) %*% B %*% c(1,1)
##外积运算%o%
##x %o% y的第i行第j列元素等于x[i]乘以y[j]
c(1,2,3) %o% c(1, -1)
##逆矩阵与线性方程组求解
solve(B)##求逆矩阵
solve(B, c(1,2))##用solve(A,b)求解线性方程组𝐴𝑥 = 𝑏中的x

## apply() 函数
##apply(A, 2, FUN) 把矩阵A的每一列分别输入到函数FUN中，得到对应于每一列的结果
D <- matrix(c(6,2,3,5,4,1), nrow=3, ncol=2); D
apply(D, 2, sum)##把D中每一列求和，2表示按列计算
##apply(A, 1, FUN) 把矩阵A的每一行分别输入到函数FUN中，得到与每一行对应的结果
apply(D, 1, mean)##把D中每一行求均值，1表示按行计算
##函数 FUN 返回多个结果
apply(D, 2, range)##矩阵的每一列是输入矩阵相应列输入到 FUN的结果，结果列数等于 A 的列数
t(apply(D, 1, range))##对每行计算 FUN 的结果，结果存入一个与输入的矩阵行数相同的矩阵

##多维数组
##给一个向量添加一个 dim 属性就可以把它变成多维数组
ara <- array(1:24, dim=c(2,3,4)); ara##定义一个三维数组，可以看成是4个2 × 3矩阵
ara[,,2]##取出第二个矩阵
ara[,2,2:3]##结果是一个2 × 2矩阵



###################################################################
#############Chapter Thirteen 数据框##############
####数据框######
##数据框--同一列数据类型相同
d <- data.frame(
  name=c(" 李明", " 张聪", " 王建"),
  age=c(30, 35, 28),
  height=c(180, 162, 175),
  stringsAsFactors=FALSE)##加选项stringsAsFactors=FALSE避免将字符型列转换成因子
print(d)
class(d$name)
##用names()函数和colnames()函数访问列名或变量名
names(d)
colnames(d)
##数据框内容访问
##数据框可以用矩阵格式访问
d[2,3]##访问单个元素。
##按列名访问
d[[2]]##访问第二列，结果为向量。
d[,2]
d[["age"]]
d[,"age"]
d$age
##访问某一行，结果还是数据框，不是向量
x <- d[2,]; x
is.data.frame(x)
d[1:2, "age"]
d[1:2, c("age", "height")]
d[1:2,]
d[d[,"age"]>=30,]
##数据框的行名
rownames(d) <- d$name;d
d$name <- NULL##删除原来name变量
d
#########################p136-137#################
####用数据框的行名可以建立一个值到多个值的对应表
####看不太懂之数据框的行名#########
dm <- data.frame(
  " 年级"=1:6,
  " 出游"=c(0, 2, 2, 2, 2, 1),
  " 疫苗"=c(T, F, F, F, T, F)
)
dm
rownames(dm) <- dm[[" 年级"]];dm
dm[[" 年级"]] <- NULL####?????教材代码
dm
ind <- c(2,1,1,3)
dm[as.character(ind),]
##结果中包含了不必要也不太合适的行名，可以去掉，以上程序改成
ind <- c(2,1,1,3)
xx <- dm[as.character(ind),];xx
rownames(xx) <- NULL####？？？？？？？？？？？啥意思
xx
##match(x, table)对x的每个元素返回其在table中出现的位置序号,找不到的元素返回NA
match(c(12, 15), 11:14)

####为啥出现na？？？？#######
dm <- data.frame(
  " 年级"=1:6,
  " 出游"=c(0, 2, 2, 2, 2, 1),
  " 疫苗"=c(T, F, F, F, T, F)
)
ind <- match(c(2,1,1,3), dm[[" 年级"]]); ind
dm[ind,]

##数据框与矩阵的区别--数据框不能作为矩阵参加矩阵运算#######
##需要时，可以用 as.matrix() 函数转换数据框或数据框的子集为矩阵
d2 <- as.matrix(d[,c("age", "height")])
d3 <- crossprod(d2); d3##crossprod(A)表示AT*A。

##tidyr::expand_grid() 函数
install.packages('tidyr')
library(tidyr)
##多个因素的试验,生成多个因素完全搭配并重复的表格
d4 <- tidyr::expand_grid(
  group=1:3,
  subgroup=1:2,
  obs=1:2)
print(d4)
##tibble 类型
##readr包的read_csv()函数它将CSV文件读入为tibble类型
library(tibble)
library(readr)
t.class <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
t.class
##tibble 类型的类属依次为 tbl_df, tbl, data.frame：
class(t.class)
##用 tibble() 函数生成小的 tibble
t.bp <- tibble(
  `序号`=c(1,5,6,9,10,15),
  `收缩压`=c(145, 110, " 未测", 150, " 拒绝", 115))
t.bp
##用 tribble 可以按类似于 CSV 格式输入一个 tibble
t.bp2 <- tribble(
  ~`序号`,~`收缩压`,
  1,145,
  5,110,
  6,NA,
  9,150,
  10,NA,
  15,115
)##注意tribble()中数据每行末尾也需要有逗号，最后一行末尾没有逗号
t.bp2
##用单重的方括号取列子集时，即使仅取一列，从tibble取出的一列结果仍是tibble而不是向量
t.bp[,"收缩压"]
##使用双方括号格式或$格式转化为向量
t.bp[["收缩压"]]
t.bp$收缩压
##tibble 类型允许其中的列是列表类型
tibble(x = 1:3,
       y = list(1, 1:2, 1:3))
#####mj
a<-tibble(x = 1:3,
       y = list(1, 1:2, 1:3));a
as.list(a)##转化为列表


###################################################################
#############Chapter Fourteen##############
####工作空间和变量赋值#####
## 工作空间
ls()##查看工作空间(global environment)中的内容
rm()##删除工作空间中的变量
##自定义函数中定义的变量都是临时的,不会保存到工作空间中
sandbox <- function(){
  cat('沙盘：接连的空行回车可以退出。\n')
  browser()
}
sandbox()##在这样的 browser 命令行中随意定义变量，定义的变量不会保存到工作空间中
##用 “Q” 命令可以退出这个沙盘环境，接连回车也可以退出

##非法变量名
##R的变量名要求由字母、数字、下划线、小数点组成
##开头不能是数字、下划线、小数点
##中间不能使用空格、减号、井号等特殊符号
##变量名不能与 if、NA 等保留字相同
##需要使用不符合规则的变量名，将变量名两边用反向单撇号 “`” 保护
`max score` <- 100
99 + `max score`
##变量名（元素名、列名等）以字符串形式使用，就不需要用 “`” 保护
x <- c("score a"=85, "score b"=66)
x
#########变量赋值与绑定p146-149略


