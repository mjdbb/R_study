######################################################
##############Chapter One##############
####r的下载和安装、扩展包的安装
##运行源程序文件
source()
##用如下程序制定镜像网站 (例子中是位于清华大学的镜像网站) 
options(repos=c(CRAN="http://mirror.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("sos")##安装指定的扩展包
print(.libPaths())##查看允许的扩展包安装位置
install.packages("sos", lib=.libPaths('D:/R/Rpackages')[1])##用lib=指定安装位置
##Github网站扩展包安装方法举例
##kjhealy是Github网站的某个作者的名称,socviz是该作者名下的一个R扩展包
if(!require(devtools)) install.packages('devtools')
devtools::install_github("kjhealy/socviz")
##一些包需要从 Bioconductor 网站安装
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("Biostrings"))

#######更新本地安装的所有有新版本的 CRAN 扩展包
options(repos=c(CRAN="http://mirror.tuna.tsinghua.edu.cn/CRAN/"))
update.packages(checkBuilt=TRUE, ask=FALSE)

#######迁移扩展包,在Rstudio不能运行代码,此处省略

######用 R 进行数据分析，不同的分析问题需要放在不同的文件夹中。
######以 MS Windows 操作系统为例，设某个分析问题的数据与程序放在了 c:\work 文件夹中。
######把 R 的快捷方式从桌面复制入此文件夹，在 Windows 资源管理器中，右键单击此快捷方式，在弹出菜单中选 “属性”，把 “快捷方式” 页面的 “起始位置” 的内容清除为空白，点击确定按钮。
######启动在 work 文件夹中的 R 快捷方式，出现命令行界面。这时，C:\work 称为当前工作目录。
######显示结果中的目录、子目录、文件之间的分隔符用了/符号，在 Windows 操作系统中一般应该使用\\符号，但是，在 R 的字符串中一个\需要写成两个，所以等价的写法是"C:\\work"。
######不同的分析项目需要存放在不同的文件夹中，每个文件夹都放置一个 “起始位置” 为空的 R 快捷方式图标，分析哪一个项目，就从对应的快捷图标启动，而不是从桌面上的 R 图标启动。
######这样做的好处时，用到源文件和数据文件时，只要文件在该项目的文件夹中，就不需要写完全路径而只需要用文件名即可。

getwd()##以显示当前工作目录位置
setwd()##设置当前工作目录位置

##问号后面跟随函数名查询某函数的帮助
?mean
##用 example(函数名) 的格式可以运行此函数的样例
example(mean)

###################################################################
#############Chapter Two R 语言入门运行样例##############
##四则运算
5 + (2.3 - 1.125)*3.2/1.1 + 1.23E3
2^10##乘方
52*51*50/(3*2)
##数学函数——平方根、指数、对数
sqrt(6.25)
exp(1)
log10(10000)
##数学函数——取整
round(1.1234, 2)##四舍五入到两位小数
round(-1.9876, 2)
floor(1.1234)##向下取整
floor(-1.1234)
ceiling(1.1234)##向上取整
ceiling(-1.1234)
##数学函数——三角函数
pi##圆周率
sin(pi/6)
cos(pi/6)
sqrt(3)/2
tan(pi/6)
##数学函数——反三角函数
pi/6
asin(0.5)
acos(sqrt(3)/2)
atan(sqrt(3)/3)
##分布函数和分位数函数
dnorm(1.98)##表示标准正态分布密度
pnorm(1.98)##表示标准正态分布函数
qnorm(0.9761482)##表示标准正态分布分位数函数。
qnorm(0.025)
##求自由度为10的t检验的双侧临界值,qt(y,df) 表示自由度为df的t分布的分位数函数
qt(1-0.05/2,10)
##显示一个表达式的结果
print(sin(pi/2))
##用cat()函数显示多项内容,包括数值和文本，文本包在两个单撇号或两个双撇号中
cat("sin(pi/2)=", sin(pi/2),"\n")##最后一项一般是"\n",表示换行.忽略此项将不换行。
##用sink()函数作运行记录
##用sink()函数打开一个文本文件开始记录文本型输出结果
##split=T表示不仅在文件中输出，也在控制台输出结果
sink("E:\\00study\\MJ\\Rlearning\\tmpres02.txt", split=TRUE)
print(sin(pi/6))
print(cos(pi/6))
cat("t(10) 的双侧 0.05 分位数（临界值）=", qt(1 - 0.05/2, 10), "\n")
sink()##结束记录时用空的sink()即可关闭文件不再记录

##用 <- 给变量赋值
x1 <- 1:10
x1
##一般的向量可以用 c() 生成
marks <- c(3,5,10,5,6)
marks
12345678901:12345678920
##向量可以和一个标量作四则运算，结果是每个元素都和这个标量作四则运算
x1 + 200
2*x1
2520/x1
##两个等长的向量可以进行四则运算，相当于对应元素进行四则运算
x2 <- x1 * 3
x2;x1;x2-x1
##R的许多函数都可以用向量作为自变量,结果是自变量的每个元素各自的函数值
sqrt(x1)##结果是 1 到 10 的整数各自的平方根

##函数曲线示例
##用curve()函数制作y=x^2函数的曲线图
curve(x^2, -2, 2)##第二、第三自变量是绘图区间
curve(sin(x), 0, 2*pi)
abline(h=0)##添加参考线
##条形图示例
##假设有10个男生,7个女生,如下程序绘制男生、女生人数的条形图
barplot(c(" 男生"=10, " 女生"=7),main=" 男女生人数")
##散点图示例
plot(1:10, sqrt(1:10))
##R软件中自带了一些演示图形，如下程序可以调用
demo("graphics")
demo("image")
##作图练习
curve(exp(x),-2,2)
curve(log(x),0.01,10)

##读入表格数据
install.packages('readr')
library(readr)
tax.tab <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\taxsamp.csv",locale=locale(encoding="GBK"))
print(head(tax.tab))##返回数据框或向量的前几项

##分类变量频数统计
##用table()函数计算每个不同值的个数，称为频数
table(tax.tab[["征收方式"]])
table(tax.tab[["申报渠道"]])
##统计 “征收方式” 和 “申报渠道” 交叉分类频数
table(tax.tab[["征收方式"]], tax.tab[["申报渠道"]])
##交叉分类频数结果制作成表格
knitr::kable(table(tax.tab[["征收方式"]], tax.tab[["申报渠道"]]) )
##数值型变量的统计
##summary()可以给出最小值,最大值,中位数,四分之一分位数,四分之三分位数和平均值
summary(tax.tab[["营业额"]])
##在mean, sd等函数中加入na.rm=TRUE选项。
mean(tax.tab[["营业额"]])
sd(tax.tab[["营业额"]])
##用 source() 函数可以运行保存在一个文本文件中的源程序
source("E:\\00study\\MJ\\Rlearning\\R.DATA\\ssq.r.txt")
##运行后source()函数后就可以调用自定义函数sum.of.squares()了
sum.of.squares(1:5) 


###################################################################
#############Chapter Three 常量与变量##############
####介绍常量和变量#########
##赋值
x5 <- 6.25
x6 = sqrt(x5)


###################################################################
#############Chapter Four 数值型向量及其运算##############
####数值型向量及其运算######
##用c()函数把多个元素或向量组合成一个向量
marks <- c(10, 6, 4, 7, 8);marks
x <- c(1:3, 10:13);x
x1 <- c(1, 2)
x2 <- c(3, 4)
x <- c(x1, x2)
x
##求x的长度
length(x)
##numeric()函数可以用来初始化一个指定元素个数而元素都等于零的数值型向量
numeric(10)##生成元素为10个0的向量

##向量运算
1.5 + 2.3 - 0.6 + 2.1*1.2 - 1.5/0.5 + 2^3
##遵从通常的优先级规则，可以用圆括号()改变运算的先后次序
1.5 + 2.3 - (0.6 + 2.1)*1.2 - 1.5/0.5 + 2^3
5 %/% 3##整除
5.1 %/% 2.5
5 %% 3##求余
5.1 %% 2.5
##向量与标量运算
##向量与标量的运算为每个元素与标量的运算
x <- c(1, 10)
x + 2;x - 2;x * 2;x / 2;x ^ 2;2 / x;2 ^ x
##缺失元素参加的运算相应结果元素仍缺失
c(1, NA, 3) + 10


##等长向量运算
##等长向量运算=对应元素两两运算
x1 <- c(1, 10)
x2 <- c(4, 2)
x1 + x2;x1 - x2;x1 * x2;x1 / x2

##不等长向量的运算
##如果其长度为倍数关系,规则是每次从头重复利用短的一个
x1 <- c(10, 20)
x2 <- c(1, 3, 5, 7)
x1 + x2;x1 * x2
##如果两个向量的长度不是倍数关系，会给出警告信息
c(1,2) + c(1,2,3)

##向量函数
## 向量化的函数
##普通的一元函数--一般会对每个元素计算
sqrt(c(1, 4, 6.25))
help.start()##查看基础函数列表
##排序函数
x <- c(33, 55, 11)
sort(x)##返回排序结果
rev(sort(x))##返回把各元素排列次序反转后的结果
order(x)## 返回排序用的下标
##返回对x从小到大排序后的结果
x[order(x)]##[]用作对对象的元素的索引

##计算累加
cumsum(1:5)
##计算累乘
cumprod(1:5)

##生成规则序列的函数
seq(5) 
seq(2,5) 
seq(11, 15, by=2)
seq(0, 2*pi, length.out=100)
seq(to=5, from=2)##在使用变量名时次序可以颠倒
seq(5,2)
##产生重复数值
x <- rep(0, 500);x##产生一个初值为零的长度为n=500的向量
rep(c(1,3),2)##把第一个自变量重复两次
rep(c(1,3), c(2,4))##对应重复
rep(c(1,3,5), each=3) ##重复完一个元素后再重复另一元素
##生成复数向量
##用Re(z)求实部,用Im(z)虚部,用Mod(z)或abs(z)模,用Arg(z)辐角,用Conj(z)共轭
##指定实部和虚部
complex(real = c(1,0,-1,0), imaginary = c(0,1,0,-1))
##以用mod和arg指定模和辐角
complex(mod=1, arg=(0:3)/2*pi)
##返回复数向量的实部/虚部
Re(complex(real = c(1,0,-1,0), imaginary = c(0,1,0,-1)))
Im(complex(real = c(1,0,-1,0), imaginary = c(0,1,0,-1)))



###################################################################
#############Chapter Five 逻辑型向量及其运算##############
##逻辑型向量只有两个值TRUE和FALSE,缺失时为NA
sele <- (log10(15) < 2); print(sele)
##向量比较结果为逻辑型向量
c(1, 3, 5) > 2
##等长向量的运算是对应元素的运算
(1:4) >= (4:1)
##与 NA 比较产生 NA
c(1, NA, 3) > 2
NA == NA##双等号表示比较是否相等
##判断向量每个元素是否NA
is.na(c(1, NA, 3) > 2)
##第i个元素的值为x的第i元素是否属于y的逻辑型值
c(1,3) %in% c(2,3,4)
c(NA,3) %in% c(2,3,4)
c(1,3) %in% c(NA, 3, 4)
c(NA,3) %in% c(NA, 3, 4)
##是对x的每个元素,找到其在y中首次出现的下标，找不到时取缺失值
match(c(1, 3), c(2,3,4,3))

##用 all(cond) 测试 cond 的所有元素为真；
##用 any(cond) 测试 cond 至少一个元素为真。
##cond 中允许有缺失值，结果可能为缺失值
c(1, NA, 3) > 2
all(c(1, NA, 3) > 2)
any(c(1, NA, 3) > 2)
all(NA)
any(NA)
which(c(FALSE, TRUE, TRUE, FALSE, NA))##返回真值对应的所有下标
which((11:15) > 12)
##函数identical(x,y)比较两个R对象x与y的内容是否完全相同,结果只会取标量TRUE与FALSE两种
identical(c(1,2,3), c(1,2,NA))
##结果假值是因为前一向量是整数型,后一向量是实数型。
identical(c(1L,2L,3L), c(1,2,3))
## all.equal在比较数值型时不区分整数型与实数型
all.equal(c(1,2,3), c(1,2,NA))
all.equal(c(1L,2L,3L), c(1,2,3))
duplicated(c(1,2,1,3,NA,4,NA))##返回每个元素是否为重复值的结果
unique(c(1,2,1,3,NA,4,NA)) ##返回去掉重复值的结果


###################################################################
#############Chapter Six 字符型数据及其处理##############
##字符型向量是元素为字符串的向量
s1 <- c('abc', '', 'a cat', NA, '李明');s1
##用来连接两个字符型向量,元素一一对应连接,默认用空格连接
paste(c("ab", "cd"), c("ef", "gh"))
paste("x", 1:3) ##作一对多连接,且可以自动把数值型向量转换为字符型向量
paste("x", 1:3, sep="")##用 sep= 指定分隔符
##collapse= 参数可以把字符型向量的各个元素连接成一个单一的字符串
##sep指定分隔符，collapse对象是向量的分隔符
paste(c("a", "b", "c"),collapse="")

##转换大小写
toupper('aB cd')##把字符型向量内容转为大写
tolower(c('aB', 'cd'))##转为小写
##计算字符型向量 x 中每个字符串的以字节为单位的长度
##中文通常一个汉字占两个字节--GBK编码，一个汉字占两个字节，UTF-8编码一个汉字占三个字节
##英文字母、数字、标点占一个字节
x=c('abd','fed','小米','yertweg')
nchar(x, type='bytes') 
##计算每个字符串的以字符个数为单位的长度，一个汉字算一个单位
nchar(x, type='chars')

##取子串
##从字符串x中取出从第start个到第stop个的子串
substr('JAN07', 1, 3)
substr(c('JAN07', 'MAR66'), 1, 3)##对字符型向量中每个元素取子串
substring(c('JAN07', 'MAR66'), 4)##从字符串 x 中取出从第 start 个到末尾的子串


##类型转换
##用as.numeric()把内容是数字的字符型值转换为数值
substr('JAN07', 4, 5)
substr('JAN07', 4, 5) + 2000##字符串不能与数值型作运算
as.numeric(substr('JAN07', 4, 5)) + 2000##转化为数值型后进行运算
as.numeric(substr(c('JAN07', 'MAR66'), 4, 5))##可以转换一个向量的每个元素为数值型
as.character((1:5)*5)##把数值型转换为字符型


### %表示输出
### %d：整型
### %ld：长整型
### %o：八进制数形式整数
### %u：十进制数unsigned(无符号)型数据
### %x：十六进制数形式整数，或字符串的地址
### %i：十进制，八进制，十六进制整数
### %c：一个字符
### %s：一个字符串
### %f：小数形式的实数，默认情况下保留小数点6位
### %e：指数形式的实数
### %g：根据大小自动选f格式或e格式，且去掉无意义的零

##用指定的格式数值型转换成字符型
sprintf('file%03d.txt', c(1, 99, 100))

##字符串拆分
x <- '10,8,7'
##把一个字符串按照某种分隔符拆分开
strsplit(x, ',', fixed=TRUE)##fixed =T,表示屏蔽正则
###补充
test <- "ew.ek.kk.ff.gg"
strsplit(test, split = ".")###使用了正则，.表示匹配任意一个字符
strsplit(test, split = ".", fixed = T)###fixed =T,精确匹配,表示屏蔽正则
##strsplit()的结果是一个列表,转化为数值型后进行运算
sum(as.numeric(strsplit(x, ',', fixed=TRUE)[[1]]))


##字符串替换功能
##用gsub()可以替换字符串中的子串
x <- '1, 3; 5'
gsub(';', ',', x, fixed=TRUE)
strsplit(gsub(';', ',', x, fixed=TRUE), ',')[[1]]

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
  compf,
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
t.class <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
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

###################################################################
#############Part 3 编程##############
#############Chapter Fifteen##############
####R 输入输出
##输入输出的简单方法
##简单的输出
##用 print() 函数显示某个变量或表达式的值
x <- 1.234
print(x)
y <- c(1,3,5)
print(y[2:3])
##用 cat() 函数把字符串、变量、表达式连接起来显示
##其中变量和表达式的类型一般是标量或向量，不能是矩阵、列表等复杂数据
cat("x =", x, "\n")##标量##字符串"\n"，即换行符。
cat("y =", y, "\n")##向量
##用 file= 选项将结果写入指定文件中，如果已有文件会把原有内容覆盖
##为了在已有文件时不覆盖原有内容而是在末尾添加，在 cat() 中使用 append=TRUE 选项
cat("=== 结果文件 ===\n", file="res.txt")
cat("x =", x, "\n", file="res.txt", append=TRUE)
cat("y =", y, "\n", file="res.txt", append=TRUE)##append=TRUE末尾添加不覆盖
##函数 sink() 把命令行窗口显示的运行结果转向保存到指定的文本文件中
sink("allres.txt", split=TRUE)
print(x)
print(y[2:3])
sink()##关闭这样的输出文件记录
##用 save()命令要求把指定的若干个变量(直接用名字，不需要表示成字符串)保存到用 file= 指定的文件中
scores<-c(1,2,3,4,5)
save(scores, file="scores.RData")
load("scores.RData")##恢复到工作空间
##保存多个变量，如 x, zeta，命令如：
save(x, zeta, file="myvars20200315.RData")
save(list = c("x", "zeta"), file="myvars20200315.RData")
##对于一个数据框，可以用 write.csv() 或 readr::write_csv() 将其保存为逗号分隔的文本文件
da <- tibble("name"=c(" 李明", " 刘颖", " 张浩"),
             "age"=c(15,17,16))
install.packages("writexl")
library(writexl)
library(readr)
write_csv(da, path="mydata.csv")
cat(1:12, "\n", file="x.txt")
x <- scan("x.txt")
x
##读入矩阵
##先把文件内容读入到一个 R 向量中，再利用 matrix() 函数转换成矩阵x
##quite=TRUE 选项使得读入时不自动显示读入的数值项数
M <- matrix(scan("mat.txt", quiet=TRUE), ncol=3, byrow=TRUE)
M

##读取 CSV 文件
##演示了内容中有逗号、有双撇号的情况
d <- read_csv("testcsv.csv");d
##从字符串读入
d.small <- read_csv("name,x,y
John, 33, 95
Kim, 21, 64
Sandy, 49, 100
")
d.small
##read_csv 选项
d.small <- read_csv("John, 33, 95
Kim, 21, 64
Sandy, 49, 100
", col_names=c("name", "x", "y") )##用 col_names= 指定各列的名字
d.small
##编码设置
##文件bp.csv以 GBK 编码
d <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv")
d <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",
              locale=locale(encoding="GBK"))##读入用 GBK 编码的中文 CSV 文件
d
##缺失值设置
##read_csv() 将空缺的值读入为缺失值，将 “NA” 也读入为缺失值。
##可以用 na= 选项改变这样的设置。也可以将带有缺失值的列先按字符型原样读入，然后再进行转换。
##先将血压列按字符型读入，再增加一列转换为数值型的列，非数值转换为 NA:
d <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",
              locale=locale(encoding="GBK"))
d[[" 收缩压数值"]] <- as.numeric(d[["收缩压"]])##增加一列转换为数值型的列，非数值转换为 NA
d
str(d)
##各列类型设置
##cols()函数可以用来规定各列类型
##col_types 选项可以指定每一列的类型，如"col_double()","col_integer()", "col_character()", "col_factor()", "col_date()", "col_datetime" 等。
d <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv", locale=locale(encoding="GBK"),
              col_types=cols(
                `序号` = col_integer(),
                `收缩压` = col_character()
              ))
d
#######???
##当猜测的文件类型有问题的时候， 可以先将所有列都读成字符型， 然后用type_convert()函数转换
d <- read_csv("filename.csv",
              col_types=cols(.default = col_character()))
d <- type_convert(d)
##因子类型设置
d.class <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
d.class <- read_csv(
  "E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv", col_types=cols(
    .default = col_double(),
    name=col_character(),
    sex=col_factor(levels=c("M", "F")) ))##转换性别列的类型为因子
str(d.class)##显示对象的主要结构和典型内容

##读入日期
d.dates <- read_csv('E:\\00study\\MJ\\Rlearning\\R.DATA\\dates.csv',
                    locale=locale(encoding="GBK"))
##用lubridate::ymd() 函数转换为 R 日期类型
library(lubridate)
d.dates[[" 出生日期 ct"]] <- lubridate::ymd(
  d.dates[["出生日期"]], tz='Etc/GMT+8')
d.dates[[" 发病日期 ct"]] <- lubridate::ymd(
  d.dates[["发病日期"]], tz='Etc/GMT+8')
d.dates
##用 R 本身的 as.POSIXct 函数转换--保存的是 POSIXct 类型
d.dates[[" 出生日期 ct"]] <- as.POSIXct(
  d.dates[["出生日期"]], format='%Y/%m/%d', tz='Etc/GMT+8')
d.dates[[" 发病日期 ct"]] <- as.POSIXct(
  d.dates[["发病日期"]], format='%Y/%m/%d', tz='Etc/GMT+8')
d.dates
knitr::kable(d.dates)##结构化展示数据
##可以用 R 本身的 as.Date 函数转换
d.dates[[" 出生日期 ct"]] <- as.Date(
  d.dates[["出生日期"]], format='%Y/%m/%d')
d.dates[[" 发病日期 ct"]] <- as.Date(
  d.dates[["发病日期"]], format='%Y/%m/%d')
d.dates##保存的是 Date 类型
##直接在 read_csv() 函数中指定某列为col_date()：
d.dates <- read_csv(
  'E:\\00study\\MJ\\Rlearning\\R.DATA\\dates.csv', locale=locale(encoding="GBK"),
  col_types=cols(
    `序号`=col_integer(),
    `出生日期`=col_date(format="%Y/%m/%d"),
    `发病日期`=col_date(format="%Y/%m/%d")
  ))
print(d.dates)
##其它函数
##read_table2() 读入用空格作为间隔的文本文件，同一行的两个数据项之间可以用一个或多个空格分隔，不需要空格个数相同，也不需要上下对齐
##read_tsv() 读入用制表符分隔的文件
##read_fwf() 读入上下对齐的文本文件
##read_lines() 函数将文本文件各行读入为一个字符型向量
##read_file() 将文件内容读入成一整个字符串
##read_file_raw() 可以不管文件编码将文件读入为一个二进制字符串

##Excel 表访问
##借助于文本格式
##用 write.csv() 把数据框保存成 CSV 格式
d1 <- tibble(" 学号"=c("101", "103", "104"),
             " 数学"=c(85, 60, 73),
             " 语文"=c(90, 78, 80))
write.csv(d1, file="tmp1.csv", row.names=FALSE)
##使用剪贴板
##在 Excel 中复制选中的区域,可以把选中部分转换成一个 R 的数据框
myDF <- read.delim("clipboard",header=FALSE)##复制的区域不含列名
myDF
##把iris数据集写入剪贴板，在打开的excel表格中直接粘贴即可
write.table(iris, file="clipboard", sep = "\t", col.names = NA)
##利用 readxl 扩展包
install.packages('rematch')
install.packages('cellranger')
install.packages('readxl')
library(readxl)
##读入.xls 和.xlsx 格式的 Excel 文件
read_excel('E:/00study/MJ/Rlearning/path.xlsx', sheet = 1, col_names = TRUE,
           col_types = NULL, na = "", skip = 0)##结果返回读入的表格为一个数据框

##文件访问
##连接的一些函数用法
## file() 生成到一个普通文件的连接
file("path", open="", blocking=T,
     encoding = getOption("encoding"),
     raw = FALSE)
##url() 生成一个到指定的 URL 的连接
url(description, open = "", blocking = TRUE,
    encoding = getOption("encoding"))
textConnection(description, open="r",
               local = FALSE,
               encoding = c("", "bytes", "UTF-8"))
##函数 gzfile, bzfile,xzfile, unz 支持对压缩过的文件的访问（不是压缩包，只对一个文件压缩）。
gzfile(description, open = "",
       encoding = getOption("encoding"),
       compression = 6)
bzfile(description, open = "",
       encoding = getOption("encoding"),
       compression = 9)
xzfile(description, open = "",
       encoding = getOption("encoding"),
       compression = 6)
unz(description, filename, open = "",
    encoding = getOption("encoding"))
##例子
url_xzg<-url(description = "http://xuzhougeng.top",open = "r",encoding = "UTF-8")
url_xzg
class(url_xzg)

##文本文件访问
##readLines 函数可以把文件各行读入为字符型向量的各个元素
ll <- readLines("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
print(head(ll, 3))
ll <- readr::read_lines("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
print(head(ll, 3))
##用 writeLines 函数可以把一个字符型向量各元素作为不同行写入一个文本型连接
vnames <- strsplit(ll, ",")[[1]];vnames##分开ll第一行元素
writeLines(vnames, "class-names.txt")##应该是一个打开的文本型写入连接
readr::write_lines(vnames, "class-names.txt")##直接给出一个要写入的文件名
##用 scan 函数读入用空格和空行分隔的字符串向量
vnames <- scan(
  "class-names.txt", what=character(),
  quiet=TRUE)
vnames
##文本文件分批读写
fin <- file("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv", "rt")
fout <- file("tmp.csv", "wt")
repeat{
  lines <- readLines(fin, n=10)##每次从 class.csv 读入至多 10 行
  cat("Read", length(lines), "lines.", "\n")
  if(length(lines)==0) break
  writeLines(lines, fout)
}
close(fout)
close(fin)
##字符型连接
fstr <-
  "name,score
王芳,78
孙莉,85
张聪,80"##字符型变量
d <- read.csv(textConnection(fstr), header=T)
print(d)
##在整理输出结果时，经常可以向一个字符型变量连接写入，最后再输出整个字符串值。
##写入用的 textConnection 的第一个参数是保存了将要写入的字符型变量名的字符串
##第二个参数表明是写入操作
tc <- textConnection("sreies", open="w")
cat("Trial of text connection.\n", file=tc)
cat(1:10, "\n", file=tc, append=T)##在原有文件中不覆盖已经有的内容
close(tc)
print(sreies)

##中文编码问题
iconvlist()##查看 R 支持的编码名称

##用基本 R 的读取函数读取
##bp.csv不能正确读取
read.csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",col.names = T)
readLines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv")
##以 UTF-8 编码的文件能正确读入
read.csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
readLines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8bom.csv")
##加 fileEncoding="编码类型"选项可以纠正编码问题：
read.csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",fileEncoding="GBK")
readLines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",encoding="GBK")##???????
##readr 包的 read_csv()、read_table2()、read_lines() 函数默认从 UTF-8 编码的文件中读取，无 BOM 或者有 BOM 都可以
read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8bom.csv")
read_lines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
read_lines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp-utf8bom.csv")
##对 GBK 编码的文件，不能直接读取
read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv")
read_lines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv")
##读取 GBK(或 GB18030) 编码的文件，在 read_csv() 和 read_lines() 函数中加入 locale=locale(encoding="GBK")选项
read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",locale=locale(encoding="GBK"))
read_lines("E:\\00study\\MJ\\Rlearning\\R.DATA\\bp.csv",locale=locale(encoding="GBK"))
##输出文件的编码
##write.csv()、writeLines() 生成的含有中文的文件的编码默认为操作系统的默认中文编码，这里是 GB18030
##readr 的 write_csv()、write_lines() 函数生成的含有中文的文件的编码默认 UTF-8 无 BOM
write_csv(tibble(" 姓名"=c(" 张三", " 李四")), "tmp.csv")##结果生成的文件编码为 UTF-8 无 BOM，这样的文件可以被 R 的 readr::read_csv() 正确读取，但是不能被MS Excel 软件正确读取
write_excel_csv(tibble(" 姓名"=c(" 张三", " 李四")), "tmp2.csv")##write_excel_csv() 可以生成带有 UTF-8 有 BOM 的 CSV 文件，这样的文件可以被 MS Office 正确识别

##SQL 数据库访问
##########初始化新 SQLite 数据库p177-191
install.packages('RSQLite')
library(RSQLite)
install.packages('goseq')
library(goseq)
f_sqlite <- "_tmp/db2020.SQLITE"
con <- dbConnect(drv=SQLite(), dbname=f_sqlite)###选择SQLite作为MySQL的驱动器，数据库名为f_s
source("https://bioconductor.org/biocLite.R")
biocLite("org.Hs.eg.db")


###################################################################
#############Chapter Sixteen##############
##程序控制结构
##表达式
##复合表达式--值为最后一个表达式的值，组合用大括号表示
{
  x <- 15
  x
}
##分支结构--if结构
##if(条件) 表达式 1
if(is.na(lambda)) lambda <- 0.5
##if(条件) 表达式1 else 表达式2
if(x>1) {
  y <- 2.5##表达式可以是用大括号包围的复合表达式
} else {
  y <- -y
}
##多个分支，可以在中间增加 else if
x <- c(0.05, 0.6, 0.3, 0.9)
for(i in seq(along=x)){
  if(x[i] <= 0.2){
    cat("Small\n")
  } else if(x[i] <= 0.8){
    cat("Medium\n")
  } else {
    cat("Large\n")
  }
}##多重 if-else 结构的程序缩进和排列方法

##用逻辑下标代替分支结构
##x为一个向量，要定义y与x等长，且y的每一个元素当且仅当x的对应元素为正数时等于1，否则等于零
y <- numeric(length(x))##numeric()函数可以用来初始化一个指定元素个数而元素都等于0的数值型向量
y[x>0] <- 1
y

##ifelse 函数
##函数ifelse()可以根据一个逻辑向量中的多个条件，分别选择不同结果
x <- c(-2, 0, 1)
y <- ifelse(x >=0, 1, 0); print(y)
##test、yes和no的配合符合向量化原则，如果有长度为1的或者长度较短但其倍数等于最长一个的长度的，短的一个自动从头循环使用
ifelse((1:6) >= 3, 1:2, c(-1,-2))

##循环结构
##计数循环
##生成 5 个标准正态分布随机数。
set.seed(101); x <- rnorm(5);x
y <- numeric(length(x))
for(i in 1:5){
  if(x[i]>=0) y[i] <- 1 else y[i] <- 0
}
print(y)
##例子
x <- 0.0; s <- 0; n <- 5
for(i in 1:n){
  x <- 2*x + 1
  s <- s + x
}
print(s)
##尽量避免 for 循环，示性函数例子实际上可以简单地写成
set.seed(101); x <- rnorm(5)
y <- ifelse(x >= 0, 1, 0)
print(y)
##使用 for 循环的注意事项：
##对一个向量元素遍历时如果用下标访问，需要用 seq_along(x) 的做法而不是 1:length(x) 的做法。
##如果直接对向量元素遍历，这有可能会丢失向量的属性（如日期型），用下标访问则不存在此问题。
x <- as.POSIXct(c("1981-05-31", "2020-02-22"))
for(xi in x){print(xi)}##直接对向量元素遍历
for(i in seq_along(x)){print(x[i])}##用下标访问

##用泰勒展开逼近计算e的值
##其中.Machine$double.eps称为机器𝜀，是最小的加1之后可以使得结果大于1的正双精度数，小于此数的正双精度数加1结果还等于1。
e0 <- exp(1.0)
s <- 1.0
x <- 1
k <- 0
repeat{
  k <- k+1
  x <- x/k
  s <- s + x
  if(x < .Machine$double.eps) break
}
err <- s - e0
cat("k=", k, " s=", s, " e=", e0, " 误差 =", err, "\n")


###################################################################
#############Chapter Seventeen##############
####函数####
##自定义没有参数的函数
f <- function() {
  x <- seq(0, 2*pi, length=50)
  y1 <- sin(x)
  y2 <- cos(x)
  plot(x, y1, type="l", lwd=2, col="red",
       xlab="x", ylab="")
  lines(x, y2, lwd=2, col="blue")
  abline(h=0, col="gray")
}
f()##在定义与调用时都不能省略圆括号
##自定义函数也可以是简单的一元函数，与数学中一元函数基本相同
f <- function(x) 1/sqrt(1 + x^2)
f(0)
f(c(-1, 0, 1, 2))##如果形式参数 x 的实参是一个向量，则结果也是向量，结果元素为实参向量中对应元素的变换值
##形式参数可以有多个，还可以指定缺省值
##y 指定了缺省值为 0
fsub <- function(x, y=0){
  cat("x=", x, " y=", y, "\n")
  x - y
}
##函数调用
fsub(3,1)
fsub(3)##有缺省值的形式参数在调用时可以省略对应的实参，省略时取缺省值。
fsub(x=3, y=1)
fsub(y=1, x=3)
fsub(x=3)
fsub(3, y=1)
fsub(1, x=3)
fsub(x=3, 1)

##写成列表
##用函数 do.call() 来表示函数调用
do.call(fsub, list(3, y=1))
##递归调用
## Recall 代表调用自身
fib1 <- function(n){
  if(n == 0) return(0)
  else if(n == 1) return(1)
  else if(n >=2 ) {
    Recall(n-1) + Recall(n-2)
  }
}
for(i in 0:10) cat("i =", i, " x[i] =", fib1(i), "\n")

##向量化
f <- function(x){
  x^2
}
f(c(1,2,3))##输入一个向量，结果也是向量
##if语句的条件必须是标量条件,不能处理向量输入
g <- function(x){
  if(abs(x) <= 1) {
    y <- x^2
  } else {
    y <- 1
  }
  y
}
g(4)
##修改1--使用非if函数
gv <- function(x){
  y <- numeric(length(x))
  sele <- abs(x) <= 1
  y[sele] <- x[sele]^2
  y[!sele] <- 1.0
  y
}
gv(c(-2,-1,0,1,2))
##修改2
gv <- function(x){
  ifelse(abs(x) <= 1, x^2, 1)
}
gv(c(-2,-1,0,1,2))
##对于没有这样简单做法的问题，可以将原来的逻辑包在循环中
gv <- function(x){
  y <- numeric(length(x))
  for(i in seq(along=x)){
    if(abs(x[i]) <= 1) {
      y[i] <- x[i]^2
    } else {
      y[i] <- 1
    }
  }
  y
}
gv(c(-2,-1,0,1,2))
##函数Vectorize可以将这样的操作自动化
g <- function(x){
  if(abs(x) <= 1) {
    y <- x^2
  } else {
    y <- 1
  }
  y
}
gv <- Vectorize(g)##将不能进行向量化预算的函数进行转化
gv(c(-2, -0.5, 0, 0.5, 1, 1.5))

##无名函数
##lapply 类的函数经常使用无名的函数对象作为输入
vapply(iris[,1:4], function(x) max(x) - min(x), 0.0)##计算极差
d <- scale(Filter(function(x) is.numeric(x), iris))##scale标准化，fliter过滤数据
integrate(function(x) sin(x)^2, 0, pi)##积分

## 变量作用域
ls()## 查看工作空间内容
ls(pattern = "^f")##显示所有以f开头的变量
object.size(fin)##查看变量占用存储大小，单位为字节
rm(x, y)##删除指定的变量
rm(list=ls(pattern="^f"))##用 list 参数指定一个要删除的变量名表

save()##保存工作空间中选择的某些变量
load()##载入保存在文件中的变量

##局部变量
######自变量的局部性
#####在函数内部对形式参数作任何修改在函数运行完成后都不影响原来的实参变量
xv <- c(1,2,3)
xl <- list(a=11:15, b="James")
if(exists("x")) rm(x)
f <- function(x, y){
  cat(" 输入的 x=", x, "\n")
  x[2] <- -1
  cat(" 函数中修改后的 x=", x, "\n")
  cat(" 输入的 y 为:\n"); print(y)
  y[[2]] <- "Mary"
  cat(" 函数中修改过的 y 为:\n"); print(y)
}
f(xv, xl)
cat(" 函数运行完毕后原来变量 xv 不变：", xv, "\n")
cat(" 函数运行完毕后原来变量 xl 不变：:\n"); print(xl)
cat(" 函数运行完毕后形式参数 x 不存在：:\n"); print(x)

##修改自变量
##为了修改某个自变量，在函数内修改其值并将其作为函数返回值，赋值给原变量
f <- function(x, inc=1){
  x <- x + inc
  x
}
x <- 100
cat(" 原始 x=", x, "\n")
x <- f(x)
cat(" 修改后 x=", x, "\n")
##函数内的局部变量
##函数内部用赋值定义的变量都是局部变量
##局部变量在函数运行结束后就会消失
if("x" %in% ls()) rm(x)
f <- function(){
  x <- 123
  cat(" 函数内：x = ", x, "\n")
}
f()
cat(" 函数运行完毕后：x=", x, "\n")
##函数试图知道自己被调用了多少次，但是因为每次函数调用完毕局部变量就消失，再次调用时的局部变量已经对应到全新的存储空间，所以如下的程序不能达到目的
f <- function(){
  if(!exists("runTimes")){
    runTimes <- 1
  } else {
    runTimes <- runTimes + 1
  }
  print(runTimes)
}
f()

##在函数内访问全局变量
##函数内部可以读取全局变量的值，但一般不能修改全局变量的值
##在函数内给全局变量赋值，结果得到的变量就变成了局部变量
x.g <- 9999
f <- function(x){
  cat(" 函数内读取：全局变量 x.g = ", x.g, "\n")
  x.g <- -1
  cat(" 函数内对与全局变量同名的变量赋值： x.g = ", x.g, "\n")
}
f()
cat(" 退出函数后原来的全局变量不变： x.g =", x.g, "\n")
##在函数内部如果要修改全局变量的值，用 <<-代替 <-进行赋值
x.g <- 9999
f <- function(x){
  cat(" 函数内读取：全局变量 x.g = ", x.g, "\n")
  x.g <<- -1
  cat(" 函数内用"<<-" 对全局变量变量赋值： x.g = ", x.g, "\n")
}
f()
cat(" 退出函数后原来的全局变量被修改了： x.g =", x.g, "\n")


###################################################################
#############Chapter Eighteen##############
####R 程序效率####
##向量化编程
##示例1：计算统计量--把这个统计量的计算变成一个 R 函数
f1 <- function(x){
  n <- length(x)
  mhat <- median(x)
  s <- 0.0
  for(i in 1:n){
    s <- s + abs(x[i] - mhat)
  }
  s <- s/n
  return(s)
}
##用 R 的向量化编程，函数体只需要一个表达式
f2 <- function(x) mean( abs(x - median(x)) )
##查看函数的计算速度
##在 R 中，用 system.time() 函数可以求某个表达式的计算时间，返回结果的第 3 项是流逝时间。
nrep <- 1000
x <- runif(10000)
y1 <- numeric(nrep); y2 <- y1
system.time(for(i in 1:nrep) y1[i] <- f1(x) )[3]
system.time(for(i in 1:nrep) y1[i] <- f2(x) )[3]
##使用R 扩展包 microbenchmark 可以用来测量比较两个表达式的运行时间
install.packages("microbenchmark")
library("microbenchmark")
x <- runif(10000)
microbenchmark::microbenchmark(
  f1(x),
  f2(x)
)
##示例2:计算函数值f(x)
f1 <- function(x){
  n <- length(x)
  y <- numeric(n)
  for(i in seq_along(x)){
    if(x[i] >= 0) y[i] <- 1
    else y[i] <- 0##可以去掉
  }
  y
}
##利用向量化与逻辑下标，程序可以写成:
f2 <- function(x){
  n <- length(x)
  y <- numeric(n)
  y[x >= 0] <- 1
  y
}
##利用 R 中内建函数 ifelse()，可以把函数体压缩到仅用一个语句：
f3 <- function(x) ifelse(x >= 0, 1, 0)
##示例3:计算P{至少两个人有生日相同}(n<=365),一个含连乘的等式
##完全用循环（两重循环），程序写成：
f1 <- function(){
  ny <- 365
  x <- numeric(ny)
  for(n in 1:ny){
    s <- 1
    for(j in 0:(n-1)){
      s <- s * (365-j)/365
    }
    x[n] <- 1 - s
  }
  x
}  
##用 prod() 函数可以向量化内层循环：
f2 <- function(){
  ny <- 365
  x <- numeric(ny)
  for(n in 1:ny){
    x[n] <- 1 - prod((365:(365-n+1))/365)
  }
  x
}
##把程序用 cumprod() 函数改写，可以完全避免循环
##cumprod()函数--向量元素累计求积
f3 <- function(){
  ny <- 365
  x <- 1 - cumprod((ny:1)/ny)
  x
}
##用 microbenchmark 比较:
microbenchmark::microbenchmark(
  f1(),
  f2(),
  f3()
)

##减少显式循环
##replicate(重复次数, 要重复的表达式)函数--用来执行某段程序若干次，常用于随机模拟
set.seed(1)
replicate(6, {
  x <- rnorm(5, 0, 1);
  c(mean(x), sd(x)) })

##避免制作副本
##制作x的副本减慢程序之x <- c(x, y), x <- rbind(x, y)
set.seed(101)
system.time({
  M <- 1E5
  x <- c()
  for(i in seq(M)){
    x <- c(x, diff(range(runif(10))))
  }
  mean(x)
})
##数据建模或随机模拟的程序都应该事先分配好用来保存结果的数据结构，在每次循环中填入相应结果
set.seed(101)
system.time({
  M <- 1E5
  x <- numeric(M)
  for(i in seq(M)){
    x[[i]] <- diff(range(runif(10)))
  }
  mean(x)
})
##在循环内修改数据框的值也会制作数据框副本，当数据框很大或者循环次数很多时会使得程序很慢
set.seed(101)
m <- 2E4; n <- 100
x <- as.data.frame(matrix(
  runif(n*m), nrow=n, ncol=m))
system.time({
  for(j in seq(m)){
    x[[j]] <- x[[j]] + 1
  }
})
##在循环内修改列表元素就不会制作副本,从而可以避免这样的效率问题
set.seed(101)
m <- 2E4; n <- 100
x <- replicate(m,runif(n),simplify=FALSE)##simplify:表示逻辑值。如果为 TRUE，则输出以向量或矩阵形式表示，否则以列表形式表示
system.time({
  for(j in seq(m)){
    x[[j]] <- x[[j]] + 1
  }
})
x <- as.data.frame(x)##也是效率较差的。将数据保存在列表中比保存在数据框中访问效率高，数据框提供的功能更丰富。

##R 的计算函数--数学函数、统计函数和特殊函数
##数学函数:abs, sign, log, log10, sqrt, exp, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh,tanh。还有 gamma, lgamma(伽玛函数的自然对数)。
##取整函数:ceiling, floor, round, trunc, signif, as.integer 等--向量化的一元函数
##choose(n,k) 返回从n中取k的组合数
##factorial(x) 返回x!结果
##combn(x,m) 返回从集合x中每次取出m个的所有不同取法，结果为一个矩阵，矩阵每列为一种取法的m个元素值
combn(10,5)

##概括函数
##sum--向量求和, prod--向量求乘积
##cumsum和cumprod--计算累计，得到和输入等长的向量结果。
##diff--计算前后两项的差分（后一项减去前一项）。
##mean 计算均值，var 计算样本方差或协方差矩阵，sd 计算样本标准差, median 计算中位数，quantile 计算样本分位数。cor 计算相关系数。
##colSums, colMeans, rowSums, rowMeans 对矩阵的每列或每行计算总和或者平均值
##rle 和 inverse.rle 用来计算数列中 “连” 长度及其逆向恢复，“连” 经常用在统计学的随机性检验中。

##最值
##max 和 min 求最大和最小，cummax 和 cummin 累进计算。
##range 返回最小值和最大值两个元素。
##对于 max, min, range，如果有多个自变量可以把这些自变量连接起来后计算。
##pmax(x1,x2,...) 对若干个等长向量计算对应元素的最大值，不等长时短的被重复使用。pmin 类似。比如，pmax(0,pmin(1,x)) 把 x 限制到 [0, 1] 内。

##排序
##sort 返回排序结果，可以用 decreasing=TRUE 选项进行降序排序。
##sort()中 partial= 选项，保证结果中 partial= 指定的下标位置是正确的
##sort()中用选项 na.last 指定缺失值的处理，取 NA 则删去缺失值，取 TRUE 则把缺失值排在最后面，取FALSE 则把缺失值排在最前面。
sort(c(3,1,4,2,5))
sort(c(3,1,4,2,5), partial=3)##只保证结果的第三个元素正确
unique(c(1,2,2,3,1))##返回去掉重复元素的结果
duplicated(c(1,2,2,3,1))##对每个元素用一个逻辑值表示是否与前面某个元素重复

##一元定积分 integrate
##integrate(f, lower, upper) 对一元函数 f 计算从 lower 到 upper 的定积分
integrate(sin, 0, pi)

##一元函数求根 uniroot--uniroot(f, interval)
######要求 f 在两个区间端点的值异号
uniroot(function(x) x*(x-1)*(x+1), c(-2, 2))

##离散傅立叶变换 fft
###########????
y[k] = sum(x * complex(
  argument = -2*pi * (0:(n-1)) * (k-1)/n))

##用 filter 函数作迭代--可以进行卷积型或自回归型的迭代
######示例 1：双侧滤波
y <- filter(c(1,3,7,12,17,23), c(0.1, 0.5, 0.4),
            method="convolution", sides=2)
y
#######示例 2: 单侧滤波
y <- filter(c(1,3,7,12,17,23), c(0.1, 0.5, 0.4),
            method="convolution", sides=1)
y
#######示例 3: 自回归迭代
y <- filter(c(0.1, -0.2, -0.1, 0.2, 0.3, -0.2),
            c(0.9, 0.1), method="recursive")
print(y, digits=3)
##如果已知 𝑦0 = 200, 𝑦−1 = 100, 迭代程序和结果为：
y <- filter(c(0.1, -0.2, -0.1, 0.2, 0.3, -0.2),
            c(0.9, 0.1), init=c(200, 100),
            method="recursive")
print(y, digits=6)

##并行计算
##例 1：完全不互相依赖的并行运算
##循环地用单线程计算
f10 <- function(k=2, n=1000){
  s <- 0.0
  for(i in seq(n)) s <- s + 1/i^k
  s
}
f11 <- function(n=1000000){
  nk <- 20
  v <- sapply(2:(nk+1), function(k) f10(k, n))
  v
}
system.time(f11())[3]
##查看本计算机的虚拟核心（线程）数
library(parallel)
detectCores()
##用 makeCluster() 建立临时的有 8 个节点的单机集群：
nNodes <- 8
cpucl <- makeCluster(nNodes)
##用 parSapply() 或者 parLapply() 关于 𝑘 并行地循环：
f12 <- function(n=1000000){
  f10 <- function(k=2, n=1000){
    s <- 0.0
    for(i in seq(n)) s <- s + 1/i^k
    s
  }
  nk <- 20
  v <- parSapply(cpucl, 2:(nk+1), function(k) f10(k, n))
  v
}
system.time(f12())[3]
##并行执行结束后，需要解散临时的集群，否则可能会有内存泄漏：
stopCluster(cpucl)
##parallel 包的 clusterExport() 函数可以用来把计算所依赖的对象预先传送到每个节点。比如，上面的 f2() 可以不包含 f0() 的定义，而是用 clusterExport() 预先传递：
cpucl <- makeCluster(nNodes)
clusterExport(cpucl, c("f10"))
f13 <- function(n=1000000){
  nk <- 20
  v <- parSapply(cpucl, 2:(nk+1), function(k) f10(k, n))
  v
}
system.time(f13())[3]
stopCluster(cpucl)
##如果需要在每个节点预先执行一些语句，可以用 clusterEvalQ() 函数执行，如
clusterEvalQ(cpucl, library(dplyr))

##例 2：使用相同随机数序列的并行计算
##不并行计算的程序示例：
wilson <- function(n, x, conf){
  hatp <- x/n
  lam <- qnorm((conf+1)/2)
  lam2 <- lam^2 / n
  p1 <- (hatp + lam2/2)/(1 + lam2)
  delta <- lam / sqrt(n) * sqrt(hatp*(1-hatp) + lam2/4) / (1 + lam2)
  c(p1-delta, p1+delta)
}
f20 <- function(cpar){
  set.seed(101)
  conf <- cpar[1]
  n <- cpar[2]
  p0 <- cpar[3]
  nsim <- 100000
  cover <- 0
  for(i in seq(nsim)){
    x <- rbinom(1, n, p0)
    cf <- wilson(n, x, conf)
    if(p0 >= cf[1] && p0 <= cf[2]) cover <- cover+1
  }
  cover/nsim
}
f21 <- function(){
  dp <- rbind(rep(c(0.8, 0.9), each=4),
              rep(rep(c(30, 100), each=2), 2),
              rep(c(0.5, 0.1), 4))
  lp <- as.list(as.data.frame(dp))
  res <- sapply(lp, f20)
  res
}
system.time(f21())[3]
##并行版本：
library(parallel)
nNodes <- 8
cpucl <- makeCluster(nNodes)
clusterExport(cpucl, c("f20", "wilson"))
f22 <- function(){
  dp <- rbind(rep(c(0.8, 0.9), each=4),
              rep(rep(c(30, 100), each=2), 2),
              rep(c(0.5, 0.1), 4))
  lp <- as.list(as.data.frame(dp))
  res <- parSapply(cpucl, lp, f20)
  res
}
system.time(f22())[3]
stopCluster(cpucl)

##例 3：使用独立随机数序列的并行计算
##单线程版本为：
f31 <- function(nsim=1E7){
  set.seed(101)
  n <- 30; p0 <- 0.01; conf <- 0.95
  cover <- 0
  for(i in seq(nsim)){
    x <- rbinom(1, n, p0)
    cf <- wilson(n, x, conf)
    if(p0 >= cf[1] && p0 <= cf[2]) cover <- cover+1
  }
  cover/nsim
}
system.time(cvg1 <- f31())[3]

library(parallel)
nNodes <- 8
cpucl <- makeCluster(nNodes)
each.seed <- function(s){
  assign(".Random.seed", s, envir = .GlobalEnv)
}
RNGkind("L'Ecuyer-CMRG")
set.seed(101)
seed0 <- .Random.seed
seeds <- as.list(1:nNodes)
for(i in 1:nNodes){ # 给每个节点制作不同的种子
  seed0 <- nextRNGStream(seed0)
  seeds[[i]] <- seed0
}
## 给每个节点传送不同种子：
junk <- clusterApply(cpucl, seeds, each.seed)
f32 <- function(isim, nsimsub=10000){
  n <- 30; p0 <- 0.01; conf <- 0.95
  cover <- 0
  for(i in seq(nsimsub)){
    x <- rbinom(1, n, p0)
    cf <- wilson(n, x, conf)
    if(p0 >= cf[1] && p0 <= cf[2]) cover <- cover+1
  }
  cover
}
clusterExport(cpucl, c("f32", "wilson"))
f33 <- function(nsim=1E7){
  nbatch <- 40
  nsimsub <- nsim / nbatch
  cvs <- parSapply(cpucl, 1:nbatch, f32, nsimsub=nsimsub)
  print(cvs)
  sum(cvs)/(nsim*nbatch)
}
system.time(cvg2 <- f33())[3] 
stopCluster(cpucl)


######################################################
#######Chapter Nineteen##########
##函数进阶
##函数调用的各种形式
##1.前缀形式
fsub=function(x,y) (x-y)
fsub(5,2)
##2.中缀形式--二元运算符实际上都是函数
5 - 2
`-`(5, 2)##因为-不是合法的 R变量名（函数名），所以在写成前缀形式时要用反向单撇号`保护
sapply(1:5, `-`, 2)##给 1:5 每个元素减去 2
##自己定义函数名如%x% 这样的中缀函数，可以用中缀格式调用
`%+%` <- function(x, y) paste0(x, y)##xy直接拼接，中间不含空格
"xyz" %+% "123"
##3.替换形式
##对属性的修改经常这样写
x <- 1:2
names(x) <- c("a", "b")
x
##制作了 x 的副本，调用 names<-函数，将 x 重新绑定到 names<-函数的返回值。
x <- 1:2
`*tmp*` <- x
x <- `names<-`(x, c("a", "b"))
rm(`*tmp*`)
x
##4.特殊形式
##取子集的特殊函数例如
x <- 1:5##取子集
x[1]
`[`(x, 1)##取子集特殊形式
x[1] <- 999##修改元素
x
x <- 1:5##修改元素特殊形式
x <- `[<-`(x, 1, 999)
x
##for 循环也是函数调用
for(i in 1:3) print(i)
`for`(i, 1:3, print(i))##等价写法

##嵌套定义与句法作用域 
##R 语言允许在函数体内定义函数
x <- -1##全局变量
f0 <- function(x){
  f1 <- function(){
    x + 100
  }
  f1()
}##内嵌的函数 f1() 称为一个 closure(闭包)
f0(1)##x 取的是 f0 的实参值 x=1，而不是全局变量 x=-1
##句法作用域--说的是如何查找某个变量对应的存储空间
##函数运行中需要使用某个变量时，从其定义时的环境向外层逐层查找，而不是在调用时的环境中查找。
f0 <- function(){
  f1 <- function(){
    x <- -1
    f2 <- function(){
      x + 100
    }
    f2()
  }
  x <- 1000
  f1()
}
f0()
##动态查找--说的是使用该存储空间何时的存储值
##函数运行时在找到某个变量对应的存储空间后，会使用该变量的当前值，而不是函数定义的时候该变量的历史值
f0 <- function(){
  x <- -1
  f1 <- function(){
    x + 100
  }
  x <- 1000
  f1()
}
f0()

##辅助嵌套函数
##内嵌函数仅仅是函数内用来实现模块化的一种工具，和正常的函数作用相同，没有任何特殊作用
solve.sqe <- function(x){
  fd <- function(a, b, c) b^2 - 4*a*c
  d <- fd(x[1], x[2], x[3])
  if(d >= 0){
    return( (-x[2] + c(1,-1)*sqrt(d))/(2*x[1]) )
  } else {
    return( complex(real=-x[2], imag=c(1,-1)*sqrt(-d))/(2*x[1]) )
  }
}
solve.sqe(c(1, -2, 1))
solve.sqe(c(1, -2, 0))
solve.sqe(c(1, -2, 2))

##懒惰求值
f <- function(x, y=ifelse(x>0, TRUE, FALSE)){
  x <- -111##形参缺省值也是只有在函数运行时用到该形参的值时才求值。
  if(y) x*2 else x*10
}
f(5)

##程序调试
##找到出错的函数--将错误定位到某一函数调用
f1 <- function(x) f2(x)
f2 <- function(x) 1/x
f1("abc")
##为了在多层次函数调用中找到出错的函数，可以用如下命令
traceback()#######??????????????

##跟踪调试示例
f <- function(x){
  for(i in 1:n){
    s <- s + x[i]
  }
}
print(f(1:5))
f <- function(x){
  browser()
  for(i in 1:n){
    s <- s + x[i]
  }
}
print(f(1:5))
f <- function(x){
  browser()
  for(i in 1:length(x)){##未定义的变量 n改为 length(x)
    s <- s + x[i]
  }
}
print(f(1:5))
f <- function(x){
  browser()
  s<-0##忘记初始化引起的,在for语句前添加 s <- 0 语句
  for(i in 1:length(x)){
    s <- s + x[i]
  }
}
print(f(1:5))
f <- function(x){
  browser()
  n <- length(x)
  s <- 0
  for(i in 1:n){
    s <- s + x[i]
  }
  s##在函数定义最后添加 s 一行
}
print(f(1:5))
f <- function(x){
  s <- 0
  for(i in seq_along(x)){
    s <- s + x[i]
  }
  s
}
print(f(1:5))##函数不需要修改后，可以把对 browser() 的调用删除或注释掉，在 RStudio 中关闭断点。

##条件断点
##在调试带有循环的程序时，发现错误发生在循环内，设已知错误发生在循环变量 i 等于 501 的时候，就可以在循环内插入：
if(i == 501) browser()

##开启对一个函数的调试
debug(f)##对函数 f 开启跟踪运行
undebug(f)##取消对 f 的自动进入跟踪运行状态

##出错调试选项
##比较长的程序可以设置成出错后自动进入跟踪模式，检查出错时的变量值
options(error=recover)

##stop()、warning()、message()
stop(s)##发现错误时，可以用 stop(s) 使程序运行出错停止,s 是一个字符型对象，用来作为显示的出错信息
warning(s)##不严重的问题可以用 warning(s) 提交一个警告信息
options(warn=2)##warn 参数可以设置警告级别，设置 warn=2 则所有警告当作错误处理，warn=0 是默认做法，warn=1 表示不延迟显示
message()##是提示性的信息输出

##预防性设计
##在编写自定义函数时，可以检查自变量输入以确保输入符合要求。
##函数 stopifnot 可以指定自变量的若干个条件，当自变量不符合条件时自动出错停止。
f <- function(x, y){
  stopifnot(is.numeric(x),
            is.numeric(y),
            length(x)==length(y))
  ## 函数体程序语句...
}

##出错处理机制
tryCatch()##用来保护可能出错的代码，并可以指定出错时用的恢复或诊断代码。
try()##保护可能出错的代码，使得出错时错误信息照样显示但不中断运行。

##函数式编程介绍
##副作用和运行环境恢复
f <- function(){
  opar <- par(mfrow=c(1,2))##绘图参数会修改运行环境
  on.exit(par(opar))##在函数结束运行前，应该恢复对运行环境的修改。此函数的参数是在函数退出前要执行的表达式或复合表达式。
  plot((-10):10)
  plot((-10):10, ((-10):10)^2)
}
f()

##泛函--函数需要用函数作为参数
##purrr::map 函数
##对列表或向量 x 的每个元素 x[[i]] 调用函数 f()，将结果保存成一个列表
##程序框架--框架而已，具体看示例
y <- vector('list', length(x))
for(i in seq_along(x)){
  y[[i]] <- f(x[[i]])
}
names(y) <- names(x)
library(purrr)
y <- map(x, f)
##map 数据框处理示例--举例说明 map 函数对数据框处理的应用
d.class <- readr::read_csv('E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv')
typeof(d.class[["age"]])
library(purrr)
map(d.class, typeof)##求每一列的存储类型
unlist(map(d.class, typeof))##将比较简单的列表转换为基本类型的向量
str(d.class)##得到更为详细的信息

##map 返回基本类型向量
##purrr::map() 总是返回列表。如果确知其调用的函数总是返回某种类型的标量值，可以用 map 的变种：
##map_lgl()：返回逻辑向量；
##map_int()：返回整型向量；
##map_dbl(): 返回双精度浮点型向量 (double 类型)；
##map_chr(): 返回字符型向量。
map_chr(d.class, typeof)##为确知 typeof() 函数对每列返回一个标量字符串
map_lgl(d.class, is.numeric)##对 d.class，可以对每一列用 is.numeric 判断是否数值型，结果为逻辑型向量

map_dbl(d.class[,3:5], mean, trim=0.10)##trim= 指定一个两边分别舍弃的值的个数比例
##用 map_lgl() 选出数据表中的数值型列
dsub <- d.class[, map_lgl(d.class, is.numeric)]
map_dbl(dsub, mean, trim=0.10)
##purrr::keep()专门用来选择数据框各列或列表元素中满足某种条件的子集
dsub <- keep(d.class, is.numeric)
map_dbl(dsub, mean, trim=0.10)
##利用 magrittr 包的管道运算符%>% 可以将对一个数据框的删选、计算过程更清晰地表达出来
##不需要 dsub 这样存储中间结果的变量
d.class %>%
  keep(is.numeric) %>%
  map_dbl(mean, trim=0.10)

##用 map 处理 strsplit 函数结果示例
s <- c('10, 8, 7',
       '5, 2, 2',
       '3, 7, 8',
       '8, 8, 9')
##对单个学生，可以用 strsplit() 函数把三个成绩拆分
strsplit(s[1], ',', fixed=TRUE)[[1]]
##拆分的结果可以用as.numeric() 转换为有三个元素的数值型向量
as.numeric(strsplit(s[1], ',', fixed=TRUE)[[1]])
##求三次小测验的总分
sum(as.numeric(strsplit(s[1], ',', fixed=TRUE)[[1]]))
##用 strsplit() 处理有 4 个字符串的字符型向量 s, 结果是长度为 4 的列表：
tmpr <- strsplit(s, ',', fixed=TRUE); tmpr
##对列表元素中的向量求和，使用管道运算符表达逐步的操作
s %>%
  strsplit(split=",", fixed=TRUE) %>%
  map(as.numeric) %>%
  map_dbl(sum)

##在 map 中使用无名函数以及简写方法
##map() 中调用的函数可以是在 map() 中直接现场定义的无名函数。
map_dbl(strsplit(s, split=",", fixed=TRUE),
        function(x) sum(as.numeric(x)))
##使用管道运算符:
s %>%
  strsplit(split=",", fixed=TRUE) %>%
  map_dbl(function(x) sum(as.numeric(x)))
##无名函数的简化写法：“~ 表达式” 格式
map_dbl(strsplit(s, split=",", fixed=TRUE),
        ~ sum(as.numeric(.)))##用. 表示只有一个自变量时的自变量名
##用.x 和.y 表示只有两个自变量时的自变量名
##用..1、..2、..3 这样的名字表示有多个自变量时的自变量名

##在 map 中提取列表元素成员的简写
od <- list(
  list(
    101, name=" 李明", age=15,
    hobbies=c(" 绘画", " 音乐")),
  list(
    102, name=" 张聪", age=17,
    hobbies=c(" 足球"),
    birth="2002-10-01")
)
##取出每个列表元素的第一项
map_dbl(od, function(x) x[[1]])
map_dbl(od, ~ .[[1]])
##可以用整数下标值表示对每个列表元素提取其中的指定成分
map_dbl(od, 1)
##可以在需要函数的地方写一个成员名，提取每个列表元素中该成员
map_chr(od, "name")
##在应该用函数的地方还可以提供一个列表，列表元素为成员序号或者成员名，进行逐层挖掘
map_chr(od, list("hobbies", 1))##表示取出每个列表元素的 hobbies 成员的第一个元素（每人的第一个业余爱好）
##取出不存在的成员会出错，但可以用一个.default 选项指定查找不到成员时的选项
map_chr(od, "birth", .default=NA)

##数据框分组处理示例
##分组建模
library(magrittr)
d.class %>%
  split(d.class[["sex"]]) %>%
  map(~ lm(weight ~ height, data=.)) %>%
  ##这里的“.”是 map 需要输入的无名函数的自变量
  map(coef) %>%
  map_dbl(2)##2表示什么？？
##purrr 包中 map 函数的变种
##输入输出类型相同的 modify 函数
d1 <- modify{d.class, ~ if(is.numeric(.x)) .x - median(.x) else .x}
d1
##modify_if()函数--可以对满足条件的列进行修改
d2 <- modify_if(d.class, is.numeric, ~ .x - median(.x))
d2
##map2() 函数--对两个自变量的相同下标元素调用函数
library(tibble)
d1 <- tibble(
  x1 = c(106, 108, 103, 110),
  x2 = c(101, 112, 107, 105) )
d2 <- tibble(
  x1 = c(104, 111, 112, 109),
  x2 = c(102, 114, 105, 107) )
map2_dbl(d1, d2, ~ (sum(.y) - sum(.x)) / sum(.x))
##如果计算结果与两个输入数据类型相同,可以用 modify2()
modify2(d1, d2, ~ (.y - .x) / .x)
##map2() 允许输入的 x 和 y 两个列表其中一个长度为 1，这时长度为 1 的列表的一个元素被重复利用。如
d1b <- d1[,1,drop=FALSE]
map2_dbl(d1b, d2, ~ (sum(.y) - sum(.x)) / sum(.x))

##不产生输出的 walk 类函数
##仅需要遍历一个数据结构调用函数进行一些显示、绘图，这称为函数的副作用，不需要返回结果。
walk(d.class, ~ cat(typeof(.x), "\n"))##显示数据框中每个变量的类别

##walk2() 函数可以接受两个数据自变量
##将 d.class 分成男女生两个子集，保存到两个 csv 文件中
dl <- split(d.class, d.class[["sex"]])
walk2(dl, paste0("class-", names(dl), ".csv"),
      ~ write.csv(.x, file=.y))
##改用管道运算符
d.class %>%
  split(d.class[["sex"]]) %>%
  walk2(paste0("class-", names(.), ".csv"), ~ write.csv(.x, file=.y))

##可同时访问下标或元素名与元素值的 imap 类函数
##显示数据框各列的变量名
iwalk(d.class, ~ cat(.y, ": ", typeof(.x), "\n"))
##返回字符型向量的写法：
imap_chr(d.class, ~ paste0(.y, " ==> ", typeof(.x))) %>% unname()
##输入数据没有元素名的演示：
dl <- list(1:5, 101:103)
iwalk(dl, ~ cat("NO. ", .y, ": ", .x[[1]], "\n"))##显示了每个列表元素的第一项

##多个数据自变量的 pmap 类函数
##将三个列表中的对应项用 c() 函数连接
x <- list(101, name=" 李明")
y <- list(102, name=" 张聪")
z <- list(103, name=" 王国")
pmap(list(x, y, z), c)
##pmap() 也可以输入一个数据框，对数据框的每一行执行函数
d <- tibble::tibble(
  x = 101:103,
  y=c(" 李明", " 张聪", " 王国"))
pmap_chr(d, function(...) paste(..., sep=":"))
##可以将列表变量名取为要调用的函数的自变量名
set.seed(101)
x <- rcauchy(1000)##柯西分布
trims <- c(0.05, 0.1, 0.2, 0.3, 0.4)
map_dbl(trims, ~ mean(x=x, trims=.))
##用 pmap() 的列表元素名自动对应到调用函数形参名
pmap_dbl(list(trims = trims), mean, x=x)
pmap_dbl(list(trims = trims), ~ mean(x))
##pmap() 的变种有 ivoke_map(.f, .x, ...)
install.packages("dplyr")
library(dplyr)
sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),##均匀分布
  "rnorm", list(sd = 5),##正态分布
  "rpois", list(lambda = 10)##泊松分布
)
sim %>%
  mutate(sim = invoke_map(f, params, n = 10))##mutate用来新增列

##purrr包中 reduce 类函数
##reduce函数--把输入列表（或向量）的元素逐次地用给定的函数进行合并计算
reduce(1:4, `+`)##实际执行的是 (((1 + 2) + 3) + 4)。
##考虑多个集合的交集的问题
##产生了 4 个集合，求交集
set.seed(5)
x <- replicate(4, sample(
  1:5, size=5, replace=TRUE), simplify=FALSE); x
##繁琐写法
##反复调用 intersect() 求出了交集
intersect(intersect(intersect(x[[1]], x[[2]]), x[[3]]), x[[4]])
##可以用 magrittr 包的%>% 符号写成:
x[[1]] %>% intersect(x[[2]]) %>% intersect(x[[3]]) %>% intersect(x[[4]])
##可以写成循环:
y <- x[[1]]
for(i in 2:4) y <- intersect(y, x[[i]])
y
##利用 purrr 包的 reduce 函数，只要写成
reduce(x, intersect)
##求各个集合的并集：
reduce(x, union)

## reduce2 函数
##purrr::accumulate()计算逐步的并集，结果的第一项保持原来的第一项不变
accumulate(x, union)
##将上述结果简化显示：
accumulate(x, union) %>%
  map(~sort(unique(.)))

## purrr 包中使用示性函数的泛函
##判断数据框中有无因子类型的列：
some(d.class, is.factor)
##判断数据框是否完全由数值型列组成：
every(d.class, is.numeric)
##返回向量中的第一个超过 100 的元素的值：
detect(c(1, 5, 77, 105, 99, 123), ~ . >= 100)
##返回向量中的第一个超过 100 的元素的下标：
detect_index(c(1, 5, 77, 105, 99, 123),
             ~ . >= 100)
##筛选出数据框的数值型列，并用 map_dbl 求每列的平方和
d.class %>%
  keep(is.numeric) %>%
  map_dbl(~ sum(. ^ 2))

##将数据框中数值型列除以 100，其它列保持不变
modify_if(d.class, is.numeric, `/`, 100)

##基本 R 的函数式编程支持
##lapply 函数--用输入的函数对数据的每个元素进行变换
lapply(d.class, typeof)##lapply 的结果总是列表
##sapply 会尽可能将结果简化为向量或矩阵
sapply(d.class, typeof)
##或使用 vapply()
vapply(d.class, typeof, "")

##Map()以一个函数作为参数，可以对其它参数的每一对应元素进行变换，结果为列表。
d <- data.frame(
  x = c(1, 7, 2),
  y = c(3, 5, 9))
Map(function(x) sum(x^2), d)
##也可以用 lapply() 改写成
lapply(d, function(x) sum(x^2))
##Map()允许对多个列表的对应元素逐一处理
##例如，为了求出 d 中每一行的最大值，可以用
Map(max, d$x, d$y)
##可以用 unlist() 函数将列表结果转换为向量
unlist(Map(max, d$x, d$y))
##mapply() 函数与 Map() 类似，但是可以自动简化结果类型
mapply(max, d$x, d$y)

set.seed(5)
x <- replicate(4, sample(
  1:5, size=5, replace=TRUE), simplify=FALSE); x
Reduce(intersect, x)

##Filter(f, x)用一个示性函数 f 作为筛选规则，从列表或向量 x 中筛选出用 f 作用后为真值的元素子集
f <- function(x) x > 0 & x < 1
Filter(f, c(-0.5, 0.5, 0.8, 1))
##这样的简单例子完全可以改写成
x <- c(-0.5, 0.5, 0.8, 1)
x[x>0 & x < 1]

##自定义泛函
summary.df.numeric <- function(df, FUN, ...){
  sapply(Filter(is.numeric, df), FUN, ...)
}
d.class <- readr::read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
summary.df.numeric(d.class, summary, na.rm=TRUE)##na.rm=true表示删除缺失值
#######上面定义的泛函是如何对数据框进行处理的说明：
summary.df.numeric2 <- function(df, FUN, ...){
  res <- c()
  nd <- c()
  for(j in seq_along(df)){
    if(is.numeric(df[[j]])){
      resj <- FUN(df[[j]], ...) # 这里！
      res <- cbind(res, resj)
      nd <- cbind(nd, names(df)[j])
    }
  }
  if(ncol(res)>0) {
    colnames(res) <- nd
    if(nrow(res) == 1) {
      res <- c(res)
      names(res) <- nd
    }
  }
  res
}
summary.df.numeric2(d.class, min, na.rm=TRUE)
summary.df.numeric2(d.class, summary, na.rm=TRUE)

##函数工厂--返回函数的函数，输出结果为一个闭包
##闭包例子--记录函数已运行次数的问题
f.gen <- function(){
  runTimes <- 0
  function(){
    runTimes <<- runTimes + 1##赋值的含义是在定义时的环境中逐层向上 (向外，向父环境方向) 查找变量是否存在，在哪一层找到变量就给那里的变量赋值
    print(runTimes)
  }
}
f <- f.gen()
f()
f()
##如果函数工厂生产出了两个闭包，这两个闭包的定义环境是不同的
c1 <- f.gen()
c2 <- f.gen()
c1()
c1()
c2()
##下面是一个类似的函数工厂例子, 产生的闭包可以显示从上次调用到下次调用之间经过的时间：
make_stop_watch <- function(){
  saved.time <- proc.time()[3]##返回当前的 R 会话已运行的时间
  function(){
    t1 <- proc.time()[3]
    td <- t1 - saved.time
    saved.time <<- t1
    cat(" 流逝时间（秒）：", td, "\n")
    invisible(td)##使变量不显示
  }
}
ticker <- make_stop_watch()
ticker()
for(i in 1:1000) sort(runif(10000))
ticker()

##动态查找和懒惰求值引起的问题
##下面的函数工厂可以生产出进行幂变换的函数：
make.pf <- function(power){
  function(x) x^power
}
p <- 2
square <- make.pf(p)
p <- 3##懒惰求值
square(4)

make.pf <- function(power){
  force(power)##用 force() 函数命令输入的参数当场求值
  function(x) x^power
}
p <- 2
square <- make.pf(p)
p <- 3
square(4)

##函数工厂的内存负担--在函数工厂内用 rm() 删除不再使用的变量

##函数算子--输入函数，输出函数
##被循环调用时可以每调用n次就显示一个小数点
dot_every <- function(f, n) {
  force(f)
  force(n)
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) cat(".")
    f(...)
  }
}
sim <- function(i){
  x <- runif(1E6)
  invisible(sort(x))
}
walk(1:100, dot_every(sim, 10))##被循环调用时可以每调用10次数就显示一个小数点

##环境
##基本概念
##基本认识
##用 rlang::env() 生成新的环境，类似于 list() 函数的用法
e1 <- rlang::env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3)
e1$e <- list(x=1, y="abcd")##对环境的修改
e1##显示一个地址信息
rlang::env_print(e1)##给出较多的信息
rlang::env_names(e1)##可以获得环境中绑定的名字组成的字符型向量

##重要环境
##rlang::current_env() 或基本 R 的 environment() 返回调用代码时所在的环境
##rlang::global_env() 和基本 R 的 globalenv() 返回全局环境
indentical(e1, e2)##比较两个环境是否同一个

##父环境
e1 <- rlang::env(a=1, b=2)
e2 <- rlang::env(e1, c=3, d=4)##用第一个无名的参数输入父环境
rlang::env_print(e1)
rlang::env_parent(e2)##获得父环境
rlang::env_parents(e2)##获得各层父环境

##在上层环境中赋值
f0 <- function(){
  x <- 0
  f1 <- function(){
    f2 <- function(){
      x <<- x+1##用 “<<-” 在各级父环境中赋值
      x##<<-首先在 f1 环境内查找 x，没有找到就继续向上在 f0 的环境内查找 x
    }
    f2()
  }
  f1
}
f01 <- f0()
f01()
f01()

##环境中名字的访问
##用 “环境 $ 名字” 格式或者 “环境 [[" 名字"]]” 读取环境的元素，不存在时返回 NULL
e1 <- rlang::env(rlang::empty_env(), x=1, y=2)
e2 <- rlang::env(e1, a=3, b=4)
e2$a
e2[["b"]]
e2$x##不能直接读取父环境中的变量
##如果希望在找不到变量时出错，可以用 rlang::env_get(环境名, " 名字")
rlang::env_get(e2, "x")
rlang::env_get(e2, "x", default=NA)
##在环境中增加绑定或重新绑定
##可以用 $ 或 [[格式直接赋值，可以用 rlang::env_poke() 或 rlang::env_bind()，rlang::env_bind() 运行同时进行多个绑定
e1 <- rlang::env(x=1, y=2)
e1$z <- 3
rlang::env_poke(e1, "a", 11)
rlang::env_bind(e1, b=12, c=13)
rlang::env_names(e1)
##检查某个环境中是否绑定了指定的名字
rlang::env_has(e1, c("x", "c", "f"))
##在环境中删除一个名字的绑定，需要用 rlang::env_unbind()，如：
rlang::env_unbind(e1, c("z", "c"))
rlang::env_names(e1)

##逐层向上访问环境
##逐层向上查找指定的名字所在的环境的自定义函数:(可以用作环境逐层向上遍历的模板)
where <- function(name, env = rlang::caller_env()) {
  if (identical(env, empty_env())) {
    # 找到了顶层都没有找到
    stop(" 找不到 ", name, call. = FALSE)
  } else if (rlang::env_has(env, name)) {
    # 在当前的 env 环境中找到了，返回找到时的环境
    env
  } else {
    # 利用递归向上层查找
    Recall(name, rlang::env_parent(env))
  }
}
search()##返回当前搜索路径
##用 rlang::fn_env(f) 可以求函数 f 的绑定环境。
f1 <- function(x) 2*x
rlang::fn_env(f1)
##这个例子显示的 f2 和 f2b 的环境都是 f1 内部的环境，在现实 f2b 的环境时虽然 f1() 已经结束运行，但是闭包可以保存其定义时的环境
f1 <- function(){
  times <- 0
  f2 <- function(){
    times <<- times + 1
    cat("NO. ", times, "\n", sep="")
  }
  print(rlang::fn_env(f2))
  f2
}
f2b <- f1()

#######Chapter20-25略###########


######################################################
#######Chapter 26 数据整理###########
##tidyverse 系统
##载入 tidyverse 包，则 magrittr 包，readr 包，dplyr 包和 tidyr 包都会被自动载入:
install.packages("tidyverse")
install.packages("NHANES")
library(tidyverse)
d.class <- read_csv(
  "E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv",
  col_types=cols(
    .default = col_double(),
    name=col_character(),
    sex=col_factor(levels=c("M", "F"))
  ))
library(NHANES)
data(NHANES)
print(dim(NHANES))
print(names(NHANES))

##用 filter() 选择行子集
d.class %>%
  filter(sex=="F", age<=13) %>%
  knitr::kable()##生成表格

##按行序号选择行子集
d.class %>%
  head(n=5) %>%##选择数据框前面n行
  knitr::kable()
d.class %>%
  slice(3:5) %>%##选择指定序号的行子集
  knitr::kable()

##用 sample_n() 对观测随机抽样
##从数据集 tbl 中随机无放回抽取 size 行
d.class %>%
  sample_n(size = 3) %>%##加选项 replace=TRUE 可以变成有放回抽样
  knitr::kable()

##用 distinct() 去除重复行
d.class %>%
  distinct(sex, age) %>%##筛选出性别与年龄的所有不同组合
  ##加选项.keep_all=TRUE保留数据框中其它变量
  knitr::kable()
##查看 NHANES 数据框中 ID 与 SurveyYr 的组合的不同值的个数
NHANES %>%
  distinct(ID, SurveyYr) %>%
  nrow()

##用 drop_na() 去除指定的变量有缺失值的行
##将 NHANES 中所有存在缺失值的行删去后数出保留的行数
NHANES %>%
  drop_na() %>%
  nrow()##所有行都有缺失值
##删去指定的AlcoholDay变量有缺失值的行后保留的行数
NHANES %>%
  drop_na(AlcoholDay) %>%
  nrow()

##用 select() 选择列子集
d.class %>%
  select(name, age) %>%
  head(n=3) %>%
  knitr::kable()
##用冒号表示列范围
d.class %>%
  select(age:weight) %>%
  head(n=3) %>%
  knitr::kable()
##用数字序号表示列范围
d.class %>%
  select(3:5) %>%
  head(n=3) %>%
  knitr::kable()
##参数中前面写负号表示扣除
d.class %>%
  select(-name,-age) %>%
  head(n=3) %>%
  knitr::kable()
##如果要选择的变量名已经保存为一个字符型向量，可以用 one_of() 函数引入，如
vars <- c("name", "sex")
d.class %>%
  select(one_of(vars)) %>%
  head(n=3) %>%
  knitr::kable()

##取出单个变量为向量--pull() 函数
d.class %>%
  head(n=3) %>%
  pull(name) %>%
  paste(collapse=":")
##先选择仅有一个变量的子数据框再用 pull()
varname <- "name"
d.class %>%
  head(n=3) %>%
  select(one_of(varname)) %>%
  pull() %>%
  paste(collapse=":")

##arrange()排序--按照数据框的某一列或某几列排序，返回排序后的结果
d.class %>%
  arrange(sex, age) %>%
  knitr::kable()
##用 desc() 包裹想要降序排列的变量，如
d.class %>%
  arrange(sex, desc(age)) %>%
  knitr::kable()
##用 rename() 修改变量名--用 “新名字 = 旧名字” 格式修改变量名
d2.class <- d.class %>%
  dplyr::rename(h=height, w=weight)##返回改了名字后的新数据框
d2.class

##用 mutate() 计算新变量
d.class %>%
  mutate(
    rwh=weight/height,
    sexc=ifelse(sex=="F", " 女", " 男")) %>%
  head(n=3) %>%
  knitr::kable()
##用 mutate() 计算新变量时如果计算比较复杂，也可以用多个语句组成复合语句
d.class %>%
  mutate(
    sexc = {
      x <- rep(" 男", length(sex))
      x[sex == "F"] <- "女"
      x
    } ) %>%
  head(n=3) %>%
  knitr::kable()##返回添加了新变量的新数据框
##计算公式中可以包含对数据框中变量的统计函数结果
d.class %>%
  mutate(
    cheight = height - mean(height)) %>%
  knitr::kable()

##用 tranmute() 生成新变量的数据框
##仅保留新定义的变量，不保留原来的所有变量
d.class %>%
  transmute(
    height_cm = round(height*2.54),
    weight_kg = round(weight*0.4535924),
    bmi = weight_kg / (height_cm / 100)^2) %>%
  head(n=3) %>%
  knitr::kable()
##直接为数据框定义的新变量赋值：
d.class[["rwh"]] <- d.class[["weight"]] / d.class[["height"]]
d.class##这样不会生成新数据框，新变量是在原数据框中增加的

##用管道连接多次操作
##对 d.class 数据，先选出所有女生，再去掉性别和 age变量：
d.class %>%
  filter(sex=="F") %>%
  select(-sex, -age) %>%
  knitr::kable()
##管道操作的结果可以保存为新的 tibble，如:
class_F <- d.class %>%
  filter(sex=="F") %>%
  select(-sex, -age)
class_F
##也可以将赋值用-> 写在最后
d.class %>%
  filter(sex=="F") %>%
  select(-sex, -age) -> class_F;class_F
##如果管道传递的变量在下一层调用中不是第一自变量，可以用. 代表
######data=.咩意思？？？？？？？？？？
d.class %>%
  lm(weight ~ height, data=.) %>%
  coef()
##为了明确表示不使用管道输入作为第一自变量，可以将管道操作的那一层加上大括号，如：
d.class %>% {
  lm(weight ~ height, data=.) } %>%
  coef()

##宽表转换为长表
##pivot_longer 函数
dwide1<-readr::read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\dwide1.csv")
knitr::kable(dwide1)
dwide1 %>%
  pivot_longer(
    `1`:`4`,##宽表转换为长表
    names_to = "time",##指定一个新变量名，将原来的列标题转换为该变量的值
    values_to = "response"##指定一个新变量名，将原来的各个列对应的测量值保存在该变量名的列中
  ) %>%
  knitr::kable()
##如果转换结果中不希望保留那些 NA，可以加 values_drop_na=TRUE:
dwide1 %>%
  pivot_longer(`1`:`4`,
               names_to = "time",
               values_to = "response",
               values_drop_na = TRUE) %>%##转换结果不保留NA
  knitr::kable()
##从列名中提取数值
dwide1 %>%
  pivot_longer(cols = paste0("FU", 1:4),
               names_to = "time",
               values_to = "response",
               names_prefix = "FU",
               names_transform = list(time = as.integer),
               values_drop_na = TRUE) %>%
  knitr::kable()


######################################################
#######Chapter 27 数据汇总###########
##用 dplyr 作数据汇总
d.cancer <- read_csv(
  "E:\\00study\\MJ\\Rlearning\\R.DATA\\cancer.csv", locale=locale(encoding="GBK"))
knitr::kable(d.cancer)
## summarise() 函数可以对数据框计算统计量
d.cancer %>%
  summarise(
    nobs = n(),
    n = sum(!is.na(age)),
    mean.age=mean(age, na.rm=TRUE),
    sd.age=sd(age, na.rm=TRUE)) %>%
  knitr::kable()

##多个变量的汇总
d.cancer %>%
  summarise(
    mean.v0=mean(v0, na.rm=TRUE),
    sd.v0 = sd(v0, na.rm=TRUE),
    mean.v1=mean(v1, na.rm=TRUE),
    sd.v1 = sd(v1, na.rm=TRUE)) %>%
  knitr::kable()
##summarse_at() 函数可以指定一批变量名与一批统计函数，自动命名结果变量
d.cancer %>%
  summarise_at(
    c("v0", "v1"),
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##变量子集也可以用序号范围表示，或者用 vars() 函数写成不加撇号的格式
d.cancer %>%
  summarise_at(
    5:6,
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##或者
d.cancer %>%
  summarise_at(
    vars(v0, v1),
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##对所有数值型变量计算某些统计量
d.cancer %>%
  summarise_if(
    is.numeric,
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##summarise_all 可以对所有变量计算统计量
d.cancer %>%
  select(v0, v1) %>%
  summarise_all(
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##包将上面的变量与统计量拆开，将变量放到不同的观测中
d.cancer %>%
  select(v0, v1) %>%
  summarise_all(
    list(avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  pivot_longer(
    everything(),
    names_sep ="_",
    names_to = c("variable", ".value")
  ) %>%
  knitr::kable()

##用 dplyr 作数据分组汇总
d.cancer %>%
  group_by(sex) %>%
  summarise(
    count=n(),
    mean.age=mean(age, na.rm=TRUE)) %>%
  knitr::kable()
##按不同性别分组计算 v0 和 v1 的人数、平均值、标准差
d.cancer %>%
  group_by(sex) %>%
  summarise_at(
    c("v0", "v1"),
    list(count = ~n(), avg = ~mean(.), std = ~sd(.)),
    na.rm=TRUE) %>%
  knitr::kable()
##将 v0_count 和 v1_count 合并
bind_cols(
  d.cancer %>%
    group_by(sex) %>%
    summarise(count = n() ),
  d.cancer %>%
    group_by(sex) %>%
    summarise_at(
      c("v0", "v1"),
      list(avg = ~mean(.), std = ~sd(.)),
      na.rm=TRUE) %>%
    select(-sex)
)
##查询数值型变量取值满足某种条件的个数和比例
d.cancer %>%
  group_by(sex) %>%
  summarise(
    nold = sum(age >= 60, na.rm=TRUE),
    pold = nold / n()) %>%
  knitr::kable()
##用 group_by() 分组后除了可以分组汇总，还可以分组筛选：
d.cancer %>%
  group_by(sex) %>%
  filter(rank(desc(v0)) <= 2) %>%##在每组中找出疗前体积排名在前两名的
  arrange(sex, desc(v0)) %>%
  knitr::kable()
##在分组后也可以根据每组的统计量用 mutate() 定义新变量
d.cancer %>%
  group_by(sex) %>%
  mutate(
    v0s = v0 / max(v0),
    v1s = v1 / max(v1))

##交叉分类的汇总
##对 d.cancer 数据框分性别与病理类型分别统计人数
d.cancer %>%
  group_by(sex, type) %>%
  summarise(freq=n()) %>%
  knitr::kable()
##用 dplyr 的 count() 函数仅计算交叉分类频数
d.cancer %>%
  count(sex, type) %>%
  knitr::kable()
##下面的程序数出 NHANES 数据框中 ID(受访者编码) 与 SurveyYr(考察年份) 每一对组合的出现次数, 筛选出二次及以上者，并降序排列，仅显示前 10 行结果：
NHANES %>%
  count(ID, SurveyYr) %>%
  filter(n >=2 ) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  knitr::kable()

##希望在用 group_by() 按照性别和病理类别交叉分类计算频数后求所有病人的总人数
d.cancer %>%
  group_by(sex, type) %>%
  summarise(freq=n()) %>%
  summarise(ntotal=sum(freq)) %>%
  knitr::kable()
##加入 ungroup() 可以不分组计算总人数
d.cancer %>%
  group_by(sex, type) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  summarise(ntotal=sum(freq)) %>%
  knitr::kable()

##tibble 中的列表列
##group_by 与 nest 配合
d.cancer %>%
  group_by(type) %>%##按 type 分类拆分为 2 个子数据框
  nest()##存入 tibble 的 data 列
##下面先定义对子数据框回归建模的函数，然后用 purrr 包的 map 函数将回归建模的函数作用到 data 列的每个元素，用 mutate 保存到列表类型的 lmr 列中：
fmod <- function(subdf) lm(v1 ~ v0, data = subdf)
mod.cancer <- d.cancer %>%
  group_by(type) %>%
  nest() %>%
  mutate(lmr = map(data, fmod))
mod.cancer
##写一个函数从一个回归模型及相应子数据框中，提取 R 方，将提取的结果保存为普通数值型列 r.squared：
frsqr <- function(mod){
  summary(mod)$r.squared
}
mod.cancer %>%
  mutate(
    r.squared = map_dbl(lmr, frsqr)) %>%
  select(-data, -lmr)
##map() 和 map_dbl() 中输入函数时可以用 purrr 包的无名函数写法
d.cancer %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    lmr = map(data, ~ lm(v1 ~ v0, data = .x)),
    r.squared = map_dbl(lmr, ~ summary(.x)$r.squared)) %>%
  select(-data, -lmr)
##也可以从每个模型提取多个值，这时，为了使得多个值在展开时能保存在同一行中，需要将每个子数据框的提取结果保存为一个一行的子数据框：
fextract <- function(mod){
  x1 <- coef(mod)
  tibble(
    intercept = x1[1],
    v0 = x1[2],
    r.squared = summary(mod)$r.squared
  )
}
mod.cancer %>%
  mutate(
    outlist = map(lmr, fextract))##结果的 outlist 列是列表类型的，每个元素是一个 1 × 3 的 tibble
##用 unnest 将每个组提取的回归结果转换为普通的数据框列
mod.cancer %>%
  mutate(
    outlist = map(lmr, fextract)) %>%
  select(-data, -lmr) %>%
  unnest(outlist)
##提取的结果也可以是一个不止一行的子数据框，例如，提取回归结果中的系数估计、标准误差、t 统计量和检验 p值组成的矩阵：
fcoefmat <- function(mod){
  as_tibble(summary(mod)$coefficients,
            rownames="term")
}
mod.cancer %>%
  mutate(
    outlist = map(lmr, fcoefmat)) %>%
  unnest(outlist) %>%
  select(-data, - lmr)
##提取统计模型的信息为规整的数据框格式
mod.cancer %>%
  mutate(
    outlist = map(lmr, broom::tidy)) %>%##tidy 函数，可以将统计模型的输出转换为数据框
  select(-data, -lmr) %>%
  unnest(outlist)
##unnest 提取出的信息也可以是一个向量，在展开时会展开到同一列中。例如，对每个组提取回归的拟合值：
mod.cancer %>%
  mutate(
    v1hat = map(lmr, ~ fitted(.))) %>%
  select(-lmr) %>%
  unnest(c(data, v1hat))

##summarise 统计量用列表表示
vnames <- expand_grid(
  var = c("v0", "v1"),
  stat = c("min", "max")) %>%
  pmap_chr(paste, sep="_")
d.cancer %>%
  group_by(type) %>%
  summarise(
    stat = list(vnames),
    value = list(c(range(v0), range(v1))) ) %>%
  unnest(c(stat, value))##将结果恢复成正常的数据框
##用长宽表转换方法变成每个统计量占一列：
d.cancer %>%
  group_by(type) %>%
  summarise(
    stat = list(vnames),
    value = list(c(range(v0), range(v1))) ) %>%
  unnest(c(stat, value)) %>%
  separate(stat, into = c("variable", "stat"), sep="_") %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )
##直接生成列表类型的列
##也可以直接生成列表类型的列，符合条件时可以用 unnest() 合并为大数据框。
d1 <- tibble(
  id = 1:2,
  df = vector("list", length=2))
d1[["df"]][1] <- list(
  tibble(x=1, y=2)
)
d1[["df"]][2] <- list(
  tibble(x=11:12, y=21:22)
)
d1 %>%
  unnest(cols = c(df))

##基本 R 的汇总功能
##对数值型向量 x，用 summary(x) 可以获得变量的平均值、中位数、最小值、最大值、四分之一和四分之三分位数。
summary(d.cancer[["v0"]])##数值型向量
summary(d.cancer[["v1"]])
##对一个数据框 d，用 summary(d) 可以获得每个连续型变量的基本统计量，和每个离散取值变量的频率。
summary(d.cancer)##数据框
##对数据框 d，用 str(d) 可以获得各个变量的类型和取值样例
str(d.cancer)##数据框
##分类变量概括
res <- table(d.cancer[["sex"]]); res##table(x) 返回 x 的每个不同值的频率（出现次数）
res["F"]
##用 as.data.frame() 函数把 table 的结果转为数据框:
as.data.frame(res)
##用 prop.table() 将频数转换成百分比
prop.table(res)
##table 作的单变量频数表可以用 barplot 表现为图形，如:
barplot(res, main=" 性别分布")
##对两个分类变量 x1 和 x2，其每个组合的出现次数可以用 table(x1,x2) 函数统计，结果叫做列联表
res2 <- with(d.cancer, table(sex, type)); res2##用 with() 函数引入一个数据框
as.data.frame(res2)##结果转为数据框
##列联表的结果可以用条形图表示
barplot(res2, legend=TRUE)##legend表示右上角图例
##或者
##beside默认为F，表示堆积条形图，为T表示并列
barplot(res2, legend=TRUE, beside=TRUE)
##对于 table() 的结果列联表，可以用 addmargins() 函数增加行和与列和
addmargins(res2)
##用 margin.table() 可以计算列联表行或列的和并返回
margin.table(res2, 1)##行和
margin.table(res2, 2)##列和
##用 prop.table(r) 把一个列联表 r 转换成百分比表
prop.table(res2)
##用 prop.table(res,1) 把列联表 res 转换成行百分比表
prop.table(res2, 1)
##用 prop.table(res,2) 把列联表 res 转换成列百分比表
prop.table(res2, 2)
##数据框概括
##如果 apply(x,1/2,FUN) 中的 FUN 对每个行/列变量得到多个 𝑚 结果，结果将是一个矩阵
apply(as.matrix(iris[,1:4]), 2,
      function(x)
        c(n=sum(!is.na(x)),
          mean=mean(x, na.rm=TRUE),
          sd=sd(x, na.rm=TRUE)))

##用基本 R 作分类概括
##用 tapply() 分组概括向量
##用 tapply() 函数进行分组概括, 格式为：tapply(X, INDEX, FUN)
##其中 X 是一个向量，INDEX 是一个分类变量，FUN 是概括统计函数。
with(
  d.cancer,
  tapply(v0, sex, mean))##分性别组计算疗前体积的均值
##用 aggregate() 分组概括数据框
##aggregate 函数对输入的数据框用指定的分组变量（或交叉分组）分组进行概括统计
aggregate(
  d.cancer[, c("age", "v0", "v1")],
  list(sex=d.cancer[["sex"]]),##按性别分组计算年龄、疗前体积、疗后体积的平均值
  mean, na.rm=TRUE)
##可以交叉分组后概括
with(
  d.cancer,
  aggregate(
    cbind(v0, v1),
    list(sex=sex, type=type),
    mean))
##用 split() 函数分组后概括
##split 函数可以把数据框的各行按照一个或几个分组变量分为子集的列表，然后可以用 sapply() 或 vapply() 对每组进行概括。
sp <- split(d.cancer[,c("v0","v1")], d.cancer[["sex"]])
sapply(sp, colMeans)


######################################################
#######Chapter 28 基本R绘图###########
##常用高级图形
##条形图
d.cancer <- readr::read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\cancer.csv", locale=locale(encoding="GBK"))
##统计男女个数并用条形图表示：
res1 <- table(d.cancer[,'sex']); print(res1)
barplot(res1)
##可以增加标题，采用不同的颜色：
barplot(res1, main=" 性别分布",
        col=c("brown2", "aquamarine1"))
##R 函数 colors() 可以返回 R 中定义的用字符串表示的六百多种颜色名字。
head(colors(), 6)
##用 width 选项与 xlim 选项配合可以调整条形宽度，如
barplot(res1, width=0.5, xlim=c(-3, 5),
        main=" 性别分布",
        col=c("brown2", "aquamarine1"))
##按性别与病理类型交叉分组后统计频数，结果称为列联表：
res2 <- with(d.cancer, table(sex, type)); res2
##用分段条形图表现交叉分组频数，交叉频数表每列为一条：
barplot(res2, legend=TRUE)
##用并排条形图表现交叉分组频数，交叉频数表每列为一组：
barplot(res2, beside=TRUE, legend=TRUE)
##增加标题，指定颜色，调整图例位置，调整条形宽度：
barplot(res2, beside=TRUE, legend=TRUE,
        main='不同种类病人的性别',
        ylim=c(0, 20),
        xlim=c(-1, 6), width=0.6,
        col=c("brown2", "aquamarine1"))
##直方图和密度估计图
##用 hist 作直方图以了解连续取值变量分布情况
x <- rnorm(30, mean=100, sd=1)
print(round(x,2))
hist(x, col=rainbow(15),
     main='正态随机数', xlab='', ylab='频数')
##函数 density() 估计核密度。下面的程序作直方图，并添加核密度曲线：
tmp.dens <- density(x)
hist(x, freq=FALSE,
     ylim=c(0,max(tmp.dens$y)),  
     col=rainbow(15),
     main='正态随机数',
     xlab='', ylab='频数')
lines(tmp.dens, lwd=2, col='blue')
##盒形图
##盒形图可以简洁地表现变量分布
with(d.cancer, boxplot(v0))
##盒形图可以很容易地比较两组或多组
with(d.cancer, boxplot(v0 ~ sex))
##可以画若干个变量的并排盒形图
with(d.cancer,
     boxplot(list('疗前'=v0, '疗后'=v1)))
##正态 QQ 图
##模拟正态分布随机数，并作正态 QQ 图
qqnorm(x)
qqline(x, lwd=2, col='blue')
##模拟对数正态数据，并作正态 QQ 图
z <- 10^rnorm(30, mean=0, sd=0.2)
qqnorm(z)
qqline(z, lwd=2, col='blue')
##散点图
d.class <- read_csv("E:\\00study\\MJ\\Rlearning\\R.DATA\\class.csv")
plot(d.class$height, d.class$weight)
##用 with() 函数简化数据框变量访问格式:
with(d.class,
     plot(height, weight))
##在 plot() 函数内用 main 参数增加标题，用 xlab 参数指定横轴标注，用 ylab 参数指定纵轴标注
with(d.class,
     plot(height, weight,
          main='体重与身高关系',
          xlab='身高', ylab='体重'))
##用 pch 参数指定不同散点形状，用 col 参数指定颜色，用 cex 参数指定大小
with(d.class,
     plot(height, weight,
          pch=16, col='blue',
          cex=2))
##用气泡大小表现第三维（年龄）：
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
##用 pairs() 函数可以做散点图矩阵：
pairs(d.class[, c('age', 'height', 'weight')])
##曲线图
##curve() 函数接受一个函数，或者一个以 x 为变量的表达式，以及曲线的自变量的左、右端点，绘制函数或者表达式的曲线图，
curve(1 - 3*x - x^2, -4, 2)
curve(sin, -2*pi, 2*pi)
##在 plot 函数中使用 type=’l’参数可以作曲线图
x <- seq(0, 2*pi, length=200)
y <- sin(x)
plot(x,y, type='l')
##仍可以用 main, xlab, ylab, col 等参数外，还可以用 lwd 指定线宽度，lty 指定虚线
plot(x,y, type='l', lwd=2, lty=3)
##多条曲线，可以用 matplot() 函数
x <- seq(0, 2*pi, length=200)
y1 <- sin(x)
y2 <- cos(x)
matplot(x, cbind(y1, y2), type='l',
        lty=1, lwd=2, col=c("red", "blue"),
        xlab="x", ylab="")
abline(h=0, col='gray')
##三维图
##下面的程序生成二元正态分布密度曲面数据：
x <- seq(-3,3, length=100)
y <- x
f <- function(x,y,ssq1=1, ssq2=2, rho=0.5){
  det1 <- ssq1*ssq2*(1 - rho^2)
  s1 <- sqrt(ssq1)
  s2 <- sqrt(ssq2)
  1/(2*pi*sqrt(det1)) * exp(-0.5 / det1 * (
    ssq2*x^2 + ssq1*y^2 - 2*rho*s1*s2*x*y))
}
z <- outer(x, y, f)
persp(x, y, z)##三维曲面图
contour(x, y, z)##等高线图
image(x, y, z)##色块图
##动态三维图
install.packages("rgl")
library(rgl)
with(iris, plot3d(
  Sepal.Length, Sepal.Width, Petal.Length,
  type="s", col=as.numeric(Species)))##type="s" 表示绘点符号是球体形状
##用 rgl 的 persp3d() 函数作曲面图。如二元正态分布密度曲面
x <- seq(-3,3, length=100)
y <- x
f <- function(x,y,ssq1=1, ssq2=2, rho=0.5){
  det1 <- ssq1*ssq2*(1 - rho^2)
  s1 <- sqrt(ssq1)
  s2 <- sqrt(ssq2)
  1/(2*pi*sqrt(det1)) * exp(-0.5 / det1 * (
    ssq2*x^2 + ssq1*y^2 - 2*rho*s1*s2*x*y))
}
z <- outer(x, y, f)
persp3d(x=x, y=y, z=z, col='red')

##低级图形函数
##abline()--在图中增加直线
with(d.class, plot(height, weight))
abline(-143, 3.9, col="red", lwd=2)##指定截距和斜率
abline(v=c(55,60,65,70), col="gray")##为竖线指定横坐标 (用参数 v)
abline(h=c(60,80,100,120,140), col="gray")##为水平线指定纵坐标(用参数 h)
##points()函数增加散点
x <- seq(0, 2*pi, length=200)
y <- sin(x)
special <- list(x=(0:4)*pi/2, y=sin((0:4)*pi/2))
plot(x, y, type='l')
points(special$x, special$y,
       col="red", pch=16, cex=2)
points(special, col="red", pch=16, cex=2)
##lines()函数增加曲线
x <- seq(0, 2*pi, length=200)
y1 <- sin(x)
y2 <- cos(x)
plot(x, y1, type='l', lwd=2, col="red")
lines(x, y2, lwd=2, col="blue")
abline(h=0, col='gray')
##图例--可以用 legend 函数增加标注
y1 <- sin(x)
y2 <- cos(x)
plot(x, y1, type='l', lwd=2, col="red")
lines(x, y2, lwd=2, col="blue")
abline(h=0, col='gray')
legend(0, -0.5, col=c("red", "blue"),
       lty=c(1,1), lwd=c(2,2),
       legend=c("sin", "cos"))##位置左下角
x <- seq(0, 2*pi, length=200)
y1 <- sin(x)
y2 <- cos(x)
plot(x, y1, type='l', lwd=2, col="red")
lines(x, y2, lwd=2, col="blue")
abline(h=0, col='gray')
legend('top', col=c("red", "blue"),
       lty=c(1,1), lwd=c(2,2),
       legend=c("sin", "cos"))##位置顶端
##axis()--用 axes=FALSE 可以取消自动的坐标轴
x <- c('一月'=15, '二月'=20,
       '三月'=18, '四月'=22)
plot(seq(along=x), x, axes=FALSE,##axes=FALSE 可以取消自动的坐标轴
     type='b', lwd=3,
     main='前四个月销售额',
     xlab='', ylab='销售额')
box()##用 box() 函数画坐标边框 
axis(2)##用 axis 函数单独绘制坐标轴
##axis 的第一个参数取 1，2，3，4，分别表示横轴、纵轴、上方和右方
##axis 的参数 at 为刻度线位置，labels 为标签。
axis(1, at=seq(along=x), labels=names(x))
##R 基本绘图支持少量的数学公式显示功能，如不用数学符号时：
x <- seq(0, 2*pi, length=200)
y1 <- sin(x)
plot(x, y1, type='l', lwd=2,
     axes=FALSE,
     xlab='x', ylab='')
abline(h=0, col='gray')
box()
axis(2)
axis(1, at=(0:4)/2*pi,
     labels=c('0', 'pi/2', 'pi', '3pi/2', '2pi'))
##使用数学符号时：
x <- seq(0, 2*pi, length=200)
y1 <- sin(x)
plot(x, y1, type='l', lwd=2,
     axes=FALSE,
     xlab='x', ylab='')
abline(h=0, col='gray')
box()
axis(2)
axis(1, at=(0:4)/2*pi,
     labels=c(0, expression(pi/2),
              expression(pi), expression(3*pi/2),
              expression(2*pi)))

##text()在坐标区域内添加文字
##mtext()在边空处添加文字
with(d.class, plot(height, weight))
lm1 <- lm(weight ~ height, data=d.class)
abline(lm1, col='red', lwd=2)
a <- coef(lm1)[1]
b <- coef(lm1)[2]
text(52, 145, adj=0, '线性回归:')
text(52, 140, adj=0,
     substitute(hat(y) == a + b*x,
                list(a=round(coef(lm1)[1], 2),
                     b=round(coef(lm1)[2], 2))))
##locator()
##locator() 函数在执行时等待用户在图形的坐标区域内点击并返回点击处的坐标
##可以用参数 n 指定要点击的点的个数，不指定个数则需要用右键菜单退出。
x <- seq(0, 2*pi, length=200)
y1 <- sin(x); y2 <- cos(x)
plot(x, y1, type='l',
     col="red")
lines(x, y2, col="blue")
legend(locator(1), col=c("red", "blue"),
       lty=c(1,1), legend=c("sin", "cos"))

##图形参数
opar <- par(mfrow=c(2,2))##画四幅画
with(d.class, {hist(height);
  boxplot(height);
  qqnorm(height); qqline(height);
  plot(height); rug(height,side=2)})
par(opar)##par 函数指定图形参数并返回原来的参数值，所以在修改参数值作图后通常应该恢复原始参数值
##在函数内，可以在函数开头修改了图形参数后，用 on.exit() 函数将恢复原始图形参数作为函数退出前必须完成的任务
f <- function(){
  opar <- par(mfrow=c(2,2)); on.exit(par(opar))##恢复原始图形参数
  with(
    d.class,
    {hist(height);
      boxplot(height);
      qqnorm(height); qqline(height);
      plot(height); rug(height,side=2)
    })
}
f()
##例子：用图形参数解决 barplot 图形横坐标值过宽
f <- function(){
  opar <- par(mar=c(8, 4, 2, 0.5)); on.exit(par(opar))##下左上右边距
  x <- 1:10
  names(x) <- paste(10000000000 + (1:10))
  barplot(x, las=2)
}
f()
##图形边空
opar <- par(mar=c(2,2,0.5,0.5),
            mgp=c(0.5, 0.5, 0), tck=0.005)###????
with(d.class, plot(height, weight,
                   xlab='', ylab=''))
par(opar)
##一页多图
##一页多图用 mfrow 参数或 mfcol 参数规定。用 oma 指定四个外边空的行数。
opar <- par(mfrow=c(2,2),
            oma=c(0,0,2,0))
with(d.class, {hist(height);
  boxplot(height);
  qqnorm(height); qqline(height);
  plot(height); rug(height,side=2)})
mtext(side=3, text='身高分布', cex=2, outer=T)##outer=T 指定在外边空添加文本
par(opar)

##图形输出
##PDF 输出
pdf(file='fig-hw.pdf', height=10/2.54,
    width=10/2.54, family='GB1')##设定生成图形的高度和宽度均为10cm,单位是英寸（1英寸等于2.54厘米）
with(d.class, plot(height, weight,
                   main='体重与身高关系'))
dev.off()##关闭当前设备并生成输出文件
##PNG 输出
png(file='fig-hw.png', height=1000, width=1000)
with(d.class, plot(height, weight,
                   main='体重与身高关系'))
dev.off()

##其它图形
##相关系数图
install.packages("corrgram")
library(corrgram)
R.iris <- cor(iris[,1:4])
print(round(R.iris, 2))
corrgram(
  R.iris, order=TRUE,
  lower.panel=panel.shade,
  upper.panel = panel.pie,
  text.panel = panel.txt
)



######################################################
#######Chapter 29 ggplot 作图入门###########
#######CH29-30略#############


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
##用 nlm() 函数求最小值点（eg求正态分布最大似然估）
norm1d.mledemo2 <- function(){
  set.seed(1)
  n <- 30
  mu <- 20
  sig <- 2
  z <- rnorm(n, mean=mu, sd=sig)
  neglogL <- function(parm) {
    -sum( dnorm(z, mean=parm[1],
                sd=exp(parm[2]), log=TRUE) )
  }##没看懂这块计算
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


























































































































































