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
d.class <- readr::read_csv('E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv')
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
d1 <- modify(d.class, ~ if(is.numeric(.x)) .x - median(.x) else .x)
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
d.class <- readr::read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
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

