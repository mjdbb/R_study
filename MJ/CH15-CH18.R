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
d <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv")
d <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",
              locale=locale(encoding="GBK"))##读入用 GBK 编码的中文 CSV 文件
d
##缺失值设置
##read_csv() 将空缺的值读入为缺失值，将 “NA” 也读入为缺失值。
##可以用 na= 选项改变这样的设置。也可以将带有缺失值的列先按字符型原样读入，然后再进行转换。
##先将血压列按字符型读入，再增加一列转换为数值型的列，非数值转换为 NA:
d <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",
              locale=locale(encoding="GBK"))
d[[" 收缩压数值"]] <- as.numeric(d[["收缩压"]])##增加一列转换为数值型的列，非数值转换为 NA
d
##各列类型设置
##cols()函数可以用来规定各列类型
##col_types 选项可以指定每一列的类型，如"col_double()","col_integer()", "col_character()", "col_factor()", "col_date()", "col_datetime" 等。
d <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv", locale=locale(encoding="GBK"),
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
d.class <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
d.class <- read_csv(
  "E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv", col_types=cols(
    .default = col_double(),
    name=col_character(),
    sex=col_factor(levels=c("M", "F")) ))##转换性别列的类型为因子
str(d.class)##显示对象的主要结构和典型内容

##读入日期
d.dates <- read_csv('E:\\00study\\why not study\\Rlearning\\R.DATA\\dates.csv',
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
  'E:\\00study\\why not study\\Rlearning\\R.DATA\\dates.csv', locale=locale(encoding="GBK"),
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
read_excel('E:/00study/why not study/Rlearning/path.xlsx', sheet = 1, col_names = TRUE,
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
ll <- readLines("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
print(head(ll, 3))
ll <- readr::read_lines("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
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
fin <- file("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv", "rt")
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
read.csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",col.names = T)
readLines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv")
##以 UTF-8 编码的文件能正确读入
read.csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
readLines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8bom.csv")
##加 fileEncoding="编码类型"选项可以纠正编码问题：
read.csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",fileEncoding="GBK")
readLines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",encoding="GBK")##???????
##readr 包的 read_csv()、read_table2()、read_lines() 函数默认从 UTF-8 编码的文件中读取，无 BOM 或者有 BOM 都可以
read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8bom.csv")
read_lines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8nobom.csv")
read_lines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp-utf8bom.csv")
##对 GBK 编码的文件，不能直接读取
read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv")
read_lines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv")
##读取 GBK(或 GB18030) 编码的文件，在 read_csv() 和 read_lines() 函数中加入 locale=locale(encoding="GBK")选项
read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",locale=locale(encoding="GBK"))
read_lines("E:\\00study\\why not study\\Rlearning\\R.DATA\\bp.csv",locale=locale(encoding="GBK"))
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


































































































