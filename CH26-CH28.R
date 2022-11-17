######################################################
#######Chapter 26 数据整理###########
##tidyverse 系统
##载入 tidyverse 包，则 magrittr 包，readr 包，dplyr 包和 tidyr 包都会被自动载入:
install.packages("tidyverse")
install.packages("NHANES")
library(tidyverse)
d.class <- read_csv(
  "E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv",
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
      x[sex == "F"] <- " 女"
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
dwide1<-read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\dwide1.csv")
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
dwide2 %>%
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
  "E:\\00study\\why not study\\Rlearning\\R.DATA\\cancer.csv", locale=locale(encoding="GBK"))
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
d.cancer <- readr::read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\cancer.csv", locale=locale(encoding="GBK"))
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
d.class <- read_csv("E:\\00study\\why not study\\Rlearning\\R.DATA\\class.csv")
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
#######略略略#############














