#' 计算带有加权的五数概括（九数概括）
#'
#' 此函数基于R语言的五数概括（min, Q1, median, Q3, max）计算一个扩展的九数概括。
#' 该函数首先通过1.5倍四分位距计算上下限，并根据这些限制调整五数概括中的最大值和最小值。
#' 然后根据上四分位数和下四分位数的差距，计算加权的中位数加值，生成九数概括。
#'
#' @param data 向量类型的数据，默认为1到20的整数向量。用于计算五数概括的输入数据。
#'
#' @return 返回一个包含九个值的向量，分别是：
#' 1. 最小值
#' 2. 下四分位数
#' 3. 中位数
#' 4. 加数1
#' 5. 加数2
#' 6. 上四分位数
#' 7. 加数3
#' 8. 加数4
#' 9. 最大值
#'
#' @examples
#' boxfivenum9(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' boxfivenum9(1:100)
boxfivenum9 <- function(data = 1:20) {
  # 计算五数概括
  five <- fivenum(data)
  IQR <- five[4] - five[2]  # 四分位距（IQR）
  upperHinge <- five[4] + 1.5 * IQR  # 上四分位数修正
  lowerHinge <- five[2] - 1.5 * IQR  # 下四分位数修正

  # 修正五数中的最大值和最小值，确保它们在上下限范围内
  five[c(1, 5)] <- pmin(pmax(five[c(1, 5)], lowerHinge), upperHinge)

  # 定义计算加权值的函数（用于生成加权的上四分位数和下四分位数）
  calculate_add <- function(from, to) {
    c(from + (to - from) * 1/3, from + (to - from) * 2/3)
  }

  # 计算下四分位数加权值和上四分位数加权值
  add12 <- calculate_add(five[3], five[4])
  add34 <- calculate_add(five[4], five[5])

  # 返回九数概括
  return(c(five[1:3], add12, five[4], add34, five[5]))
}

#' 获取买入或卖出点
#'
#' 该函数根据输入的时间序列数据，通过线性回归计算残差，并使用九数概括法生成相应的买卖信号。还会计算每日的涨幅。
#'
#' @param ss001 一个包含股票调整后收盘价（Adjusted）的数据框或 xts 对象。
#'
#' @return 返回一个包含以下字段的数据框或 xts 对象：
#' \item{Q1}{Q1值，通过拟合值加上残差的九数概括得出。}
#' \item{Q2}{Q2值，通过拟合值加上残差的九数概括得出。}
#' \item{Q3}{Q3值，通过拟合值加上残差的九数概括得出。}
#' \item{Q4}{Q4值，通过拟合值加上残差的九数概括得出。}
#' \item{Q5}{Q5值，通过拟合值加上残差的九数概括得出。}
#' \item{Q6}{Q6值，通过拟合值加上残差的九数概括得出。}
#' \item{Q7}{Q7值，通过拟合值加上残差的九数概括得出。}
#' \item{Q8}{Q8值，通过拟合值加上残差的九数概括得出。}
#' \item{Q9}{Q9值，通过拟合值加上残差的九数概括得出。}
#' \item{judge}{根据残差的值判断买入或卖出信号，可能的值有："strongbuy", "buy", "holdbuy", "holdselldown",
#' "holdsell", "holdsellup", "selldown", "sell", "sellup", "strongsell"。}
#' \item{increase}{每日涨幅，计算方法为每日收盘价的变化百分比。}
#'
#' @examples
#' # 假设 ss001 是一个包含股票调整后收盘价的 xts 数据
#' result <- getsellpoint(ss001)
#'
#' @export
#'
#library(xts)
getsellpoint <- function(ss001) {
  # 删除缺失值并重建数据框
  mydata <- na.omit(ss001)
  mydata$number <- seq_along(mydata$Adjusted)  # 使用 seq_along 更简洁

  # 线性回归计算拟合值和残差
  mylm <- lm(Adjusted ~ number, data = mydata)
  mydata$residuals <- as.vector(residuals(mylm))
  mydata$fitted.values <- as.vector(fitted(mylm))

  # 获取九数概括的值
  myboxninenum <- boxfivenum9(as.vector(mydata$residuals))

  # 直接计算 Q1 到 Q9 的值
  mydata$Q1 <- mydata$fitted.values + myboxninenum[1]
  mydata$Q2 <- mydata$fitted.values + myboxninenum[2]
  mydata$Q3 <- mydata$fitted.values + myboxninenum[3]
  mydata$Q4 <- mydata$fitted.values + myboxninenum[4]
  mydata$Q5 <- mydata$fitted.values + myboxninenum[5]
  mydata$Q6 <- mydata$fitted.values + myboxninenum[6]
  mydata$Q7 <- mydata$fitted.values + myboxninenum[7]
  mydata$Q8 <- mydata$fitted.values + myboxninenum[8]
  mydata$Q9 <- mydata$fitted.values + myboxninenum[9]
  #要增加的列
  mydata$judge <- NA

  # 使用 cut 函数，并确保将因子转换为字符型
  myjudge<-cut(mydata$residuals,
               breaks = c(-Inf, myboxninenum, Inf),
               labels = c("strongbuy", "buy", "holdbuy", "holdselldown",
                          "holdsell", "holdsellup", "selldown",
                          "sell", "sellup", "strongsell"))

  myjudge<-xts(myjudge,order.by=index(mydata))
  mydata$judge <-myjudge
  # # 计算每日涨幅
  #mydata$increase <- dailyReturn(mydata$Adjusted)

  todayP<-as.numeric(mydata$Adjusted)
  beforeP<-c(todayP[1],todayP[-length(todayP)])
  mydata$increase<-as.character((todayP-beforeP)/beforeP*100)
  return(mydata)
}


#' 获得股票的前 n 天数据
#'
#' 该函数从传入的股票数据中删除最后 n 天的数据。
#'
#' @param sz50 一个列表，每个元素是一个时间序列数据（例如 xts 对象），表示股票的每日数据。
#' @param deleN 一个整数，表示要删除的天数。如果 deleN 为 0，则返回原始数据。
#'
#' @return 返回一个列表，包含每个股票去除最后 n 天的数据。
#'
#' @examples
#' # 假设 sz50 是一个包含股票数据的列表
#' # 删除每个股票数据的最后 5 天数据
#' result <- deleFromEnd(sz50, deleN = 5)
#'
#' @export
deleFromEnd <- function(sz50, deleN = 0) {
  # 如果 deleN 为 0，直接返回原始数据
  if (deleN == 0) {
    return(sz50)
  }

  # 计算要删除的天数
  num <- deleN - 1

  # 对每个股票数据删除最后 n 天的数据
  mydata <- lapply(sz50, function(x) x[-(nrow(x) - num):nrow(x), ])

  return(mydata)
}


#' 更新和保存股票数据
#'
#' 该函数将新的股票数据添加到已有的数据框 `saved_statable` 中，并去除重复的日期项，按日期排序后保存。
#'
#' @param my 数据框，默认值为 `statable`，表示需要更新的数据。此数据框应包含股票数据，并有一个 `Date` 列。
#'
#' @return 返回更新后的 `saved_statable` 数据框，包含去重和排序后的股票数据。
#'
#' @examples
#' updated_data <- satstock(new_data)
satstock <- function(my = statable) {
  setwd("E:/FangCloudSync/R_WD360/Project/Stocks/shangzheng50")  # 设置工作目录
  load("statalbe")  # 加载已有的统计数据

  # 将新的数据与已有的数据合并
  saved_statable <- rbind(saved_statable, my)

  # 转换为数据框并去重
  saved_statable <- data.frame(saved_statable)
  saved_statable <- saved_statable[!duplicated(saved_statable$Date), ]

  # 按日期排序
  saved_statable <- saved_statable[order(saved_statable$Date), ]

  # 保存更新后的数据
  save(saved_statable, file = "statalbe")

  return(saved_statable)  # 返回更新后的数据框
}


#' 检查股票在指定日期范围内的买入和卖出情况
#'
#' 该函数根据输入的股票数据和指定的日期范围，
#' 检查在前一段时间（before）和当前时间（current）内，
#' 指定类型的股票（如“strongsell”）是否在两个时间段内都有出现，
#' 或者在某一时间段内出现。
#'
#' @param sz50 数据框或xts对象，包含股票的历史数据，默认使用 `mydata` 数据集。
#' @param before 前一个时间段的天数，默认为1，表示在当前时间之前的1天。
#' @param current 当前时间段的天数，默认为0，表示当前时间。
#' @param stock_type 股票的买入或卖出类型，默认为 "strongsell"，表示强烈卖出类型。
#'
#' @return 返回一个列表 `mylist`，其中包含以下三个元素：
#' \item{common}{在两个时间段内都有出现的股票代码。}
#' \item{in_}{在当前时间段内出现，但在前一个时间段内未出现的股票代码。}
#' \item{out_}{在前一个时间段内出现，但在当前时间段内未出现的股票代码。}
#'
#' @examples
#' \dontrun{
#' # 假设已经加载了 sz50 数据
#' result <- check_in_out(sz50 = mydata, before = 5, current = 0, stock_type = "strongsell")
#' print(result)
#' }
check_in_out <- function(sz50 = mydata, before = 1, current = 0, stock_type = "strongsell") {
  myd1 <- deleFromEnd(sz50, before)
  myd2 <- deleFromEnd(sz50, current)

  # 获取前一个时间段的股票卖出点
  myd1 <- lapply(myd1, getsellpoint)
  myd1_last <- lapply(myd1, tail, 1)
  myd1_last <- data.table::rbindlist(myd1_last)

  # 获取当前时间段的股票卖出点
  myd2 <- lapply(myd2, getsellpoint)
  myd2_last <- lapply(myd2, tail, 1)
  myd2_last <- data.table::rbindlist(myd2_last)

  # 根据股票类型筛选
  type1 <- myd1_last$name[myd1_last$judge == stock_type]
  type2 <- myd2_last$name[myd2_last$judge == stock_type]

  # 返回三类股票：在两个时间段内都有出现的、在当前时间段内出现的、在前一个时间段内出现的
  mylist <- list(
    common = intersect(type1, type2),  # 两个时间段都出现的股票
    in_ = setdiff(type2, type1),       # 仅当前时间段出现的股票
    out_ = setdiff(type1, type2)       # 仅前一个时间段出现的股票
  )

  return(mylist)
}


