#' 绘制股票分析图（包含残差分析和蜡烛图）
#'
#' 该函数获取指定股票的历史数据，进行线性回归残差分析，并生成蜡烛图，保存为 PDF 文件。
#'
#' @param stockname 字符串类型，表示股票代码，默认为 \code{"600048.SS"}。
#'
#' @return 无返回值。函数生成一个 PDF 文件，包含蜡烛图、残差的箱线图和直方图。
#'
#' @details
#' 函数获取指定股票的日频历史数据，并对其进行分析：
#' \itemize{
#'   \item 蜡烛图：显示股票的开盘、最高、最低和收盘价的变化趋势。
#'   \item 箱线图：展示线性回归残差的分布特征。
#'   \item 直方图：展示残差的分布情况。
#' }
#'
#' @examples
#' \dontrun{
#' plot_stock("600048.SS")
#' }
#'
#' @export
plot_stock <- function(stockname = "600048.SS") {
  # 获取股票数据
  ss001 <- getStocks(stock = stockname, start = "2020-01-01", end = Sys.Date())

  # 计算残差
  mydata <- na.omit(ss001)
  mydata$number <- 1:nrow(mydata)
  mylm <- lm(Adjusted ~ number, mydata)
  mydata$residuals <- as.vector(mylm$residuals)

  # 保存 PDF 图表，宽度为高度的两倍
  pdf(paste(stockname, ".pdf", sep = ""), width = 12, height = 6)

  # 调整布局，3 行 1 列
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  # 调整边距

  # 绘制蜡烛图
  library(quantmod)
  chartSeries(ss001, name = paste("蜡烛图:", stockname), theme = chartTheme("white"))

  # 箱线图
  myboxplot <- boxplot(mydata$residuals, xlab = paste("残差箱线图:", stockname))
  abline(h = tail(mydata$residuals, 1), col = "red")

  # 直方图
  hist(mydata$residuals, breaks = 100,
       col = 'darkgray', border = 'white',
       xlab = paste("残差直方图:", stockname))
  abline(v = tail(mydata$residuals, 1), col = "red")
  abline(v = myboxplot$stats[2], col = "blue")
  abline(v = myboxplot$stats[3], col = "blue")
  abline(v = myboxplot$stats[4], col = "blue")

  dev.off()
}



