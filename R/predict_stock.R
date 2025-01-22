# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# install.packages("quantmod")
# install.packages("TTR")
# install.packages("dplyr")
# install.packages("ggplot2")


#' 获取股票数据
#'
#' 这个函数从 Yahoo Finance 获取指定股票代码的历史数据。
#' 用户可以指定股票代码、开始日期和结束日期来获取相应的股票历史数据。
#'
#' @param stock_code 一个字符型参数，指定股票代码，例如 "AAPL" 或 "000001.SS"。
#' @param start_date 一个字符型参数，指定开始日期，格式为 "YYYY-MM-DD"（默认为 "2020-01-01"）。
#' @param end_date 一个字符型参数，指定结束日期，格式为 "YYYY-MM-DD"（默认为当前日期）。
#' @return 返回一个 xts 对象，包含指定股票的历史数据，包括开盘价、最高价、最低价、收盘价、成交量等。
#' @examples
#' \dontrun{
#' stock_data <- get_stock_data("AAPL", "2020-01-01", "2021-01-01")
#' }
#' @export
get_stock_data <- function(stock_code, start_date = "2020-01-01", end_date = Sys.Date()) {
  library(quantmod)
  stock_data <- getSymbols(stock_code, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  return(stock_data)
}


predict_stock <- function(stock_data) {
  # 简单的股票预测模型示例（比如使用移动平均线）
  library(TTR)
  ma <- SMA(Cl(stock_data), n = 20)
  return(ma)
}
