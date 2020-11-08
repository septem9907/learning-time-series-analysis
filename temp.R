library(tidyverse)
library(lubridate)
library(quantmod)

lubridate::ymd("2018-07-02")


x <- scan("bp500-2691.txt")

d <- read_table2(
  "./data/d-ibm-0110.txt", col_types=cols(
    .default=col_double(),
    date=col_date(format="%Y%m%d")))


d.apple <- tibble(
  date=c(ymd("2011-12-02"),
         ymd("2011-12-05") + days(0:4)),
  price=c(389.70, 393.01, 390.95, 389.09, 390.66, 393.62)
)
knitr::kable(d.apple)

AAPL <- getSymbols("AAPL", src="yahoo", auto.assign = FALSE)
