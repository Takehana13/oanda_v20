library(readr)
data <- read_csv("~/OneDrive/FXTF/EURUSD_M1.csv", col_names = FALSE)
colnames(data) <- c("datetime", "open", "high", "low", "close", "volume")

data$date <- data$datetime %>% substr(1, 10)
data$time <- data$datetime %>% substr(12, 16)

Y <- data$date %>% str_sub(1, 4)
M <- data$date %>% str_sub(6, 7)
D <- data$date %>% str_sub(9, 10)
h <- data$time %>% str_sub(1, 2)
m <- data$time %>% str_sub(4 ,5)

date_ <- str_c(Y, M, D, sep = "/")
time_ <- str_c(h, m, sep = ":")

open_ <- data$open
high_ <- data$high
low_ <- data$low
close_ <- data$close

DATA <- data.frame(date = date_, time = time_, open = open_, high = high_, low = low_, close = close_)

write_csv(DATA, "test.csv")
