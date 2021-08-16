library("tidyverse")
library("dplyr")
library("mosaic")
s = sum(stocks_bonds$SP500 <= -0.1)
n = nrow(stocks_bonds)
mean = s/n
SE = sqrt(mean*(1-mean)/n)
LN = mean - 1.96*SE
UN = mean + 1.96*SE
LN
UN
