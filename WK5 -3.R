library("tidyverse")
library("dplyr")
library("mosaic")
set = c(12, 18,15,8,17,13,22,13,13,13,12,11,15,15,12,8,20,12,14,11,9,15,16,20,9,15,13,19,18,14)
s = sum(set)
size = length(set)
a = s/size
SE = sqrt(a/size)

LN = a - 1.96*SE
UN = a + 1.96*SE
LN
UN

poissonsample = rpois(30, a)

boot = do(1000)*{
  btx = resample(poissonsample)
  mean(btx)
}

confint(boot)
hist(boot$result)