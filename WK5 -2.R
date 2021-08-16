library("tidyverse")
library("dplyr")
library("mosaic")
group1 = GasPrices %>% filter(GasPrices$Highway == "Y")
group1
group2 = GasPrices %>% filter(GasPrices$Highway == "N")
group2
mean1 = mean(group1$Price)
mean1
mean2 = mean(group2$Price)
mean2
var1 = var(group1$Price)
var1
var2 = var(group2$Price)
var2
n1 = nrow(group1)
n1
n2 = nrow(group2)
n2

diff = mean1 - mean2
diff

se = sqrt((var1/n1) + (var2/n2))
se

LN = diff - 1.96*se
UN = diff + 1.96*se
LN
UN

boot = do(10000)*{
  btx = resample(group1)
  phat = mean(btx$Price)
  bty = resample(group2)
  qhat = mean(bty$Price)
  phat - qhat
}

confint(boot)
hist(boot$result)

z = (diff - 0)/se
pvalue_onesided = pnorm(-abs(z))
pvalue_onesided
pvalue_twosided = 2*pnorm(-abs(z))
pvalue_twosided

