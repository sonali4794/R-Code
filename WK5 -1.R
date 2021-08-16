library("tidyverse")
library("dplyr")
library("mosaic")

xtabs(~group+event, data=predimed) %>% 
  prop.table %>% 
  round(2)
d1 = replace(predimed$group, predimed$group == "MedDiet + VOO"|predimed$group == "MedDiet + Nuts", "Med Diet")
d1
d2 = predimed %>% mutate(d1)
d2
xtabs(~d1+event, data=d2) %>% 
  prop.table %>% 
  round(4)

p = 0.0153
q = 0.0245

onlycontrol = d2 %>% filter(d2$d1 == "Control")
onlymed = d2 %>% filter(d2$d1 == "Med Diet")
N = nrow(onlycontrol)
M = nrow(onlymed)
N
M

diff = p-q
var = p*(1-p)/N + q*(1-q)/M
se = sqrt(var)
se

LN = diff - 1.96*se
UN = diff + 1.96*se

x = rbinom(N, 1, p)
y = rbinom(M, 1, q)

boot = do(1000)*{
  btx = resample(x)
  phat = sum(btx)/N
  bty = resample(y)
  qhat = sum(bty)/M
  phat - qhat
}

confint(boot)
hist(boot$result)

z = (diff - 0)/se
pvalue = pnorm(-abs(z))
pvalue