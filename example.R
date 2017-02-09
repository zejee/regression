#clear everything
rm(list = ls())

require(sandwich)
require(lmtest)

# reads the data
data = read.csv("OkunData.csv")
unemp = ts(data[,2],start=c(1949,1), frequency=4) 
ly = ts(data[,3],start=c(1949,1), frequency=4)
rec = ts(data[,4],start=c(1949,1), frequency=4)

# transform the data
dunemp = diff(unemp)
dly = diff(ly)
plot(dly, dunemp)

# run a linear regression
reg = lm(dunemp ~ dly)
summary(reg)

# shor the relationship between correlation and OLS coefficient
cor(dunemp, dly)

cor(dunemp, dly)*sd(dunemp)/sd(dly)

# run a linear regression
reg2 = lm(dly~dunemp)
summary(reg2)

1/coef(reg)[2]

reg$hacse<-vcovHC(reg)
coeftest(reg,vcov. = reg$hacse)