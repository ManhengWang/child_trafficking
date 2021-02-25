
#Unable to correspond "robust sd errors" in OLS model




# Loading the required libraries
library(plm)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(AER)

data=ProvTraffick
# Pooled OLS model
ols1 <- lm(childrenpm ~fine+factor(year), data)
ols2 <- lm(childrenpm ~lggb0+factor(year), data)
ols3 <- lm(childrenpm ~fine+lggb0+factor(year), data)
ols4 <- lm(childrenpm ~fine+lggb0+gdppc+popr+factor(year), data)
vcov1=cluster.vcov(ols1, ProvTraffick$pro)
cse1 = coeftest(ols1, vcov1)
vcov2=cluster.vcov(ols2, ProvTraffick$pro)
cse2 = coeftest(ols2, vcov2)
vcov3=cluster.vcov(ols3, ProvTraffick$cityid)
cse3 = coeftest(ols3, vcov3)
vcov4=cluster.vcov(ols4,ProvTraffick$cityid)
cse4 = coeftest(ols4, vcov4)

#Random Effect models
re1 <- plm(childrenpm ~fine+factor(year), data, model = "random", index = c("pro","year"))
re2 <- plm(childrenpm ~lggb0+factor(year), data, model = "random", index = c("pro","year"))
re3 <- plm(childrenpm ~fine+lggb0+factor(year), data, model = "random", index = c("pro","year"))
re4 <- plm(childrenpm ~fine+lggb0+gdppc+popr+factor(year), data, model = "random", index = c("pro","year"))

#Fixed Effect model
fe1 <- plm(childrenpm ~fine+factor(year), data, model = "within", index = c("pro","year"))
#Instrumental Variable models
iv1 = ivreg(childrenpm~lggb0+fine+factor(year)| fine+factor(year)+lggb1 , data = data)
iv2 = ivreg(childrenpm~fine+lggb0+gdppc+popr+factor(year) | lggb1+fine+gdppc+popr+factor(year) , data = data)

#TableS2
stargazer(ols1, ols2,ols3,ols4,re1,re2,re3,re4,fe1,iv1,iv2,
          column.labels =c("ols1","ols2","ols3","ols4","re1","re2","re3","re4","fe1","iv1","iv2"),
          dep.var.labels = "Number of Trafficked Childern divided by the Population (milion)",
          type = "text", df = FALSE, digits = 4, omit = "year", out = "TableS2.html")
