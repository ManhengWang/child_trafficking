# Loading the required libraries
setwd("~/Desktop/child trafficking/data_code/data")
rm(list=ls())
library(plm)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(AER)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(haven)

CityTraffick = read_dta("CityTraffick.dta")

# Pooled OLS model [[[index????]]]
#ols1 <- plm(children~fine+popscale, data=CityTraffick, model="pooling", index = c("year", "cityid"))
ols1 <- lm(children~fine+popscale+factor(year), data=CityTraffick, index = c("year", "cityid"))
ols2 <- lm(children~lggb0+popscale+factor(year), data=CityTraffick, index = c("year", "cityid"))
ols3 <- lm(children~fine+lggb0+popscale+factor(year), data=CityTraffick, index = c("year", "cityid"))


#Instrumental Variable models
iv4 = ivreg(children~lggb0+popscale+factor(year) | lggb1+popscale+factor(year) , data = CityTraffick, index = c("year", "cityid"))
iv5 = ivreg(children~fine+lggb0+popscale+factor(year) | lggb1+fine+popscale+factor(year) , data = CityTraffick, index = c("year", "cityid"))
# Fixed effects model
fe6 <- plm(children~fine+factor(year), data=CityTraffick, model="within", index = c("cityid", "year"))

vcov1=cluster.vcov(ols1, CityTraffick$cityid)
cse1 = coeftest(ols1, vcov1)
vcov2=cluster.vcov(ols2, CityTraffick$cityid)
cse2 = coeftest(ols2, vcov2)
vcov3=cluster.vcov(ols3, CityTraffick$cityid)
cse3 = coeftest(ols3, vcov3)
vcov4=cluster.vcov(iv4, CityTraffick$cityid)
cse4 = coeftest(iv4, vcov4)
vcov5=cluster.vcov(iv5, CityTraffick$cityid)
cse5 = coeftest(iv5, vcov3)

#Table 1
stargazer(ols1, ols2, ols3, iv4, iv5, fe6, se = list(cse1[,2], cse2[,2], cse3[,2], cse4[,2], cse5[,2]),
           type = "text", df = FALSE, digits = 4, omit = "year", out = "Table1.html")

aggdata <-aggregate(CityTraffick, by=list(CityTraffick$year),  FUN=mean, na.rm=TRUE)
aggdata$Group.1 = NULL
                    
tmp_date <- data %>% sample_frac(0.3)


aggdata %>% 
  ggplot(aes(x=fine, y=children, label=year)) +
  geom_point() +     geom_text_repel(size = 5) +
  geom_segment(aes(
    xend=c(tail(fine, n=-1), NA), 
    yend=c(tail(children, n=-1), NA)
  ),
  arrow=arrow(length=unit(0.3,"cm"))
  )  + theme_bw() + theme(text = element_text(size = 20), axis.text = element_text(size = 20),
                          legend.text = element_text(size = 20))
