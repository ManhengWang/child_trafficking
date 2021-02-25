# Loading the required libraries
library(plm)
library(lmtest)
library(multiwayvcov)
library(stargazer)
library(AER)

# Pooled OLS model
ols1 <- lm(adoption~fine+lggb0, data=Adoption)
ols2 <- lm(adoption~fine+lggb0+mino+agri, data=Adoption)

#Instrumental Variable models
iv3 = ivreg(adoption~fine+lggb0 | lggb1 +fine, data = Adoption)
iv4 = ivreg(adoption~fine+lggb0+mino+agri | lggb1+fine+mino+agri , data = Adoption)

#cluster
vcov1=cluster.vcov(ols1, Adoption$citygb)
cse1 = coeftest(ols1, vcov1)
vcov2=cluster.vcov(ols2, Adoption$citygb)
cse2 = coeftest(ols2, vcov2)
vcov3=cluster.vcov(iv3, Adoption$citygb)
cse3 = coeftest(iv3, vcov3)
vcov4=cluster.vcov(iv4, Adoption$citygb)
cse4 = coeftest(iv4, vcov4)


#Table 2
stargazer(ols1, ols2,iv3,iv4, se = list(cse1[,2], cse2[,2], cse3[,2], cse4[,2]),
           type = "text", df = FALSE, digits = 4, out = "Table2.html")



