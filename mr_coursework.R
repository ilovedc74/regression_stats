setwd("~/Documents/Panc/Statistics/Coursework")
dat = read.csv("MSc_2018_19_coursework_data.csv")

# install.packages("data.table")
library(data.table)
dat = as.data.table(dat)
# View(dat)
# summary(dat)

# install.packages("scales")
library("scales")
# rescale(c(-10, -9, -5, 2, 6), to = c(0, 100)) ## Use scales:::rescale() if you have several packages loaded using the same function name
 # 0.00   6.25  31.25  75.00 100.00


# install.packages("VGAM")
library("VGAM")

dat[,n_visatt := rescale(reciprocal(dat$visatt), to = c(0,1)),]
dat[,n_cp := rescale(reciprocal(dat$cp), to = c(0,1)),]
dat[,age := dat$X...age,]
summary(dat)
View(dat)

colnames(dat)
??setname
# install.packages("Hmisc", dependencies = T)
library('Hmisc')
dat.cor = rcorr(as.matrix(dat))
print(dat.cor)
# dat.cor$r

# install.packages("corrplot")
library(corrplot)      
corrplot(dat.cor$r, type = "upper", 
         tl.col = "black", tl.srt = 45)
# cem
fit1 = lm(dat$cwm ~ dat$n_visatt + dat$n_cp + dat$cem + dat$awm)
summary(fit1)

# Best model fit1.1
fit1.1 = lm(dat$cwm ~ dat$n_visatt + dat$n_cp + dat$awm)
summary(fit1.1)

fit0 = lm(dat$cwm ~ dat$n_visatt + dat$n_cp)
summary(fit0)

# cem
fit1.2 = lm(dat$cwm ~ dat$n_visatt + dat$cem + dat$awm)
summary(fit1.2)

fit2 = lm(dat$cwm ~ dat$n_visatt + dat$n_cp + dat$cem + dat$awm + dat$gi)
summary(fit2)

fit2.1 = lm(dat$cwm ~ dat$n_visatt + dat$n_cp + dat$awm)
summary(fit2.1)

fit2.2 = lm(dat$cwm ~ dat$n_visatt + dat$cem + dat$awm + dat$gi)
summary(fit2.2)


# nrow(dat)
# sapply(dat, hist, main=colnames(dat[i]), xlab="x")

library(ggplot2)

# curvilinearity
pairs(~age+yoe+n_visatt+n_cp+cem+awm+gi+cwm,data=dat, 
        main="Simple Scatterplot Matrix")
# normality
# install.packages("moments")
library(moments)
skewness(dat)
kurtosis(dat)

install.packages("e1071")

library(rela)
D = as.matrix(dat)
itemanal(D, SE.par = 1.96)

for (i in dat) {
  print(shapiro.test(i))
}
# install.packages("lmSupport")
install.packages("lm.beta")
library(lm.beta)
library(lmSupport)
# best with awm
fit1.1 = lm(cwm ~ n_visatt + n_cp + awm, data = dat)
lm.sumSquares(fit1.1)
summary(fit1.1)
lm.beta(fit1.1)

fit3.1 = lm(cwm ~ age + yoe, data = dat)
lm.sumSquares(fit3.1)
summary(fit3.1)
lm.beta(fit3.1)
fit3.2 = lm(cwm ~ age + yoe + n_visatt + n_cp + awm, data = dat)
lm.sumSquares(fit3.2)
fit3.3 = lm(cwm ~ age + yoe + n_visatt + n_cp, data = dat)
lm.sumSquares(fit3.3)
modelCompare(fit3.1, fit3.2)
modelCompare(fit3.1, fit3.3)
 

# Hierarchical
m0 = lm(cwm ~ 1, data = dat)
m1 = lm(cwm ~ age + yoe, data = dat)
m1.5 = lm(cwm ~ age + yoe + n_visatt, data = dat)
m2 = lm(cwm ~ age + yoe + n_visatt + 
                   n_cp, data = dat)
anova(m0)
anova(m1, m1.5, m2)
summary(m1)
summary(m1.5)
summary(m2)
# hierarchical standardized
m0.z = lm(scale(cwm) ~ 1, data = dat)
m1.z = lm(scale(cwm) ~ scale(age) + 
                     scale(yoe), data = dat)
m1.5.z = lm(scale(cwm) ~ scale(age) + 
            scale(yoe) + scale(n_visatt), data = dat)
m2.z = lm(scale(cwm) ~ scale(age) + 
                     scale(yoe) + scale(n_visatt) + 
                     scale(n_cp), data = dat)
summary(m0.z)
summary(m1.z)
summary(m1.5.z)
summary(m2.z)
# Scale standardized
# age.z = scale(dat$age)
# yoe.z = scale(dat$yoe)
# visatt.z = scale(dat$visatt)
# cp.z = scale(dat$cp)
# cem.z = scale(dat$cem)
# awm.z = scale(dat$awm)
# gi.z = scale(dat$gi)
# cwm.z = scale(dat$cwm)
# n_visatt.z = scale(dat$n_visatt)
# n_cp.z = scale(dat$n_cp)
# fit3.3 = lm(cwm.z ~ n_cp.z + n_visatt.z, data = dat)
# summary(fit3.3)


fit4.1 = lm(cwm ~ n_visatt + n_cp, data = dat)
lm.sumSquares(fit4.1)
modelEffectSizes(fit4.1)
summary(fit4.1)
lm.beta(fit4.1)

# 1,2
fit4.2 = lm(cwm ~ age + yoe + n_visatt + n_cp, data = dat)
modelCompare(fit3.1, fit4.2)

# (iii) whether colour processing mediates the relationship between 
# visual attention and colour working memory.

fitmed1 = lm (n_cp ~ n_visatt, data = dat) 
# summary(fitmed1)
fitmed2 = lm (cwm ~ n_cp, data = dat)
# summary(fitmed2)
fitmed3 = lm (cwm ~ n_visatt, data = dat)
# summary(fitmed3)
fit4.1 = lm (cwm ~ n_visatt + n_cp, data = dat)
# summary(fit4.1)
# modelCompare(fitmed3, fit4.1)

par(mfrow = c(2, 2)) 
plot(fit4.1)
par(mfrow = c(1, 1))



# for (i in 1:ncol(dat)) {
#   # print(colnames(dat)[i])
#   qqnorm(paste("dat$",colnames(dat)[i],sep=""))
#   qqline(paste("dat$",colnames(dat)[i],sep=""), 
#          col = "steelblue", lwd = 2)
# }
# colnames(dat)
# qqnorm(paste("dat$",colnames(dat)[1],sep=""))
# varName = paste("dat$",colnames(dat)[1],sep="")
# varName = as.strin
par(mfrow = c(2, 3))
# qqnorm(dat$age, main = "Age")
# qqline(dat$age, col = "steelblue", lwd = 2)
# qqnorm(dat$yoe, main = "Years of education")
# qqline(dat$yoe, col = "steelblue", lwd = 2)
qqnorm(dat$visatt, main = "visatt")
qqline(dat$visatt, col = "steelblue", lwd = 2)
qqnorm(dat$cp, main = "cp")
qqline(dat$cp, col = "steelblue", lwd = 2)
qqnorm(dat$cem, main = "cem")
qqline(dat$cem, col = "steelblue", lwd = 2)
qqnorm(dat$awm, main = "awm")
qqline(dat$awm, col = "steelblue", lwd = 2)
qqnorm(dat$gi, main = "gi")
qqline(dat$gi, col = "steelblue", lwd = 2)
qqnorm(dat$cwm, main = "cwm")
qqline(dat$cwm, col = "steelblue", lwd = 2)
# qqnorm(dat$n_visatt, main = "n_visatt")
# qqline(dat$n_visatt, col = "steelblue", lwd = 2)
# qqnorm(dat$n_cp, main = "cp")
# qqline(dat$n_cp, col = "steelblue", lwd = 2)
par(mfrow = c(3, 3)) 
# for (i in 1:ncol(dat)) {
#   print(dat[,i])
#   # qqnorm(i)
#   # qqline(i, col = "steelblue", lwd = 2)
#   
# }
par(mfrow = c(1, 1))
install.packages("dplyr")
install.packages("ggpubr")
library("dplyr")
library("ggpubr")
ggdensity(dat$visatt, 
          main = "visatt")



# ....
install.packages("MBESS")
library(MBESS)
mediation(dat$n_visatt, dat$n_cp, dat$cwm)
upsilon(dat$n_visatt, dat$n_cp, dat$cwm)
# for boot strap
install.packages("mediation") 
library(mediation)

model.c = lm (cwm ~ n_visatt, data = dat)
model.m = lm (n_cp ~ n_visatt, data = dat) 
model.y = lm (cwm ~ n_visatt + n_cp, data = dat)
medresults = mediate(model.m, model.y, treat = "n_visatt", 
        mediator = "n_cp", boot = TRUE, sims=1000) 
summary(medresults)
# for model comparison--> sobel test
install.packages("multilevel")
library(multilevel)
