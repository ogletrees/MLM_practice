# 2017-01-30
# MLM Practice

d <- read.csv("alda/berkeley_pp.csv")
library(ggplot2)
library(nlme)
qplot(age, iq, data = d, geom = "point")
dd <- read.csv("alda/wages_small_pp.csv")
qplot(lnw, exper, data = dd, geom = "point")
mod1 <- lm(lnw ~ exper, data = dd)
summary(mod1)
mod2 <- lme(lnw ~ 1, data = dd, random = ~1 | black)
summary(mod2)
mod3 <- lme(lnw ~ exper, data = dd, random = ~1 | black, method = "ML")
summary(mod3)

fixef.b <- fixef(mod3)
fit.b <- fixef.b[[1]] + dd$exper[1:13]*fixef.b[[2]]
plot(dd$exper[1:13], fit.b, ylim=c(0, 2), type="b", 
     ylab="predicted lnw", xlab="exper")   
title("Model B \n Unconditional growth model")