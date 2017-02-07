# 2017-02-07
# Exploring multilevel modeling

library(lme4)
library(nlme)
library(dplyr)
library(ggplot2)

df <- read.csv("popular2.csv")
str(df)
df$UID <- 1:nrow(df)

# so if we ignore all class and teacher factors we would just start with popularity. There are students and they self-rated their popularity.
summary(df$popular)
# so on average students are 5.076 popular with the least being 0.00 and the most being 9.5. We can plot this too
qplot(x = popular, data = df, geom = "histogram")
# students are popular in a normal way that is normally distributed, no surprise there. What could explain their popularity???
# 2 things, is there a difference between boys and girls? Does extroversion explain popularity? Are they correlated? Lots of questions...
# is there a difference, could t-test that
b <- df %>% filter(sex == 0)
g <- df %>% filter(sex == 1)
t.gender <- t.test(b$popular, g$popular)
t.gender
qplot(factor(sex), popular, data = df, geom = "boxplot")
# so girls feel more popular than boys, statisically significantly so. This is a broad brush view across all students in the system.
# let's look at som plain regression on this. Does extroversion help explain popularity?
# First, a scatter plot
plot(df$popular, df$extrav)
mod1 <- lm(popular ~ extrav, data = df)
summary(mod1)
plot(mod1)
# we could do some diagnostics to see if this is a good model...
# and with just 2 variables, look at the correlation...
cor.test(df$popular, df$extrav)
ggplot(df, aes(popular, extrav, color = sex)) + geom_point() + geom_jitter()
# plotting with teacher experience as a color we can kind of see some differentiation
# so we could say that "popularity depends on gender and teacher experience". Hard to change gender there...