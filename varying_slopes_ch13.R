# 2018-05-19

library(dplyr)
library(MASS)
library(lme4)
library(rethinking)


a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- (-0.7)       # correlation between intercepts and slopes

Mu <- c( a , b )

cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

N_cafes <- 20

set.seed(5)
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)
# this results in 20 cafes, each with a slope and intercept.

a_cafe <- vary_effects[,1] # intercepts
b_cafe <- vary_effects[,2] # slopes

# To biuld a set of oberservations
N_visits <- 10 # 10 visits to each cafe
afternoon <- rep(0:1,N_visits*N_cafes/2) # creates the morning and afternoon category
cafe_id <- rep( 1:N_cafes , each=N_visits ) # gives each cafe an ID
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon # calculates the Y for each
sigma <- 0.5  # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma ) # creates random wait times based on the simulated data
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

head(d)


# book method -------------------------------------------------------------

m13.1 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ dmvnorm2(c(a,b),sigma_cafe,Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )

post <- extract.samples(m13.1)
dens( post$Rho[,1,2] )

mean(post$a_cafe)
mean(post$b_cafe)
mean(post$a)
mean(post$b)
mean(post$sigma_cafe)
mean(post$sigma)
mean(post$Rho)

# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1



# lmer method -------------------------------------------------------------

mod_1 <- lmer(wait ~ afternoon + (afternoon|cafe), data = d, REML = F)
summary(mod_1)

preds <- predict(mod_1)
resids <- residuals(mod_1)
mod_out <- cbind(preds, resids)
mean(mod_out[, 1])
mean(mod_out[, 2])
sd(mod_out[, 1])
sd(mod_out[, 2])

cc <- cbind(d, mod_out)
head(cc)
summary(cc)
cc %>% group_by(cafe) %>% summarise(sig_cafe = mean(resids)) %>% summarise(cafe_sig = mean(sig_cafe))
