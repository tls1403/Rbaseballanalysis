library(Lahman)
a <- subset(Batting,yearID>2010&yearID<2017&G>150)
b <- subset(Master,sel=c("playerID","weight"))

c <- merge(a,b,by = "playerID")
c$slg <- with(c,((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)
with(c,plot(weight,slg,type="n"))
abline(lm(slg~weight,c))
fit <- lm(slg~weight,c)
fit_res <- resid(fit)
plot(c$weight,fit_res)
abline(0,0)
qqnorm(fit_res)
qqline(fit_res)

a <- rbinom(10000,5,0.46)
table(a)/10000

OBP <- 0.2
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base)
EV <- P*case
EV
barplot(EV)

a <- subset(Batting,yearID>2011&yearID<2016&AB>300)
hist(a$H,main = "949 players",breaks = seq(from =0, to = 300, by = 30))

View(Teams)

library(pwr)
pwr.f2.test(1,NULL,0.01,0.05,0.95)

a <- subset(Teams,lgID = "AL"|lgID == "NL")
b <- sample(1:nrow(a),1302)
c <- a[b,]
c
c$avg <- with(c,H/AB)
c$r_g <- with(c,R/G)
d <- lm(r_g~avg,data=c)
summary(d)
