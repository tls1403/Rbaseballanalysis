library(Lahman)
a <- subset(Batting,yearID>2010&yearID<2017&G>150)
b <- subset(Master,sel = c("playerID","weight"))
b
c <- merge(a,b,by = "playerID")
c$slg <- with(c,((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)
with(c,plot(weight,slg,type="n"))
abline(lm(slg~weight,c)) #각 점들과 선의 오차가 최소로 되도록 선을 긋는 것을 Linear Regression model이라고 한다.
fit <- lm(slg~weight,c)
fit_res <- resid(fit)#잔차 구하기
plot(c$weight,fit_res)
abline(0,0)
qqnorm(fit_res)선
qqline(fit_res)

a <- rbinom(10000,5,0.46) #전체 10000번 5번의 베르누이분포(타석), 출루율 0.46
table(a)/10000

curve(log(10*x^2*(1-x)^3),0,1,ylab="Log probability",xlab="OBP")

OBP <- 0.4
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base) #확률의 Combination 과 같다.
EV <- P*case
EV
barplot(EV)

par(mfrow=c(2,2))

#출루율이 2할
OBP <- 0.2
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base)
EV <- P*case
barplot(EV,main="OBP=0.2 (20.48%)",ylab="positibillity")

#출루율이 3할
OBP <- 0.3
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base)
EV <- P*case
barplot(EV,main="OBP=0.3 (30.87%)",ylab="positibillity")

#출루율이 4할
OBP <- 0.4
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base)
EV <- P*case
barplot(EV,main="OBP=0.4 (34.56%)",ylab="positibillity")

#출루율이 5할
OBP <- 0.5
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case <- choose(5,base)
EV <- P*case
barplot(EV,main="OBP=0.5 (31.25%)",ylab="positibillity")


library(sand)
library(igraph)
g <-graph.formula(1-5,1-7,2-9,2-4,3-5,4-5,4-6,4-7)
V(g)
E(g)
plot(g)


a <- subset(Batting,yearID>2011&yearID<2016&AB>=300)
hist(a$H,main = "949 Players",breaks = seq(from =0,to =300,by=30))

a <- subset(Batting,yearID>2011&yearID<2016&AB>=300&teamID=="NYA")
hist(a$H,main="32 players",breaks = seq(from =0 , to = 300, by = 30))
library(arules)
par(mfrow=c(1,2))
x <- c(1,2,4,8,16)
y <- c(1,2,3,4,5)

plot(x,y,type = "b",lwd =3,main = "Before Transformation")
x_adj <- log(x)
plot(x_adj,y,type="b",lwd=3,main = "After Transformaiton")

library(Lahman)
rec <- subset(Teams,yearID ==2014)
rec$wp <- rec$W/rec$G #이긴게임/전체게임 = 승률
a <- lm(wp~R,rec)
library(lmtest)
bptest(a)

#표준오차
rec <- subset(Teams,yearID == 2014)
rec$wp <- rec$W/rec$G
rec$avg <- rec$H/rec$AB
avg_model <- lm(wp~avg,rec)
ERA_model <- lm(wp~ERA,rec)
summary(avg_model)
summary(ERA_model)

a <- subset(Teams,yearID == 2015)
a$avg <- a$H/a$AB
a$obp <- with(a,(H+BB+HBP)/(AB+BB+HBP+SF))#출루율
a$r_g <- a$R/162 #R = 홈을 밟아 점수를 획득하는 득점 /162= 게임당 득점율
lm(r_g~avg,a)

#pwr.f2.test(파라미터개수,자유도,효과의크기,유의수준,검증력)
pwr.f2.test(1,NULL,0.01,0.05,0.95) 


