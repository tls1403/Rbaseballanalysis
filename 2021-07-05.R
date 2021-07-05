batting <- read.csv(file = "D:/FanGraphs Leaderboard.csv",header = TRUE,encoding = "UTF-8")
head(batting)
View(Batting)
View(Master)
View(Pitching)
data("Pitching")
a <- Pitching[Pitching$playerID == "kershcl01",]
View(a)

a1 <- c("A","B","C","D","E")
b1 <- c(0.280,0.257,0.312,0.266,0.295)

c <- cbind(a1,b1)
c
colnames(c) <- c("player","avg")
c
age <- c(26,23,31,27,24)
d <- cbind(c,age)

f1 <- c("D","E","F")
g1 <- c(0.245,0.266,0.287)
h1 <- c(24,33,21)
h <- cbind(f1,g1,h1)
colnames(h) <- c("plyer","avg","age")
h

result <- rbind(d,h)
result

d <- matrix(c("C","D","E","B","A",26,23,21,27,24),ncol = 2)
d
colnames(d) <- c("player","age")
d
d
c

e <- merge(c,d,by="player")
e

str(e)

e$age <- as.numeric(e$age)
str(e)
e$player <- as.vector(e$player)
str(e)

a <- function(H,AB){H/AB} #타율, H는 안타 AB는 타석수
a(10,35)
a(17,55)
a(14,57)

View(Master)

a <- subset(Batting,playerID == "altuvjo01"| playerID == "zobribe01")

b <- subset(a,yearID>2011 & yearID<2017)

c <- subset(b,!(yearID == 2014)|yearID==2015)

d <- subset(c,select = c("playerID","HR","X3B"))


a <- subset(Batting,yearID ==2014)
b <- subset(Batting,yearID ==2015)

c <- merge(a,b,by = "playerID")
d <- c[c$AB.x>10&c$AB.y>10,]
d <- with(c,c[AB.x>10&AB.y>10,]) #10타석을 넘는 선수만
with(d,cor(HR.x,HR.y)) #홈런간의 상관관계
with(d,cor(H.x/AB.x,H.y/AB.y)) #타율 상관관계


library(dplyr)
library(plyr)
data <- subset(Batting,yearID>2014&yearID<2017)
data$teamID <- as.character(data$teamID)
data$playerID <- as.character(data$playerID)
a <- arrange(data,playerID,yearID) #playerID와 yearID를 기준으로 정렬, 동일한 타자의 2015,2016 팀이 동일한지 나누기위해



a$p_teamID <- as.character(sapply(1:nrow(a),function(x){a$teamID[x-1]}))
