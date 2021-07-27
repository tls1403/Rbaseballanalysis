#팀연봉의 표준편차와 팀 승리확률의 관계
library(Lahman)
a <- subset(Salaries,yearID>2010&yearID<2017)
b <- subset(Batting,yearID>2010&yearID<2017)
c <- merge(a,b,by="playerID")
c$teamyear <- paste(c$teamID.x,c$yearID.x,sep = "")
c <- subset(c,AB>400)
require(plyr)
func_1 <- function(c){return(data.frame(sd=sd(c$salary)))} #팀연봉의 표준편차를 구해서 데이터 프레임을 만든뒤 
team_sd <- ddply(c, .(teamyear), func_1) #ddply는 데이터 프레임을 입력으로 받아 데이터 프레임을 내보내는 함수
d <- subset(Teams,yearID>2010)
d$wp <- d$W/d$G
d$teamyear <- paste(d$teamID,d$yearID,sep = "")
e <- merge(team_sd,d,by="teamyear")
plot(e$sd,e$wp)
lines(smooth.spline(e$sd,e$wp))

summary(lm(wp~sd, data = e))

#토마존 수술을 받고 성공복귀 가능?
a <- subset(Pitching,playerID == 'johnto01') #토미존 선수의 데이터 추출
sum(a$W)

sum(a$W[a$yearID>1974])

surgery <- data.frame(matrix(c("johnto01","smoltjo01","zimmejo02","strasst01", #행렬 만듬
                               "carpech01",1974,2000,2009,2010,2007),ncol=2))
colnames(surgery) <- c('playerID','date')
surgery$date <- as.numeric(as.character(surgery$date))
a <- subset(Pitching,playerID =="johnto01"| playerID == "smoltjo01"| 
              playerID == "zimmejo02"| playerID == "strasst01"|
              playerID == "carpech01")
b <- merge(a,surgery,by = "playerID")
b$age <- b$yearID - b$date
c <- subset(b,age>-6&age<6) #수술전 5년과 수술후 5년을 관측기간에 포함시킴킴
plot(c$age,c$W)
lines(smooth.spline(c$age,c$W))
