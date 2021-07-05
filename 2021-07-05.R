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


#전년도 데이터 배열한다.
a$p_teamID <- as.character(sapply(1:nrow(a),function(x){a$teamID[x-1]}))
a$p_playerID <- as.character(sapply(1:nrow(a),function(x){a$playerID[x-1]}))
a$p_RBI <- as.numeric(sapply(1:nrow(a),function(x){a$RBI[x-1]}))
a$p_AB <- as.numeric(sapply(1:nrow(a),function(x){a$AB[x-1]}))
a$p_SF <- as.numeric(sapply(1:nrow(a),function(x){a$SF[x-1]}))
a$p_SH <- as.numeric(sapply(1:nrow(a),function(x){a$SH[x-1]}))
a$p_H <- as.numeric(sapply(1:nrow(a),function(x){a$H[x-1]}))
a$same_person <- ifelse(a$playerID == a$p_playerID,"same","diffrent") #같은사람이면 Same, 다른사람이면 different

View(a)


b <- a[a$same_person == "same",] #same 인것만 b에 저장한다.
b$moved <- ifelse(b$teamID == b$p_teamID,"no","yes")  #1년전의 팀과 지금팀이 같으면 no , 다르면 yes
c <- b[b$moved == "yes",] #팀 옮긴 사람들만 c 에 저장

c$p_avg <- with(c,p_H/p_AB) # 작년의 타수에서 안타를 나누면 나오는 값으로 avg라는 변수생성
c$sac <- with(c,p_SF+p_SH) #희생플라이 와 희생타를 더해서 sac에 저장
d <- subset(c,AB>400&p_AB>400) #400타수 넘는 선수들만
d$change_rbi <- with(d,RBI/p_RBI) #타점을 작년타점으로 나누면 증가했는지 안증가했는지 나옴

with(d,cor(p_avg,change_rbi)) #전년도 타율과 이번 시즌 타점변화의 상관계수

#그래프그리기
library(ggplot2)
ggplot(d,aes(p_avg,change_rbi,lgID))+geom_point(size=2,aes(shape=lgID))+ # x축 p_avg, y축 change_rbi, 추가하고 싶은 3의 변수 lgID(league ID)
  annotate("text",x= 0.3,y=1.6,label="r=-0.49",size=5)+ #주석
  stat_smooth(method = "lm",col="black")+ #오차를 최소로 만드는 선을 linear model로 추가
  labs(x="Batting Average of Prior Year",y = "Change in RBI")

ggplot(d,aes(p_avg,change_rbi))+geom_point(size=2)+
  stat_smooth(method = "lm",col="blue")+facet_wrap(~d$lgID)+ #리그 각각에서 독립적으로 실시
  labs(x="Batting Average of Prior year", y = "Change in RBI")

with(d,cor(sac,change_rbi))

ggplot(d,aes(sac,change_rbi))+geom_point(size=2,aes(shape=lgID))+#점은 lgID 를 구분해서 찍는다
  annotate("text",x=4,y=1.6,label = "r=0.50",size =5)+
  stat_smooth(method = "lm",col="black")+
  labs(x="Sacrifice Flies&Hits", y = "Change in RBI")
  
install.packages("tableHTML")
library(tableHTML)  
e <- with(d,data.frame(change_rbi,sac,p_avg))
colnames(e) <- c("C_RBI","Sacrifice","AVG")
cor(e)
tableHTML(round(cor(e),3))

a <- subset(Batting, yearID>2010&yearID<2016,select = c(playerID,teamID))
a$teamID <- factor(a$teamID)
a$teamID <- as.character(a$teamID)

move <- dcast(setDT(a)[,idx := 1:.N,by = playerID],
              playerID~idx, value.var = c("teamID"))
move[is.na(move)] <- ""
move[,1] <- NULL
move
write.csv(move,file = "move.csv")

library(arules)
move <- read.transactions("move.csv",sep = ",")
summary(move)

itemFrequencyPlot(move,support=0.01,cex.names=0.6)

pattern <- apriori(move,list(support=0.0015,confidence=0.50,minlen=2)) 
summary(inspect(pattern))
library(stringr)
library(Lahman)
a <- subset(Pitching,yearID>2014&yearID<2017&G>35,select=c("playerID","yearID","teamID"))
a$yearID <- str_remove(a$yearID,'20')#앞에 2016에서 20을 제거
a$teamyear <- paste(a$teamID,a$yearID,sep = "") #ARI16이런식으로 팀과 년도를 합쳐줌
b <- subset(Managers,yearID>2014 & yearID<2017, select=c("playerID","yearID","teamID"))
b$yearID <- str_remove(b$yearID,'20')
b$teamyear <- paste(b$teamID,b$yearID,sep = "")
c <- merge(a,b,by = "teamyear")
d <- subset(c,select=c("playerID.x","playerID.y"))
colnames(d) <- c("pitcher_ID","manager_ID")
d
library(igraph)

mlb_network <- graph.data.frame(d,directed = FALSE)
V(mlb_network)$label <- ifelse(V(mlb_network)$name %in% c(b$playerID)>0,as.character(b$teamyear),NA) #V는 그래프의 모든 정점을 포함하는 정점 시퀀스를 만든다.

mlb_network

manager <- V(mlb_network)$name %in% c(b$playerID)+1
plot(mlb_network,shapes="none",vertex.label.cex=1.5,vertex.label.color="black",
     vertex.label.font=2,vertex.label.dist=1,
     vertex.size=c(3,0)[manager],vertex.color=c("gray","white")[manager])

a <-subset(Teams,yearID==2015)
b <- barplot(a$HR)
text(b,par("usr")[3],labels = a$teamID,srt=60,adj = c(1,0.5),xpd =TRUE)#par("usr")은 팀이름 위치를 차트아랫부분에 둔다

a <- subset(Batting,playerID == "jeterde01")
hist(a$HR,xlab = "Homerun",main = "Histogram of Jeter's HR",las =1,
     breaks = seq(from=0,to=27,by=3))
