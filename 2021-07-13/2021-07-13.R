library(Lahman)
library(tidyverse)
df <- Batting %>% subset(playerID == 'rodrial01' | playerID == 'pujolal01'|
                     playerID == 'mcgwima01') %>% 
  select(playerID,HR)

df$name <- factor(df$playerID, levels = c("rodrial01",'pujolal01','mcgwima01'),
                  labels = c('Rodriguez','Pujols','McGwire'))
library(rstatix)
library(ggpubr)
ana <- 
  df %>% 
  group_by(name) %>% 
  get_summary_stats()

box <- ggboxplot(df,"name","HR",add = "jitter",ggtheme = theme_bw()) #jitter은 오버피팅 방지
stat <- ggsummarytable(ana,"name",c("n","max","q3","median","q1","min"),
                       ggtheme = theme_bw())+clean_table_theme()

ggarrange(box,stat,ncol =1,align = 'v',heights = c(0.7,0.3))
ana

sort(df$HR[df$name == 'Pujols'],decreasing = FALSE)

fivenum(df$HR[df$name == 'Pujols'])

#이상치찾기
a <- subset(Teams,teamID == "PIT"&yearID<2016)
a$AVG <- a$H/a$AB
b <- lm(R~AVG,data = a)
plot(b)

plot(b,which = 4)


library(plyr)
a <- subset(Batting,yearID>1975&yearID<2017,select = c("yearID","AB","H","G"))
a$avg <- with(a,H/AB)
a <- na.omit(a)

func <- function(a){return(data.frame(
  sd = sd(a$avg[a$AB>400]),mean = mean(a$avg[a$AB>400])))} #sd = 표준편차를 계산한다.

env <- ddply(a,.(yearID),func) #yearID 별로 행을 나눈뒤 함수를 적용한다.
env

env$z <- (0.3-env$mean)/env$sd #3할의 표준점수로 나타냄,표준점수 식을 대입한다.

env$per <- pnorm(0.3,env$mean,env$sd, lower.tail = TRUE) #백분위로 보여준다.

par(mfrow=c(1,2))
plot(env$yearID,env$z,xlab="year",ylab = "z score of 0.3 AVG")
lines(smooth.spline(env$yearID,env$z))
plot(env$yearID,env$per,xlab="year",ylab = "Percentle of 0.3 AVG")
lines(smooth.spline(env$yearID,env$per))


#관중수 구하기

b <- subset(Teams,yearID>1975&yearID<2017,
            select = c("teamID","G","yearID","attendance"))
b$att <- (b$attendance/b$G) #게임당 관중수

#40년의 관측기간 동안 존재해왔던 21개팀만 걸러낸다.
c <- subset(b,teamID == 'ATL'|teamID == "BAL"|teamID == "BOS"|teamID == "CHA"|teamID == "CHN"|teamID == "CIN"|
              teamID == 'CLE'|teamID == 'DET'|teamID == 'HOU'|teamID == 'KCA'|teamID == 'LAN'|teamID == 'MIN'|
              teamID == 'NYA'|teamID == 'NYN'|teamID == 'OAK'|teamID == 'PHI'|teamID == 'PIT'|teamID == "SDN"|
              teamID == 'SFN'|teamID == 'SLN'|teamID == 'TEX') 
func_l <- function(c){return(data.frame(sum = sum(c$att)))}
attend <- ddply(c,.(yearID),func_l) #년도별로 관중수합계
attend$apg <- attend$sum/21 #21개팀이므로 21일로 나누면 평균관중수
d <- merge(env,attend,by = 'yearID')#표준편차 데이터와 관중수 데이터를 합친다
d

d$lag_apg <- as.numeric(sapply(1:nrow(d),function(x){d$apg[x-1]}))#저번시즌
d$ratio <- d$apg/d$lag_apg #이번시즌에서 저번시즌 관중수 나눔
d
d <- subset(d,!(yearID == 1976 | yearID == 1994 | yearID == 1995 | yearID == 1996))
summary(lm(ratio~sd+mean+yearID,data = d))

par(mfrow=c(1,1))
plot(d$sd,d$ratio,ylab = "Change in attendance",xlab = "AVG standard deviation")
abline(lm(ratio~sd,data=d))

#235p 부터터
