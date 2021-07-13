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
