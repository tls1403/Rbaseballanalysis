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

