library(Lahman)
a <- subset(Teams,yearID>2014&yearID<2017)
a$d_win <- ifelse(a$DivWin == 'Y',1,0) #지구우승이 Y이면 1
div <- (glm(d_win~ERA, data = a,family = binomial(link = "logit"))) #로지스틱 회귀분석
summary(div)

a <- 10.241-2.966*3.2 #방어율이 3.2일때 오즈비
exp(a) #로그제거

b <- 10.241-2.966*3.1 #방어율이 3.1일때 오즈비
exp(b) #로그제거

exp(b)/exp(a)

b <- data.frame(success = a$d_win, ERA = a$ERA , fit = predict(div,a)) #success 는 우승이면 1
b$prob <- exp(b$fit)/(1+exp(b$fit)) #
c <- data.frame(ERA = seq(min(a$ERA),max(a$ERA)))
c$d_win <- predict(div,newdata = c, type = "response")
ggplot(b,aes(x=b$ERA,y= b$success))+
  geom_point()+geom_line(aes(x=b$ERA,y=b$prob))


Teams$AVG <- Teams$H/Teams$AB #타율
Teams$X3R <- Teams$X3B/Teams$AB #타석 대비 3루타
Teams$RR <- Teams$R/Teams$RA #실점 대비 득점

b <- subset(Teams,yearID>1900&yearID<2016)
b <- data.frame(b$yearID,b$DivWin,b$ERA,b$AVG,b$X3R,b$RR)
colnames(b) <- c("yearID","Divwin","ERA","AVG","X3R","RR")
b <- na.omit(b) #결측치 제거

#traindata 와 testdata를 만든다

train <- subset(b,!(yearID<2016&yearID>1995))

test <- subset(b,yearID<2016&yearID>1995)

library(nnet)
train$ID_a = class.ind(train$Divwin) #문자로 되어있는 Divwin을 0과 1로만 이뤄진 가변수로 만듬
test$ID_b = class.ind(test$Divwin)
View(train)

fitnn = nnet(ID_a~ERA+AVG+X3R+RR,train, size=3,softmax = TRUE)

fitnn

summary(fitnn)


c <- table(data.frame(predicted= predict(fitnn,test)[,2]>0.5,
                      actual=test$ID_b[,2]>0.5))
c

(c[1,1]+c[2,2])/(c[1,1]+c[2,1]+c[1,2]+c[2,2])

library(neuralnet)
b$win=b$Divwin=='Y'
b$not_win=b$Divwin=='N'
View(b)
d <- neuralnet(win+not_win~ERA+AVG+X3R+RR,b,hidden = 3,stepmax = 1e6)
d$result.matrix
plot(d)


#모수선형분석
library(MASS)
ldafit <- lda(Divwin~ERA+AVG+X3R+RR,data = test)
ldapred <- predict(ldafit)
d <- table(ldapred$class,test$Divwin)
(d[1,1]+d[2,2])/(d[1,1]+d[1,2]+d[2,1]+d[2,2])


#모수비선형분석
qdafit <- qda(Divwin~ERA+AVG+X3R+RR,data = test)
qdapred <- predict(qdafit)
e <- table(qdapred$class,test$Divwin)
(e[1,1]+e[2,2])/(e[1,1]+e[1,2]+e[2,1]+e[2,2])




team <- subset(Teams,yearID == 2015, select = c(teamID,X2B,X3B,ERA,RA))
View(team)
rownames(team) <- team[,1] #숫자로되어있던 인덱스 값들을 1열의 팀이름으로 바꿔줌
team[,1] <- NULL # 그리고 팀1열의 값들은 NULL 처리
team_st <- scale(team)#표준화

install.packages("factoextra")
library(factoextra)

fviz_nbclust(team_st,kmeans,method = "gap_stat") #군집할 개수를 구함


residual <- kmeans(team_st,3,nstart = 25)
residual

fviz_cluster(residual,data = team_st)


library(FactoMineR)
pca.res <- PCA(team_st, graph = FALSE)
par(mfrow=c(1,1))
fviz_contrib(pca.res,choice = "var",axes = 1,top = 4)
fviz_contrib(pca.res,choice = "var",axes = 2,top = 4)

x <- -5:5
y <- x^2+x
plot(x,y,type = "b")



