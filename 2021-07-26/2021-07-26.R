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

fitnn = nnet(ID_a~ERA+AVG+X3R+RR,train, size=3,softmax = TRUE)
