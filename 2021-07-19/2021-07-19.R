library(readr)
pop <- read_csv("multiTimeline.csv")
colnames(pop) <- c("month","ny","boston","la","chicago","st")
a <- strsplit(as.character(pop$month),"-")
b <- data.frame(matrix(unlist(a), ncol = 2,byrow = TRUE)) #1행2열 짜리 행렬로 만든뒤 데이터프레임으로 변환시킨다.
c <- cbind(pop,b)
c$X2 <- as.numeric(as.character(c$X2)) #x2(월) 변수를 스트링으로 만든뒤 숫자형으로 바꾼다.
d <- c[!((c$X2==1)|(c$X2) == 2 | (c$X2 ==3)| (c$X2) ==10 |(c$X2 == 11)| (c$X2 == 12)),] #겨울시즌이 아닐때의 데이터만 추출

with(d, boxplot(ny,boston,la,chicago,st,
                names=c("ny","boston","la","chicago","st")))
d[c(1,7,8)] <- NULL #month,x1,x2 null로 바꿈
df <- stack(d)
df
anova <- aov(values~ind,df)
summary(anova)
TukeyHSD(anova,which = "ind")
