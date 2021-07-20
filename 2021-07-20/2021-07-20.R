player <- c("A","B","C","D","E","F")
AVG <- c(0.315,0.257,0.266,0.198,0.227,0.305)
HR <- c(20,6,14,6,7,15)
a <- data.frame(player,AVG,HR)
a$class <- substr(a$AVG,3,3) #할 변수
a$playerclass <- paste(a$player,a$class)
a

library(Lahman)
a <- subset(Teams,yearID>1990)
a$ERA <- format(a$ERA,digits = 3)
a$ERA
b <- strsplit(as.character(a$ERA),"\\.") #정수와 소수점을 나눈다.
c <- data.frame(matrix(unlist(b),ncol = 2,byrow = TRUE)) #수지계산을 위해 vetcor을 만들어주는 unlist 함수

Team_name <- c("Atlanta Braves","Baltimore Orioles","Boston Red Sox")
b <- substr(Team_name,7,9)
b
c <- substr(Team_name,2,nchar(Team_name)-3)
c

a <- c("Atlanta","Baltimore","Boston")
b <- c("Braves","Orioles","Red Sox")
c <- paste(a,b)

c <- paste(a,b,sep = "")
c


player <- c("ZaB-A","XaB-B","SaB-C","TaB-D","UaB-E","KaB-F")
AVG <- c(0.315,0.257,0.266,0.198,0.227,0.305)
a <- data.frame(player,AVG)

#-만 제거
gsub("-","",a$player) #gsub(pattern, replacement, x) 

gsub("*\\-","",a$player)


#-와 바로 앞 기호 제거
gsub(".\\-","",a$player)

#-와 앞 2개 기호 제거
gsub("..\\-","",a$player)

#-와 앞 3개 기호 제거
gsub("...\\-","",a$player)

#-와 앞 모든 기호 제거
gsub(".*\\-","",a$player)
