library(xlsx)
library(readr)
library(haven)
library(dplyr)

travel_dt <-read.csv("./travel/travel_t.csv")

##column W 는 재방문 , x 한국을 추천한다.
table(travel_dt$X)
table(travel_dt$W)


travel_dt$C2[travel_dt$C2 == 1 ] <- 1 ##첫방문
travel_dt$C2[travel_dt$C2 > 1 ] <- 0 ## 재방문
table(travel_dt$C2)

travel_dt$X[travel_dt$X <= 3 ] <- 0##추천안함
travel_dt$X[travel_dt$X > 3 ] <- 1 ## 추천함
table(travel_dt$X)


travel_dt$W[travel_dt$W <= 3 ] <- 0 ## 재방문 안함
travel_dt$W[travel_dt$W > 3 ] <- 1  ## 재방문 함
table(travel_dt$W)

## 추천안함&재방문함, 추천안함&재방문안함, 추천함&재방문안함 <- 1 , 추천함&재방문함 <- 2
# travel_dt$XW[(travel_dt$X == 1 & travel_dt$W == 1) |(travel_dt$X == 2 & travel_dt$W == 1) 
#              | (travel_dt$X == 1 & travel_dt$W == 2)] <- 0
# 
# travel_dt$XW[(travel_dt$X == 2 & travel_dt$W == 2)] <- 1

travel_dt <- travel_dt[,-c(170:171)]##가중치, 연도
travel_dt <- travel_dt[,-c(165)]##city
travel_dt <- travel_dt[,-c(159:163)]##여행유형별 금액과 id,차수
travel_dt <- travel_dt[,-c(148:156)]##숙박형태
travel_dt <- travel_dt[,-c(121:122)]##여행기간,
travel_dt <- travel_dt[,-c(91:119)]## 권역별, 지역별삭제
travel_dt <- travel_dt[,-c(86:90)]## n44~n48
travel_dt <- travel_dt[,-c(32:38)]## 동반자
travel_dt <- travel_dt[,-c(22:24)]## 검토국가2개국만
travel_dt <- travel_dt[,-c(9:18)]## 직전1개국제외,직후방문,결정시점
travel_dt <- travel_dt[,-c(3:7)]## 방문횟수, 한국만방문
travel_dt <- travel_dt[,-c(2)]## 첫방문여부


str(travel_dt)

table(travel_dt$C2)
table(travel_dt$nat)

for (i in 1:85){
  travel_dt[,i] <- as.factor(travel_dt[,i])
}



#################################################################################################
##################################################################################################

for (i in 88:92){
  travel_dt[,i] <- as.integer(travel_dt[,i])
}

travel_dt$W <- factor(travel_dt$W, levels=c(0,1), 
                      labels=c("NO", "YES"))


# travel_dt$W <- factor(travel_dt$W, levels=c(0,1), 
#                       labels=c("NO", "YES"))

travel_dt <- rename(travel_dt, "재방문여부"="W")
travel_dt <- rename(travel_dt, "추천여부"="X")
travel_dt <- rename(travel_dt, "첫방문여부"="C2")
travel_dt <- rename(travel_dt, "전반적만족도"="T2")

travel_dt_1 <- travel_dt%>%
  filter(추천여부 == 0)
travel_dt_1 <- travel_dt_1[,-c(82)]## 추천여부
travel_dt_1 <- travel_dt_1[,-c(17:59)]## 추천여부


for (i in 17:59){
  travel_dt_1[,i] <- as.numeric(travel_dt_1[,i])
}

for (i in 17:59){
  travel_dt_1[,i][travel_dt_1[,i] > 20 ] <- NA
}
travel_dt_1$N2_1 <- as.numeric(travel_dt_1$N2_1)
travel_dt_1$N2_1[travel_dt_1$N2_1 > 20 ] <- 997 ## 재방문 안함
travel_dt_1$N2_1 <- as.factor(travel_dt_1$N2_1)
table(travel_dt_1$N2_1)

travel_dt_11 <- travel_dt_1%>%
  filter(N2_1 != '997')
table(travel_dt_11$N2_1)


travel_dt_111 <- travel_dt_11%>%
  filter(첫방문여부 == '1')

travel_dt_1111 <- travel_dt_111%>%
  filter(전반적만족도 == '1' | 전반적만족도 == '2' | 전반적만족도 == '3'  )

table(travel_dt_1111$nat)

table(travel_dt_111$재방문여부)
table(travel_dt_11$첫방문여부)

library(randomForest)

#데이터 분리를 위한 패키지 불러오기
library(caret)

#데이터 분리(7대3비율)
set.seed(1234)
train <- sample(nrow(travel_dt_11), 0.7*nrow(travel_dt_11))
df.train <- travel_dt_11[train,]
df.validate <- travel_dt_11[-train,]
table(travel_dt_11$'재방문여부')

####################################################################################################
####################################################################################################
######################################################################################################
###########################################################################################3
##############################################################################################
############# 고전적의사결정나무#############################################################
##############################################################################################
library(rpart)

table(df.train$'재방문여부')
set.seed(1234)
## grows the tree
dtree <- rpart(재방문여부 ~ ., data=df.train, method="class",      
                    parms=list(split="information"))
summary(dtree)

dtree$cptable
plotcp(dtree)

## prunes the tree
dtree.pruned <- prune(dtree, cp=.013) 

library(rpart.plot)

prp(dtree.pruned, type = 2, extra = 104,  
    fallen.leaves = TRUE, main="Decision Tree")


## classifies new cases
dtree.pred <- predict(dtree.pruned, df.validate, type="class")
dtree.perf <- table(df.validate$'재방문여부', dtree.pred, 
                    dnn=c("Actual", "Predicted"))
dtree.perf
#         Predicted
# Actual  NO   YES
# NO    897  1874
# YES   459 15604
######################################################################################################
######################################################################################################
#################### 조건적 추리나무 #################################################################
#####################################################################################################
library(party)

fit.ctree <- ctree(재방문여부~., data=df.train)
plot(fit.ctree, main="Conditional Inference Tree")
summary(fit.ctree)
ctree.pred <- predict(fit.ctree, df.validate, type="response")
ctree.perf <- table(df.validate$'재방문여부', ctree.pred, 
                    dnn=c("Actual", "Predicted"))
ctree.perf
# 
# Predicted         0.208
# Actual    NO   YES
# NO      1003  2667
# YES      577 14587

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
#########################   재방문    ############################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

travel_jb <- travel_dt
travel_jb <- tran_2016_t

travel_jb$C2[travel_jb$C2 == 1 ]
travel_jb$C2[travel_jb$C2 == 2 ] <- 0


table(travel_jb$C2)

travel_jb <- travel_jb[,-c(170:171)]##가중치, 연도
travel_jb <- travel_jb[,-c(165)]##city
travel_jb <- travel_jb[,-c(159:163)]##여행유형별 금액과 id,차수
travel_jb <- travel_jb[,-c(148:157)]##숙박형태
travel_jb <- travel_jb[,-c(121:125)]##여행기간,인상깊은지역
travel_jb <- travel_jb[,-c(91:119)]## 권역별, 지역별삭제
travel_jb <- travel_jb[,-c(43:90)]## n44~n48
travel_jb <- travel_jb[,-c(32:38)]## 동반자
travel_jb <- travel_jb[,-c(22:24)]## 검토국가2개국만
travel_jb <- travel_jb[,-c(9:18)]## 직전1개국제외,직후방문,결정시점
travel_jb <- travel_jb[,-c(2:7)]## 방문횟수, 한국만방문


str(travel_jb)

for (i in 1:45){
  travel_jb[,i] <- as.factor(travel_jb[,i])
}

travel_jb$mday_av0 <- as.numeric(travel_jb$mday_av0)
################################################################################33
#######################333 데이터 전처리  끝 #########################################
##################################################################################

set.seed(1234)
train <- sample(nrow(travel_jb), 0.7*nrow(travel_jb))
df.train <- travel_jb[train,]
df.validate <- travel_jb[-train,]
table(df.train$C2)


#########################################3########################################
############################## 고전적 의사결정나무###############################
#################################################################################3

library(rpart)

set.seed(1234)
## grows the tree
dtree <- rpart(C2 ~ ., data=df.train, method="class",      
               parms=list(split="information"))
summary(dtree)

dtree$cptable
plotcp(dtree)

## prunes the tree
dtree.pruned <- prune(dtree, cp=.011) 

library(rpart.plot)

prp(dtree.pruned, type = 2, extra = 104,  
    fallen.leaves = TRUE, main="Decision Tree")

## classifies new cases
dtree.pred <- predict(dtree.pruned, df.validate, type="class")
dtree.perf <- table(df.validate$Z1_1, dtree.pred, 
                    dnn=c("Actual", "Predicted"))
dtree.perf
#   Predicted       0.212
# Actual    NO   YES
# NO       508  3162
# YES       137 15027
##############################################################################################33
###############################################################################################
###############################################################################################
###################################################################################################
####################################################################################################
#####################################################################################################
# travel_dt_1 <-read.csv("./travel/travel_t2.csv")
# 
# for (i in 43:62){
#   travel_dt_1[,i][travel_dt_1[,i] == "1" ] <- "culture" ## culture
#   travel_dt_1[,i][travel_dt_1[,i] == '2' ] <- 'tour' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '3' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '4' ] <- 'tour' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '5' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '6' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '7' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '8' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '9' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '10' ] <- 'culture' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '11' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '12' ] <- 'tour' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '13' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '14' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '15' ] <- 'culture' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '16' ] <- 'culture' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '17' ] <- 'culture' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '18' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '19' ] <- 'place' ## 
#   travel_dt_1[,i][travel_dt_1[,i] == '20' ] <- 'place' ## 
#   
# }
# 
# for (i in 43:62){
#   travel_dt_1[,i] <- as.factor(travel_dt_1[,i])
# }
# 
# 
# str(travel_dt_1)
# write.csv(travel_dt_1, "travel_total.csv", row.names = F, quote = F)
# travel_111 
# table(travel_dt_1$N2_1)
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
# 
# travel_gg <- travel_dt
# 
# travel_ex <- travel_dt
# 
# travel_ex$N1_1[travel_ex$N1_1 > 501] <- 888
# travel_ex$N1_2[travel_ex$N1_2 > 501] <- 888
# 
# table(travel_ex$N1_1)
# table(travel_ex$N1_2)
# 
# 
# travel_ex1 <- travel_ex %>%
#   filter(N1_1 == 501)
# 
# table(travel_ex1$E)
# ### 일일평균지출금액
# travel_gg$mday_av0[travel_gg$mday_av0 <= 500 ] <- 1
# travel_gg$mday_av0[travel_gg$mday_av0 > 500 & travel_gg$mday_av0 <= 1000 ] <- 2
# travel_gg$mday_av0[travel_gg$mday_av0 > 1000 & travel_gg$mday_av0 <= 1500 ] <- 3
# travel_gg$mday_av0[travel_gg$mday_av0 > 1500 & travel_gg$mday_av0 <= 2000 ] <- 4
# travel_gg$mday_av0[travel_gg$mday_av0 > 2000 & travel_gg$mday_av0 <= 2500 ] <- 5
# travel_gg$mday_av0[travel_gg$mday_av0 > 2500 & travel_gg$mday_av0 <= 3000 ] <- 6
# travel_gg$mday_av0[travel_gg$mday_av0 > 3000 ] <- 7 


