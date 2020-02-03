#Set Directory
setwd("C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856")
getwd()

#Importing bond price data
bon<-read.csv("Bd.csv", sep=",", header=T)

#Droping Bond17 and Bond20 from data:
bon<-bon[(bon$Bond!="Bond17" & bon$Bond!="Bond20"),]

#Calculating dirty price
bon$dp=((bon$ncd/360)*bon$Coupon) + bon$Price

#Value received at maturity
bon$not=100+(bon$Coupon/2)

#Coupon payment by semi-annual
bon$cou=bon$Coupon/2

#Drop unnecessary columns:
bon<-subset(bon,select=-c(coupon_start,coupon_end,mayr,mamo))

#Subset the bon dataset into each bond parts:
bond2<-subset(bon, bon$Bond=="Bond2")
bond4<-subset(bon, bon$Bond=="Bond4")
bond6<-subset(bon, bon$Bond=="Bond4")
bond12<-subset(bon, bon$Bond=="Bond12")
bond15<-subset(bon, bon$Bond=="Bond15")
bond16<-subset(bon, bon$Bond=="Bond16")
bond18<-subset(bon, bon$Bond=="Bond18")
bond19<-subset(bon, bon$Bond=="Bond19")
bond21<-subset(bon, bon$Bond=="Bond21")
bond23<-subset(bon, bon$Bond=="Bond23")
bond24<-subset(bon, bon$Bond=="Bond24")

#Calculate r(2/12) using bond2 data:
for(i in 1:10){
  bond2$r2_12[i]<-((-1)*log((bond2$dp[i])/bond2$not[i]))/(2/12)
}

#Calculate r(8/12) using bond4 data:
bond4$r2_12<-bond2$r2_12
for (i in 1:10){
  bond4$r8_12[i]<-(-12/8)*log((1/bond4$not[i])*(bond4$dp[i]-(bond4$cou[i]*exp((-2/12)*bond4$r2_12[i]))))
}

#Calculate r(14/12) using bond6 data:
bond6$r2_12<-bond2$r2_12
bond6$r8_12<-bond4$r8_12
for(i in 1:10){
  bond6$r14_12[i]<-(-12/14)*log((1/bond6$not[i])*(bond6$dp[i]-(bond6$cou[i]*exp((-2/12)*bond6$r2_12[i]))-
                             ((bond6$cou[i])*exp((-8/12)*bond6$r8_12[i]))))
}

#Calculate r(20/12) using bond12 data:
bond12$r2_12<-bond2$r2_12
bond12$r8_12<-bond4$r8_12
bond12$r14_12<-bond6$r14_12
for(i in 1:10){
  bond12$r20_12[i]<-(-12/20)*log((1/bond12$not[i])*(bond12$dp[i]-(bond12$cou[i]*exp((-2/12)*bond12$r2_12[i]))-
                              (bond12$cou[i]*exp((-8/12)*bond12$r8_12[i]))-(bond12$cou[i]*exp((-14/12)*
                              bond12$r14_12[i]))))
}

#Calculate r(26/12) using bond15 data:
bond15$r2_12<-bond2$r2_12
bond15$r8_12<-bond4$r8_12
bond15$r14_12<-bond6$r14_12
bond15$r20_12<-bond12$r20_12
for(i in 1:10){
  bond15$r26_12[i]<-(-12/26)*log((1/bond15$not[i])*(bond15$dp[i]-(bond15$cou[i]*exp((-2/12)*bond15$r2_12[i]))-
                              (bond15$cou[i]*exp((-8/12)*bond15$r8_12[i]))-(bond15$cou[i]*exp((-14/12)*bond15$r14_12[i]))-
                              (bond15$cou[i]*exp((-20/12)*bond15$r20_12[i]))))
}

#Calculate r(5/12) r(11/12) r(17/12) r(23/12) r(29/12) using bond16 data:
bond16$r2_12<-bond2$r2_12
bond16$r8_12<-bond4$r8_12
bond16$r14_12<-bond6$r14_12
bond16$r20_12<-bond12$r20_12
bond16$r26_12<-bond15$r26_12
bond16$r5_12<-0.5*(bond16$r2_12 + bond16$r8_12)
bond16$r11_12<-0.5*(bond16$r8_12 + bond16$r14_12)
bond16$r17_12<-0.5*(bond16$r14_12 + bond16$r20_12)
bond16$r23_12<-0.5*(bond16$r20_12 + bond16$r26_12)
for(i in 1:10){
  bond16$r29_12[i]<-(-12/29)*log((1/bond16$not[i])*(bond16$dp[i]-(bond16$cou[i]*exp((-5/12)*bond16$r5_12[i]))-
                              (bond16$cou[i]*exp((-11/12)*bond16$r11_12[i]))-(bond16$cou[i]*exp((-17/12)*bond16$r17_12[i]))-
                              (bond16$cou[i]*exp((-23/12)*bond16$r23_12[i]))))
}

#Calculate r(32/12) and r(38/12) using bond18 data:
bond18$r2_12<-bond2$r2_12
bond18$r8_12<-bond4$r8_12
bond18$r14_12<-bond6$r14_12
bond18$r20_12<-bond12$r20_12
bond18$r26_12<-bond15$r26_12
bond18$r5_12<-bond16$r5_12
bond18$r11_12<-bond16$r11_12
bond18$r17_12<-bond16$r17_12
bond18$r23_12<-bond16$r23_12
bond18$r29_12<-bond16$r29_12

tmp1<-bond18[,c("r2_12","r8_12","r14_12","r20_12","r26_12","r5_12","r11_12","r17_12","r23_12","r29_12")]
tmp2<-c("0.167","0.667","1.167","1.667","2.167","0.4167","0.9167","1.4167","1.9167","2.4167")
tmp2<-as.numeric(tmp2)
library(data.table)
dat1=transpose(tmp1[1,])
dat1$tim<-tmp2
dat2=transpose(tmp1[2,])
dat2$tim<-tmp2
dat3=transpose(tmp1[3,])
dat3$tim<-tmp2
dat4=transpose(tmp1[4,])
dat5=transpose(tmp1[5,])
dat6=transpose(tmp1[6,])
dat7=transpose(tmp1[7,])
dat8=transpose(tmp1[8,])
dat9=transpose(tmp1[9,])
dat10=transpose(tmp1[10,])
dat4$tim<-tmp2
dat5$tim<-tmp2
dat6$tim<-tmp2
dat7$tim<-tmp2
dat8$tim<-tmp2
dat9$tim<-tmp2
dat10$tim<-tmp2

r1<-lm(V1~tim, data=dat1)
r2<-lm(V1~tim, data=dat2)
r3<-lm(V1~tim, data=dat3)
r4<-lm(V1~tim, data=dat4)
r5<-lm(V1~tim, data=dat5)
r6<-lm(V1~tim, data=dat6)
r7<-lm(V1~tim, data=dat7)
r8<-lm(V1~tim, data=dat8)
r9<-lm(V1~tim, data=dat9)
r10<-lm(V1~tim, data=dat10)
rnew<-c(predict(r1, newdata = data.frame(tim=2.6667)),predict(r2, newdata = data.frame(tim=2.6667)),
        predict(r3, newdata = data.frame(tim=2.6667)),predict(r4, newdata = data.frame(tim=2.6667)),
        predict(r5, newdata = data.frame(tim=2.6667)),predict(r6, newdata = data.frame(tim=2.6667)),
        predict(r7, newdata = data.frame(tim=2.6667)),predict(r8, newdata = data.frame(tim=2.6667)),
        predict(r9, newdata = data.frame(tim=2.6667)),predict(r10, newdata = data.frame(tim=2.6667)))

bond18$r32_12<-rnew

rm(list=c("tmp1","tmp2","dat1","dat2","dat3","dat4","dat5","dat6","dat7","dat8","dat9","dat10","r1","r2",
          "r3","r4","r5","r6","r7","r8","r9","r10","rnew"))

for(i in 1:10){
  bond18$r38_12[i]<-(-12/32)*log((1/bond18$not[i])*(bond18$dp[i]-(bond18$cou[i]*exp((-2/12)*bond18$r2_12[i]))-
                              (bond18$cou[i]*exp((-8/12)*bond18$r8_12[i]))-(bond18$cou[i]*exp((-14/12)*bond18$r14_12[i]))-
                              (bond18$cou[i]*exp((-20/12)*bond18$r20_12[i]))-(bond18$cou[i]*exp((-26/12)*bond18$r26_12[i]))-
                                (bond18$cou[i]*exp((-32/12)*bond18$r32_12[i]))))
}

#Calculate r(35/12), r(41/12) using Bond19:
bond19$r2_12<-bond2$r2_12
bond19$r8_12<-bond4$r8_12
bond19$r14_12<-bond6$r14_12
bond19$r20_12<-bond12$r20_12
bond19$r26_12<-bond15$r26_12
bond19$r5_12<-bond16$r5_12
bond19$r11_12<-bond16$r11_12
bond19$r17_12<-bond16$r17_12
bond19$r23_12<-bond16$r23_12
bond19$r29_12<-bond16$r29_12
bond19$r32_12<-bond18$r32_12
bond19$r38_12<-bond18$r38_12
bond19$r35_12<-0.5*(bond19$r32_12 + bond19$r38_12)

for (i in 1:10){
  bond19$r41_12[i]<-(-12/41)*log((1/bond19$not[i])*(bond19$dp[i]-(bond19$cou[i]*exp((-5/12)*bond19$r5_12[i]))-
                            (bond19$cou[i]*exp((-11/12)*bond19$r11_12[i]))-(bond19$cou[i]*exp((-17/12)*bond19$r17_12[i]))-
                            (bond19$cou[i]*exp((-23/12)*bond19$r23_12[i]))-(bond19$cou[i]*exp((-29/12)*bond19$r29_12[i]))-
                            (bond19$cou[i]*exp((-35/12)*bond19$r35_12[i]))))
}

#Calculate r(44/12) and r(50/12) using bond21:
bond21$r2_12<-bond2$r2_12
bond21$r8_12<-bond4$r8_12
bond21$r14_12<-bond6$r14_12
bond21$r20_12<-bond12$r20_12
bond21$r26_12<-bond15$r26_12
bond21$r5_12<-bond16$r5_12
bond21$r11_12<-bond16$r11_12
bond21$r17_12<-bond16$r17_12
bond21$r23_12<-bond16$r23_12
bond21$r29_12<-bond16$r29_12
bond21$r32_12<-bond18$r32_12
bond21$r38_12<-bond18$r38_12
bond21$r35_12<-bond19$r35_12
bond21$r41_12<-bond19$r41_12

tmp1<-bond21[,c("r2_12","r8_12","r14_12","r20_12","r26_12","r5_12","r11_12","r17_12","r23_12","r29_12","r32_12",
                "r38_12","r35_12","r41_12")]
tmp2<-c("0.167","0.667","1.167","1.667","2.167","0.4167","0.9167","1.4167","1.9167","2.4167","2.6667","3.1667",
        "2.9167","3.4167")
tmp2<-as.numeric(tmp2)
library(data.table)
dat1=transpose(tmp1[1,])
dat1$tim<-tmp2
dat2=transpose(tmp1[2,])
dat2$tim<-tmp2
dat3=transpose(tmp1[3,])
dat3$tim<-tmp2
dat4=transpose(tmp1[4,])
dat5=transpose(tmp1[5,])
dat6=transpose(tmp1[6,])
dat7=transpose(tmp1[7,])
dat8=transpose(tmp1[8,])
dat9=transpose(tmp1[9,])
dat10=transpose(tmp1[10,])
dat4$tim<-tmp2
dat5$tim<-tmp2
dat6$tim<-tmp2
dat7$tim<-tmp2
dat8$tim<-tmp2
dat9$tim<-tmp2
dat10$tim<-tmp2

r1<-lm(V1~tim, data=dat1)
r2<-lm(V1~tim, data=dat2)
r3<-lm(V1~tim, data=dat3)
r4<-lm(V1~tim, data=dat4)
r5<-lm(V1~tim, data=dat5)
r6<-lm(V1~tim, data=dat6)
r7<-lm(V1~tim, data=dat7)
r8<-lm(V1~tim, data=dat8)
r9<-lm(V1~tim, data=dat9)
r10<-lm(V1~tim, data=dat10)
rnew<-c(predict(r1, newdata = data.frame(tim=3.6667)),predict(r2, newdata = data.frame(tim=3.6667)),
        predict(r3, newdata = data.frame(tim=3.6667)),predict(r4, newdata = data.frame(tim=3.6667)),
        predict(r5, newdata = data.frame(tim=3.6667)),predict(r6, newdata = data.frame(tim=3.6667)),
        predict(r7, newdata = data.frame(tim=3.6667)),predict(r8, newdata = data.frame(tim=3.6667)),
        predict(r9, newdata = data.frame(tim=3.6667)),predict(r10, newdata = data.frame(tim=3.6667)))

bond21$r44_12<-rnew

rm(list=c("tmp1","tmp2","dat1","dat2","dat3","dat4","dat5","dat6","dat7","dat8","dat9","dat10","r1","r2",
          "r3","r4","r5","r6","r7","r8","r9","r10","rnew"))

for(i in 1:10){
  bond21$r50_12[i]<-(-12/50)*log((1/bond21$not[i])*(bond21$dp[i]-(bond21$cou[i]*exp((-2/12)*bond21$r2_12[i]))-
                             (bond21$cou[i]*exp((-8/12)*bond21$r8_12[i]))-(bond21$cou[i]*exp((-14/12)*bond21$r14_12[i]))-
                              (bond21$cou[i]*exp((-20/12)*bond21$r20_12[i]))-(bond21$cou[i]*exp((-26/12)*bond21$r26_12[i]))-
                              (bond21$cou[i]*exp((-32/12)*bond21$r32_12[i]))-(bond21$cou[i]*exp((-38/12)*bond21$r38_12[i]))-
                               (bond21$cou[i]*exp((-44/12)*bond21$r44_12[i]))))
}

#Calculate r(56/12) using bond23:
bond23$r2_12<-bond2$r2_12
bond23$r8_12<-bond4$r8_12
bond23$r14_12<-bond6$r14_12
bond23$r20_12<-bond12$r20_12
bond23$r26_12<-bond15$r26_12
bond23$r5_12<-bond16$r5_12
bond23$r11_12<-bond16$r11_12
bond23$r17_12<-bond16$r17_12
bond23$r23_12<-bond16$r23_12
bond23$r29_12<-bond16$r29_12
bond23$r32_12<-bond18$r32_12
bond23$r38_12<-bond18$r38_12
bond23$r35_12<-bond19$r35_12
bond23$r41_12<-bond19$r41_12
bond23$r44_12<-bond21$r44_12
bond23$r50_12<-bond21$r50_12

for(i in 1:10){
  bond23$r56_12[i]<-(-12/56)*log((1/bond23$not[i])*(bond23$dp[i]-(bond23$cou[i]*exp((-2/12)*bond23$r2_12[i]))-
                            (bond23$cou[i]*exp((-8/12)*bond23$r8_12[i]))-(bond23$cou[i]*exp((-14/12)*bond23$r14_12[i]))-
                            (bond23$cou[i]*exp((-20/12)*bond23$r20_12[i]))-(bond23$cou[i]*exp((-26/12)*bond23$r26_12[i]))-
                            (bond23$cou[i]*exp((-32/12)*bond23$r32_12[i]))-(bond23$cou[i]*exp((-38/12)*bond23$r38_12[i]))-
                            (bond23$cou[i]*exp((-44/12)*bond23$r44_12[i]))-(bond23$cou[i]*exp((-50/12)*bond23$r50_12[i]))))
}

#Calculate r(62/12) using bond24:
bond24$r2_12<-bond2$r2_12
bond24$r8_12<-bond4$r8_12
bond24$r14_12<-bond6$r14_12
bond24$r20_12<-bond12$r20_12
bond24$r26_12<-bond15$r26_12
bond24$r5_12<-bond16$r5_12
bond24$r11_12<-bond16$r11_12
bond24$r17_12<-bond16$r17_12
bond24$r23_12<-bond16$r23_12
bond24$r29_12<-bond16$r29_12
bond24$r32_12<-bond18$r32_12
bond24$r38_12<-bond18$r38_12
bond24$r35_12<-bond19$r35_12
bond24$r41_12<-bond19$r41_12
bond24$r44_12<-bond21$r44_12
bond24$r50_12<-bond21$r50_12
bond24$r56_12<-bond23$r56_12

for(i in 1:10){
  bond24$r62_12[i]<-(-12/62)*log((1/bond24$not[i])*(bond24$dp[i]-(bond24$cou[i]*exp((-2/12)*bond24$r2_12[i]))-
                             (bond24$cou[i]*exp((-8/12)*bond24$r8_12[i]))-(bond24$cou[i]*exp((-14/12)*bond24$r14_12[i]))-
                             (bond24$cou[i]*exp((-20/12)*bond24$r20_12[i]))-(bond24$cou[i]*exp((-26/12)*bond24$r26_12[i]))-
                             (bond24$cou[i]*exp((-32/12)*bond24$r32_12[i]))-(bond24$cou[i]*exp((-38/12)*bond24$r38_12[i]))-
                             (bond24$cou[i]*exp((-44/12)*bond24$r44_12[i]))-(bond24$cou[i]*exp((-50/12)*bond24$r50_12[i]))-
                              (bond24$cou[i]*exp((-56/12)*bond24$r56_12[i]))))
}


#weighted Average intrapolation of the spot rates using bond24 data:
spot<-data.frame("jan"= bond24$January)
spot$r6_12<-NA
spot$r12_12<-NA
spot$r18_12<-NA
spot$r24_12<-NA
spot$r30_12<-NA
spot$r36_12<-NA
spot$r42_12<-NA
spot$r48_12<-NA
spot$r54_12<-NA
spot$r60_12<-NA

spot$r6_12<-((1/6)*bond24$r2_12)+((5/6)*bond24$r8_12)
spot$r12_12<-((1/6)*bond24$r8_12)+((5/6)*bond24$r14_12)
spot$r18_12<-((1/6)*bond24$r14_12)+((5/6)*bond24$r20_12)
spot$r24_12<-((1/6)*bond24$r20_12)+((5/6)*bond24$r26_12)
spot$r30_12<-((1/6)*bond24$r26_12)+((5/6)*bond24$r32_12)
spot$r36_12<-((1/6)*bond24$r32_12)+((5/6)*bond24$r38_12)
spot$r42_12<-((1/6)*bond24$r38_12)+((5/6)*bond24$r44_12)
spot$r48_12<-((1/6)*bond24$r44_12)+((5/6)*bond24$r50_12)
spot$r54_12<-((1/6)*bond24$r50_12)+((5/6)*bond24$r56_12)
spot$r60_12<-((1/6)*bond24$r56_12)+((5/6)*bond24$r62_12)

#Constructing the spot curve using spot data:
spotg<-transpose(spot)
tmp2<-c("06-2020","01-2021","06-2021","01-2022","06-2022","01-2023",
        "06-2023","01-2021","06-2024","01-2025")
spotg<-spotg[-1,]
spotg$time<-tmp2

#Write csv to make graphs in excel
write.csv(spotg,'C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856/spotg.csv',row.names=FALSE)

########################################YTM#########################
#Converting issue and maturity to dates
bond2$Issue.Date<-as.Date(bond2$Issue.Date)
bond4$Issue.Date<-as.Date(bond4$Issue.Date)
bond6$Issue.Date<-as.Date(bond6$Issue.Date)
bond12$Issue.Date<-as.Date(bond12$Issue.Date)
bond15$Issue.Date<-as.Date(bond15$Issue.Date)
bond16$Issue.Date<-as.Date(bond16$Issue.Date)
bond18$Issue.Date<-as.Date(bond18$Issue.Date)
bond19$Issue.Date<-as.Date(bond19$Issue.Date)
bond21$Issue.Date<-as.Date(bond21$Issue.Date)
bond23$Issue.Date<-as.Date(bond23$Issue.Date)
bond24$Issue.Date<-as.Date(bond24$Issue.Date)

bond2$Maturity.Date<-as.Date(bond2$Maturity.Date)
bond4$Maturity.Date<-as.Date(bond4$Maturity.Date)
bond6$Maturity.Date<-as.Date(bond6$Maturity.Date)
bond12$Maturity.Date<-as.Date(bond12$Maturity.Date)
bond15$Maturity.Date<-as.Date(bond15$Maturity.Date)
bond16$Maturity.Date<-as.Date(bond16$Maturity.Date)
bond18$Maturity.Date<-as.Date(bond18$Maturity.Date)
bond19$Maturity.Date<-as.Date(bond19$Maturity.Date)
bond21$Maturity.Date<-as.Date(bond21$Maturity.Date)
bond23$Maturity.Date<-as.Date(bond23$Maturity.Date)
bond24$Maturity.Date<-as.Date(bond24$Maturity.Date)

#Calculating coupon rate:
bond2$cr=bond2$cou/100
bond4$cr=bond2$cou/100
bond6$cr=bond6$cou/100
bond12$cr=bond12$cou/100
bond15$cr=bond15$cou/100
bond16$cr=bond16$cou/100
bond18$cr=bond18$cou/100
bond19$cr=bond19$cou/100
bond21$cr=bond21$cou/100
bond23$cr=bond23$cou/100
bond24$cr=bond24$cou/100

#Calculating YTM in % Jan2-Jan15:
library(jrvFinance)

yb2<-bond.yields(bond2$Issue.Date,bond2$Maturity.Date,
                   bond2$cr,freq=2,bond2$dp)
yb4<-bond.yields(bond4$Issue.Date,bond4$Maturity.Date,
                       bond4$cr,freq=2,bond4$dp)
yb6<-bond.yields(bond6$Issue.Date,bond6$Maturity.Date,
                 bond6$cr,freq=2,bond6$dp)
yb12<-bond.yields(bond12$Issue.Date,bond12$Maturity.Date,
                  bond12$cr,freq=2,bond12$dp)
yb15<-bond.yields(bond15$Issue.Date,bond15$Maturity.Date,
                  bond15$cr,freq=2,bond15$dp)
yb16<-bond.yields(bond16$Issue.Date,bond16$Maturity.Date,
                  bond16$cr,freq=2,bond16$dp)
yb18<-bond.yields(bond18$Issue.Date,bond18$Maturity.Date,
                  bond18$cr,freq=2,bond18$dp)
yb19<-bond.yields(bond19$Issue.Date,bond19$Maturity.Date,
                  bond19$cr,freq=2,bond19$dp)
yb21<-bond.yields(bond21$Issue.Date,bond21$Maturity.Date,
                  bond21$cr,freq=2,bond21$dp)
yb23<-bond.yields(bond23$Issue.Date,bond23$Maturity.Date,
                  bond23$cr,freq=2,bond23$dp)
yb24<-bond.yields(bond24$Issue.Date,bond24$Maturity.Date,
                  bond24$cr,freq=2,bond24$dp)

ytm<-data.frame("jan"= bond24$January)
ytm$yb2<-yb2
ytm$yb4<-yb4
ytm$yb6<-yb6
ytm$yb12<-yb12
ytm$yb15<-yb15
ytm$yb16<-yb16
ytm$yb18<-yb18
ytm$yb19<-yb19
ytm$yb21<-yb21
ytm$yb23<-yb23
ytm$yb24<-yb24

#Write data to csv for graph:
ytmg<-transpose(ytm)
write.csv(ytmg,'C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856/ytmg.csv',row.names=FALSE)

#########################################################

#Calculating Forward Rates using Spot data:
f1_2=(((1+spot$r24_12)^2)/(1+spot$r12_12))-1
f1_3=((((1+spot$r36_12)^3)/(1+spot$r12_12))^0.5)-1
f1_4=((((1+spot$r48_12)^4)/(1+spot$r12_12))^(1/3))-1
f1_5=((((1+spot$r60_12)^5)/(1+spot$r12_12))^0.25)-1

forward<-data.frame("jan"= bond24$January)
forward$f1_2<-f1_2
forward$f1_3<-f1_3
forward$f1_4<-f1_4
forward$f1_5<-f1_5

forwardg<-transpose(forward)
forwardg<-forwardg[-1,]

#Write data to csv for graph:
write.csv(forwardg,'C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856/forwardg.csv',row.names=FALSE)

#################################################################

#Create Matrix Using Spot data
row1<-rep(NA,times=9)
for(j in 1:9){
  row1[j]<-matrix(log((spot$r12_12[j+1])/(spot$r12_12[j])))
}

row2<-rep(NA,times=9)
for(j in 1:9){
  row2[j]<-matrix(log((spot$r24_12[j+1])/(spot$r24_12[j])))
}

row3<-rep(NA,times=9)
for(j in 1:9){
  row3[j]<-matrix(log((spot$r36_12[j+1])/(spot$r36_12[j])))
}

row4<-rep(NA,times=9)
for(j in 1:9){
  row4[j]<-matrix(log((spot$r48_12[j+1])/(spot$r48_12[j])))
}

row5<-rep(NA,times=9)
for(j in 1:9){
  row5[j]<-matrix(log((spot$r60_12[j+1])/(spot$r60_12[j])))
}

spm<-cbind(row1,row2,row3,row4,row5)

#Calculate convariance of Spot Matrix:
cpm<-cov(spm)

#Create Matrix for Forward Rates
now1<-rep(NA,times=9)
for(j in 1:9){
  now1[j]<-matrix(log((forward$f1_2[j+1])/(forward$f1_2[j])))
}

now2<-rep(NA,times=9)
for(j in 1:9){
  now2[j]<-matrix(log((forward$f1_3[j+1])/(forward$f1_3[j])))
}

now3<-rep(NA,times=9)
for(j in 1:9){
  now3[j]<-matrix(log((forward$f1_4[j+1])/(forward$f1_4[j])))
}

now4<-rep(NA,times=9)
for(j in 1:9){
  now4[j]<-matrix(log((forward$f1_5[j+1])/(forward$f1_5[j])))
}

fpm<-cbind(now1,now2,now3,now4)

#Calculate convariance of Spot Matrix:
epm<-cov(fpm)

#Calculate egen values
ecpm<-eigen(cpm)#For spot rate covariance matrix
eepm<-eigen(epm)#For forward rate covariance matrix

#Using stargazer package for LaTeX:
library(stargazer)
stargazer(spm)
stargazer(cpm)
stargazer(fpm)
stargazer(epm)
stargazer(ecpm$values)
stargazer(ecpm$vectors)
stargazer(eepm$values)
stargazer(eepm$vectors)

