#Set Directory
setwd("C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856")
getwd()

#Importing bond price data
bd<-read.csv("Bond Prices.csv", sep=",", header=T)

#Arranging data
bd$mayr<-substring(bd$Maturity.Date,1,4) #Obtaining maturity month
bd$mamo<-substring(bd$Maturity.Date,6,7) #Obtaining maturity month
bd$mayr<-as.numeric(as.character(bd$mayr)) #Converting character to numeric
bd$mamo<-as.numeric(as.character(bd$mamo)) #Converting character to numeric
bd$Maturity.Date<-as.character(bd$Maturity.Date) #Converting character to numeric
bd<-bd[order(bd$mayr), ] #Ordering data based on maturity year

#Selecting the Bonds by Maturity Dates
bd<-subset(bd, bd$Maturity.Date=="2020-03-01" | bd$Maturity.Date=="2020-09-01" |
           bd$Maturity.Date=="2021-03-01"| bd$Maturity.Date=="2021-09-01" |
            bd$Maturity.Date=="2022-03-01" | bd$Maturity.Date=="2022-06-01" |
             bd$Maturity.Date=="2023-03-01" | bd$Maturity.Date=="2023-06-01" |
             bd$Maturity.Date=="2024-03-01" | bd$Maturity.Date=="2024-09-01" |
             bd$Maturity.Date=="2025-03-01") #Creating new dataset of only 32 bonds

#Write data to csv
write.csv(bd,'C:/Users/sara_/Documents/ACADEMICS/2019-2020-FY/MAT1856/bd.csv',row.names=FALSE)
rm("bd")

