library(plm)
library(RODBC)


require(igraph)
require(dplyr) 
require(ggplot2)



require(plotrix)

Cox5to25wrep<-readRDS(file="C:/Dropbox/BigdataJ/Cable_Hu2018/newCoxanlaysis5to25_wrep.rds")

rm(matcheddata_05m)

summary(matcheddata_05m)




con <- odbcConnect("P2PLending",uid = "sa", pwd = "birg2014")

SelectedP2PLending_3m <- sqlQuery(con, "SELECT distinct [BorrowerSiteID], BorrowDate, BorrowMoney, treat_BJ, After_BJ
                                            from [jlsj_180319].[Browsersdetail_all_SID]
                                             where BorrowDate >= '2016-12-22' and BorrowDate<='2017-06-22'
                                            and BorrowMoney>0
                                         ")
summary(SelectedP2PLending_3m)
str(SelectedP2PLending_3m)
#change factor to date
SelectedP2PLending_3m$BorrowDate<-as.Date(SelectedP2PLending_3m$BorrowDate, format = "%Y-%m-%d")

#compare pre and post from all borrowers
pdim(SelectedP2PLending_3m)$balanced

SelectedP2PLending_3m_bal<-make.pbalanced(SelectedP2PLending_3m, balance.type = "fill",index=c("BorrowerSiteID", "BorrowDate"))

str(SelectedP2PLending_3m_bal)
summary(SelectedP2PLending_3m_bal)

#set the values for missing dates

SelectedP2PLending_3m_bal[is.na(SelectedP2PLending_3m_bal$BorrowMoney),]$BorrowMoney<-0




Preban<-SelectedP2PLending_3m[which(SelectedP2PLending_3m$After_BJ == 0),]
Afterban<-SelectedP2PLending_3m[which(SelectedP2PLending_3m$After_BJ == 1),]
summary(Preban)
se(Preban$BorrowMoney)
se(Afterban$BorrowMoney)

t.test(Preban$BorrowMoney,Afterban$BorrowMoney,paired=TRUE)

panel_P2Pdata<- pdata.frame(SelectedP2PLending_3m, index=c("BorrowerSiteID", "BorrowDate"))


#make the data blanced by inserting unioned time slots with individuals

summary(panel_P2Pdata)


make.pbalanced(panel_P2Pdata)

results1<-plm(BorrowMoney~After_BJ,data=panel_P2Pdata,model="within") 

summary(results1)



