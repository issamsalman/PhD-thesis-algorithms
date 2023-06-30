pdf(file="results_QA_ALGO1.pdf")

pdf(file="results10.pdf", width=0.5, height=0.25)
x1<-read.table("normal_TAN_roc_0.62.dat", sep=",")
x2<-read.table("roc-tan-alg1.dat", sep=",")
x3<-read.table("tan_roc_algo2_0.83.dat",  sep=",")
x4<-read.table("TAN_SMOTE.dat",  sep=",")  # D.DISCR roc=0.802
x5<-read.table("chow-liu-roc-0.75.dat",  sep=",") # chow-liu for incomplate data
x6<-read.table("roc-chow-liu-in-im-0.476.dat",  sep=",") # chow-liu for incomplate and impalanced data
x7<-read.table("STAN_ROC_0.891.dat",  sep=",") # D.DISCR algo 3
x8<-read.table("stan_in_im_roc_0.931.dat",  sep=",") # D.DISCR
x9<-read.table("roc_bin_algo1-0.93.dat",  sep=",") # D.Bin
x10<-read.table("tan_inc_imp_bin_.0.95.dat",  sep=",") # D.Bin
x11<-read.table("algo1 bin.dat", sep=",")
x12<-read.table("TANI_Bin.dat", sep=",")
x13<-read.table("TAN_EM.dat", sep=",")
x14<-read.table("TANI_Dis.dat", sep=",")
x15<-read.table("ALOG1_dis.dat", sep=",")
x16<-read.table("EM_TAN_DIS.dat", sep=",")




FP.rate <- x16[,1]
TP.rate<- x16[,2]
plot(FP.rate,TP.rate , col = "blue", lwd = 3,lty=1,type = "l",xlab="SIZE of Data",ylab="AUC")
#x7<- read.table("1000_MCAR_AUC.dat", sep=",")
# AUC<- x7[,2]
#MCAR.Rate <- x7[,1]
#plot(MCAR.Rate ,AUC,   col = "blue", lty=1,type = "l",xlab="MCAR Rate", ylab=" AUC")
# x8<- read.table("2000_MCAR_AUC .dat", sep=",")
#AUC<- x8[,2]
#MCAR.Rate <- x8[,1]
# lines(AUC~MCAR.Rate , col = "BLACK",lty=2, lwd = 3)
FP.rate <- x15[[1]]
TP.rate<- x15[[2]]
lines(TP.rate ~ FP.rate, col = "BLACK",lty=2, lwd = 3)
FP.rate <- x14[[1]]
TP.rate<- x14[[2]]
lines(TP.rate ~ FP.rate, col = "red",lty=3, lwd = 3)

legend(x="bottomright", -1,c("Algo 1","TANI", "(Francois and Leray, 2006)"),
       lty=c(1,2,3),lwd=c(3,3,3),col=c("blue", "BLACK", "red")) 
dev.off()



#  QA graph

x8<- read.table("xy.dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
plot(MCAR.Rate ,AUC,   col = "white",lwd=0.1, lty=1,type = "l",xlab="MCAR Rate", ylab=" AUC")

x8<- read.table("10000_MCAR_AUC.dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
lines(AUC~MCAR.Rate , col = "grey",lty=1, lwd = 2)
x8<- read.table("7000_MCAR_AUC.dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
lines(AUC~MCAR.Rate , col = "red",lty=2, lwd = 2)
x8<- read.table("5000_MCAR_AUC.dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
lines(AUC~MCAR.Rate , col = "BLACK",lty=2, lwd = 2)
x8<- read.table("2000_MCAR_AUC .dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
lines(AUC~MCAR.Rate , col = "BLUE",lty=4, lwd = 2)
x8<- read.table("1000_MCAR_AUC.dat", sep=",")
MCAR.Rate <- x8[,1]
AUC<- x8[,2]
lines(AUC~MCAR.Rate , col = "BLACK",lty=1, lwd = 2)

legend(x="bottomleft", -1,c("1000 size","2000 size", "5000 size", "7000 size", "10000 size"),
       lty=c(1,2,3,4,5),lwd=c(1.5,1.5,1.5,1.5,1.5),col=c( "BLACK","blue",  "BLACK" ,"red","grey")) 

