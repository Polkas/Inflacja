###PAKIETY

library(dplyr)
library(tidyr)
library(vars)
library(urca)
library(ggplot2)

setwd("C:/Users/Combo/Desktop/Inflacja")
data_raw<-read.csv("raw.csv",sep=";",colClasses = c("Date",rep("numeric",27)))

data_raw_tbl<-tbl_df(data_raw)

data_raw_tbl_NA<-data_raw_tbl 
data_raw_tbl_NA<-data_raw_tbl_NA %>% mutate(PKBPL_PROC = PKBPL-100) %>% arrange(Data) 

###########FUN

returns_n<-function(variable,n){
  
  ret<-vector("numeric",length(variable))
  
  ret[1:n]<-0
  
  for(i in (n+1):(length(variable))) ret[i]<-variable[i]/variable[i-n]-1
  
  return(round(ret,3))
}

avg_n<-function(variable,n){
  
  avg<-vector("numeric",length(variable))
  
  avg[(1):(n-1)]<-0
  
  for(i in n:(length(variable))) avg[i]<-(sum(variable[i:(i-n+1)]))/n
  
  return(round(avg,3))
}

#avg_n(data_raw_tbl_NA$M3,12)
#returns_n(data_raw_tbl_NA$M3,12)

############


var2change<-colnames(data_raw_tbl_NA)[3:ncol(data_raw_tbl_NA)]

for( i in var2change){
  
  for(n in 1:10){
    
    data_raw_tbl_NA[paste0(i,'_RET',n)]<-returns_n(as.vector(unlist(data_raw_tbl_NA[,i])),n)
  }
  
}

var2change_2<-colnames(data_raw_tbl_NA)[3:ncol(data_raw_tbl_NA)]

for( i in var2change_2){
  
  for(n in 2:10){
    
    data_raw_tbl_NA[paste0(i,'_AVG',n)]<-avg_n(as.vector(unlist(data_raw_tbl_NA[,i])),n)
  }
  
}

data_noLAG<-data_raw_tbl_NA
data_raw_tbl_NA[,-1] <-data_raw_tbl_NA[,-1] %>% mutate_each(funs(replace(., which(is.na(.)), 0)))
data_raw_tbl_NA[,-1] <-data_raw_tbl_NA[,-1] %>% mutate_each(funs(replace(., which(is.infinite(.)), 0)))
data_noLAG_NA<-data_raw_tbl_NA

##################################### LAGI

#var2change_3<-colnames(data_raw_tbl_NA)[3:ncol(data_raw_tbl_NA)]

#for( i in var2change_3){

# for(n in 1:5){

#  data_raw_tbl_NA[paste0(i,'_LAG',n)]<-lag(as.vector(unlist(data_raw_tbl_NA[,i])),n)
# }

#}


########################## CPI PL correlation with base variables

#data_raw_tbl_NA[,-1] <-data_raw_tbl_NA[,-1] %>% mutate_each(funs(replace(., which(is.na(.)), 0)))
#data_raw_tbl_NA[,-1] <-data_raw_tbl_NA[,-1] %>% mutate_each(funs(replace(., which(is.infinite(.)), 0)))


#corr<-cor(data_raw_tbl_NA[,-1],method = "spearman")
#all_cor<-(round(corr[,1:2],3))

#vect<-cumsum(c(ncol(data_noLAG),rep(c(4,1,4,1),ncol(data_noLAG)/2-1)))

#sum_cor_MM<-rep(0,length(vect)/2)
#sum_cor_YY<-rep(0,length(vect)/2)

#for( i in 1:((length(vect)-1)/2)){
#sum_cor_MM[i]<-sum(all_cor[vect[2*i-1]:vect[2*i],1])
#names(sum_cor_MM)[i]<-rownames(all_cor)[vect[i*2]]
#}


#for( i in 1:((length(vect)-1)/2)){
#sum_cor_YY[i]<-sum(all_cor[vect[2*i-1]:vect[2*i],2])
# names(sum_cor_YY)[i]<-rownames(all_cor)[vect[i*2]]
#}


#uu=sort(abs(sum_cor_MM))

#uu2=sort(abs(sum_cor_YY))

#############STATIONARITY TREND

DFt<-rep(1,length=ncol(data_noLAG_NA)-1)
a<-1
for(i in 2:ncol(data_noLAG_NA)){
  dft<-ur.df(as.vector(unlist(data_noLAG_NA[,i])), type ='trend', selectlags ="AIC")
  if((abs(dft@teststat[1])-abs(dft@cval[1,3]))>0)
  {DFt[a]<-0}
  a<-a+1
}
names(DFt)<-colnames(data_noLAG_NA)[-1]

################### NO TREND

DFn<-rep(1,length=ncol(data_noLAG_NA)-1)
a<-1
for(i in 2:ncol(data_raw_tbl_NA)){
  dft<-ur.df(as.vector(unlist(data_noLAG_NA[,i])), type ='none', selectlags ="AIC")
  if((abs(dft@teststat[1])-abs(dft@cval[1,3]))>0)
  {DFn[a]<-0}
  a<-a+1
}
names(DFn)<-colnames(data_noLAG_NA)[-1]


KPSStau<-rep(0,length=ncol(data_noLAG_NA)-1)
a<-1
for(i in 2:ncol(data_noLAG_NA)){
  dft<-ur.kpss(as.vector(unlist(data_noLAG_NA[,i])), type ='tau', lags="short")
  if((abs(dft@teststat[1])-abs(dft@cval[1,3]))>0)
  {KPSStau[a]<-1}
  a<-a+1
}
names(KPSStau)<-colnames(data_noLAG_NA)[-1]

KPSSmu<-rep(0,length=ncol(data_noLAG_NA)-1)
a<-1
for(i in 2:ncol(data_raw_tbl_NA)){
  dft<-ur.kpss(as.vector(unlist(data_noLAG_NA[,i])), type ='mu', lags="short")
  if((abs(dft@teststat[1])-abs(dft@cval[1,3]))>0)
  {KPSSmu[a]<-1}
  a<-a+1
}
names(KPSSmu)<-colnames(data_noLAG_NA)[-1]

stationar<-cbind(DFt,DFn,KPSStau,KPSSmu)
stationar[1:24,]

############################ PLOTIN VARIABLES 

par(mfrow=c(4,6))
for(i in 1:24){
  plot((as.vector(unlist(data_raw_tbl_NA[,i+1]))),ylab=colnames(data_raw_tbl_NA)[i+1])
}

par(mfrow=c(1,1))

data_plot<-data_raw_tbl_NA[,1:24]%>% gather(key,value,-Data)

qplot(Data,log(value),data=data_plot,colour=key)

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#SIMULATION
'
nr_lag<-10

col_names<-c("CPIUS_MM","PKBPL_PROC_RET1","Commodity.Index_RET1","M3_RET1","USD_RET1","UR_RET1","SALDO_RET1","FED_RET1","BEZROB_RET1","BRENT_RET1","LIBOR3mEUR_RET1","ZYWN_A_RET1","PKBUS_RET1","WIBOR3M_RET1")
com<-(combn(length(col_names),2,simplify = FALSE))
for(i in 3:8){
  com<-c(com,combn(length(col_names),i,simplify = FALSE))
}

all_R2<-array(NA,c(length(com),2,nr_lag))

####################################
for(g in 1:nr_lag){
  
  #########################################KOMBINACJE
  
  #########################
  R2<-vector("numeric",length(com))
  SERIAL<-vector("numeric",length(com))
  for(i in 1:length(com)){
    
    model1<-VAR(data_noLAG_NA[,c("CPIPL_MM",col_names[com[[i]]])],p=g,type="const")
    
    sum_VAR<-summary(model1$varresult$CPIPL_MM)
    
    R2[i]<-sum_VAR$adj.r.squared
    SERIAL[i]<-serial.test(model1)$serial$p.value
    print(paste(i/length(com),g))
    
  }
  all_R2[,1,g]<-R2
  all_R2[,2,g]<-SERIAL
}
'

#data.frame(R2=all_R2[,1,7],serial=all_R2[,2,7]) %>% round(2) %>% View()

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
# FINAL MODEL

var<-c('CPIPL_MM','BRENT_RET1','BEZROB_RET1','USD_RET1','Commodity.Index_RET1')  
n_ahead<-10

model1<-VAR(data_noLAG_NA[,var],p=7,type='const')

w<-summary(model1$varresult$CPIPL_MM)

serial.test(model1)

f<-predict(model1,n.ahead = n_ahead, ci = 0.95)

#Inf_Year<-data.frame(Y=c(sort(rep(seq(1,11),12)),rep(12,10)),I=c(data_raw_tbl_NA$CPIPL_MM,(f$fcst$CPIPL_MM)[,1])) %>% group_by(Y) %>% summarise(sum(I))

#########################################################################
########################################################################

setwd("C:/Users/Combo/Desktop/Inflacja")

ZMIDEX<-read.csv("Copy of Inflacja 2016.csv")

ggplot(ZMIDEX,aes(x=ZMIDEX$Odp.w..*100)) +
  geom_histogram(colour = 'orange',binwidth=1,fill="#FF6600")+
  xlab("INFLACJA W %")+  
  theme(
  panel.background = element_rect(fill = 'white'),
  axis.title = element_text(colour="#666666",size=15, face="bold"),
  axis.text = element_text(size=14,colour="black"),
  panel.grid.major = element_line(colour = "light gray")
)
  



ZMIDEX_P<-mean(ZMIDEX$Odp.w..)*100
NBP<- 0.9
MODEL<-0.27

all_p<-data.frame(Inflacja=c(ZMIDEX_P,NBP,MODEL,mean(c(ZMIDEX_P,NBP,MODEL)),mean(c(ZMIDEX_P,NBP)),mean(c(ZMIDEX_P+MODEL)),mean(c(NBP+MODEL))))

rownames(all_p)<-c("ZMIDEX","NBP","MODEL","AVG(ZMIDEX+MODEL+NBP)","AVG(ZMIDEX+NBP","AVG(ZMIDEX+MODEL)","AVG(NBP+MODEL)")

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
interval<-sapply(10:1,function(x) 1.96*(sum(((c(rev(model1$varresult$CPIPL_MM$residuals)[1:x],rev(f$fcst$CPIPL_MM[,4])[-c(1:x)]/1.96)))^2)^(1/2)))

main<-stats::filter(c(data_raw_tbl_NA$CPIPL_MM,(f$fcst$CPIPL_MM)[,1]),c(rep(1,11)),method="convolution",sides=1)

plot_base<-data.frame(Y=c(seq(from=as.Date("2005-01-28"),to=as.Date("2016-10-28"),by="month"),rep(seq(from=as.Date("2016-01-28"),to=as.Date("2016-10-28"),by="month"),2)),
                      type=c(rep("old",132),rep("predict",n_ahead),rep("predict lower",n_ahead),rep("predict upper",n_ahead)),
                      I=(c(main,rev(rev(main)[1:10])-interval,rev(rev(main)[1:10])+interval)))

plot_base_short<-plot_base[-c(1:80),]

type<-plot_base_short$type

ggplot(plot_base_short,aes(y=I,x=Y)) + 
  geom_line(aes(colour=type),size = 2)  + 
  geom_smooth()+
  geom_hline(yintercept=c(1.5,2.5,3.5), linetype="dashed",size=1) + 
  geom_hline(yintercept=c(0), linetype="solid",size=1.5,colour="#999999") +
  ylab("Inflacja %") + 
  xlab("")+
  scale_colour_manual(values=c("#FF9966","#FF6600","#FF3300","#FF3300"))+  
  ggtitle("INFLACJA R/R w %") + 
  theme(
    plot.title = element_text(colour="#666666",size=18, face="bold"),
    panel.background = element_rect(fill = 'light gray'),
    axis.title = element_text(colour="#666666",size=15, face="bold"),
    axis.text = element_text(size=14),
    panel.grid.major = element_line(colour = "light gray")
  )

plot_base_big<-plot_base[-c(1:10),]

type<-plot_base_big$type

ggplot(plot_base_big,aes(y=I,x=Y)) + 
  geom_line(aes(colour=type),size = 2)  + 
  geom_smooth(colour="orange",alpha=0.2,size=1,method="loess")+
  geom_hline(yintercept=c(1.5,2.5,3.5), linetype="dashed",size=1) + 
  geom_hline(yintercept=c(0), linetype="solid",size=1.5,colour="#999999") +
  ylab("Inflacja %") + 
  xlab("")+
  scale_colour_manual(values=c("#FF9966","#FF6600","#FF6600","#FF6600"))+  
  ggtitle("INFLACJA R/R w %") + 
  theme(
    plot.title = element_text(colour="#666666",size=18, face="bold"),
    panel.background = element_rect(fill = 'white'),
    axis.title = element_text(colour="#666666",size=15, face="bold"),
    axis.text = element_text(size=14,colour="black"),
    panel.grid.major = element_line(colour = "light gray"),
    legend.text=element_text(size=rel(1),face="bold")
  )
#########################################################
#########################################################
#########################################################

model1<-VAR(data_noLAG_NA[,var],p=7,type='const')

w<-summary(model1$varresult$CPIPL_MM)

serial.test(model1)

impulse_USD<-irf(model1,impulse="USD_RET1",response="CPIPL_MM",cumulative = FALSE,n.ahead=12)

IRF_USD<-data.frame(IRF=c(impulse_USD$irf$USD_RET1,impulse_USD$Lower$USD_RET1,impulse_USD$Upper$USD_RET1),type=c(rep("impulse",13),rep("lower",13),rep("upper",13)),Y=rep(seq(1:13),3))

IRF_I_USD<-ggplot(IRF_USD,aes(y=IRF,x=Y)) + 
  geom_line(aes(colour=type),size = 1.5)  + 
  ylab("CPIPL_MM") + xlab("Liczba miesiecy od poczatku 2016") +
  ggtitle("Impuls na zmienna USD_RET1")+ 
  scale_colour_manual(values=c("#FF6600","#FF9966","#FF9966"))+
  geom_hline(yintercept=c(0), linetype="solid",size=1.5,colour="#999999")+
  theme(
    axis.title = element_text(colour="#666666",size=16, face="bold"),
    plot.title = element_text(colour="#666666",size=20, face="bold"),
    panel.background = element_rect(fill = 'white'),
    axis.text = element_text(size=14),
    panel.grid.major = element_line(colour = "light gray")
    )

impulse_BEZROB<-irf(model1,impulse="BEZROB_RET1",response="CPIPL_MM",cumulative = FALSE,n.ahead=12)

IRF_BEZROB<-data.frame(IRF=c(impulse_BEZROB$irf$BEZROB_RET1,impulse_BEZROB$Lower$BEZROB_RET1,impulse_BEZROB$Upper$BEZROB_RET1),type=c(rep("impulse",13),rep("lower",13),rep("upper",13)),Y=rep(seq(1:13),3))

IRF_I_BEZROB<-ggplot(IRF_BEZROB,aes(y=IRF,x=Y)) + 
  geom_line(aes(colour=type),size = 1.5)  + 
  ylab("CPIPL_MM") + xlab("Liczba miesiecy od poczatku 2016") +
  ggtitle("Impuls na zmienna BEZROB_RET1")+ 
  scale_colour_manual(values=c("#FF6600","#FF9966","#FF9966"))+
  geom_hline(yintercept=c(0), linetype="solid",size=1.5,colour="#999999")+
  theme(
    axis.title = element_text(colour="#666666",size=16, face="bold"),
    plot.title = element_text(colour="#666666",size=20, face="bold"),
    panel.background = element_rect(fill = 'white'),
    axis.text = element_text(size=15),
    panel.grid.major = element_line(colour = "light gray")
    )

impulse_BRENT<-irf(model1,impulse="BRENT_RET1",response="CPIPL_MM",cumulative = FALSE,n.ahead=12)

IRF_BRENT<-data.frame(IRF=c(impulse_BRENT$irf$BRENT_RET1,impulse_BRENT$Lower$BRENT_RET1,impulse_BRENT$Upper$BRENT_RET1),type=c(rep("impulse",13),rep("lower",13),rep("upper",13)),Y=rep(seq(1:13),3))

IRF_I_BRENT<-ggplot(IRF_BRENT,aes(y=IRF,x=Y)) + 
  geom_line(aes(colour=type),size = 1.5)  + 
  ylab("CPIPL_MM") + xlab("Liczba miesiecy od poczatku 2016") +
  ggtitle("Impuls na zmienna BRENT_RET1")+ 
  scale_colour_manual(values=c("#FF6600","#FF9966","#FF9966"))+
  geom_hline(yintercept=c(0), linetype="solid",size=1.5,colour="#999999")+
  theme(
    axis.title = element_text(colour="#666666",size=16, face="bold"),
    plot.title = element_text(colour="#666666",size=20, face="bold"),
    panel.background = element_rect(fill = 'white'),
    axis.text = element_text(size=15),
    panel.grid.major = element_line(colour = "light gray")
  )
