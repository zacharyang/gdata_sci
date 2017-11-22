library(ggplot2)
library(reshape2)

### ------ Read in data ------ ## 
attendance_records= read.csv('https://raw.githubusercontent.com/zacharyang/gdata_sci/master/attendance_records_20160324.csv')
psle_grade_range=read.csv('https://raw.githubusercontent.com/zacharyang/gdata_sci/master/PSLE%20Grade%20Score%20Range.csv')
demo_info=read.csv('https://raw.githubusercontent.com/zacharyang/gdata_sci/master/demographic_info_20160324.csv')
results=data.frame(read.csv('https://raw.githubusercontent.com/zacharyang/gdata_sci/master/results_info_20160324.csv'))

print(c(nrow(demo_info),nrow(results),nrow(attendance_records))) ## Checking number of records for each df ## 

summary(attendance_records) ## Attendance records only available for EXCEL ##

### ------ Merge Demographic info and results into one table ------ ### 

master_df=merge(demo_info,results,by="NRIC") ## Data merged by common identifier, NRIC ### 
colnames(master_df)[2]='School'
colnames(master_df)[3]='Programme'
master_df=subset(master_df,select=-c(School.y,Programme.y))


### ------ Summary Statistics ------ ## 

summary(master_df)

attach(master_df)

### ------ Recoding Variables ------ ###

## Change PSLE Result to ordinal variable ## 
print(psle_grade_range)
master_df$PSLE_Result_num = NA
master_df$PSLE_Result_num[PSLE_Result=='A*'] = 7
master_df$PSLE_Result_num[PSLE_Result=='A'] = 6
master_df$PSLE_Result_num[PSLE_Result=='B'] = 5
master_df$PSLE_Result_num[PSLE_Result=='C'] = 4
master_df$PSLE_Result_num[PSLE_Result=='D'] = 3
master_df$PSLE_Result_num[PSLE_Result=='E'] = 2
master_df$PSLE_Result_num[PSLE_Result=='U'] = 1

## Change Past Year Result to ordinal variable ## 

master_df$A2014_Result=master_df$A2015_Result=NA

master_df$A2014_Result[X2014_Result<50 & X2014_Result>=35]= 3
master_df$A2014_Result[X2014_Result<35 & X2014_Result>=20]= 2
master_df$A2014_Result[X2014_Result<20]= 1

master_df$A2015_Result[X2015_Result<90 & X2015_Result>=75]= 6
master_df$A2015_Result[X2015_Result<75 & X2015_Result>=60]= 5
master_df$A2015_Result[X2015_Result<60 & X2015_Result>=50]= 4
master_df$A2015_Result[X2015_Result<50 & X2015_Result>=35]= 3
master_df$A2015_Result[X2015_Result<35 & X2015_Result>=20]= 2
master_df$A2015_Result[X2015_Result<20]= 1

detach(master_df)

## Creating value-added outcome variable ## 

master_df$improv_grade=master_df$PSLE_Result_num>master_df$A2015_Result
master_df$improv_maintain_grade=master_df$PSLE_Result_num>=master_df$A2015_Result

## Excel Programme subset and attendance records ## 

excel_df=subset(master_df, Programme=="EXCEL")
attendance_records$total_sessions=rowSums(attendance_records[,4:23]=='P')
excel_attendance=subset(attendance_records,select=c(NRIC,total_sessions))
excel_new_df=merge(excel_df,excel_attendance,by="NRIC")

summary(master_df)
summary(excel_new_df)

attach(master_df)

### ------ Visualisations for Descriptive Analysis ------ ### 

## Boxplots ##

## 2014, 2015 Results by School## 
b1plot<-boxplot(X2014_Result~School,ylim=c(0,100),col='cadetblue',xlab='School',ylab='2014 Results')
points(aggregate(X2014_Result~School,FUN=mean),pch=20)
b2plot<-boxplot(X2015_Result~School,ylim=c(0,100),col='mediumseagreen',xlab='School',ylab='2015 Results')
points(aggregate(X2015_Result~School,FUN=mean),pch=20)

## 2014,2015 Results by Programme ##

b3plot<-boxplot(X2014_Result~Programme,ylim=c(0,100),col='cadetblue',xlab='Programme',ylab='2014 Results')
points(aggregate(X2014_Result~Programme,FUN=mean),pch=20)
b4plot<-boxplot(X2015_Result~Programme,ylim=c(0,100),col='mediumseagreen',xlab='Programme',ylab='2015 Results')
points(aggregate(X2015_Result~Programme,FUN=mean),pch=20)

## Attendance of EXCEL by School ## 

b5plot<-boxplot(excel_new_df$total_sessions~excel_new_df$School,ylim=c(5,25),col='lightcyan',xlab='School',ylab="Number of Sessions attended",main="Boxplot of Number of Sessions attended by School")

## Barplot ### 

## Barplot of School by Programme ##

br1plot<-barplot(table(School,Programme),col=c('steelblue','paleturquoise','palegreen'),ylab="Number of Students",main="Barplot of Number of Students in PASS or EXCEL by School")
legend(x=0.3,y=100,rownames(table(School,Programme)),col=c('steelblue','paleturquoise','palegreen'),pch=15)

## Barplot of Grade by Programme ## 

br2plot<-barplot(table(Programme,PSLE_Result_num), xaxt='n',col=c('steelblue','darkseagreen1'),legend=rownames(table(Programme,PSLE_Result_num)),main="Grade Distribution of PSLE Results by Programme",xlab="Grade",ylab="Count")
axis(1,at=unlist(xplot),labels=c('U','E','D','C','B','A','A*'),col=NA)

### Heatmap to show students' improvement from 2015 to PSLE ### 

m=melt(table(A2015_Result,PSLE_Result_num))
names(m)<-c("A2015_Result","PSLE_Result_num","Count")
heatmap1<-ggplot(data=m,aes(x=A2015_Result,y=PSLE_Result_num,fill=Count))+geom_tile()+scale_fill_gradient(low="lightcyan",high="skyblue4")
heatmap1 + scale_x_discrete(limits=c(2,4,6),labels=c('E','C','A')) + scale_y_discrete(limits=c(2,4,6),labels=c('E','C','A')) + labs(title="Heatmap of PSLE Results vs 2015 Results",x="2015 Results",y="PSLE Results") + theme(plot.title=element_text(family="Helvetica",face="bold",size=18),axis.title.x=element_text(family="Helvetica",size=14),axis.title.y=element_text(family="Helvetica",size=14))

## Histogram describing attendance at EXCEL ##

hist(excel_new_df$total_sessions,col="steelblue",main="Frequency Histogram of Number of Sessions at EXCEL",xlab="Number of Sessions")
sum(excel_new_df$total_sessions>=16)/nrow(excel_new_df)

### ------ Modelling ------ ### 

## Result-only outcome variable model; Simple Linear Regression##

mfit1<-glm(PSLE_Result_num~School+Programme+Gender+PCHI+Flat_Type,data=master_df) 
summary(mfit1)

## Value-added outcome variable model; Logistic Regression##

mfit2<-glm(improv_grade~Programme,family=binomial(),data=master_df)
mfit3<-glm(improv_grade~School+Programme+Gender+PCHI+Flat_Type,family=binomial(),data=master_df) 
summary(mfit2)
summary(mfit3)

## Test if model incorporating demographic is better ##

L = mfit2$deviance-mfit3$deviance 
print(1-pchisq(L,4))

## Interaction model for school and programme ## 

EXCEL_Ind=Programme=='EXCEL'
mfit4<-glm(improv_grade~School+Gender+PCHI+Flat_Type+School*EXCEL_Ind,family=binomial(),data=master_df)
table(master_df$improv_grade[EXCEL_Ind],School[EXCEL_Ind])
summary(mfit4)

## For EXCEL students, to see if attendance affects improvement of grades ##


mfit5<-glm(improv_grade~total_sessions*School+Gender+PCHI,data=excel_new_df)
summary(mfit5)