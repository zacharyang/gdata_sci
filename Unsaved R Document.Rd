setwd('/users/zachang/data_sci/moe')
## Read in data ## 
attendance_records= read.csv('attendance_records_20160324.csv')
demo_info=read.csv('demographic_info_20160324.csv')
psle_grade_range=read.csv('PSLE Grade Score Range.csv')
results=data.frame(read.csv('results_info_20160324.csv'))

print(c(nrow(demo_info),nrow(results),nrow(attendance_records))

summary(attendance_records)

names(results)
attach(results)
detach(results)

### Merge Demographic info and results into one table ### 

master_df=merge(demo_info,results,by="NRIC")
colnames(master_df)[2]='School'
colnames(master_df)[3]='Programme'
drops=('School.y','Programme.y')
master_df=subset(master_df,select=-c(School.y,Programme.y))

### Summary Statistics ## 

attach(master_df)
summary(master_df)
progbyresults=table(Programme,PSLE_Result)
barplot(progbyresults,legend=rownames(progbyresults))
which(X2014_Result>X2015_Result)
master_df
