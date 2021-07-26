#### Capstone Project: Telecom Churn

library(gains)
library(dplyr)
library(irr)
library(caret)


### Importing the csv file into a data set
telecom <- read.csv("telecomfinal.csv")


### Data Exploration 

str(telecom)
summary(telecom)


# Determining the number of unique values in each variable
length(unique(telecom$mou_Mean)) 
length(unique(telecom$totmrc_Mean))
length(unique(telecom$rev_Range))
length(unique(telecom$mou_Range))
length(unique(telecom$change_mou))
length(unique(telecom$drop_blk_Mean))
length(unique(telecom$drop_vce_Range))
length(unique(telecom$owylis_vce_Range))
length(unique(telecom$mou_opkv_Range))
length(unique(telecom$months))
length(unique(telecom$totcalls))
length(unique(telecom$income))
length(unique(telecom$eqpdays))
length(unique(telecom$custcare_Mean))
length(unique(telecom$callwait_Mean))
length(unique(telecom$iwylis_vce_Mean))
length(unique(telecom$callwait_Range))
length(unique(telecom$ccrndmou_Range))
length(unique(telecom$adjqty))
length(unique(telecom$ovrrev_Mean))
length(unique(telecom$rev_Mean))
length(unique(telecom$ovrmou_Mean))
length(unique(telecom$comp_vce_Mean))
length(unique(telecom$plcd_vce_Mean))
length(unique(telecom$avg3mou))
length(unique(telecom$avgmou))
length(unique(telecom$avg3qty)) 
length(unique(telecom$avgqty))
length(unique(telecom$avg6mou))
length(unique(telecom$avg6qty))
length(unique(telecom$crclscod))
length(unique(telecom$asl_flag))
length(unique(telecom$prizm_social_one))
length(unique(telecom$area))
length(unique(telecom$refurb_new))
length(unique(telecom$hnd_webcap))
length(unique(telecom$marital)) 
length(unique(telecom$ethnic))
length(unique(telecom$age1))  
length(unique(telecom$age2))
length(unique(telecom$models))
length(unique(telecom$hnd_price))
length(unique(telecom$actvsubs))
length(unique(telecom$uniqsubs))
length(unique(telecom$forgntvl))
length(unique(telecom$dwlltype))
length(unique(telecom$dwllsize))
length(unique(telecom$mailordr))
length(unique(telecom$occu1))
length(unique(telecom$opk_dat_Mean))
length(unique(telecom$mtrcycle))
length(unique(telecom$numbcars))
length(unique(telecom$retdays))
length(unique(telecom$truck))
length(unique(telecom$wrkwoman))
length(unique(telecom$roam_Mean))
length(unique(telecom$recv_sms_Mean))
length(unique(telecom$blck_dat_Mean)) 
length(unique(telecom$mou_pead_Mean))
length(unique(telecom$churn))
length(unique(telecom$solflag))
length(unique(telecom$proptype))
length(unique(telecom$mailresp))
length(unique(telecom$cartype))  
length(unique(telecom$car_buy))  
length(unique(telecom$children))
length(unique(telecom$csa))  
length(unique(telecom$da_Mean)) 
length(unique(telecom$da_Range))
length(unique(telecom$datovr_Mean)) 
length(unique(telecom$datovr_Range))
length(unique(telecom$div_type))
length(unique(telecom$drop_dat_Mean))
length(unique(telecom$drop_vce_Mean))
length(unique(telecom$adjmou))
length(unique(telecom$totrev))
length(unique(telecom$adjrev))
length(unique(telecom$avgrev))
length(unique(telecom$Customer_ID)) 


# Calculating 5%,10%,25%,50%,75%,90% and 95% percentiles for every variable
quantile(telecom$mou_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$totmrc_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$rev_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$mou_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$change_mou, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$drop_blk_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$drop_vce_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$owylis_vce_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$mou_opkv_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$months, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$totcalls, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$income, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$eqpdays, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$custcare_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$callwait_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$iwylis_vce_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$callwait_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$ccrndmou_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$adjqty, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$ovrrev_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$rev_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$ovrmou_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$comp_vce_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$plcd_vce_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avg3mou, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avgmou, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avg3qty, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avgqty, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avg6mou, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$avg6qty, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$age1, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$age2, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$models, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$hnd_price, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$actvsubs, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$uniqsubs, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$forgntvl, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$opk_dat_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$mtrcycle, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$numbcars, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$retdays, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$truck, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$roam_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$recv_sms_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$blck_dat_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$mou_pead_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$churn, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$da_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$da_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$datovr_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$datovr_Range, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95), na.rm=TRUE)
quantile(telecom$drop_dat_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$drop_vce_Mean, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$adjmou, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$totrev, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$adjrev, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$avgrev, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))
quantile(telecom$Customer_ID, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))

summary(telecom)

### Missing Value Treatment

## 1.  Deleting variables containing more than 10% missing values
telecom<-telecom[,-c(48,49,52,53,55,61,62,63,64,66,72)]


## 2.  Removing observations with large no. of missing values
telecom<-telecom[!(is.na(telecom$mou_Mean) & is.na(telecom$totmrc_Mean) & is.na(telecom$rev_Range) &
                     is.na(telecom$mou_Range) & is.na(telecom$change_mou) & is.na(telecom$ovrrev_Mean) &
                     is.na(telecom$rev_Mean) & is.na(telecom$roam_Mean) & is.na(telecom$da_Mean) & 
                     is.na(telecom$da_Range) & is.na(telecom$datovr_Mean) & is.na(telecom$datovr_Range)),]

## 3. Removing observations with large no. of missing values
telecom<-telecom[!(is.na(telecom$marital) & is.na(telecom$ethnic) & is.na(telecom$age1) &
                     is.na(telecom$age2) & is.na(telecom$forgntvl) & is.na(telecom$mtrcycle) &
                     is.na(telecom$truck) & is.na(telecom$car_buy)),]

## 3. Variables avg6mou and avg6qty
# These variables contain 1976 missing values.
# Out of these,924 observations are found to be not missing on further examination and are ignored.
# The remaining records are missing at random and are deleted.
telecom<-telecom[!((telecom$months!=6) & is.na(telecom$avg6mou)),]


## 4. Variable change_mou
telecom$change_mou[is.na(telecom$change_mou)]<-mean(telecom$change_mou,na.rm=TRUE)


## 5. Variable hnd_price
telecom$hnd_price[is.na(telecom$hnd_price)]<-mean(telecom$hnd_price,na.rm=TRUE)

## 6. Variable eqpdays
# Observation containing missing value of eqpdays is deleted since  model information is also missing.
telecom<-telecom[!(is.na(telecom$eqpdays)),]

# 7. Variable income
telecom$income[is.na(telecom$income)]<-mean(telecom$income,na.rm=TRUE)

### Outliers Treatment

## 1. totmrc_Mean variable
# This variable is the average of total monthly recurring charge
# It cannot have negative values as records. These are outliers and have to be imputed with the positive sign.
telecom$totmrc_Mean<-ifelse(telecom$totmrc_Mean<0,mean(telecom$totmrc_Mean),telecom$totmrc_Mean)


## 2. age1 and age2 variables
quantile(telecom$age1,p=(1:100)/100)
quantile(telecom$age2,p=(1:100)/100)

# In age1 and age2 variables, there are huge number of records with value 0. 
# This implies that either there is no primary household member(i.e the subscriber may be the primary household member) or no secondary member.
# Or these records may be missing.
# Since the number of these records are huge,imputing the data with mean will immensely skew the data.
# Therefore these outliers are ignored.

## 3. eqpdays
# The outliers are -5,-4,-3,-2,-1 and 0.
# 0 implies that the record is missing
# The other outliers contain the wrong sign. These can be rectified by imputing with mean.

telecom$eqpdays<-ifelse(telecom$eqpdays<=0,mean(telecom$eqpdays),telecom$eqpdays)

## 4. Variables containing values which are higher than 95% percentile
# Values that are higher than 95% percentile are outliers and can skew the data immensely.
# These outliers are imputed with mean 

telecom$mou_Mean<-ifelse(telecom$mou_Mean>1590.75,mean(telecom$mou_Mean),telecom$mou_Mean)
telecom$totmrc_Mean<-ifelse(telecom$totmrc_Mean>85,mean(telecom$totmrc_Mean),telecom$totmrc_Mean)
telecom$rev_Range<-ifelse(telecom$rev_Range>171.955,mean(telecom$rev_Range),telecom$rev_Range)
telecom$mou_Range<-ifelse(telecom$mou_Range>1171.25,mean(telecom$mou_Range),telecom$mou_Range)
telecom$change_mou<-ifelse(telecom$change_mou>346.25,mean(telecom$change_mou),telecom$change_mou)
telecom$drop_blk_Mean<-ifelse(telecom$drop_blk_Mean>35.333333,mean(telecom$drop_blk_Mean),telecom$drop_blk_Mean)
telecom$drop_vce_Range<-ifelse(telecom$drop_vce_Range>19,mean(telecom$drop_vce_Range),telecom$drop_vce_Range)
telecom$owylis_vce_Range<-ifelse(telecom$owylis_vce_Range>56,mean(telecom$owylis_vce_Range),telecom$owylis_vce_Range)
telecom$mou_opkv_Range<-ifelse(telecom$mou_opkv_Range>438.494,mean(telecom$mou_opkv_Range),telecom$mou_opkv_Range)
telecom$months<-ifelse(telecom$months>37,mean(telecom$months),telecom$months)
telecom$totcalls<-ifelse(telecom$totcalls>8962,mean(telecom$totcalls),telecom$totcalls)
telecom$eqpdays<-ifelse(telecom$eqpdays>858,mean(telecom$eqpdays),telecom$eqpdays)
telecom$custcare_Mean<-ifelse(telecom$custcare_Mean>9.333333,mean(telecom$custcare_Mean),telecom$custcare_Mean)
telecom$callwait_Mean<-ifelse(telecom$callwait_Mean>4.6666667,mean(telecom$callwait_Mean),telecom$callwait_Mean)
telecom$iwylis_vce_Mean<-ifelse(telecom$iwylis_vce_Mean>36,mean(telecom$iwylis_vce_Mean),telecom$iwylis_vce_Mean)
telecom$callwait_Range<-ifelse(telecom$callwait_Range>8,mean(telecom$callwait_Range),telecom$callwait_Range)
telecom$ccrndmou_Range<-ifelse(telecom$ccrndmou_Range>37,mean(telecom$ccrndmou_Range),telecom$ccrndmou_Range)
telecom$adjqty<-ifelse(telecom$adjqty>8862,mean(telecom$adjqty),telecom$adjqty)
telecom$ovrrev_Mean<-ifelse(telecom$ovrrev_Mean>63.45,mean(telecom$ovrrev_Mean),telecom$ovrrev_Mean)
telecom$rev_Mean<-ifelse(telecom$rev_Mean>135.77813,mean(telecom$rev_Mean),telecom$rev_Mean)
telecom$ovrmou_Mean<-ifelse(telecom$ovrmou_Mean>190,mean(telecom$ovrmou_Mean),telecom$ovrmou_Mean)
telecom$comp_vce_Mean<-ifelse(telecom$comp_vce_Mean>338,mean(telecom$comp_vce_Mean),telecom$comp_vce_Mean)
telecom$plcd_vce_Mean<-ifelse(telecom$plcd_vce_Mean>452,mean(telecom$plcd_vce_Mean),telecom$plcd_vce_Mean)
telecom$avg3mou<-ifelse(telecom$avg3mou>1611,mean(telecom$avg3mou),telecom$avg3mou)
telecom$avgmou<-ifelse(telecom$avgmou>1374.51,mean(telecom$avgmou),telecom$avgmou)
telecom$avg3qty<-ifelse(telecom$avg3qty>548.2,mean(telecom$avg3qty),telecom$avg3qty)
telecom$avgqty<-ifelse(telecom$avgqty>495.254,mean(telecom$avgqty),telecom$avgqty)
telecom$avg6mou<-ifelse(telecom$avg6mou>1530,mean(telecom$avg6mou,na.rm=T),telecom$avg6mou)
telecom$avg6qty<-ifelse(telecom$avg6qty>528,mean(telecom$avg6qty,na.rm=T),telecom$avg6qty)
telecom$hnd_price<-ifelse(telecom$hnd_price>199.98999,mean(telecom$hnd_price),telecom$hnd_price)
telecom$opk_dat_Mean<-ifelse(telecom$opk_dat_Mean>0.6666667,mean(telecom$opk_dat_Mean),telecom$opk_dat_Mean)
telecom$roam_Mean<-ifelse(telecom$roam_Mean>5.0175,mean(telecom$roam_Mean),telecom$roam_Mean)
telecom$recv_sms_Mean<-ifelse(telecom$recv_sms_Mean>0,mean(telecom$recv_sms_Mean),telecom$recv_sms_Mean)
telecom$blck_dat_Mean<-ifelse(telecom$blck_dat_Mean>0,mean(telecom$blck_dat_Mean),telecom$blck_dat_Mean)
telecom$mou_pead_Mean<-ifelse(telecom$mou_pead_Mean>0.8333333,mean(telecom$mou_pead_Mean),telecom$mou_pead_Mean)
telecom$da_Mean<-ifelse(telecom$da_Mean>4.2075,mean(telecom$da_Mean),telecom$da_Mean)
telecom$da_Range<-ifelse(telecom$da_Range>6.93,mean(telecom$da_Range),telecom$da_Range)
telecom$datovr_Mean<-ifelse(telecom$datovr_Mean>0.585,mean(telecom$datovr_Mean),telecom$datovr_Mean)
telecom$datovr_Range<-ifelse(telecom$datovr_Range>1.95,mean(telecom$datovr_Range),telecom$datovr_Range)
telecom$drop_dat_Mean<-ifelse(telecom$drop_dat_Mean>0,mean(telecom$drop_dat_Mean),telecom$drop_dat_Mean)
telecom$drop_vce_Mean<-ifelse(telecom$drop_vce_Mean>22,mean(telecom$drop_vce_Mean),telecom$drop_vce_Mean)
telecom$adjmou<-ifelse(telecom$adjmou>22657.2,mean(telecom$adjmou),telecom$adjmou)
telecom$totrev<-ifelse(telecom$totrev>2538.344,mean(telecom$totrev),telecom$totrev)
telecom$adjrev<-ifelse(telecom$adjrev>2442.91,mean(telecom$adjrev),telecom$adjrev)
telecom$avgrev<-ifelse(telecom$avgrev>123.532,mean(telecom$avgrev),telecom$avgrev)

summary(telecom)

### Variable Profiling 

# 1. mou_Mean
telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_mouMean
p_mouMean$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_mouMean$churn_percent<-p_mouMean$n/p_mouMean$N
p_mouMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
p_mouMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
p_mouMean$varname<-rep("mou_Mean",nrow(p_mouMean))

plot(p_mouMean$dec,p_mouMean$churn_percent,type="l")

# 2. totmrc_Mean
telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_totmrcMean
p_totmrcMean$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_totmrcMean$churn_percent<-p_totmrcMean$n/p_totmrcMean$N
p_totmrcMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
p_totmrcMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
p_totmrcMean$varname<-rep("totmrc_Mean",nrow(p_totmrcMean))

plot(p_totmrcMean$dec,p_totmrcMean$churn_percent,type="l")

# 3. rev_Range
telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_revRange
p_revRange$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
p_revRange$churn_percent<-p_revRange$n/p_revRange$N
p_revRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
p_revRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
p_revRange$varname<-rep("rev_Range",nrow(p_revRange))

plot(p_revRange$dec,p_revRange$churn_percent,type="l")

# 4. mou_Range
telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_mouRange
p_mouRange$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
p_mouRange$churn_percent<-p_mouRange$n/p_mouRange$N
p_mouRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
p_mouRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
p_mouRange$varname<-rep("mou_Range",nrow(p_mouRange))

plot(p_mouRange$dec,p_mouRange$churn_percent,type="l")

# 5. change_mou
telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_changemou
p_changemou$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
p_changemou$churn_percent<-p_changemou$n/p_changemou$N
p_changemou$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
p_changemou$LessThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
p_changemou$varname<-rep("change_mou",nrow(p_changemou))

plot(p_changemou$dec,p_changemou$churn_percent,type="l")

# 6. drop_blk_Mean
telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_dropblkMean
p_dropblkMean$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_dropblkMean$churn_percent<-p_dropblkMean$n/p_dropblkMean$N
p_dropblkMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
p_dropblkMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
p_dropblkMean$varname<-rep("drop_blk_Mean",nrow(p_dropblkMean))

plot(p_dropblkMean$dec,p_dropblkMean$churn_percent,type="l")

# 7. drop_vce_Range
telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_dropvceRange
p_dropvceRange$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
p_dropvceRange$churn_percent<-p_dropvceRange$n/p_dropvceRange$N
p_dropvceRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
p_dropvceRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
p_dropvceRange$varname<-rep("drop_vce_Range",nrow(p_dropvceRange))

plot(p_dropvceRange$dec,p_dropvceRange$churn_percent,type="l")

# 8. owylis_vce_Range
telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_owylisvceRange
p_owylisvceRange$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
p_owylisvceRange$churn_percent<-p_owylisvceRange$n/p_owylisvceRange$N
p_owylisvceRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
p_owylisvceRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
p_owylisvceRange$varname<-rep("owylis_vce_Range",nrow(p_owylisvceRange))

plot(p_owylisvceRange$dec,p_owylisvceRange$churn_percent,type="l")

# 9. mou_opkv_Range
telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_mouopkvRange
p_mouopkvRange$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
p_mouopkvRange$churn_percent<-p_mouopkvRange$n/p_mouopkvRange$N
p_mouopkvRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
p_mouopkvRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
p_mouopkvRange$varname<-rep("mou_opkv_Range",nrow(p_mouopkvRange))

plot(p_mouopkvRange$dec,p_mouopkvRange$churn_percent,type="l")

# 10. months
telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_months
p_months$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
p_months$churn_percent<-p_months$n/p_months$N
p_months$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
p_months$LessThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
p_months$varname<-rep("months",nrow(p_months))

plot(p_months$dec,p_months$churn_percent,type="l")

# 11. totcalls
telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_totcalls
p_totcalls$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
p_totcalls$churn_percent<-p_totcalls$n/p_totcalls$N
p_totcalls$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
p_totcalls$LessThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
p_totcalls$varname<-rep("totcalls",nrow(p_totcalls))

plot(p_totcalls$dec,p_totcalls$churn_percent,type="l")

# 12. eqpdays
telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_eqpdays
p_eqpdays$N<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
p_eqpdays$churn_percent<-p_eqpdays$n/p_eqpdays$N
p_eqpdays$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
p_eqpdays$LessThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
p_eqpdays$varname<-rep("eqpdays",nrow(p_eqpdays))

plot(p_eqpdays$dec,p_eqpdays$churn_percent,type="l")

# 13. custcare_Mean
telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%count(churn,dec)%>%filter(churn==1)->p_custcareMean
p_custcareMean$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%count(dec)%>%unname())[[2]]
p_custcareMean$churn_percent<-p_custcareMean$n/p_custcareMean$N
p_custcareMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
p_custcareMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
p_custcareMean$varname<-rep("custcare_Mean",nrow(p_custcareMean))

plot(p_custcareMean$dec,p_custcareMean$churn_percent,type="l")

# 14. callwait_Mean 
telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->p_callwaitMean
p_callwaitMean$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
p_callwaitMean$churn_percent<-p_callwaitMean$n/p_callwaitMean$N
p_callwaitMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
p_callwaitMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
p_callwaitMean$varname<-rep("callwait_Mean",nrow(p_callwaitMean))

plot(p_callwaitMean$dec,p_callwaitMean$churn_percent,type="l")

# 15. iwylis_vce_Mean
telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->p_iwylisvceMean
p_iwylisvceMean$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
p_iwylisvceMean$churn_percent<-p_iwylisvceMean$n/p_iwylisvceMean$N
p_iwylisvceMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
p_iwylisvceMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
p_iwylisvceMean$varname<-rep("iwylis_vce_Mean",nrow(p_iwylisvceMean))

plot(p_iwylisvceMean$dec,p_iwylisvceMean$churn_percent,type="l")

# 16. callwait_Range
telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)->p_callwaitRange
p_callwaitRange$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(dec)%>%unname())[[2]]
p_callwaitRange$churn_percent<-p_callwaitRange$n/p_callwaitRange$N
p_callwaitRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
p_callwaitRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
p_callwaitRange$varname<-rep("callwait_Range",nrow(p_callwaitRange))

plot(p_callwaitRange$dec,p_callwaitRange$churn_percent,type="l")

# 17. ccrndmou_Range
telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)->p_ccrndmouRange
p_ccrndmouRange$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(dec)%>%unname())[[2]]
p_ccrndmouRange$churn_percent<-p_ccrndmouRange$n/p_ccrndmouRange$N
p_ccrndmouRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
p_ccrndmouRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
p_ccrndmouRange$varname<-rep("ccrndmou_Range",nrow(p_ccrndmouRange))

plot(p_ccrndmouRange$dec,p_ccrndmouRange$churn_percent,type="l")

# 18. adjqty
telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_adjqty
p_adjqty$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
p_adjqty$churn_percent<-p_adjqty$n/p_adjqty$N
p_adjqty$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
p_adjqty$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
p_adjqty$varname<-rep("adjqty",nrow(p_adjqty))

plot(p_adjqty$dec,p_adjqty$churn_percent,type="l")

# 19. ovrrev_Mean
telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->p_ovrrevMean
p_ovrrevMean$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
p_ovrrevMean$churn_percent<-p_ovrrevMean$n/p_ovrrevMean$N
p_ovrrevMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
p_ovrrevMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
p_ovrrevMean$varname<-rep("ovrrev_Mean",nrow(p_ovrrevMean))

plot(p_ovrrevMean$dec,p_ovrrevMean$churn_percent,type="l")

# 20. rev_Mean
telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_revMean
p_revMean$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_revMean$churn_percent<-p_revMean$n/p_revMean$N
p_revMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
p_revMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
p_revMean$varname<-rep("rev_Mean",nrow(p_revMean))

plot(p_revMean$dec,p_revMean$churn_percent,type="l")

# 21. ovrmou_Mean
telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->p_ovrmouMean
p_ovrmouMean$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
p_ovrmouMean$churn_percent<-p_ovrmouMean$n/p_ovrmouMean$N
p_ovrmouMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
p_ovrmouMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
p_ovrmouMean$varname<-rep("ovrmou_Mean",nrow(p_ovrmouMean))

plot(p_ovrmouMean$dec,p_ovrmouMean$churn_percent,type="l")

# 22. comp_vce_Mean
telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_compvceMean
p_compvceMean$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_compvceMean$churn_percent<-p_compvceMean$n/p_compvceMean$N
p_compvceMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
p_compvceMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
p_compvceMean$varname<-rep("comp_vce_Mean",nrow(p_compvceMean))

plot(p_compvceMean$dec,p_compvceMean$churn_percent,type="l")

# 23. plcd_vce_Mean
telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_plcdvceMean
p_plcdvceMean$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_plcdvceMean$churn_percent<-p_plcdvceMean$n/p_plcdvceMean$N
p_plcdvceMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
p_plcdvceMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
p_plcdvceMean$varname<-rep("plcd_vce_Mean",nrow(p_plcdvceMean))

plot(p_plcdvceMean$dec,p_plcdvceMean$churn_percent,type="l")

# 24. avg3mou
telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_avg3mou
p_avg3mou$N<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
p_avg3mou$churn_percent<-p_avg3mou$n/p_avg3mou$N
p_avg3mou$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
p_avg3mou$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
p_avg3mou$varname<-rep("avg3mou",nrow(p_avg3mou))

plot(p_avg3mou$dec,p_avg3mou$churn_percent,type="l")

# 25. avgmou
telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_avgmou
p_avgmou$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
p_avgmou$churn_percent<-p_avgmou$n/p_avgmou$N
p_avgmou$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
p_avgmou$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
p_avgmou$varname<-rep("avgmou",nrow(p_avgmou))

plot(p_avgmou$dec,p_avgmou$churn_percent,type="l")

# 26. avg3qty
telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_avg3qty
p_avg3qty$N<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
p_avg3qty$churn_percent<-p_avg3qty$n/p_avg3qty$N
p_avg3qty$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
p_avg3qty$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
p_avg3qty$varname<-rep("avg3qty",nrow(p_avg3qty))

plot(p_avg3qty$dec,p_avg3qty$churn_percent,type="l")

# 27. avgqty
telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_avgqty
p_avgqty$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
p_avgqty$churn_percent<-p_avgqty$n/p_avgqty$N
p_avgqty$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
p_avgqty$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
p_avgqty$varname<-rep("avgqty",nrow(p_avgqty))

plot(p_avgqty$dec,p_avgqty$churn_percent,type="l")

# 28. avg6mou
telecom%>%mutate(dec=ntile(avg6mou,n=11))%>%count(churn,dec)%>%filter(churn==1)->p_avg6mou
p_avg6mou$N<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=11))%>%count(dec)%>%unname())[[2]]
p_avg6mou$churn_percent<-p_avg6mou$n/p_avg6mou$N
p_avg6mou$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=11))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
p_avg6mou$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=11))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
p_avg6mou$varname<-rep("avg6mou",nrow(p_avg6mou))

plot(p_avg6mou$dec,p_avg6mou$churn_percent,type="l")

# 29. avg6qty
telecom%>%mutate(dec=ntile(avg6qty,n=11))%>%count(churn,dec)%>%filter(churn==1)->p_avg6qty
p_avg6qty$N<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=11))%>%count(dec)%>%unname())[[2]]
p_avg6qty$churn_percent<-p_avg6qty$n/p_avg6qty$N
p_avg6qty$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=11))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
p_avg6qty$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=11))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
p_avg6qty$varname<-rep("avg6qty",nrow(p_avg6qty))

plot(p_avg6qty$dec,p_avg6qty$churn_percent,type="l")

# 30. opk_dat_Mean
telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_opkdatMean
p_opkdatMean$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_opkdatMean$churn_percent<-p_opkdatMean$n/p_opkdatMean$N
p_opkdatMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
p_opkdatMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
p_opkdatMean$varname<-rep("opk_dat_Mean",nrow(p_opkdatMean))

plot(p_opkdatMean$dec,p_opkdatMean$churn_percent,type="l")

# 31. roam_Mean
telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_roamMean
p_roamMean$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_roamMean$churn_percent<-p_roamMean$n/p_roamMean$N
p_roamMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
p_roamMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
p_roamMean$varname<-rep("roam_Mean",nrow(p_roamMean))

plot(p_roamMean$dec,p_roamMean$churn_percent,type="l")

# 32. recv_sms_Mean
telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_recvsmsMean
p_recvsmsMean$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_recvsmsMean$churn_percent<-p_recvsmsMean$n/p_recvsmsMean$N
p_recvsmsMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
p_recvsmsMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
p_recvsmsMean$varname<-rep("recv_sms_Mean",nrow(p_recvsmsMean))

plot(p_recvsmsMean$dec,p_recvsmsMean$churn_percent,type="l")

# 33. blck_dat_Mean
telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_blckdatMean
p_blckdatMean$N<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_blckdatMean$churn_percent<-p_blckdatMean$n/p_blckdatMean$N
p_blckdatMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
p_blckdatMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
p_blckdatMean$varname<-rep("blck_dat_Mean",nrow(p_blckdatMean))

plot(p_blckdatMean$dec,p_blckdatMean$churn_percent,type="l")

# 34. mou_pead_Mean
telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_moupeadMean
p_moupeadMean$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_moupeadMean$churn_percent<-p_moupeadMean$n/p_moupeadMean$N
p_moupeadMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
p_moupeadMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
p_moupeadMean$varname<-rep("mou_pead_Mean",nrow(p_moupeadMean))

plot(p_moupeadMean$dec,p_moupeadMean$churn_percent,type="l")

# 35. da_Mean
telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->p_daMean
p_daMean$N<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
p_daMean$churn_percent<-p_daMean$n/p_daMean$N
p_daMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
p_daMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
p_daMean$varname<-rep("da_Mean",nrow(p_daMean))

plot(p_daMean$dec,p_daMean$churn_percent,type="l")

# 36. da_Range
telecom%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->p_daRange
p_daRange$N<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
p_daRange$churn_percent<-p_daRange$n/p_daRange$N
p_daRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
p_daRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
p_daRange$varname<-rep("da_Range",nrow(p_daRange))

plot(p_daRange$dec,p_daRange$churn_percent,type="l")

# 37. datovr_Mean
telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_datovrMean
p_datovrMean$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_datovrMean$churn_percent<-p_datovrMean$n/p_datovrMean$N
p_datovrMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
p_datovrMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
p_datovrMean$varname<-rep("datovr_Mean",nrow(p_datovrMean))

plot(p_datovrMean$dec,p_datovrMean$churn_percent,type="l")

# 38. datovr_Range
telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_datovrRange
p_datovrRange$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]
p_datovrRange$churn_percent<-p_datovrRange$n/p_datovrRange$N
p_datovrRange$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
p_datovrRange$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
p_datovrRange$varname<-rep("datovr_Range",nrow(p_datovrRange))

plot(p_datovrRange$dec,p_datovrRange$churn_percent,type="l")

# 39. drop_dat_Mean
telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->p_dropdatMean
p_dropdatMean$N<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
p_dropdatMean$churn_percent<-p_dropdatMean$n/p_dropdatMean$N
p_dropdatMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
p_dropdatMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
p_dropdatMean$varname<-rep("drop_dat_Mean",nrow(p_dropdatMean))

plot(p_dropdatMean$dec,p_dropdatMean$churn_percent,type="l")

# 40. drop_vce_Mean
telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_dropvceMean
p_dropvceMean$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
p_dropvceMean$churn_percent<-p_dropvceMean$n/p_dropvceMean$N
p_dropvceMean$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
p_dropvceMean$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
p_dropvceMean$varname<-rep("drop_vce_Mean",nrow(p_dropvceMean))

plot(p_dropvceMean$dec,p_dropvceMean$churn_percent,type="l")

# 41. adjmou
telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_adjmou
p_adjmou$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
p_adjmou$churn_percent<-p_adjmou$n/p_adjmou$N
p_adjmou$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
p_adjmou$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
p_adjmou$varname<-rep("adjmou",nrow(p_adjmou))

plot(p_adjmou$dec,p_adjmou$churn_percent,type="l")

# 42. totrev
telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_totrev
p_totrev$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
p_totrev$churn_percent<-p_totrev$n/p_totrev$N
p_totrev$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
p_totrev$LessThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
p_totrev$varname<-rep("totrev",nrow(p_totrev))

plot(p_totrev$dec,p_totrev$churn_percent,type="l")

# 43. adjrev
telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_adjrev
p_adjrev$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
p_adjrev$churn_percent<-p_adjrev$n/p_adjrev$N
p_adjrev$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
p_adjrev$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
p_adjrev$varname<-rep("adjrev",nrow(p_adjrev))

plot(p_adjrev$dec,p_adjrev$churn_percent,type="l")

# 44. avgrev
telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->p_avgrev
p_avgrev$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
p_avgrev$churn_percent<-p_avgrev$n/p_avgrev$N
p_avgrev$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
p_avgrev$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
p_avgrev$varname<-rep("avgrev",nrow(p_avgrev))

plot(p_avgrev$dec,p_avgrev$churn_percent,type="l")

# 45. hnd_price
telecom%>%mutate(dec=ntile(hnd_price,n=6))%>%count(churn,dec)%>%filter(churn==1)->p_hndprice
p_hndprice$N<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=6))%>%count(dec)%>%unname())[[2]]
p_hndprice$churn_percent<-p_hndprice$n/p_hndprice$N
p_hndprice$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=6))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
p_hndprice$LessThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=6))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
p_hndprice$varname<-rep("hnd_price",nrow(p_hndprice))

plot(p_hndprice$dec,p_hndprice$churn_percent,type="l")

# 45. income
telecom%>%mutate(dec=ntile(income,n=6))%>%count(churn,dec)%>%filter(churn==1)->p_income
p_income$N<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%count(dec)%>%unname())[[2]]
p_income$churn_percent<-p_income$n/p_income$N
p_income$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(min(income)))[[2]]
p_income$LessThan<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(max(income)))[[2]]
p_income$varname<-rep("income",nrow(p_income))

plot(p_income$dec,p_income$churn_percent,type="l")

## Categorical Variables

# 1. actvsubs
telecom%>%count(churn,levels=actvsubs)%>%filter(churn==1)->p_actvsubs
p_actvsubs$N<-unclass(telecom%>%filter(actvsubs%in%p_actvsubs$levels)%>%count(actvsubs))[[2]]
p_actvsubs$churn_percent<-p_actvsubs$n/p_actvsubs$N
p_actvsubs$varname<-rep("actvsubs",nrow(p_actvsubs))

# 2. age1
telecom%>%count(churn,levels=age1)%>%filter(churn==1)->p_age1
p_age1$N<-unclass(telecom%>%filter(age1%in%p_age1$levels)%>%count(age1))[[2]]
p_age1$churn_percent<-p_age1$n/p_age1$N
p_age1$varname<-rep("age1",nrow(p_age1))

# 3. age2
telecom%>%count(churn,levels=age2)%>%filter(churn==1)->p_age2
p_age2$N<-unclass(telecom%>%filter(age2%in%p_age2$levels)%>%count(age2))[[2]]
p_age2$churn_percent<-p_age2$n/p_age2$N
p_age2$varname<-rep("age2",nrow(p_age2))

# 4. area
telecom%>%count(churn,levels=area)%>%filter(churn==1)->p_area
p_area$N<-unclass(telecom%>%filter(area%in%p_area$levels)%>%count(area))[[2]]
p_area$churn_percent<-p_area$n/p_area$N
p_area$varname<-rep("area",nrow(p_area))

# 5. asl_flag
telecom%>%count(churn,levels=asl_flag)%>%filter(churn==1)->p_aslflag
p_aslflag$N<-unclass(telecom%>%filter(asl_flag%in%p_aslflag$levels)%>%count(asl_flag))[[2]]
p_aslflag$churn_percent<-p_aslflag$n/p_aslflag$N
p_aslflag$varname<-rep("asl_flag",nrow(p_aslflag))

# 6. car_buy
telecom%>%count(churn,levels=car_buy)%>%filter(churn==1)->p_carbuy
p_carbuy$N<-unclass(telecom%>%filter(car_buy%in%p_carbuy$levels)%>%count(car_buy))[[2]]
p_carbuy$churn_percent<-p_carbuy$n/p_carbuy$N
p_carbuy$varname<-rep("car_buy",nrow(p_carbuy))

# 7. crclscod
telecom%>%count(churn,levels=crclscod)%>%filter(churn==1)->p_crclscod
p_crclscod$N<-unclass(telecom%>%filter(crclscod%in%p_crclscod$levels)%>%count(crclscod))[[2]]
p_crclscod$churn_percent<-p_crclscod$n/p_crclscod$N
p_crclscod$varname<-rep("crclscod",nrow(p_crclscod))

# 8. csa
telecom%>%count(churn,levels=csa)%>%filter(churn==1)->p_csa
p_csa$N<-unclass(telecom%>%filter(csa%in%p_csa$levels)%>%count(csa))[[2]]
p_csa$churn_percent<-p_csa$n/p_csa$N
p_csa$varname<-rep("csa",nrow(p_csa))

# 9. ethnic
telecom%>%count(churn,levels=ethnic)%>%filter(churn==1)->p_ethnic
p_ethnic$N<-unclass(telecom%>%filter(ethnic%in%p_ethnic$levels)%>%count(ethnic))[[2]]
p_ethnic$churn_percent<-p_ethnic$n/p_ethnic$N
p_ethnic$varname<-rep("ethnic",nrow(p_ethnic))

# 10. forgntvl
telecom%>%count(churn,levels=forgntvl)%>%filter(churn==1)->p_forgntvl
p_forgntvl$N<-unclass(telecom%>%filter(forgntvl%in%p_forgntvl$levels)%>%count(forgntvl))[[2]]
p_forgntvl$churn_percent<-p_forgntvl$n/p_forgntvl$N
p_forgntvl$varname<-rep("forgntvl",nrow(p_forgntvl))

# 11. marital
telecom%>%count(churn,levels=marital)%>%filter(churn==1)->p_marital
p_marital$N<-unclass(telecom%>%filter(marital%in%p_marital$levels)%>%count(marital))[[2]]
p_marital$churn_percent<-p_marital$n/p_marital$N
p_marital$varname<-rep("marital",nrow(p_marital))

# 12. mtrcycle
telecom%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->p_mtrcycle
p_mtrcycle$N<-unclass(telecom%>%filter(mtrcycle%in%p_mtrcycle$levels)%>%count(mtrcycle))[[2]]
p_mtrcycle$churn_percent<-p_mtrcycle$n/p_mtrcycle$N
p_mtrcycle$varname<-rep("mtrcycle",nrow(p_mtrcycle))

# 13. models 
telecom%>%count(churn,levels=models)%>%filter(churn==1)->p_models
p_models$N<-unclass(telecom%>%filter(models%in%p_models$levels)%>%count(models))[[2]]
p_models$churn_percent<-p_models$n/p_models$N
p_models$varname<-rep("models",nrow(p_models))

# 14. refurb_new
telecom%>%count(churn,levels=refurb_new)%>%filter(churn==1)->p_refurbnew
p_refurbnew$N<-unclass(telecom%>%filter(refurb_new%in%p_refurbnew$levels)%>%count(refurb_new))[[2]]
p_refurbnew$churn_percent<-p_refurbnew$n/p_refurbnew$N
p_refurbnew$varname<-rep("refurb_new",nrow(p_refurbnew))

# 15. truck
telecom%>%count(churn,levels=truck)%>%filter(churn==1)->p_truck
p_truck$N<-unclass(telecom%>%filter(truck%in%p_truck$levels)%>%count(truck))[[2]]
p_truck$churn_percent<-p_truck$n/p_truck$N
p_truck$varname<-rep("truck",nrow(p_truck))

# 16. uniqsubs
telecom%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->p_uniqsubs
p_uniqsubs$N<-unclass(telecom%>%filter(uniqsubs%in%p_uniqsubs$levels)%>%count(uniqsubs))[[2]]
p_uniqsubs$churn_percent<-p_uniqsubs$n/p_uniqsubs$N
p_uniqsubs$varname<-rep("uniqsubs",nrow(p_uniqsubs))

# 17. prizm_social_one
telecom%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->p_prizmsocialone
p_prizmsocialone$N<-unclass(telecom%>%filter(prizm_social_one%in%p_prizmsocialone$levels)%>%count(prizm_social_one))[[2]]
p_prizmsocialone$churn_percent<-p_prizmsocialone$n/p_prizmsocialone$N
p_prizmsocialone$varname<-rep("prizm_social_one",nrow(p_prizmsocialone))

# 18. hnd_webcap
telecom%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->p_hndwebcap
p_hndwebcap$N<-unclass(telecom%>%filter(hnd_webcap%in%p_hndwebcap$levels)%>%count(hnd_webcap))[[2]]
p_hndwebcap$churn_percent<-p_hndwebcap$n/p_hndwebcap$N
p_hndwebcap$varname<-rep("hnd_webcap",nrow(p_hndwebcap))

# 19. dwllsize
telecom%>%count(churn,levels=dwllsize)%>%filter(churn==1)->p_dwllsize
p_dwllsize$N<-unclass(telecom%>%filter(dwllsize%in%p_dwllsize$levels)%>%count(dwllsize))[[2]]
p_dwllsize$churn_percent<-p_dwllsize$n/p_dwllsize$N
p_dwllsize$varname<-rep("dwllsize",nrow(p_dwllsize))

# 18. dwlltype
telecom%>%count(churn,levels=dwlltype)%>%filter(churn==1)->p_dwlltype
p_dwlltype$N<-unclass(telecom%>%filter(dwlltype%in%p_dwlltype$levels)%>%count(dwlltype))[[2]]
p_dwlltype$churn_percent<-p_dwlltype$n/p_dwlltype$N
p_dwlltype$varname<-rep("dwlltype",nrow(p_dwlltype))


# Exporting datasets to excel for further analysis
library(xlsx)

filename<-"Variable Profiles.xls"

# Creating a xlsx workbook
wb<-createWorkbook(type="xls")

# Creating worksheets
sheet1<-createSheet(wb,sheetName="Continuous Variables")
sheet2<-createSheet(wb,sheetName="Categorical Variables")

# Exporting the dataframes to the specified worksheet

# 1. Continuous Variables
addDataFrame(as.data.frame(p_mouMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =3)
addDataFrame(as.data.frame(p_totmrcMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =15)
addDataFrame(as.data.frame(p_revRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =27)
addDataFrame(as.data.frame(p_mouRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =39)
addDataFrame(as.data.frame(p_changemou),sheet1,col.names=TRUE,row.names = FALSE,startRow =51)
addDataFrame(as.data.frame(p_dropblkMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =62)
addDataFrame(as.data.frame(p_dropvceRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =74)
addDataFrame(as.data.frame(p_owylisvceRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =86)
addDataFrame(as.data.frame(p_mouopkvRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =98)
addDataFrame(as.data.frame(p_months),sheet1,col.names=TRUE,row.names = FALSE,startRow =110)
addDataFrame(as.data.frame(p_totcalls),sheet1,col.names=TRUE,row.names = FALSE,startRow =122)
addDataFrame(as.data.frame(p_eqpdays),sheet1,col.names=TRUE,row.names = FALSE,startRow =134)
addDataFrame(as.data.frame(p_custcareMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =146)
addDataFrame(as.data.frame(p_callwaitMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =151)
addDataFrame(as.data.frame(p_iwylisvceMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =157)
addDataFrame(as.data.frame(p_callwaitRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =165)
addDataFrame(as.data.frame(p_ccrndmouRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =170)
addDataFrame(as.data.frame(p_adjqty),sheet1,col.names=TRUE,row.names = FALSE,startRow =175)
addDataFrame(as.data.frame(p_ovrrevMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =187)
addDataFrame(as.data.frame(p_revMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =193)
addDataFrame(as.data.frame(p_ovrmouMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =205)
addDataFrame(as.data.frame(p_compvceMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =211)
addDataFrame(as.data.frame(p_plcdvceMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =223)
addDataFrame(as.data.frame(p_avg3mou),sheet1,col.names=TRUE,row.names = FALSE,startRow =235)
addDataFrame(as.data.frame(p_avgmou),sheet1,col.names=TRUE,row.names = FALSE,startRow =247)
addDataFrame(as.data.frame(p_avg3qty),sheet1,col.names=TRUE,row.names = FALSE,startRow =259)
addDataFrame(as.data.frame(p_avgqty),sheet1,col.names=TRUE,row.names = FALSE,startRow =271)
addDataFrame(as.data.frame(p_avg6mou),sheet1,col.names=TRUE,row.names = FALSE,startRow =283)
addDataFrame(as.data.frame(p_avg6qty),sheet1,col.names=TRUE,row.names = FALSE,startRow =297)
addDataFrame(as.data.frame(p_opkdatMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =311)
addDataFrame(as.data.frame(p_recvsmsMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =315)
addDataFrame(as.data.frame(p_roamMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =319)
addDataFrame(as.data.frame(p_blckdatMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =323)
addDataFrame(as.data.frame(p_moupeadMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =327)
addDataFrame(as.data.frame(p_daMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =331)
addDataFrame(as.data.frame(p_daRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =337)
addDataFrame(as.data.frame(p_datovrMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =343)
addDataFrame(as.data.frame(p_datovrRange),sheet1,col.names=TRUE,row.names = FALSE,startRow =347)
addDataFrame(as.data.frame(p_dropdatMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =351)
addDataFrame(as.data.frame(p_dropvceMean),sheet1,col.names=TRUE,row.names = FALSE,startRow =355)
addDataFrame(as.data.frame(p_adjmou),sheet1,col.names=TRUE,row.names = FALSE,startRow =367)
addDataFrame(as.data.frame(p_totrev),sheet1,col.names=TRUE,row.names = FALSE,startRow =379)
addDataFrame(as.data.frame(p_adjrev),sheet1,col.names=TRUE,row.names = FALSE,startRow =391)
addDataFrame(as.data.frame(p_avgrev),sheet1,col.names=TRUE,row.names = FALSE,startRow =403)
addDataFrame(as.data.frame(p_hndprice),sheet1,col.names=TRUE,row.names = FALSE,startRow =415)
addDataFrame(as.data.frame(p_income),sheet1,col.names=TRUE,row.names = FALSE,startRow =423)


# 2.Categorical Variables
addDataFrame(as.data.frame(p_actvsubs),sheet2,col.names=TRUE,row.names = FALSE,startRow =3)
addDataFrame(as.data.frame(p_age1),sheet2,col.names=TRUE,row.names = FALSE,startRow =12)
addDataFrame(as.data.frame(p_age2),sheet2,col.names=TRUE,row.names = FALSE,startRow =54)
addDataFrame(as.data.frame(p_area),sheet2,col.names=TRUE,row.names = FALSE,startRow =99)
addDataFrame(as.data.frame(p_aslflag),sheet2,col.names=TRUE,row.names = FALSE,startRow =121)
addDataFrame(as.data.frame(p_carbuy),sheet2,col.names=TRUE,row.names = FALSE,startRow =125)
addDataFrame(as.data.frame(p_crclscod),sheet2,col.names=TRUE,row.names = FALSE,startRow =129)
addDataFrame(as.data.frame(p_csa),sheet2,col.names=TRUE,row.names = FALSE,startRow =175)
addDataFrame(as.data.frame(p_ethnic),sheet2,col.names=TRUE,row.names = FALSE,startRow =791)
addDataFrame(as.data.frame(p_forgntvl),sheet2,col.names=TRUE,row.names = FALSE,startRow =810)
addDataFrame(as.data.frame(p_hndwebcap),sheet2,col.names=TRUE,row.names = FALSE,startRow =814)
addDataFrame(as.data.frame(p_marital),sheet2,col.names=TRUE,row.names = FALSE,startRow =820)
addDataFrame(as.data.frame(p_mtrcycle),sheet2,col.names=TRUE,row.names = FALSE,startRow =827)
addDataFrame(as.data.frame(p_models),sheet2,col.names=TRUE,row.names = FALSE,startRow =831)
addDataFrame(as.data.frame(p_prizmsocialone),sheet2,col.names=TRUE,row.names = FALSE,startRow =844)
addDataFrame(as.data.frame(p_refurbnew),sheet2,col.names=TRUE,row.names = FALSE,startRow =852)
addDataFrame(as.data.frame(p_truck),sheet2,col.names=TRUE,row.names = FALSE,startRow =856)
addDataFrame(as.data.frame(p_uniqsubs),sheet2,col.names=TRUE,row.names = FALSE,startRow =860)
addDataFrame(as.data.frame(p_dwllsize),sheet2,col.names=TRUE,row.names = FALSE,startRow =874)
addDataFrame(as.data.frame(p_dwlltype),sheet2,col.names=TRUE,row.names = FALSE,startRow =892)

# Saving the workbook 
saveWorkbook(wb,filename)


### Data Preparation

## Creating a new dataset containing only required variables after variable profiling
telecom1<-telecom[,-c(15,16,20,22,58)]

# Dummy variables are created to group deciles (3 or more) having similar event rate

## Creating dummy variables for continuous variables

# 1. mou_Mean
telecom1$mou_Mean.d<-ifelse(telecom1$mou_Mean>=122.00 & telecom1$mou_Mean<=1590.75,0,1)

# 2. rev_Range
telecom1$rev_Range.d<-ifelse(telecom1$rev_Range>=0.00000 & telecom1$rev_Range<28.20000,0,1)

# 3. mou_Range
telecom1$mou_Range.d<-ifelse(telecom1$mou_Range>=50.0000 & telecom1$mou_Range<313.0000 |
                                      telecom1$mou_Range>=372.8499 & telecom1$mou_Range<=1171.0000,0,1)

# 4. change_mou
telecom1$change_mou.d<-ifelse(telecom1$change_mou>=-110.25000 & telecom1$change_mou<=-25.25000 |
                                      telecom1$change_mou>=-9.24082 & telecom1$change_mou<346.25000,0,1)

# 5. drop_blk_Mean
telecom1$drop_blk_Mean.d<-ifelse(telecom1$drop_blk_Mean>=1.3333333 & telecom1$drop_blk_Mean<7.666667 |
                                      telecom1$drop_blk_Mean>=10.122020 & telecom1$drop_blk_Mean<35.000000,0,1)

# 6. mou_opkv_Range
telecom1$mou_opkv_Range.d<-ifelse(telecom1$mou_opkv_Range>=22.26 & telecom1$mou_opkv_Range<=438.49,0,1)

# 7. adjqty
telecom1$adjqty.d<-ifelse(telecom1$adjqty>=0.00 & telecom1$adjqty<1017.00 |
                                      telecom1$adjqty>=2900.38 & telecom1$adjqty<3498.00 |
                                      telecom1$adjqty>=4944.00 & telecom1$adjqty<=8861.00,0,1)

# 8. rev_Mean
telecom1$rev_Mean.d<-ifelse(telecom1$rev_Mean>=31.08500 & telecom1$rev_Mean<=135.77500,0,1)

# 9. comp_vce_Mean
telecom1$comp_vce_Mean.d<-ifelse(telecom1$comp_vce_Mean>=22.00000 & telecom1$comp_vce_Mean<=338.00000,0,1)

# 10. plcd_vce_Mean
telecom1$plcd_vce_Mean.d<-ifelse(telecom1$plcd_vce_Mean>=29.33333 & telecom1$plcd_vce_Mean<=452.00000,0,1)

# 11. avg3mou
telecom1$avg3mou.d<-ifelse(telecom1$avg3mou>=54 & telecom1$avg3mou<1611,0,1)

# 12. avgmou
telecom1$avgmou.d<-ifelse(telecom1$avgmou>=455.87 & telecom1$avgmou<514.10 |
                                      telecom1$avgmou>=657.00 & telecom1$avgmou<1374.50,0,1)

# 13. avg3qty
telecom1$avg3qty.d<-ifelse(telecom1$avg3qty>=21 & telecom1$avg3qty<=548,0,1)

# 14. avg6mou
telecom1$avg6mou.d<-ifelse(is.na(telecom1$avg6mou)==T,NA,ifelse(telecom1$avg6mou>=120.0000 & telecom1$avg6mou<184.0000 |
                                      telecom1$avg6mou>=253.0000 & telecom1$avg6mou<415.0000 |
                                      telecom1$avg6mou>=520.2849 & telecom1$avg6mou<1530.0000,0,1))

# 15. avg6qty
telecom1$avg6qty.d<-ifelse(is.na(telecom1$avg6qty)==T,NA,ifelse(telecom1$avg6qty>=22 & telecom1$avg6qty<=528,0,1))

# 16. drop_vce_Mean
telecom1$drop_vce_Mean.d<-ifelse(telecom1$drop_vce_Mean>=0.0000000 & telecom1$drop_vce_Mean<4.3333333 |
                                      telecom1$drop_vce_Mean>=6.0233223 & telecom1$drop_vce_Mean<=22.0000000,0,1)

# 17. adjmou
telecom1$adjmou.d<-ifelse(telecom1$adjmou>=0.000 & telecom1$adjmou<2908.000 |
                                      telecom1$adjmou>=7706.157 & telecom1$adjmou<9782.000 |
                                      telecom1$adjmou>=13569.00 & telecom1$adjmou<22656.00,0,1)

# 18. adjrev
telecom1$adjrev.d<-ifelse(telecom1$adjrev>=7.15 & telecom1$adjrev<397.83,0,1)

# 19. avgrev
telecom1$avgrev.d<-ifelse(telecom1$avgrev>=30.04 & telecom1$avgrev<42.60 |
                                      telecom1$avgrev>=69.59 & telecom1$avgrev<=123.53,0,1)

# 20. avgqty
telecom1$avgqty.d<-ifelse(telecom1$avgqty>=0.00 & telecom1$avgqty<29.32 |
                            telecom1$avgqty>=23.06 & telecom1$avgqty<=495.25,0,1)


## Creating dummy variables for categorical variables

# 1. actvsubs
telecom1$actvsubs.d<-ifelse(telecom1$actvsubs>=0 & telecom1$actvsubs<=3,0,1)

# 2. age1
telecom1$age1.d<-ifelse(telecom1$age1<=40,0,ifelse(telecom1$age1>40 & telecom1$age1<=66,0,1))

# 3. age2
telecom1$age2.d<-ifelse(telecom1$age2<=42,0,ifelse(telecom1$age2>42 & telecom1$age2<=70,0,1))

# 4. car_buy
telecom1$car_buy.d<-ifelse(telecom1$car_buy=="New",1,0)

# 5. crclscod

# Based on similar event rate, the observations are coded as follows

telecom1$crclscod.d<-ifelse(telecom1$crclscod=="A" | telecom1$crclscod=="AA" | telecom1$crclscod=="B"
                            & telecom1$crclscod=="BA" | telecom1$crclscod=="EM" | telecom1$crclscod=="G"
                            & telecom1$crclscod=="GA" | telecom1$crclscod=="GY" | telecom1$crclscod=="JF"
                            & telecom1$crclscod=="M" | telecom1$crclscod=="Z" | telecom1$crclscod=="Z1"
                            & telecom1$crclscod=="Z2" | telecom1$crclscod=="ZA" | telecom1$crclscod=="ZY",
                            0,ifelse(telecom1$crclscod=="A2" | telecom1$crclscod=="A3" | telecom1$crclscod=="TP"
                                        & telecom1$crclscod=="J",1,2))

# 6. csa

# Imputing missing values
#The missing values have a churn rate of 17.64% which is equal to that of 'SHEEDI540'(17.64%).
# Therefore, SHEDDI540 should be imputed to the missing values.

telecom1$csa[is.na(telecom1$csa)]<-"SHEEDI540"

telecom1$csa_nlevels<-as.numeric(factor(telecom1$csa,labels=c(1:759)))

telecom1$csa.d<-ifelse(telecom1$csa_nlevels>=0 & telecom1$csa_nlevels<253,0,
                       ifelse(telecom1$csa_nlevels>=253 & telecom1$csa_nlevels<506,1,2))


# 7. ethnic
# Based on similar event rate, the observations are coded
telecom1$ethnic.d<-ifelse(telecom1$ethnic=="C" | telecom1$ethnic=="P" | telecom1$ethnic=="X"
                            | telecom1$ethnic=="Z",0,
                            ifelse(telecom1$ethnic=="F" | telecom1$ethnic=="G" | telecom1$ethnic=="I"
                                     | telecom1$ethnic=="J" | telecom1$ethnic=="M" | telecom1$ethnic=="N"
                                     | telecom1$ethnic=="R" | telecom1$ethnic=="S",1,2))


# 8. models
# Based on similar event rate, the observations are coded
telecom1$models.d<-ifelse(telecom1$models==3 | telecom1$models==5 | telecom1$models==6
                          & telecom1$models==7 | telecom1$models==9,0,
                          ifelse(telecom1$models==1 | telecom1$models==2 | telecom1$models==4
                                 & telecom1$models==8 | telecom1$models==10 | telecom1$models=="S",0,1))

# 9. refurb_new
telecom1$refurb_new.d<-ifelse(telecom1$refurb_new=="N",1,0)

# 10. uniqsubs
telecom1$uniqsubs.d<-ifelse(telecom1$uniqsubs>=1 & telecom1$uniqsubs<=6,0,1)

# 11. marital
telecom1$marital.d<-ifelse(telecom1$marital=="M" | telecom1$marital=="S",0,1)

# 12. area

# Imputing missing values
#The missing values have a churn rate of 17.6% which is closest to that of 'TENNESSEE AREA'(20.9%).
# Therefore, TENNESSEE AREA should be imputed to the missing values.
telecom1$area[is.na(telecom1$area)]<-"TENNESSEE AREA"

# Based on similar event rate, the observations are coded in the dummy variable
telecom1$area.d<-ifelse(telecom1$area=="ATLANTIC SOUTH AREA" | telecom1$area=="CENTRAL/SOUTH TEXAS AREA"
                        | telecom1$area=="DC/MARYLAND/VIRGINIA AREA" | telecom1$area=="GREAT LAKES AREA"
                        | telecom1$area=="HOUSTON AREA" | telecom1$area=="MIDWEST AREA"
                        | telecom1$area=="OHIO AREA" | telecom1$area=="TENNESSEE AREA",0,
                        ifelse(telecom1$area=="CALFORNIA NORTH AREA" | telecom1$area=="CHICAGO AREA"
                               | telecom1$area=="DALLAS AREA" | telecom1$area=="LOS ANGELES AREA"
                               | telecom1$area=="NEW ENGLAND AREA" | telecom1$area=="NEW YORK CITY AREA"
                               | telecom1$area=="NORTH FLORIDA AREA" | telecom1$area=="PHILADELPHIA AREA"
                               | telecom1$area=="SOUTHWEST AREA",1,2))

# 13. asl_flag
telecom1$asl_flag.d<-ifelse(telecom1$asl_flag=="N",0,1)

# 14. prizm_social_one 
# Imputing missing values
#The missing values have a churn rate of 25.07% which is closest to that of 'T'(25.26%).
# Therefore, 'T' should be imputed to the missing values.
telecom1$prizm_social_one[is.na(telecom1$prizm_social_one)]<-"T"

telecom1$prizm_social_one.d<-ifelse(telecom1$prizm_social_one=="C" | telecom1$prizm_social_one=="S" |
                                    telecom1$prizm_social_one=="U",0,1)

# 15. hnd_webcap 
# Imputing missing values
#The missing values have a churn rate of 29.3% which is closest to that of 'WC'(31.3%).
# Therefore, 'WC' should be imputed to the missing values.
telecom1$hnd_webcap[is.na(telecom1$hnd_webcap)]<-"WC"

telecom1$hnd_webcap.d<-ifelse(telecom1$hnd_webcap=="UNKW" | telecom1$hnd_webcap=="WCMB",0,1)

# 16. dwllsize

# Imputing missing values
#The missing values have a churn rate of 25.2% which is closest to that of 'J'(25.3%).
# Therefore,'J' should be imputed to the missing values.
telecom1$dwllsize[is.na(telecom1$dwllsize)]<-"J"

# Based on similar event rate, the observations are coded in the dummy variable
telecom1$dwllsize.d<-ifelse(telecom1$dwllsize=="A" | telecom1$dwllsize=="B"
                        | telecom1$dwllsize=="C" | telecom1$dwllsize=="E"
                        | telecom1$dwllsize=="K" | telecom1$dwllsize=="L"
                        | telecom1$dwllsize=="N",0,1)

# 15. dwlltype 
# Imputing missing values
#The missing values have a churn rate of 25.4% which is closest to that of 'M'(24.3%).
# Therefore, 'M' should be imputed to the missing values.
telecom1$dwlltype[is.na(telecom1$dwlltype)]<-"M"

telecom1$dwlltype.d<-ifelse(telecom1$dwlltype=="S",0,1)

## Creating derived variables

 # 1. Percentage of Calls Completed
telecom1$completed_percent<-(telecom1$comp_vce_Mean/telecom1$plcd_vce_Mean)*100  

# Missing values are formed when comp_vce_Mean and plcd_vce_Mean are 0.
telecom1$completed_percent[is.na(telecom1$completed_percent)]<-0

# 2. Number of failed data calls
telecom1$failed_dat_Mean<-(telecom1$drop_dat_Mean + telecom1$blck_dat_Mean)

# 3. Percentage of overage revenue
# Total Overage revenue = Mean of overage * no. of months
telecom1$overage_percent<-(telecom$ovrrev_Mean*telecom$months)/telecom$totrev*100
summary(telecom1)

## Cleaning the dataset 
# Removing unnecessary variables and retaining required variables
data<-telecom1[,-c(1,3,4,5,6,9,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,39,40,42,43,52,53,58,59,61,62,90)]

# Ignoring missing values
data1<-na.omit(data)

### Data Partitioning into training and test data sets
set.seed(100)
index<-sample(1:nrow(data1),0.70*nrow(data1),replace=F)

# Training data set
training<-data1[index,]

# Test data set
test<-data1[-index,]

table(training$churn)/nrow(training)
table(training$churn)/nrow(training)

### Regression Model
model1<-glm(churn~.,data=training[,-c(26)],family="binomial")
summary(model1)

step(model1,direction="both",steps=1000,k=2)

model2<- glm(formula = churn ~ totmrc_Mean + drop_vce_Range + months + 
               totcalls + eqpdays + custcare_Mean + hnd_price + mou_Mean.d + 
               rev_Range.d + change_mou.d + mou_opkv_Range.d + adjqty.d + 
               avg3mou.d + avg3qty.d + avg6mou.d + avg6qty.d + drop_vce_Mean.d + 
               adjmou.d + adjrev.d + avgrev.d + actvsubs.d + age1.d + crclscod.d + 
               ethnic.d + refurb_new.d + uniqsubs.d + marital.d + area.d + 
               asl_flag.d + prizm_social_one.d + hnd_webcap.d + dwlltype.d + 
               completed_percent + overage_percent, family = "binomial", 
             data = training[, -c(26)])
summary(model2)

model3<- glm(formula = churn ~ totmrc_Mean + drop_vce_Range + months + 
               totcalls + eqpdays + custcare_Mean + hnd_price + mou_Mean.d + 
               rev_Range.d + change_mou.d + adjqty.d + 
               avg3mou.d + avg3qty.d + avg6mou.d + avg6qty.d + drop_vce_Mean.d + 
               adjmou.d + adjrev.d + avgrev.d + age1.d + crclscod.d + 
               ethnic.d + refurb_new.d + uniqsubs.d + marital.d + area.d + 
               asl_flag.d + prizm_social_one.d + hnd_webcap.d + dwlltype.d + 
               completed_percent + overage_percent, family = "binomial", 
             data = training[, -c(26)])
summary(model3)

model4<- glm(formula = churn ~ totmrc_Mean + drop_vce_Range + months + 
               totcalls + eqpdays + custcare_Mean + hnd_price + mou_Mean.d + 
               rev_Range.d + change_mou.d + adjqty.d + totrev + 
               avg3mou.d + avg3qty.d + avg6mou.d + avg6qty.d + drop_vce_Mean.d + 
               adjmou.d + adjrev.d + avgrev.d + age1.d + crclscod.d + 
               ethnic.d + refurb_new.d + uniqsubs.d + marital.d + area.d + 
               asl_flag.d + prizm_social_one.d + hnd_webcap.d + dwlltype.d + 
               completed_percent + overage_percent, family = "binomial", 
             data = training[, -c(26)])
summary(model4)


## Final Regression Model

model4<- glm(formula = churn ~ totmrc_Mean + drop_vce_Range + months + 
               totcalls + eqpdays + custcare_Mean + hnd_price + mou_Mean.d + 
               rev_Range.d + change_mou.d + adjqty.d + totrev + 
               avg3mou.d + avg3qty.d + avg6mou.d + avg6qty.d + drop_vce_Mean.d + 
               adjmou.d + adjrev.d + avgrev.d + age1.d + crclscod.d + 
               ethnic.d + refurb_new.d + uniqsubs.d + marital.d + area.d + 
               asl_flag.d + prizm_social_one.d + hnd_webcap.d + dwlltype.d + 
               completed_percent + overage_percent, family = "binomial", 
             data = training[, -c(26)])
summary(model4)



exp(model4$coefficients)
confint(model4)
exp(confint(model4))

# To assess variable importance
varImp(model4)

### Model Validation
actual<-test$churn

predicted<-predict(model4,type="response",newdata=test)
head(predicted)

# To set the cutoff for probabilities of getting 1
predicted1<-ifelse(predicted>0.5,1,0)



## Model Evaluation Metrics

# 1. Confusion Matrix
confusionMatrix(predicted1,actual,positive="1")


# 2. Gains 
gains<-gains(actual,predict(model4,type="response",newdata=test),groups=10)
print.gains(gains)
plot.gains(gains)


# 3. Area under the ROC curve
# ROC Curve is Receiver Operating Characteristic curve
library(ROCR)
ROCRpred<-prediction(actual,predicted1)

# To determine the number of true positives and false positives
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,col="red",main = "ROC Curve")
abline(0,1,lty=8,col="grey")


# Area under the ROC curve
auc<-performance(ROCRpred,"auc")
auc@y.values
unlist(auc@y.values)


# 4. Multicollinearity
library(car)
vif(model4)



### Creating customer segments
#Finding the cut points of probability scores based on decile groups
quantile(predicted,1:10/10)


# Creating decile bins for totrev
totrev_decile<-test%>%mutate(Decile=ntile(totrev,10))%>%group_by(Decile)%>%
  summarise('<='=min(totrev),'>'=max(totrev),Sum=sum(totrev),
            Average=mean(totrev),Num_Obs=n())%>%ungroup()%>%
  mutate(Rev_Contribution=(Sum/sum(Sum))*100)%>%mutate(Cumulative=cumsum(Rev_Contribution))

# Exporting the dataset into a xls file for further analysis
library(xlsx)

filename2<-"Customer Segments.xls"
wb1<-createWorkbook(type="xls")
sheet<-createSheet(wb1,sheetName="Categories")
addDataFrame(as.data.frame(totrev_decile),sheet,col.names=TRUE,row.names = FALSE,startRow =4)
saveWorkbook(wb1,filename2)
