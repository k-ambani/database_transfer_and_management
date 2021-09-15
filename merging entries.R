setwd("~/R/Merging Databases/11. Sleep diaries/4. Merging UA and BB entries")
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)

rm(list=ls())
#loading data ----
qual_diaries <- data.frame(fread("REDCap upload file.csv"))
#saving useful vars
#entries <- as.numeric(dim(qual_diaries)[1])
unique_IDs <- unique(qual_diaries$record_id)
rec_IND<-17
#unique_IDs[no_entries == 1]
unique_ID_chosen<- unique_IDs[rec_IND]
#print(paste("rec_ID",unique_ID_chosen))

#subsetting and ordering subdiaries ----
sub_diaries <- qual_diaries[qual_diaries$record_id==unique_ID_chosen,]
no_entries <- dim(sub_diaries)[1]
#print(paste("total number of entries",no_entries))

timestamp_start_end <- paste(sub_diaries$startdate_qltrcs,'-',sub_diaries$enddate_qltrcs)
sub_diaries<-data.frame(sub_diaries,timestamp_start_end)

names(sub_diaries)
UA_sub_diaries<-sub_diaries[sub_diaries$time_of_record_qltrcs%in%1,]
BB_sub_diaries<-sub_diaries[sub_diaries$time_of_record_qltrcs%in%4,]

# Number of entries in UA and BB 
UA_entries <- as.numeric(dim(UA_sub_diaries)[1])
BB_entries <- as.numeric(dim(BB_sub_diaries)[1])
  
#print(paste("number of UA entries",UA_entries))
#print(paste("number of BB entries",BB_entries))
  
UA_sub_di_ts_1 <-as.POSIXct(UA_sub_diaries$startdate_qltrcs, tz= "", format = "%d/%m/%Y %H:%M")
BB_sub_di_ts_1 <-as.POSIXct(BB_sub_diaries$startdate_qltrcs, tz= "", format = "%d/%m/%Y %H:%M")

UA_sub_diaries<-UA_sub_diaries[order(UA_sub_di_ts_1),]
BB_sub_diaries<-BB_sub_diaries[order(BB_sub_di_ts_1),]
  
UA_sub_di_ts <-as.POSIXct(UA_sub_diaries$startdate_qltrcs, tz= "", format = "%d/%m/%Y %H:%M")
BB_sub_di_ts <-as.POSIXct(BB_sub_diaries$startdate_qltrcs, tz= "", format = "%d/%m/%Y %H:%M")
  
  #checking if UA entries are too close together
  # Checking for doubles, etc----
UA_diff <- as.numeric(difftime(UA_sub_di_ts[2:UA_entries],UA_sub_di_ts[1:UA_entries-1],units = 'hours'))
BB_diff <- as.numeric(difftime(BB_sub_di_ts[2:BB_entries],BB_sub_di_ts[1:BB_entries-1],units = 'hours'))
  
time_min_threshold <- 15 #hours
  
weird_UA_inds1<-UA_diff<time_min_threshold
weird_UA_inds2<-c(FALSE,weird_UA_inds1[1:length(weird_UA_inds1)-1])
weird_UA_ts1<-UA_sub_di_ts[weird_UA_inds1] 
weird_UA_ts2<-UA_sub_di_ts[weird_UA_inds2] 
  
#print(weird_UA_ts1)
#print(weird_UA_ts2)

#checking if BB entries are too close together
weird_BB_inds1<-BB_diff<time_min_threshold
weird_BB_inds2<-c(FALSE,weird_BB_inds1[1:length(weird_BB_inds1)-1])
weird_BB_ts1<-BB_sub_di_ts[weird_BB_inds1] 
weird_BB_ts2<-BB_sub_di_ts[weird_BB_inds2] 
  
#print(weird_BB_ts1)
#print(weird_BB_ts2)
  
# initialising 
BB_ind <- rep(NaN,length(UA_sub_di_ts))
UA_ind <- 1:length(UA_sub_di_ts)
  
# matching code ---- 
for(t in 1:length(UA_ind)){
  BB_b4_UA_ts<-BB_sub_di_ts[UA_sub_di_ts[t]>BB_sub_di_ts] #save all BB before the UA in question
  time_diff<-min(abs(as.numeric(difftime(UA_sub_di_ts[t],BB_b4_UA_ts,units = c('hours')))))
  print(time_diff)#saving the time diff between the chosen BB and the UA
    if(TRUE%in%c(UA_sub_di_ts[t]>BB_sub_di_ts)){
      if(as.numeric(time_diff)<24){
          BB_ind_temp<-which.min(abs(UA_sub_di_ts[t]-BB_b4_UA_ts))
              if(!BB_ind_temp%in%BB_ind){BB_ind[t] <-BB_ind_temp}else{UA_ind[t]<-NaN}
                              }else{UA_ind[t]<-NaN}   
                                                }else{UA_ind[t]<-NaN}
                            }  #for loop end
      
#print(UA_ind)
#print(BB_ind) 
#saving unpaired UA and BB entries
UA_sub_diaries_unpair<-UA_sub_diaries[c(1:UA_entries)[!1:UA_entries%in%UA_ind],]
names(UA_sub_diaries_unpair)[names(UA_sub_diaries_unpair)=="timestamp_start_end"] <- "timestamp_ua"
names(UA_sub_diaries_unpair)

BB_sub_diaries_unpair<-BB_sub_diaries[c(1:BB_entries)[!1:BB_entries%in%BB_ind],]
names(BB_sub_diaries_unpair)[names(BB_sub_diaries_unpair)=="timestamp_start_end"] <- "timestamp_bb"
names(BB_sub_diaries_unpair)

#Getting field names that are just UA and BB 
UA_fields<-names(UA_sub_diaries[c(1:25,length(sub_diaries)-1,length(sub_diaries))])
BB_fields<-names(BB_sub_diaries[c(26:length(sub_diaries))])

#print(UA_fields)
#print(BB_fields)

#Subsetting fields that are just UA
UA_sub_diaries<-UA_sub_diaries[UA_fields]
names(UA_sub_diaries)[names(UA_sub_diaries)=="timestamp_start_end"] <- "timestamp_ua"
#Subsetting fields that are just BB 
BB_sub_diaries<-BB_sub_diaries[BB_fields]
names(BB_sub_diaries)[names(BB_sub_diaries)=="timestamp_start_end"] <- "timestamp_bb"

#Subsetting diaries by those are paired
UA_sub_diaries_pair<-UA_sub_diaries[UA_ind[!is.nan(UA_ind)],]
BB_sub_diaries_pair<-BB_sub_diaries[BB_ind[!is.nan(BB_ind)],]

UA_BB_di<-data.frame(UA_sub_diaries_pair,BB_sub_diaries_pair)
UA_BB_di<-subset(UA_BB_di,select = -c(diary_note,diary_note.1))
#Making diary notes field
diary_note<-paste(BB_sub_diaries_pair$diary_note,UA_sub_diaries_pair$diary_note)
UA_BB_di<-data.frame(UA_BB_di,diary_note)

UA_BB_di<-bind_rows(UA_BB_di,UA_sub_diaries_unpair)
UA_BB_di<-bind_rows(UA_BB_di,BB_sub_diaries_unpair)
UA_BB_di<-UA_BB_di[rowSums(is.na(UA_BB_di)) != ncol(UA_BB_di), ]       # Remove rows with only NAs

##timestamp, needs to be as.posixct for ordering to work
UA_BB_ts <-as.POSIXct(UA_BB_di$startdate_qltrcs, tz= "", format = "%d/%m/%Y %H:%M") 
#ordering UA_BB_di entries so that repeat instance is in order
UA_BB_di<-UA_BB_di[order(UA_BB_ts),]

UA_BB_di$redcap_repeat_instrument<- rep('sleep_diary',dim(UA_BB_di)[1])
UA_BB_di$redcap_repeat_instance <- 1:dim(UA_BB_di)[1]

#remove 
UA_BB_di<-subset(UA_BB_di,select = -c(startdate_qltrcs,time_of_record_qltrcs,enddate_qltrcs))
print(data.frame(UA_BB_di$timestamp_bb,UA_BB_di$timestamp_ua))

filename<-paste('ind',rec_IND,'recID',unique_ID_chosen, 'merged.csv')
write.csv(UA_BB_di,filename,na = "",row.names = FALSE)

