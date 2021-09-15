setwd("~/R/Coreplus exports")

library(data.table)
library(tidyverse)
library(stringr)


CRN<-fread("HSC_CRN COREPLUS DATA 20210719_k.csv")
DETAILS<-fread("HSC_DETAILS COREPLUS DATA 20210719_k.csv")
REDCap_exp <- fread("REDCap_export.csv")

## Cleaning CRN ----

CRN<-CRN[!is.na(CRN$clientID)]

CRN_unique_IDs<-unique(CRN$clientID)
CRN_ind<-1:length(CRN$clientID)

CRN_uni<-data.frame(matrix(nrow =length(CRN_ind),ncol = length(names(CRN))))
colnames(CRN_uni)<-names(CRN)

for (c in 1:length(CRN_unique_IDs)){
uni_ind<-min(CRN_ind[CRN$clientID%in%CRN_unique_IDs[c]])

CRN_uni[c,]<-CRN[uni_ind,]

}

a<-names(CRN_uni)
CRNnames<-a[c(1,2,3,11,12)]
CRN_uni_new<-CRN_uni[!is.na(CRN_uni$clientID),c(CRNnames)]

join_id <- full_join(CRN_uni_new, DETAILS, by = c("clientID"="clientID"))

join_id$appointmentTitle <- str_trim(str_remove_all(join_id$appointmentTitle,"[0123456789/]"))
CRN_clean <- tibble(select(join_id, c(clientID, appointmentTitle,DOB,email)))

write.csv(CRN_clean,'joined_dfs.csv',row.names = FALSE)

## Cleaning REDCap ---- 


c<-REDCap_exp %>% separate(fullname,c("First_Initial","Last_name"),fill = "right",extra = "merge")
  
c$Last_name[is.na(c$Last_name)]<-c$First_Initial[is.na(c$Last_name)]

c$Last_name<-toupper(c$Last_name)

c$Name<-paste(c$Last_name,c$fname, sep = " ")
REDCap_clean<-tibble(select(c,c(record_id,Name,dob,email)))

write.csv(REDCap_clean,'REDcap_clean.csv',row.names = FALSE)

## JOINING! ----


matching <- stringdist_left_join(REDCap_clean,CRN_clean,by = c(Name = "appointmentTitle"),distance_col = "distance",max_dist = 0.2,method = "jw")
write.csv(matching,'match.csv')


