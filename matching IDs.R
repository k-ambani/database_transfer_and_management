setwd("~/R/Tasks for other people/Elliot Brooker Data")
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)

rm(list=ls())
COMISA_EB <- data.frame(fread("COMISA_Age_Sex_BMI.csv"))
REDCap_exp <- data.frame(fread('E. Brooker REDCap latest export for R studio matching.csv')) #all IDs in this csv should meet the criteria to be matched with COMISA

COMISA_ID <- COMISA_EB$COMISA_ID
COMISA_SEX <- COMISA_EB$Sex
COMISA_AGE <- COMISA_EB$Age
COMISA_BMI <-COMISA_EB$BMI

REDCap_IDS <- REDCap_exp$record_id
REDCap_sex <- REDCap_exp$gender
REDCap_AGE <- REDCap_exp$age
REDCap_BMI <- REDCap_exp$osa_bmi
REDCap_AHI <- REDCap_exp$osa_ahi

REDCap_AHI_exists <- REDCap_IDS[!REDCap_AHI%in%NA]
REDCap_BMI_not_exists <- REDCap_IDS[REDCap_BMI%in%NA]
age_lower<- COMISA_AGE-5
age_upper<- COMISA_AGE+5
bmi_lower<- COMISA_BMI-5
bmi_upper<- COMISA_BMI+5
matched_ID_comment <- NULL
matched_ID_rec <- NULL

for (t in 1:length(COMISA_ID)){

REDCap_sex_match_list <- REDCap_IDS[REDCap_sex%in%COMISA_SEX[t]] #REDCap IDs that has match COMISA ppt sex
REDCap_age_match_list  <- REDCap_IDS[REDCap_AGE>=age_lower[t] & REDCap_AGE <= age_upper[t]] #REDCap IDs that match COMISA ppt age
REDCap_bmi_match_list  <- REDCap_IDS[REDCap_BMI>=bmi_lower[t] & REDCap_BMI <= bmi_upper[t]] #REDCap IDs that match COMISA ppt bmi


REDCap_sex_age_matched<- intersect(REDCap_sex_match_list, REDCap_age_match_list)
REDCap_sex_age_AHI_matched<- intersect(REDCap_sex_age_matched, REDCap_AHI_exists)
REDCap_sex_age_bmi_matched<- intersect(REDCap_sex_age_matched, REDCap_bmi_match_list)
REDCap_sex_age_bmi_AHI_matched <- intersect(REDCap_sex_age_bmi_matched, REDCap_AHI_exists)
      if (!(is_empty(REDCap_sex_age_bmi_AHI_matched))){
       matched_ID_rec[t] <- toString(as.character(REDCap_sex_age_bmi_AHI_matched))
       matched_ID_comment[t]<- toString( "sex, age, BMI matched, AHI>10" )
      }else if (!(is_empty(REDCap_sex_age_matched))){
        REDCap_sex_age_AHI_matched <- intersect(REDCap_sex_age_matched,REDCap_BMI_not_exists)
        matched_ID_rec[t] <- toString(as.character(REDCap_sex_age_AHI_matched))
        matched_ID_comment[t]<- toString( "sex & age matched, BMI info not available, AHI info maybe available" )
      }else{
        matched_ID_rec[t] <- toString(as.character(""))
        matched_ID_comment[t]<- toString( "sex matched, no age match" )  
        
      }
      

} #end for loop

comb_match <- data.frame(COMISA_ID,COMISA_SEX,COMISA_AGE,COMISA_BMI,matched_ID_rec,matched_ID_comment)

write.csv(comb_match, file = 'matched4.csv',row.names = FALSE)
      

