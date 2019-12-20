
packages_needed <- c("randomForest","DMwR","AUC","sqldf","readr","dummies","data.table","glmnet",
                     "reshape","reshape2","rpart","partykit","dplyr","lubridate")
install_lib<-packages_needed[!packages_needed %in% installed.packages()]
for(lib in install_lib) install.packages(lib,dependencies=TRUE)
library(RODBC)
library(randomForest)
library(DMwR)
library(AUC)
library(sqldf)
library(dummies)
library(data.table)
library(glmnet)
library(reshape)
library(reshape2)
library(rpart)
library(partykit)
library(readr)
library(dplyr)
library(lubridate)

# Connection with mssql server and getting data environment
 conn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};
                           Server=genpactbidev.database.windows.net; Database=GenpactEDW;
                           Uid=ML_USER; Pwd=Genp@ct$8765")
df <- sqlQuery(conn, "SELECT * FROM ML.ML_Opportunity_Details_tbl;")
sfdcdat <- df

# Structuring the column names
colnames(sfdcdat) <- gsub("[[:punct:]]","_" , tolower(colnames(sfdcdat)))
colnames(sfdcdat) <- gsub(" ","_" , colnames(sfdcdat))

# Removing all the rows with NA's in opportunity ID
sfdcdat <- subset(sfdcdat, !is.na(opportunity_id))

# Considering only those columns which dosent have opportunity sub source value as "Renewal"
sfdcdat2 <- sfdcdat[sfdcdat$opportunity_sub_source != 'Renewal',] # All data except Renewal Source

# Taking out the columns with missing values. Currently the threshold is set at 15%
sfdcdat2[sapply(sfdcdat2,is.character)] <- lapply(sfdcdat2[sapply(sfdcdat2, is.character)],as.factor)

Miss <- colSums(is.na(sfdcdat2))/nrow(sfdcdat2)*100
Miss_Value <- Miss[Miss<15]
Miss_Value.df <- data.frame(Miss_Value)
Miss_Value.df$Variable <- row.names(Miss_Value.df)

# Considering those columns whose Missing Value is less than threshold for further investigation.
sfdcoptydat <- sfdcdat2[Miss_Value.df$Variable]
# sfdcoptydat <- sfdcdat2[,Miss_Value.df$Variable, with=F]

# Changing the date format

sfdcoptydat$created_date <- as.Date(sfdcoptydat$created_date, format = "%m/%d/%Y")
sfdcoptydat$opty_created_date <- as.Date(sfdcoptydat$created_date, format = "%m/%d/%Y")
sfdcoptydat$opty_closed_date <- as.Date(sfdcoptydat$actual_opp_closure_dt, format = "%m/%d/%Y")
sfdcoptydat$opty_expected_closed_date <- as.Date(sfdcoptydat$msa_sow_closure_date, format = "%m/%d/%Y")

# Removing the wrong data entries
# where created date >= Closed date
# where created date >= Expected Closed date
# sdfcoptydat_new <- subset(sfdcoptydat,! opty_created_date >= opty_closed_date )
# sdfcoptydat_new <- subset(sdfcoptydat_new, ! opty_created_date >= opty_expected_closed_date)
sfdcoptydat <- subset(sfdcoptydat,! (opty_created_date >= opty_closed_date ))
sfdcoptydat <- subset(sfdcoptydat, ! (opty_created_date >= opty_expected_closed_date))

####For Open Optys the Sales cycle will be current date when the data is extracted - When it was created 
### Since the data dump was taken the latest one, we will consider the sales cycle till that day.
sfdcoptydat$opty_open_asoff_date <- Sys.Date()

sfdcoptydat2 <- sqldf("select
                      *,
                      (case 
                      when opty_closed_date is NULL then (opty_open_asoff_date - opty_created_date) 
                      else (opty_closed_date - opty_created_date) end) sales_cycle
                      from sfdcoptydat")

# AS per Sharmaji Directions,Only post 2018 many columns were added which are used in r code
optydat_oneyear <- subset(sfdcoptydat2,opty_created_date >= as.Date("2018-01-01") & opty_closed_date <= Sys.Date())

# Removing all custom ID fields,names fields, currency fields and considering only TCV for initial analysis  
Final_data <- optydat_oneyear[,c("opportunity_id","opportunity_source","opportunity_sub_source",
                                 "service_line","nature_of_work","product_name","delivering_org","sub_delivering_org","stage",
                                 "ge_gc_l1","account_name","hunting_mining","sales_region","deal_type","tcv",    
                                 "contract_term","industry_vertical","sub_industry_vertical","aging_of_deal_in_pipeline","archetype",
                                 "qsrm_status","qsrm_type","opty_created_date","opty_closed_date","opty_expected_closed_date",
                                 "opty_open_asoff_date","sales_cycle")]

# Creating new column opportunity_status having won or lost, based on stages
Final_data_status <- sqldf("select *, 
                           (case when Stage = '6. Signed Deal' then 'Won' else 'Lost' end) as opportunity_status
                           from Final_data")

# considering Data with Level 1 Hierarchy for initial analysis dropping product and account name as characteristics needs to considered.
Final_data_L1 <- Final_data_status[,c("opportunity_id","opportunity_source","service_line","nature_of_work","delivering_org",
                                      "ge_gc_l1","hunting_mining","sales_region","deal_type","tcv","contract_term",
                                      "industry_vertical","aging_of_deal_in_pipeline", "archetype","qsrm_status",
                                      "qsrm_type","opty_created_date","opty_closed_date","opty_expected_closed_date",
                                      "opty_open_asoff_date","sales_cycle","opportunity_status")]

Final_data_L1_TCV_Aging <- sqldf("select * from Final_data_L1 where sales_cycle > 0 and tcv >1000")

    Final_data_L1_TCV_Aging_Comb <- Final_data_L1_TCV_Aging[!duplicated(Final_data_L1_TCV_Aging),]
    Final_data_L1_TCV_Aging_Comb$opportunity_id <- droplevels(Final_data_L1_TCV_Aging_Comb$opportunity_id)

# dim(Final_data_L1_TCV_Aging_Comb)
#####There are duplicates in the data so splitting the data into non-duplicate and duplicate records
##Nonduplicate records
# Final_data_L1_TCV_Aging_1R <- sqldf("select opportunity_id from 
#                                     (select distinct opportunity_id, count(opportunity_id) record_cnt 
#                                     from Final_data_L1_TCV_Aging 
#                                     group by opportunity_id) where record_cnt = 1")
# Final_data_L1_TCV_Aging_1Row <- sqldf("select * from Final_data_L1_TCV_Aging 
#                                       where opportunity_id in (select * from Final_data_L1_TCV_Aging_1R)")
# ##Duplicate Records
# Final_data_L1_TCV_Aging_MR <- sqldf("select opportunity_id from 
#                                     (select distinct opportunity_id, count(opportunity_id) record_cnt 
#                                     from Final_data_L1_TCV_Aging 
#                                     group by opportunity_id) where record_cnt > 1")
# Final_data_L1_TCV_Aging_MRow <- sqldf("select * from Final_data_L1_TCV_Aging 
#                                       where opportunity_id in (select * from Final_data_L1_TCV_Aging_MR)")
# 
# Final_data_L1_TCV_Aging_MROW_NDUP <- Final_data_L1_TCV_Aging_MRow[!duplicated(Final_data_L1_TCV_Aging_MRow),]

#####Merging the data back after taking out the duplicates#######
# Final_data_L1_TCV_Aging_Comb <- rbind(Final_data_L1_TCV_Aging_1Row,Final_data_L1_TCV_Aging_MROW_NDUP)
# dim(Final_data_L1_TCV_Aging_Comb)
# length(unique(Final_data_L1_TCV_Aging_Comb$opportunity_id))
# View(Final_data_L1_TCV_Aging_Comb)

#_____________________END OF FIRST FILE__________________________#


 conn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};
                           Server=genpactbidev.database.windows.net; Database=GenpactEDW;
                           Uid=ML_USER; Pwd=Genp@ct$8765")
# # importing Closure date movement file from SQL DB and converting colnames to lower
df1 <- sqlQuery(conn, "SELECT * FROM ML.ML_Closure_date_movements_tbl;")
optyhistdat <- df1
colnames(optyhistdat) <- gsub("[[:punct:]]","_" , tolower(colnames(optyhistdat)))

##Selecting Field event as MSA closure date
# Selecting the Field event as CloseDate
opty_msa_movement <- subset(optyhistdat, field_event == "CloseDate")

# Converting old value and new value in date format 
opty_msa_movement$old_value <- format(opty_msa_movement$old_value,format ="%a %b %e %H:%M:%S %Z %Y", usetz = FALSE)
opty_msa_movement$old_value <- gsub("IST","",opty_msa_movement$old_value)
opty_msa_movement$old_value <- parse_datetime(opty_msa_movement$old_value, "%a %b %e %H:%M:%S %Y")
opty_msa_movement$old_value <- gsub("UTC","",opty_msa_movement$old_value)
opty_msa_movement$old_value <- ymd(opty_msa_movement$old_value)

opty_msa_movement$new_value <- format(opty_msa_movement$new_value,format ="%a %b %e %H:%M:%S %Z %Y", usetz = FALSE)
opty_msa_movement$new_value <- gsub("IST","",opty_msa_movement$new_value)
opty_msa_movement$new_value <- parse_datetime(opty_msa_movement$new_value, "%a %b %e %H:%M:%S %Y")
opty_msa_movement$new_value <- gsub("UTC","",opty_msa_movement$new_value)
opty_msa_movement$new_value <- ymd(opty_msa_movement$new_value)

# Formatting edit date and msa_sow closure date format
opty_msa_movement$edit_date <- as.Date(opty_msa_movement$edit_date, format = "%Y-%m-%d")
opty_msa_movement$msa_sow_closure_date <- as.Date(opty_msa_movement$msa_sow_closure_date, format = "%Y-%m-%d")

opty_msa_movement <- opty_msa_movement[,c("opportunity_id","stage","account_name","edited_by","edit_date","field_event",
                                          "old_value","new_value","msa_sow_closure_date","actual_opp_closure_date",
                                          "tcv_currency","tcv","probability","forecast_category")]

# Adding ageing_of_current_stage_in_days from sfdc dump into opty_msa_movement dataframe
# opty_msa_movement <- merge(opty_msa_movement,sfdcdat[,c("stage_duration","opportunity_id")])
# colnames(opty_msa_movement)[15] <- "ageing_of_current_stage_in_days"

# sort opty_msa_movement on the basis of opportunity_id, edit_date
# opty_msa_movement2 <- sqldf("select * from opty_msa_movement order by opportunity_id,edit_date asc")
opty_msa_movement2 <- opty_msa_movement[with(opty_msa_movement, order(opportunity_id,edit_date)),]

##### Selecting the Starting value of the MSA 
movement_number <- sqldf("select distinct opportunity_id, count(opportunity_id) as number_of_times_moved 
                         from opty_msa_movement2 
                         group by opportunity_id")
opty_msa_movement_start <- setDT(opty_msa_movement2)[order(edit_date), head(.SD, 1L), by = opportunity_id]

# Creating a column closed date movements days, close date movement
opty_close_dt_movement <- sqldf("select opportunity_id, 
                                (case when old_value = msa_sow_closure_date then 'OnPath'
                                when old_value > msa_sow_closure_date then 'Prepone'
                                when old_value < msa_sow_closure_date then 'Postpone' end) as closedatemovement,
                                abs(old_value - msa_sow_closure_date) as closeddatemovementdays,
                                stage
                                from opty_msa_movement_start")


opty_close_dt_movement_count <- sqldf("select b.*, 
                                      a.number_of_times_moved from movement_number a, 
                                      opty_close_dt_movement b 
                                      where a.opportunity_id = b.opportunity_id")
opty_close_dt_movement_count$opportunity_id <- droplevels(opty_close_dt_movement_count$opportunity_id)
# dim(opty_close_dt_movement_count)
# str(opty_close_dt_movement_count)

#####Merging both the data frames####
# Final_data_L1_TCV_Aging_Comb$opportunity_id[!Final_data_L1_TCV_Aging_Comb$opportunity_id %in% opty_close_dt_movement_count$opportunity_id]
Final_data_Merge <- merge(Final_data_L1_TCV_Aging_Comb, opty_close_dt_movement_count, by ="opportunity_id")
# dim(Final_data_Merge)
Final_data_Merge_Combined <- Final_data_Merge[,c( "opportunity_id","opportunity_source","service_line","nature_of_work",
                                                  "delivering_org","ge_gc_l1","hunting_mining","sales_region","deal_type",
                                                  "tcv","contract_term","industry_vertical", 
                                                  "archetype","qsrm_status","qsrm_type","sales_cycle","opportunity_status",
                                                  "closedatemovement","closeddatemovementdays",
                                                  "number_of_times_moved")]

# Final_data_Merge_Combined$opportunity_id <- as.factor(Final_data_Merge_Combined$opportunity_id)
# Final_data_Merge_Combined$opportunity_status <- as.factor(Final_data_Merge_Combined$opportunity_status)
# Final_data_Merge_Combined$closedatemovement <- as.factor(Final_data_Merge_Combined$closedatemovement)
# Final_data_Merge_Combined[,c("opportunity_id","opportunity_status","closedatemovement")] <- 
#     lapply(Final_data_Merge_Combined[,c("opportunity_id","opportunity_status","closedatemovement")],factor)

# If all the factors columns are in character format then run below code
Final_data_Merge_Combined[sapply(Final_data_Merge_Combined,is.character)] <- lapply(Final_data_Merge_Combined[sapply(Final_data_Merge_Combined, is.character)],as.factor)

# Considering factor and numerical data for various operations
combined_cat_data <- Final_data_Merge_Combined[,sapply(Final_data_Merge_Combined,is.factor)]
combined_num_data <- Final_data_Merge_Combined[,sapply(Final_data_Merge_Combined,is.numeric)]

##Getting the unique ID in numeric vectors
combined_num_data$opportunity_id <- combined_cat_data$opportunity_id

#####Making Opportunity ID as Unique in the numeric vector data######
combined_num_data2 <- sqldf("select 
                            distinct opportunity_id, 
                            sum(tcv) tcv, 
                            max(contract_term) contract_term,
                            sales_cycle,
                            closeddatemovementdays,
                            number_of_times_moved 
                            from combined_num_data 
                            group by opportunity_id")

#####Making Opportunity ID and opportunity status as Unique in the categorical vector data######
combined_cat_data_colnames <- colnames(combined_cat_data[ ,-which(names(combined_cat_data) 
                                                                  %in% c("opportunity_id","opportunity_status"))])
data_wide_form_combined <- dummy.data.frame(combined_cat_data,combined_cat_data_colnames)
colnames(data_wide_form_combined) <- gsub(" ", "_", colnames(data_wide_form_combined))
colnames(data_wide_form_combined) <- gsub("[[:punct:]]","_" , colnames(data_wide_form_combined))

data_wide_form_comb_agg <- aggregate(. ~opportunity_id + opportunity_status, data=data_wide_form_combined, sum, na.rm=TRUE)

one_and_zero <- function(x) ifelse(x>=1,1,0)
data_wide_form_comb_agg[,-c(1,2)] <- apply(data_wide_form_comb_agg[,-c(1,2)],2,one_and_zero)
# for( i in 1:nrow(data_wide_form_comb_agg))
#     for( j in 3:ncol(data_wide_form_comb_agg))
#         if(data_wide_form_comb_agg[i,j] >= 1)
#         {data_wide_form_comb_agg[i,j]=1} else
#         {data_wide_form_comb_agg[i,j]=0}

#####Combining the data post removing duplicates and all other preprocessing activities
data_wide_merge <- merge(data_wide_form_comb_agg, combined_num_data2,by ="opportunity_id")
data_wide_merge$opportunity_id <- droplevels(data_wide_merge$opportunity_id)
# dim(data_wide_merge)
# length(unique(data_wide_merge$opportunity_id))
# str(data_wide_merge)
#_______________________END OF FILE 2_____________________________#

# Stage Duration File processing
library(RODBC)
conn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};
                           Server=genpactbidev.database.windows.net; Database=GenpactEDW;
                           Uid=ML_USER; Pwd=Genp@ct$8765")
# 
df3 <- sqlQuery(conn, "SELECT * FROM ML.ML_Stage_Duration_tbl_S1 order by opportunityid, from_date;")
#df3 <- sqlQuery(conn, "SELECT * FROM ML.ML_temp_Stage_Duration_2_tbl order by opportunityid, from_date;")

opty_stage_duration <- df3

colnames(opty_stage_duration) <- tolower(colnames(opty_stage_duration))
colnames(opty_stage_duration)[3] <- "opportunity_id" 

opty_stage_duration$opty_closed_date <- as.Date(opty_stage_duration$actual_opp_closure_date,format="%Y-%m-%d")

# selected unique opp_id and created date records from sfdcdat
# and merged that records with opty_stage_duration file
temp_sfdcdat <- sfdcdat[,c("opportunity_id","created_date")]
temp_sfdcdat_2 <- sqldf("select distinct(opportunity_id),created_date from temp_sfdcdat")
# temp_sfdcdat_2$created_date <- as.Date(temp_sfdcdat_2$created_date,format="%Y-%m-%d")
# str(temp_sfdcdat_2)
# length(unique(temp_sfdcdat_2$opportunity_id))
# 
# str(opty_stage_duration)
# length(unique(opty_stage_duration$opportunity_id))
opty_stage_duration$opportunity_id <- as.character(opty_stage_duration$opportunity_id)
temp_sfdcdat_2$opportunity_id <- as.character(temp_sfdcdat_2$opportunity_id)
opty_stage_duration_2 <- left_join(opty_stage_duration,temp_sfdcdat_2,by="opportunity_id")

####Formatting the data
opty_stage_duration_2$from_stage_format <- gsub('[[:digit:]]+', '', opty_stage_duration_2$from_stage)
opty_stage_duration_2$from_stage_format <- gsub('[[:punct:]]', '', opty_stage_duration_2$from_stage_format)
opty_stage_duration_2$from_stage_format <- trimws(tolower(opty_stage_duration_2$from_stage_format))

opty_stage_duration_2$to_stage_format <- gsub('[[:digit:]]+', '', opty_stage_duration_2$to_stage)
opty_stage_duration_2$to_stage_format <- gsub('[[:punct:]]', '', opty_stage_duration_2$to_stage_format)
opty_stage_duration_2$to_stage_format <- trimws(tolower(opty_stage_duration_2$to_stage_format)) 

# rearranging in order
opty_stage_duration_2 <- opty_stage_duration_2[,c("opportunity_id","from_date","to_date","from_stage","to_stage",
                                                  "stage_duration","tcv","msa_sow_closure_date","actual_opp_closure_date",
                                                  "from_stage_format","to_stage_format","opty_closed_date","created_date")]

# dropping prediscover dropped from from_stage and to_stage
# opty_stage_duration_final2 <- sqldf("select * from opty_stage_duration_2 where from_stage != 'Pre-discover Dropped'")
# opty_stage_duration_final2 <- sqldf("select * from opty_stage_duration_final2 where to_stage != 'Pre-discover Dropped' ")
opty_stage_duration_final2 <- subset(opty_stage_duration_2, from_stage != 'Pre-discover Dropped')
opty_stage_duration_final2 <- subset(opty_stage_duration_final2, to_stage != 'Pre-discover Dropped')
opty_stage_duration_final2$from_stage <- droplevels(opty_stage_duration_final2$from_stage)
opty_stage_duration_final2$to_stage <- droplevels(opty_stage_duration_final2$to_stage)

####For closed opportunities
# opty_stage_duration_final2_1 <- sqldf("select * from opty_stage_duration_final2 where opty_closed_date is NOT NULL")
# opty_stage_duration_final2_1 <-  subset(opty_stage_duration_final2_1,!is.na(opty_stage_duration_final2_1$opty_closed_date))
opty_stage_duration_final2_1 <- sqldf("select * from opty_stage_duration_final2 where opty_closed_date is NOT NULL")
summary(opty_stage_duration_final2_1)
table(opty_stage_duration_final2_1$from_stage)
table(opty_stage_duration_final2_1$to_stage)


# opty_stage_duration_final3 <- opty_stage_duration_final2_1[,c(1,5,8)]
opty_stage_duration_final3 <- opty_stage_duration_final2_1[,c("opportunity_id","from_stage_format","stage_duration")]
colnames(opty_stage_duration_final3) <- c("Opportunity_ID","From_Stage_format","value")
opty_stage_duration_final4 <- cast(opty_stage_duration_final3,Opportunity_ID ~ From_Stage_format, max)
opty_stage_duration_final5 <- as.data.frame(opty_stage_duration_final4)
opty_stage_duration_final5_1 <- do.call(data.frame,lapply(opty_stage_duration_final5, function(x) replace(x, is.infinite(x),NA)))
opty_stage_duration_final6 <- replace(opty_stage_duration_final5_1, is.na(opty_stage_duration_final5_1), 0)
opty_stagewise_duration_1 <- opty_stage_duration_final6[,c("Opportunity_ID","prediscover","discover","define","on.bid",
                                                           "down.select","confirmed","signed.deal","lost","dropped")]
colnames(opty_stagewise_duration_1) <- gsub('[[:punct:]]',"_" , colnames(opty_stagewise_duration_1))

###Selecting till down select stage
# optystage_durations_sequence_data_1 <- opty_stagewise_duration_1[,c("Opportunity_ID","prediscover","discover","define","on_bid","down_select","confirmed","signed_deal","lost","dropped")]
optystage_durations_sequence_data_1 <- opty_stagewise_duration_1

#### For Open Opportunities
# opty_stage_duration_final2_2 <- sqldf("select * from opty_stage_duration_final2 where opty_closed_date is NULL")
# opty_stage_duration_final2_1 <-  subset(opty_stage_duration_final2_1,is.na(opty_stage_duration_final2_1$opty_closed_date))
opty_stage_duration_final2_2 <- sqldf("select * from opty_stage_duration_final2 where opty_closed_date is NULL")

opty_stage_duration_final2_2$from_digit <- as.numeric(gsub("\\D","",opty_stage_duration_final2_2$from_stage))
opty_stage_duration_final2_2$from_digit <- replace(opty_stage_duration_final2_2$from_digit, 
                                                   is.na(opty_stage_duration_final2_2$from_digit), 0)

opty_stage_duration_final2_2$to_digit <- as.numeric(gsub("\\D","",opty_stage_duration_final2_2$to_stage))
opty_stage_duration_final2_2$to_digit <- replace(opty_stage_duration_final2_2$to_digit, 
                                                 is.na(opty_stage_duration_final2_2$to_digit), 0)

opty_stage_duration_final2_2$opty_open_asoff_date <- Sys.Date() #as.Date("2019-01-22")

opty_stage_duration_final2_2$from_date <- as.Date(opty_stage_duration_final2_2$from_date, format="%Y-%m-%d")
opty_stage_duration_final2_2$to_date <- as.Date(opty_stage_duration_final2_2$to_date, format="%Y-%m-%d")
opty_stage_duration_final2_2$created_date <- as.Date(opty_stage_duration_final2_2$created_date, format="%Y-%m-%d")


# Separating those rows which has created date as NA
# Creating current stage days columns by subtracting opty_open_asoff_date from from_date
# finally will merge them with the file
opty_stage_duration_final2_2_withNA <- subset(opty_stage_duration_final2_2, is.na(opty_stage_duration_final2_2$created_date))

opty_stage_duration_final2_2_withNA_1R <- opty_stage_duration_final2_2_withNA %>%
    group_by(opportunity_id) %>%
    filter(row_number()==1)

opty_stage_duration_final2_2_withNA_1R$current_Stage_days <- as.numeric(opty_stage_duration_final2_2_withNA_1R$opty_open_asoff_date-
                                                                            opty_stage_duration_final2_2_withNA_1R$from_date)

opty_stage_duration_final2_2_withNA_1R_1 <- inner_join(opty_stage_duration_final2_2_withNA,
                                                       opty_stage_duration_final2_2_withNA_1R[,c("opportunity_id","current_Stage_days")])


# Subset the rows which dosent have NA's in created date column
opty_stage_duration_final2_2_withoutNA <- subset(opty_stage_duration_final2_2, !is.na(opty_stage_duration_final2_2$created_date))
opty_stage_duration_final2_2_withoutNA$current_Stage_days <- as.numeric(opty_stage_duration_final2_2_withoutNA$opty_open_asoff_date - 
                                                                            opty_stage_duration_final2_2_withoutNA$created_date)

# Merging with NA and without NA files of closed data
opty_stage_duration_final2_2 <- rbind(opty_stage_duration_final2_2_withoutNA,opty_stage_duration_final2_2_withNA_1R_1)

# opty_stage_duration_final2_3 <- opty_stage_duration_final2_2[,c(1,5,7,8,11,14,17,18,19)]
opty_stage_duration_final2_3 <- opty_stage_duration_final2_2[,c("opportunity_id","from_stage_format","tcv","stage_duration",
                                                                "actual_opp_closure_date","to_stage_format",
                                                                "from_digit","to_digit")]

current_stage_det <- sqldf("select
                           opportunity_id,
                           (curr_stage_open_days-stage_duration)curr_stage_days,
                           to_stage_digit
                           from
                           (
                           select 
                           distinct opportunity_id,
                           sum(stage_duration)stage_duration,
                           max(to_digit) to_stage_digit,
                           max(current_Stage_days) curr_stage_open_days
                           from opty_stage_duration_final2_2
                           group by opportunity_id
                           )")
openopty_currstg_durations <- sqldf("select
                                    a.*,
                                    (case when a.to_digit = b.to_stage_digit then b.curr_stage_days else 0 end)Curr_Stage_Days
                                    from opty_stage_duration_final2_3 a, current_stage_det b 
                                    where a.opportunity_id = b.opportunity_id")
#From Stage
t3_1 <- openopty_currstg_durations[,c("opportunity_id","from_stage_format","stage_duration")]
colnames(t3_1) <- c("Opportunity_ID","From_Stage_format","value")
t3_1$From_Stage_format <- gsub('[[:digit:]]+', '', t3_1$From_Stage_format)
t3_1$From_Stage_format <- gsub('[[:punct:]]', '', t3_1$From_Stage_format)
t3_1$From_Stage_format <- trimws(t3_1$From_Stage_format)
#To Stage
t3_2 <- openopty_currstg_durations[,c("opportunity_id","to_stage_format","Curr_Stage_Days")]
colnames(t3_2) <- c("Opportunity_ID","To_Stage_format","value")
t3_2$To_Stage_format <- gsub('[[:digit:]]+', '',t3_2$To_Stage_format)
t3_2$To_Stage_format <- gsub('[[:punct:]]', '', t3_2$To_Stage_format)
t3_2$To_Stage_format <- trimws(t3_2$To_Stage_format)
# From Stage
t3_1_1 <- cast(t3_1,Opportunity_ID ~ From_Stage_format, max)
t11_1 <- do.call(data.frame,lapply(t3_1_1, function(x) replace(x, is.infinite(x),NA)))
t11_1_1 <- replace(t11_1, is.na(t11_1), 0)
# Creating a lost deal column with all values 0
# same scenario is found in other code
t11_1_1$lost <- 0
t4 <- t11_1_1[,c("Opportunity_ID","prediscover","discover","define","on.bid","down.select","confirmed","signed.deal",   
                 "lost","dropped")]

#To Stage
t3_2_2 <- cast(t3_2,Opportunity_ID ~ To_Stage_format, max)
t22_1 <- do.call(data.frame,lapply(t3_2_2, function(x) replace(x, is.infinite(x),NA)))
t22_2_2 <- replace(t22_1, is.na(t22_1), 0)
t5 <- t22_2_2[,c("Opportunity_ID","prediscover","discover","define","on.bid","down.select","confirmed","signed.deal",   
                 "lost","dropped")]

t6 <- aggregate(. ~ Opportunity_ID, rbind(t4,t5), FUN=sum)
colnames(t6) <- gsub('[[:punct:]]',"_" , colnames(t6))
###Considering till down select
t7 <- t6[,c("Opportunity_ID","prediscover","discover","define","on_bid","down_select","confirmed","signed_deal","lost","dropped")]

# Binding Closed and open opportunities details
optystage_durations_sequence_data_2 <- rbind(optystage_durations_sequence_data_1,t7)
colnames(optystage_durations_sequence_data_2) <- c("Opportunity_ID","prediscover_dur","discover_dur",
                                                   "define_dur","on_bid_dur","down_select_dur","confirmed_dur",
                                                   "signed_deal_dur","lost_dur","dropped_dur")

colnames(optystage_durations_sequence_data_2) <- tolower(colnames(optystage_durations_sequence_data_2))


# Creating a New_Stage duration column which has difference of each stages from Signed Deal
# head(opty_stage_duration)
# b<- opty_stage_duration

str(opty_stage_duration)
opty_stage_duration$from_date <- as.Date(opty_stage_duration$from_date, format="%Y-%m-%d")
opty_stage_duration$to_date <- as.Date(opty_stage_duration$to_date, format="%Y-%m-%d")
table(opty_stage_duration$from_stage)
table(opty_stage_duration$to_stage)

opty_stage_duration_comp <- sqldf("select * from opty_stage_duration where to_date is NOT NULL")
str(opty_stage_duration_comp)
summary(opty_stage_duration_comp)
table(opty_stage_duration_comp$from_stage)
table(opty_stage_duration_comp$to_stage)

# Selecting all those deals which is WON and follow some stage paths
opty_stage_duration_comp_sd_1 <- sqldf("select opportunity_id
                                    from opty_stage_duration_comp
                                    where to_stage = '6. Signed Deal'")
opty_stage_duration_comp_sd <- sqldf("select * from opty_stage_duration_comp
                                      where opportunity_id in (select * from opty_stage_duration_comp_sd_1)")
str(opty_stage_duration_comp_sd)
table(opty_stage_duration_comp_sd$from_stage)
table(opty_stage_duration_comp_sd$to_stage)

opty_stage_duration_comp_sd <- subset(opty_stage_duration_comp_sd, to_stage != '8. Dropped')
opty_stage_duration_comp_sd <- subset(opty_stage_duration_comp_sd, to_stage != '7. Lost')
# 
# View(opty_stage_duration_comp_sd)

str(last(opty_stage_duration_comp_sd$to_date))

opty_stage_duration_new <- opty_stage_duration_comp_sd %>%
    group_by(opportunity_id) %>%
    mutate(Stage_Duration_new = last(to_date) - from_date )


lastx <- function(x) { tail(x, n = 1) }

a <- opty_stage_duration_comp_sd %>%
  group_by(opportunity_id) %>%
  mutate(Stage_Duration_new = lastx(to_date) - from_date )
a$Stage_Duration_new <- as.numeric(a$Stage_Duration_new)


opty_stage_duration_new2 <- as.data.frame(opty_stage_duration_new)
opty_stage_duration_new2$Stage_Duration_new <- as.numeric(opty_stage_duration_new2$Stage_Duration_new)

summary(opty_stage_duration_new2)
opty_stage_duration_new2_1 <- opty_stage_duration_new2[,c("opportunity_id","from_stage","Stage_Duration_new")]

opty_stage_duration_new2_2 <- dcast(setDT(opty_stage_duration_new2_1), opportunity_id~from_stage, value.var=c('Stage_Duration_new'))
# dim(opty_stage_duration_new2_2)
# length(unique(opty_stage_duration_new2_2$opportunity_id))
opty_stage_duration_new2_2 <- replace(opty_stage_duration_new2_2, is.na(opty_stage_duration_new2_2), 0)
opty_stage_duration_new2_2 <- opty_stage_duration_new2_2[,c("opportunity_id","Prediscover","1. Discover","2. Define","3. On Bid","4. Down Select","5. Confirmed")]
colnames(opty_stage_duration_new2_2) <-  c("opportunity_id","predis_sd_dur","dis_sd_dur","def_sd_dur","ob_sd_dur","dwn_sd_dur","con_sd_dur")


# optystage_sequence_data is the output of Opportunity movements csv
# hence creating those files
optystage <- opty_stage_duration

# Selecting columns and renaming them
optystage2 <- optystage[,c("opportunity_id","from_stage","to_stage","to_date","from_date")]
colnames(optystage2) <- c("opportunity_id","old_value","new_value","edit_date","created_date")

# updating stages categories
optystage2$Old_Value_format <- gsub('[[:digit:]]+', '', optystage2$old_value)
optystage2$Old_Value_format <- gsub('[[:punct:]]', '', optystage2$Old_Value_format)
optystage2$Old_Value_format <- trimws(optystage2$Old_Value_format)
optystage2$New_Value_format <- gsub('[[:digit:]]+', '', optystage2$new_value)
optystage2$New_Value_format <- gsub('[[:punct:]]', '', optystage2$New_Value_format)
optystage2$New_Value_format <- trimws(optystage2$New_Value_format)

optystage_old_value <- sqldf("select Opportunity_ID,Old_Value_format as format from optystage2")
optystage_new_value <- sqldf("select Opportunity_ID,New_Value_format as format from optystage2")
# optystage_date <- sqldf("select Opportunity_ID,Edit_Date,Created_Date from optystage2")
optystage_ov_wide_form <- dummy.data.frame(optystage_old_value,names = c("format"))
optystage_nv_wide_form <- dummy.data.frame(optystage_new_value,names = c("format"))
optystage_common_cols <- intersect(colnames(optystage_ov_wide_form), colnames(optystage_nv_wide_form))
optystage_merge <- cbind(optystage_ov_wide_form[-c(1)],optystage_nv_wide_form[-c(1,12)])
optystage_merge$Opportunity_ID <- optystage_ov_wide_form$opportunity_id
optystage_common_cols <- aggregate(. ~Opportunity_ID , data=optystage_merge, sum, na.rm=TRUE)
optystage_common_cols$`formatPrediscover Dropped` <- NULL
colnames(optystage_common_cols) <- c("Opportunity_ID","Confirmed","Define","Discover","Down_Select",
                                     "Dropped","Lost","On_Bid","Prediscover","Signed_Deal")
##Arranging as per order
optystage_sequence_cols <- optystage_common_cols[,c("Opportunity_ID","Prediscover","Discover","Define","On_Bid","Down_Select","Confirmed","Signed_Deal","Lost","Dropped")]

one_and_zero <- function(x) ifelse(x>=1,1,0)
optystage_sequence_cols[,-c(1)] <- apply(optystage_sequence_cols[,-c(1)],2,one_and_zero) 
# for( i in 1:nrow(optystage_sequence_cols))
#     for( j in 2:ncol(optystage_sequence_cols))
#         if(optystage_sequence_cols[i,j] >= 1)
#         {optystage_sequence_cols[i,j]=1} else
#         {optystage_sequence_cols[i,j]=0}
###Selecting till confirmed stage
optystage_sequence_data <- optystage_sequence_cols[,c(1:6)]
colnames(optystage_sequence_data) <- tolower(colnames(optystage_sequence_data))


######################################################################################################################
optystage_durations_sequence_data <- merge(optystage_sequence_data, 
                                           optystage_durations_sequence_data_2,by = "opportunity_id")
data_wide_merge2 <- merge(data_wide_merge,optystage_durations_sequence_data, by = "opportunity_id")


##### END OF ALL FILES #####


######Removing TCV, Contract Term, Sales Cycle, Stages and their durations
data_wide_merge_no_tcv_ct_sc <- subset(data_wide_merge2, 
                                       select = -c(tcv,contract_term,sales_cycle,prediscover,discover,define,
                                                   on_bid,down_select,prediscover_dur,discover_dur,
                                                   define_dur,on_bid_dur,down_select_dur,confirmed_dur,
                                                   signed_deal_dur,lost_dur,dropped_dur))

# Finding the mean and SD of the training dataset  
trainmean_insights <- apply(data_wide_merge_no_tcv_ct_sc[ ,-which(names(data_wide_merge_no_tcv_ct_sc) %in% 
                                                                      c("opportunity_id","opportunity_status"))],2,mean)
trainsd_insights <- apply(data_wide_merge_no_tcv_ct_sc[ ,-which(names(data_wide_merge_no_tcv_ct_sc) %in% 
                                                                    c("opportunity_id","opportunity_status"))],2,sd)

# Centering the data using the train data mean and SD and also applying it on test dataset.
norm_trainset_LASSO_insights <- sweep(sweep(data_wide_merge_no_tcv_ct_sc[,-which(names(data_wide_merge_no_tcv_ct_sc) %in% 
                                                                                     c("opportunity_id","opportunity_status"))], 
                                            2L, trainmean_insights), 2, trainsd_insights, "/")

#Adding Opportunity ID and Opportunity Status to the normalized dataset
norm_trainset_LASSO_insights$opportunity_id <- data_wide_merge_no_tcv_ct_sc$opportunity_id
norm_trainset_LASSO_insights$opportunity_status <- data_wide_merge_no_tcv_ct_sc$opportunity_status

# dropping those columns which has all the values as NA
norm_trainset_LASSO_insights <- norm_trainset_LASSO_insights[,colSums(is.na(norm_trainset_LASSO_insights)) < nrow(norm_trainset_LASSO_insights)]

######################
# Model building using Least Absolute Shrinkage and Selection Operator
Model_Build_insights <- data.matrix(norm_trainset_LASSO_insights)
x_insights <- subset(Model_Build_insights,select=-c(opportunity_id,opportunity_status))
y_insights <- subset(Model_Build_insights,select = c(opportunity_status))
cv_glmmod_insights<-cv.glmnet(x_insights, y_insights,alpha = 1, family = "binomial",nfolds = 10) # Ridge Model
model_coeff_insights <- coef.cv.glmnet(cv_glmmod_insights,s=c("lambda.1se","lambda.min"))
model_coeff_df_insights <- data.frame(Attr_name = model_coeff_insights@Dimnames[[1]][model_coeff_insights@i + 1], Attr_coefficients = model_coeff_insights@x)
imp_features <- as.character(model_coeff_df_insights$Attr_name[-1])
imp_features_df <- data.frame(imp_features)
imp_features_df$imp_features <- as.character(imp_features_df$imp_features)

#########################
###Similar Opportunity Analysis using Decision Trees
#########################
similar_opty_cols <- data_wide_merge_no_tcv_ct_sc[imp_features_df$imp_features]
similar_opty_cols$opportunity_id <- data_wide_merge_no_tcv_ct_sc$opportunity_id
similar_opty_cols$opportunity_status <- data_wide_merge_no_tcv_ct_sc$opportunity_status
num_cols <- c("opportunity_id","opportunity_status","closeddatemovementdays","number_of_times_moved")
similar_opty_cols_num <- subset(similar_opty_cols,select = num_cols)
similar_opty_cols_fact <- similar_opty_cols[,-which(names(similar_opty_cols)%in% num_cols)]
similar_opty_cols_fact2 <- as.data.frame(sapply(similar_opty_cols_fact,as.factor))
similar_opty_cols_fact2$opportunity_id <- similar_opty_cols$opportunity_id
similar_opty_cols_fact2$opportunity_status <- similar_opty_cols$opportunity_status

###Merge Num and Cat columns
similar_opty_cols_final <- merge(similar_opty_cols_fact2,similar_opty_cols_num, 
                                 by =c("opportunity_id","opportunity_status"))

similar_opty_model_rpart <- rpart(opportunity_status~.,data=similar_opty_cols_final[,-which(names(similar_opty_cols_final)%in%
                                                                                                c("opportunity_id"))])
# summary(similar_opty_model_rpart)
# a <- predict(similar_opty_model_rpart,
#              similar_opty_cols_final[,-which(names(similar_opty_cols_final) %in% c("opportunity_id","opportunity_status"))],
#              type = "class")
# confMat <- table(similar_opty_cols_final$opportunity_status,a)
# accuracy <- sum(diag(confMat))/sum(confMat)

similar_opty_model_nodes <- rownames(similar_opty_model_rpart$frame)[similar_opty_model_rpart$where]
similar_opty_cols_final$Nodes <- similar_opty_model_nodes

pathanal <- function(object, ...)
{
    ## coerce to "party" object if necessary
    if(!inherits(object, "party")) object <- as.party(object)
    ## get standard predictions (response/prob) and collect in data frame
    rval <- data.frame(response = predict(object, type = "response", ...))
    rval$prob <- predict(object, type = "prob", ...)
    ## get rules for each node
    rls <- partykit:::.list.rules.party(object)
    ## get predicted node and select corresponding rule
    rval$rule <- rls[as.character(predict(object, type = "node", ...))]
    return(rval)
}
similar_opty_path <- pathanal(similar_opty_model_rpart)
similar_opty_cols_final$RULE <- similar_opty_path$rule
similar_opty_nodes_predset <- similar_opty_model_rpart
similar_opty_nodes_predset$frame$yval = as.numeric(rownames(similar_opty_nodes_predset$frame))
data_wide_merge_node_rule <- sqldf("select 
                                   a.*, 
                                   b.Nodes
                                   from data_wide_merge2 a, similar_opty_cols_final b 
                                   where 
                                   a.opportunity_id = b.opportunity_id")


####Coming up with the insights
## Considering Won Opportunities
data_insight_won_1 <- subset(data_wide_merge_node_rule,opportunity_status =='Won')
data_insight_won_1$Nodes <- as.numeric(data_insight_won_1$Nodes)

length(unique(data_insight_won_1$opportunity_id))
length(unique(opty_stage_duration_new2_2$opportunity_id))


# Inner join with the data which got created from stage duration
data_insight_won <- inner_join(data_insight_won_1,opty_stage_duration_new2_2,by="opportunity_id")
# a <- setDT(sfdcoptydat2[,c("opportunity_id","opty_expected_closed_date")])[order(-opty_expected_closed_date, opportunity_id), 
#                                                                            head(.SD, 1), by = opportunity_id]
# b <- merge(data_insight_won,a)

# a <- setDT(sfdcoptydat2[,c("opportunity_id","sales_cycle")])[, .(sales_cycle= mean(sales_cycle)), by = opportunity_id]
# data_insight_won <- left_join(data_insight_won, a, by="opportunity_id")

opty_insight_won <- sqldf("select 
                          closedatemovementOnPath,
                          closedatemovementPostpone,
                          closedatemovementPrepone,
                          tcv,
                          contract_term,
                          sales_cycle,
                          closeddatemovementdays,
                          number_of_times_moved,
                          prediscover,
                          discover,
                          define,
                          on_bid,
                          down_select,
                          prediscover_dur,
                          discover_dur,
                          define_dur,
                          on_bid_dur,
                          down_select_dur,
                          confirmed_dur,
                          signed_deal_dur,
                          lost_dur,
                          dropped_dur,
                          predis_sd_dur,
                          dis_sd_dur, 
                          def_sd_dur,
                          ob_sd_dur,
                          dwn_sd_dur,
                          con_sd_dur,
                          Nodes 
                          from
                          data_insight_won")

opty_insight_won <- sqldf("select
                          Nodes,
                          sum(closedatemovementOnPath)OnPath_cnt,
                          sum(closedatemovementPostpone)Postpone_cnt,
                          sum(closedatemovementPrepone)Prepone_cnt,
                          min(tcv) min_TCV_USD,
                          max(tcv) max_TCV_USD,
                          avg(tcv) mean_TCV_USD,
                          min(contract_term) min_Contract_term,
                          max(contract_term) max_Contract_term,
                          avg(sales_cycle)avg_sales_cycle,
                          min(closeddatemovementdays) min_movement_days,
                          max(closeddatemovementdays) max_movement_days,  
                          avg(closeddatemovementdays) mean_movement_days,
                          avg(number_of_times_moved) mean_times_moved,
                          (Prediscover+Discover+Define+On_Bid+Down_Select) Number_of_stages,
                          min(prediscover_dur)min_predicover_dur,
                          max(prediscover_dur)max_predicover_dur,
                          avg(prediscover_dur)mean_predicover_dur,
                          min(discover_dur) min_discover_dur,
                          max(discover_dur) max_discover_dur,
                          avg(discover_dur) mean_discover_dur,
                          min(define_dur) min_define_dur,
                          max(define_dur) max_define_dur,
                          avg(define_dur) mean_define_dur,
                          min(on_bid_dur) min_on_bid_dur,
                          max(on_bid_dur) max_on_bid_dur,
                          avg(on_bid_dur) mean_on_bid_dur,  
                          min(down_select_dur) min_down_select_dur,
                          max(down_select_dur) max_down_select_dur,
                          avg(down_select_dur) mean_down_select_dur,
                          min(confirmed_dur) min_confirmed_dur,
                          max(confirmed_dur) max_confirmed_dur,
                          avg(confirmed_dur) mean_confirmed_dur,
                          min(signed_deal_dur) min_signed_deal_dur,
                          max(signed_deal_dur) max_signed_deal_dur,
                          avg(signed_deal_dur) mean_signed_deal_dur,
                          min(lost_dur) min_lost_dur,
                          max(lost_dur) max_lost_dur,
                          avg(lost_dur) mean_lost_dur,
                          min(dropped_dur) min_dropped_dur,
                          max(dropped_dur) max_dropped_dur,
                          avg(dropped_dur) mean_dropped_dur,
                          avg(sales_cycle) mean_sales_cycle,
                          avg(predis_sd_dur) mean_predis_sd_dur,
                          avg(dis_sd_dur) mean_dis_sd_dur, 
                          avg(def_sd_dur) mean_def_sd_dur,
                          avg(ob_sd_dur)  mean_ob_sd_dur,
                          avg(dwn_sd_dur) mean_dwn_sd_dur,
                          avg(con_sd_dur) mean_con_dis_sd_dur
                          from
                          data_insight_won
                          group by Nodes")

#######################
#######Applying the historical insights to open opportunities.
#######################
# openoptys <- subset(sfdcoptydat2, is.na(actual_opp_closure_dt))
openoptys <- subset(sfdcdat2, is.na(actual_opp_closure_dt))
openoptys$opportunity_status <- openoptys$stage

# Changing the date format
openoptys$opty_created_date <- ymd(openoptys$created_date)
openoptys$opty_closed_date <- ymd(openoptys$actual_opp_closure_dt)
openoptys$opty_expected_closed_date <- ymd(openoptys$msa_sow_closure_date)


####For Open Optys the Sales cycle will be current date when the data is extracted - When it was created 
### Since the data dump was taken on 2019-01-22 we will consider the sales cycle till that day.
openoptys$opty_open_asoff_date <- Sys.Date()

openoptys <- sqldf("select
                   *,
                   (case 
                   when opty_closed_date is NULL then (opty_open_asoff_date - opty_created_date) 
                   else (opty_closed_date - opty_created_date) end) sales_cycle
                   from openoptys")
openoptys$days_remaining_from_msa <- as.numeric(openoptys$opty_expected_closed_date - openoptys$opty_open_asoff_date)

# # Considering opp_id which has been created after 01-01-2018
# openoptys <- subset(openoptys,opty_created_date >= as.Date("2018-01-01") & opty_closed_date <= Sys.Date())
openoptys_data2 <- subset(openoptys, select=names(Final_data_L1))
openoptys_TCV_Aging <- sqldf("select * from openoptys_data2 where tcv >1000 and sales_cycle > 0")

#####There are duplicates in the data so splitting the data into non-duplicate and duplicate records
##Nonduplicate records
openoptys_TCV_Aging_1R <- sqldf("select opportunity_id from 
                                (select distinct opportunity_id, count(opportunity_id) Record_cnt 
                                from openoptys_TCV_Aging 
                                group by opportunity_id) where Record_cnt = 1")
openoptys_TCV_Aging_1Row <- sqldf("select * from openoptys_TCV_Aging where opportunity_id in (select * from openoptys_TCV_Aging_1R)")
##Duplicate Records
openoptys_TCV_Aging_MR <- sqldf("select opportunity_id from 
                                (select distinct opportunity_id, count(opportunity_id) Record_cnt 
                                from openoptys_TCV_Aging 
                                group by opportunity_id) where Record_cnt > 1")
openoptys_TCV_Aging_MRow <- sqldf("select * from openoptys_TCV_Aging where opportunity_id in (select * from openoptys_TCV_Aging_MR)")

openoptys_TCV_Aging_MROW_NDUP <- openoptys_TCV_Aging_MRow[!duplicated(openoptys_TCV_Aging_MRow),]
#####Merging the data back after taking out the duplicates#######
openoptys_TCV_Aging_Comb <- rbind(openoptys_TCV_Aging_1Row,openoptys_TCV_Aging_MROW_NDUP)

#######Getting Closed Date Movement Information###
##Formating the column names
colnames(optyhistdat) <- gsub("[[:punct:]]","_" , colnames(optyhistdat))
##Selecting Field event as MSA closure date
opty_msa_movement <- subset(optyhistdat, field_event == "CloseDate" )

str(opty_msa_movement)
head(opty_msa_movement)
# Converting old value and new value in date format 
opty_msa_movement$old_value <- format(opty_msa_movement$old_value,format ="%a %b %e %H:%M:%S %Z %Y", usetz = FALSE)
opty_msa_movement$old_value <- gsub("IST","",opty_msa_movement$old_value)
opty_msa_movement$old_value <- parse_datetime(opty_msa_movement$old_value, "%a %b %e %H:%M:%S %Y")
opty_msa_movement$old_value <- gsub("UTC","",opty_msa_movement$old_value)
opty_msa_movement$old_value <- as.Date(opty_msa_movement$old_value)

opty_msa_movement$new_value <- format(opty_msa_movement$new_value,format ="%a %b %e %H:%M:%S %Z %Y", usetz = FALSE)
opty_msa_movement$new_value <- gsub("IST","",opty_msa_movement$new_value)
opty_msa_movement$new_value <- parse_datetime(opty_msa_movement$new_value, "%a %b %e %H:%M:%S %Y")
opty_msa_movement$new_value <- gsub("UTC","",opty_msa_movement$new_value)
opty_msa_movement$new_value <- as.Date(opty_msa_movement$new_value)

## Formating the Date columns
opty_msa_movement$edit_date <- as.Date(opty_msa_movement$edit_date, format = "%m/%d/%Y")
opty_msa_movement$old_value <- as.Date(opty_msa_movement$old_value, format = "%m/%d/%Y")
opty_msa_movement$new_value <- as.Date(opty_msa_movement$new_value, format = "%m/%d/%Y")
opty_msa_movement$msa_sow_closure_date <- as.Date(opty_msa_movement$msa_sow_closure_date, format = "%Y-%m-%d")

# opty_msa_movement2 <- sqldf("select * from opty_msa_movement order by opportunity_id,edit_date asc")
opty_msa_movement2 <- opty_msa_movement[with(opty_msa_movement, order(opportunity_id,edit_date)),] 
##### Selecting the Starting value of the MSA 
movement_number <- sqldf("select distinct opportunity_id, count(opportunity_id) as number_of_times_moved 
                         from opty_msa_movement2 
                         group by opportunity_id")
opty_msa_movement_start <- setDT(opty_msa_movement2)[order(edit_date), head(.SD, 1L), by = opportunity_id]
opty_close_dt_movement <- sqldf("select opportunity_id, 
                                (case when old_value = msa_sow_closure_date then 'OnPath'
                                when old_value > msa_sow_closure_date then 'Prepone'
                                when old_value < msa_sow_closure_date then 'Postpone' end) as closedatemovement,
                                abs(old_value - msa_sow_closure_date) as closeddatemovementdays,
                                stage
                                from opty_msa_movement_start")

opty_close_dt_movement_count <- sqldf("select b.*, 
                                      a.number_of_times_moved from movement_number a, 
                                      opty_close_dt_movement b 
                                      where a.opportunity_id = b.opportunity_id")

#####Merging both the data frames####
openoptys_data_Merge <- merge(openoptys_TCV_Aging_Comb,opty_close_dt_movement_count, by ="opportunity_id")
openoptys_Merge_Combined <- openoptys_data_Merge[,c( "opportunity_id","opportunity_source","service_line","nature_of_work",
                                                     "delivering_org","ge_gc_l1","hunting_mining","sales_region","deal_type",
                                                     "tcv","contract_term","industry_vertical", 
                                                     "archetype","qsrm_status","qsrm_type","sales_cycle","opportunity_status",
                                                     "closedatemovement","closeddatemovementdays",
                                                     "number_of_times_moved")]
# openoptys_Merge_Combined$opportunity_id <- as.factor(openoptys_Merge_Combined$opportunity_id)
# openoptys_Merge_Combined$opportunity_status <- as.factor(openoptys_Merge_Combined$opportunity_status)
# openoptys_Merge_Combined$closedatemovement <- as.factor(openoptys_Merge_Combined$closedatemovement)
openoptys_Merge_Combined$closeddatemovementdays <- as.numeric(openoptys_Merge_Combined$closeddatemovementdays)

# If all the factors columns are in character format then run below code
openoptys_Merge_Combined[sapply(openoptys_Merge_Combined,is.character)] <- lapply(openoptys_Merge_Combined[sapply(openoptys_Merge_Combined, is.character)],as.factor)

####Splitting into categorical data and numeric data.
combined_open_cat_data <- openoptys_Merge_Combined[,sapply(openoptys_Merge_Combined,is.factor)]
combined_open_num_data <- openoptys_Merge_Combined[,sapply(openoptys_Merge_Combined,is.numeric)]

##Getting the unique ID in numeric vectors
combined_open_num_data$opportunity_id <- combined_open_cat_data$opportunity_id

#####Making Opportunity ID as Unique in the numeric vector data######
combined_open_num_data2 <- sqldf("select 
                                 distinct Opportunity_ID, 
                                 sum(tcv) TCV_USD, 
                                 max(contract_term) Contract_Term,
                                 Sales_Cycle,
                                 CloseddateMovementdays,
                                 number_of_times_moved 
                                 from combined_open_num_data 
                                 group by Opportunity_ID")
#####Making Opportunity ID as Unique in the categorical vector data######
combined_open_cat_data_colnames <- colnames(combined_open_cat_data[ ,-which(names(combined_open_cat_data) %in% 
                                                                                c("opportunity_id","opportunity_status"))])
open_data_wide_form_combined <- dummy.data.frame(combined_open_cat_data,combined_open_cat_data_colnames)
colnames(open_data_wide_form_combined) <- gsub(" ", "_", colnames(open_data_wide_form_combined))
colnames(open_data_wide_form_combined) <- gsub("[[:punct:]]","_" , colnames(open_data_wide_form_combined))
open_data_wide_form_comb_agg <- aggregate(. ~opportunity_id + opportunity_status, data=open_data_wide_form_combined, sum, na.rm=TRUE)

one_and_zero <- function(x) ifelse(x>=1,1,0)
open_data_wide_form_comb_agg[,-c(1,2)] <- apply(open_data_wide_form_comb_agg[,-c(1,2)],2,one_and_zero) 
# for( i in 1:nrow(open_data_wide_form_comb_agg))
#     for( j in 3:ncol(open_data_wide_form_comb_agg))
#         if(open_data_wide_form_comb_agg[i,j] >= 1)
#         {open_data_wide_form_comb_agg[i,j]=1} else
#         {open_data_wide_form_comb_agg[i,j]=0}

#####Combining the data post removing duplicates and all other preprocessing activities
openopty_data_wide_merge <- merge(open_data_wide_form_comb_agg, combined_open_num_data2,by ="opportunity_id")
openopty_data_wide_merge2 <- merge(openopty_data_wide_merge,optystage_durations_sequence_data, by = "opportunity_id")
t1 <- setdiff(colnames(similar_opty_cols_fact2),colnames(openopty_data_wide_merge2))
if(length(t1)==0){openopty_data_wide_merge2}else
{
    openopty_merge_col_diff_mat <- matrix(data = 0, nrow = nrow(openopty_data_wide_merge2), ncol = length(t1))
    openopty_merge_col_diff_mat2 <- as.data.frame(openopty_merge_col_diff_mat)
    colnames(openopty_merge_col_diff_mat2) <- t1
    openopty_data_wide_merge2 <- cbind(openopty_data_wide_merge2,openopty_merge_col_diff_mat2)
}
pred_data_forinsights <- subset(openopty_data_wide_merge2, select = names(similar_opty_cols_fact2))
pred_data_forinsights_fact <- as.data.frame(sapply(pred_data_forinsights,as.factor))
num_cols <- c("opportunity_id","opportunity_status","closeddatemovementdays","number_of_times_moved")
pred_data_forinsights_num <- subset(openopty_data_wide_merge2,select = num_cols)
pred_data_forinsights_final <- merge(pred_data_forinsights_fact,pred_data_forinsights_num, by=c("opportunity_id","opportunity_status"))

###Predicting the nodes for open opportunities
openopty_data_wide_merge2$Nodes <- predict(similar_opty_nodes_predset, 
                                           pred_data_forinsights_final[,-which(names(pred_data_forinsights_final)%in%
                                                                                   c("opportunity_id","opportunity_status"))], 
                                           type="vector")
# pred_data_forinsights_final$Nodes <- predict(similar_opty_nodes_predset, 
#                                            pred_data_forinsights_final[,-which(names(pred_data_forinsights_final)%in%
#                                                                                    c("opportunity_id","opportunity_status"))], 
#                                            type="vector")
# 
# openopty_data_wide_merge2$Nodes <- as.factor(pred_data_forinsights_final$Nodes)


insights_open_opty <- sqldf("select * from openopty_data_wide_merge2 a, opty_insight_won b where a.Nodes = b.Nodes")
insights_open_opty$Opportunity_Status2 <- (gsub('[[:digit:]]+', '', insights_open_opty$opportunity_status))
insights_open_opty$Opportunity_Status2 <- gsub('[[:punct:]]', '', insights_open_opty$Opportunity_Status2)
insights_open_opty$Opportunity_Status2 <- trimws(insights_open_opty$Opportunity_Status2)

curr_sd_stage_duration <- sqldf("select
                             opportunity_id,
                             (case 
                             when Opportunity_Status2 = 'Prediscover' then mean_predis_sd_dur
                             when Opportunity_Status2 = 'Discover' then mean_dis_sd_dur
                             when Opportunity_Status2 = 'Define' then mean_def_sd_dur
                             when Opportunity_Status2 = 'On Bid' then mean_ob_sd_dur
                             when Opportunity_Status2 = 'Down Select' then mean_dwn_sd_dur
                             when Opportunity_Status2 = 'Confirmed' then mean_con_dis_sd_dur
                             end)curr_sd_stage_duration from insights_open_opty")


curr_stage_duration <- sqldf("select
                             opportunity_id,
                             (case 
                             when Opportunity_Status2 = 'Prediscover' then prediscover_dur
                             when Opportunity_Status2 = 'Discover' then discover_dur
                             when Opportunity_Status2 = 'Define' then define_dur
                             when Opportunity_Status2 = 'On Bid' then on_bid_dur
                             when Opportunity_Status2 = 'Down Select' then down_select_dur
                             when Opportunity_Status2 = 'Confirmed' then confirmed_dur
                             when Opportunity_Status2 = 'Signed Deal' then signed_deal_dur
                             when Opportunity_Status2 = 'Lost' then lost_dur
                             when Opportunity_Status2 = 'Dropped' then dropped_dur
                             end)curr_stage_duration from insights_open_opty")

min_historical_won_stage_duration <- sqldf("select
                                           opportunity_id,
                                           (case 
                                           when Opportunity_Status2 = 'Prediscover' then min_predicover_dur
                                           when Opportunity_Status2 = 'Discover' then min_discover_dur
                                           when Opportunity_Status2 = 'Define' then min_define_dur
                                           when Opportunity_Status2 = 'On Bid' then min_on_bid_dur
                                           when Opportunity_Status2 = 'Down Select' then min_down_select_dur
                                           when Opportunity_Status2 = 'Confirmed' then min_confirmed_dur
                                           when Opportunity_Status2 = 'Signed Deal' then min_signed_deal_dur
                                           when Opportunity_Status2 = 'Lost' then min_lost_dur
                                           when Opportunity_Status2 = 'Dropped' then min_dropped_dur
                                           end)min_historical_won_stage_duration from insights_open_opty")
max_historical_won_stage_duration <- sqldf("select
                                           opportunity_id,
                                           (case 
                                           when Opportunity_Status2 = 'Prediscover' then max_predicover_dur
                                           when Opportunity_Status2 = 'Discover' then max_discover_dur
                                           when Opportunity_Status2 = 'Define' then max_define_dur
                                           when Opportunity_Status2 = 'On Bid' then max_on_bid_dur
                                           when Opportunity_Status2 = 'Down Select' then max_down_select_dur
                                           when Opportunity_Status2 = 'Confirmed' then max_confirmed_dur
                                           when Opportunity_Status2 = 'Signed Deal' then max_signed_deal_dur
                                           when Opportunity_Status2 = 'Lost' then max_lost_dur
                                           when Opportunity_Status2 = 'Dropped' then max_dropped_dur
                                           end)max_historical_won_stage_duration from insights_open_opty")
mean_historical_won_stage_duration <- sqldf("select
                                            opportunity_id,
                                            (case 
                                            when Opportunity_Status2 = 'Prediscover' then mean_predicover_dur
                                            when Opportunity_Status2 = 'Discover' then mean_discover_dur
                                            when Opportunity_Status2 = 'Define' then mean_define_dur
                                            when Opportunity_Status2 = 'On Bid' then mean_on_bid_dur
                                            when Opportunity_Status2 = 'Down Select' then mean_down_select_dur
                                            when Opportunity_Status2 = 'Confirmed' then max_confirmed_dur
                                            when Opportunity_Status2 = 'Signed Deal' then mean_signed_deal_dur
                                            when Opportunity_Status2 = 'Lost' then mean_lost_dur
                                            when Opportunity_Status2 = 'Dropped' then mean_dropped_dur
                                            end)mean_historical_won_stage_duration from insights_open_opty")
#########
###Insights using Closed Date Movements, days and times
#########
#Direction
cd_movement <- sqldf("select 
                     opportunity_id,
                     (case 
                     when closedatemovementonPath = 1 then 'OnPath'
                     when closedatemovementPostpone = 1 then 'Postponed'
                     when closedatemovementPrepone = 1 then 'Preponed' end)cd_movement
                     from insights_open_opty")
########
###Insights w.r.t opportunity owner
########
opty_owner <- sqldf("select opportunity_owner,
                    round(sum_TCV/opty_cnt)AVG_TCV
                    from
                    (
                    select
                    opportunity_owner,
                    count(distinct(opportunity_id))opty_cnt,
                    sum(tcv)sum_TCV
                    from sfdcoptydat
                    where stage = '6. Signed Deal'
                    group by opportunity_owner) ")
# opty_owner <- sqldf("select opportunity_owner,
#                     round(sum_TCV/opty_cnt)AVG_TCV
#                     from
#                     (
#                     select
#                     opportunity_owner,
#                     count(distinct(opportunity_id))opty_cnt,
#                     sum(tcv)sum_TCV
#                     from sfdcdat2
#                     where stage = '6. Signed Deal'
#                     group by opportunity_owner) ")

open_opty_owners <- sqldf("select 
                          distinct opportunity_id,
                          opportunity_owner 
                          from openoptys 
                          group by opportunity_id")

open_opty_owner_TCV <- merge(open_opty_owners,opty_owner, by=c("opportunity_owner"))
chk_for2owners <- sqldf("select opportunity_id, 
                        count(opportunity_owner)owner_cnt 
                        from open_opty_owner_TCV 
                        group by opportunity_id")

##One Owner
open_opty_one_owners <- sqldf("select * 
                              from open_opty_owner_TCV 
                              where opportunity_id 
                              in
                              (select opportunity_id from chk_for2owners where owner_cnt=1)")

##Multi Owner
open_opty_multi_owners <- sqldf("select * 
                                from open_opty_owner_TCV 
                                where opportunity_id 
                                in(select opportunity_id from chk_for2owners where owner_cnt>1)")
if(length(open_opty_multi_owners$AVG_TCV)==0)
{open_opty_owner_TCV} else
{
    open_opty_multi_owners_1 <- open_opty_multi_owners[,c("opportunity_id","opportunity_owner")]
    open_opty_multi_owners_1$Opportunity_Owner <- as.character(open_opty_multi_owners_1$opportunity_owner)
    open_opty_multi_owners_SR <- aggregate( .~ opportunity_id, open_opty_multi_owners_1, function(x) toString(unique(x)))
    open_opty_multi_owners_avg_tcv <- sqldf("select distinct opportunity_id, avg(AVG_TCV)AVG_TCV from open_opty_multi_owners")    
    open_opty_multi_owners_2 <- merge(open_opty_multi_owners_avg_tcv,open_opty_multi_owners_SR, by=c("opportunity_id"))
    open_opty_owner_TCV <- rbind(open_opty_one_owners,open_opty_multi_owners_2)
}


#######
insights_open_opty_1 <- merge(insights_open_opty,curr_stage_duration, by=c("opportunity_id"))
insights_open_opty_2 <- merge(min_historical_won_stage_duration,max_historical_won_stage_duration,by = c("opportunity_id"))
insights_open_opty_3 <- merge(insights_open_opty_2, mean_historical_won_stage_duration, by = c("opportunity_id"))                              
insights_open_opty_4 <- merge(insights_open_opty_1,insights_open_opty_3, by = c("opportunity_id"))                            
insights_open_opty_5 <- merge(insights_open_opty_4,cd_movement, by = c("opportunity_id")) 
insights_open_opty_6 <- sqldf('select a.*, 
                              b.opportunity_owner,b.AVG_TCV 
                              from insights_open_opty_5 a 
                              left outer join open_opty_owner_TCV b 
                              on a.opportunity_id = b.opportunity_id')

expected_days_before_closing <- setDT(openoptys[,c("opportunity_id","days_remaining_from_msa")])[order(-days_remaining_from_msa, opportunity_id),
                                                                                                 head(.SD, 1), by = opportunity_id]
insights_open_opty_6$opportunity_id <- as.character(insights_open_opty_6$opportunity_id)
expected_days_before_closing$opportunity_id <- as.character(expected_days_before_closing$opportunity_id)

insights_open_opty_7 <- inner_join(insights_open_opty_6,expected_days_before_closing)
insights_open_opty_8 <- merge(insights_open_opty_7,curr_sd_stage_duration, by = c("opportunity_id")) 

colnames(insights_open_opty_8) <- tolower(colnames(insights_open_opty_8))
insights_open_opty_8[is.na(insights_open_opty_8)] <- 0

# insights1 <- sqldf("select
#                    'Text' as Description,
#                    opportunity_id,
#                    ('The current open Opportunity has an age of'||' '||sales_cycle||'days. Historical, similar WON Opportunities had average sales cycle of'||' '||round(avg_sales_cycle)||'days.' )sales_Cycle_comparison,
#                    ('The current opportunity is in'||' '||opportunity_status2||' Stage for'||' '||curr_stage_duration||'days,the average'||' '||opportunity_status2||' stage duration for the historical similar WON deals is'||' '||round(mean_historical_won_stage_duration)||'.')Curr_Stage_comparison,
#                    ('The MSA/SOW Closure date for the Opportunity is'||' '||cd_movement||' '||number_of_times_moved||' times from the initial date mentioned, the closure date is moved by '||closeddatemovementdays||' days. In comparison, the historical similar WON deals were moved '||round(mean_times_moved)||' times by '||round(mean_movement_days)||' days.')closed_date_movements,
#                    (case when tcv_usd > avg_tcv then 'This deal is unusual compared to the average deal size won by '||opportunity_owner||'. The Usual average deal size won by '||opportunity_owner||' is $'||avg_tcv||'.' else ' The current deal is within the average historical WON deal size of '||opportunity_owner||'.which is $'||avg_tcv||'.' end)Sales_Rep,                   
#                    ('The TCV for the current opportunity is'||' $'||tcv_usd||',historical similar WON opportunities have TCV ranging between'||' $'||min_tcv_usd||' '||'and $'||max_tcv_usd||'.')TCV_Comparison
#                    from
#                    insights_open_opty_6")
# insights1 <- sqldf("select
#                    'Text' as Description,
#                    opportunity_id,
#                    ('Deal is in the pipeline since'||' '||sales_cycle||'days, compared to similar won deals in the past that had a total sales cycle time of'||' '||round(avg_sales_cycle)||'days. You may want to move the deal to next stage or move the MSA/SOW closure date ahead. If deal is no longer active, you may want to drop the deal.')sales_Cycle_comparison,
#                    ('Deal is at'||' '||opportunity_status2||' Stage since'||' '||curr_stage_duration||'days,compared to similar won deals in the past that had an ageing of'||' '||round(mean_historical_won_stage_duration)||' at '||' '||opportunity_status2||'.You may want to move the deal to next stage.')Curr_Stage_comparison,
#                    ('The MSA/SOW Closure date for the Opportunity is'||' '||cd_movement||' '||number_of_times_moved||' times from the initial date mentioned, the closure date is moved by '||closeddatemovementdays||' days. In comparison, the historical similar WON deals were moved '||round(mean_times_moved)||' times by '||round(mean_movement_days)||' days.')closed_date_movements,
#                    (case when tcv_usd > avg_tcv then 'This deal is unusual compared to the average deal size won by '||opportunity_owner||'. The Usual average deal size won by '||opportunity_owner||' is $'||avg_tcv||'.' else ' The current deal is within the average historical WON deal size of '||opportunity_owner||'.which is $'||avg_tcv||'.' end)Sales_Rep,
#                    ('Deal value is'||' $'||tcv_usd||'compared to similar won deals in the past that had a value ranging between'||' $'||min_tcv_usd||' '||'and $'||max_tcv_usd||'. You may want to check and correct the deal value.')TCV_Comparison
#                    from
#                    insights_open_opty_6")
# insights1 <- sqldf("select
#                    'Text' as Description,
#                    opportunity_id,
#                    ('Deal is in the pipeline since'||' '||sales_cycle||'days, compared to similar won deals in the past that had a total sales cycle time of'||' '||round(avg_sales_cycle)||'days. You may want to move the deal to next stage or move the MSA/SOW closure date ahead. If deal is no longer active, you may want to drop the deal.')sales_Cycle_comparison,
#                    ('Deal is at'||' '||opportunity_status2||' Stage since'||' '||curr_stage_duration||'days,compared to similar won deals in the past that had an ageing of'||' '||round(mean_historical_won_stage_duration)||' at '||' '||opportunity_status2||'.You may want to move the deal to next stage.')Curr_Stage_comparison,
#                    ('The MSA/SOW Closure date for the Opportunity is'||' '||cd_movement||' '||number_of_times_moved||' times from the initial date mentioned, the closure date is moved by '||closeddatemovementdays||' days. In comparison, the historical similar WON deals were moved '||round(mean_times_moved)||' times by '||round(mean_movement_days)||' days.')closed_date_movements,
#                    (case when tcv_usd > avg_tcv then 'This deal is unusual compared to the average deal size won by '||opportunity_owner||'. The Usual average deal size won by '||opportunity_owner||' is $'||avg_tcv||'.' else ' The current deal is within the average historical WON deal size of '||opportunity_owner||'.which is $'||avg_tcv||'.' end)Sales_Rep,
#                    ('MSA/SOW closure date is just'||' '||days_remaining_from_msa||' '||'days ahead; similar won deals in the past took'||' '||round(mean_sales_cycle)||' '||'days to close from the current sales stage. You may want to move the MSA/SOW closure date ahead.')MSA_SOW_check_closedate,
#                    ('Deal value is'||' $'||tcv_usd||'compared to similar won deals in the past that had a value ranging between'||' $'||min_tcv_usd||' '||'and $'||max_tcv_usd||'. You may want to check and correct the deal value.')TCV_Comparison
#                    from
#                    insights_open_opty_7")

library(sqldf)
insights1 <- sqldf("select
                   'Text' as Description,
                   opportunity_id,
(case when sales_cycle > avg_sales_cycle then 'Deal is in the pipeline since'||' '||sales_cycle||' '||'days, compared to similar won deals in the past that had a total sales cycle time of'||' '||round(avg_sales_cycle)||' '||'days. You may want to move the deal to next stage or move the MSA/SOW closure date ahead. If deal is no longer active, you may want to drop the deal.' else NULL end)Sales_Cycle_Comparison,
(case when curr_stage_duration > mean_historical_won_stage_duration then 'Deal is at'||' '||opportunity_status2||' Stage since'||' '||curr_stage_duration||' '||'days,compared to similar won deals in the past that had an ageing of'||' '||round(mean_historical_won_stage_duration)||' at '||' '||opportunity_status2||'.You may want to move the deal to next stage.' else NULL end)Curr_Stage_Comparison,
('The MSA/SOW Closure date for the Opportunity is'||' '||cd_movement||' '||number_of_times_moved||' times from the initial date mentioned, the closure date is moved by '||closeddatemovementdays||' days. In comparison, the historical similar WON deals were moved '||round(mean_times_moved)||' times by '||round(mean_movement_days)||' days.')Closed_Date_Movements,
(case when tcv_usd > avg_tcv then 'This deal is unusual compared to the average deal size won by '||opportunity_owner||'. The Usual average deal size won by '||opportunity_owner||' is $'||avg_tcv||'.' else ' The current deal is within the average historical WON deal size of '||opportunity_owner||'.which is $'||avg_tcv||'.' end)Sales_Rep,
(case when days_remaining_from_msa < curr_sd_stage_duration then 'MSA/SOW closure date is just'||' '||days_remaining_from_msa||' '||'days ahead; similar won deals in the past took'||' '||round(curr_sd_stage_duration)||' '||'days to close from the current sales stage. You may want to move the MSA/SOW closure date ahead.' else NULL end)MSA_SOW_Check_Closedate,
(case when tcv_usd not between min_tcv_usd and max_tcv_usd then 'Deal value is'||' $'||tcv_usd||' '||'compared to similar won deals in the past that had a value ranging between'||' $'||round(min_tcv_usd,2)||' '||'and $'||round(max_tcv_usd,2)||'. You may want to check and correct the deal value.'  else NULL end)TCV_Comparison
from insights_open_opty_8")    

Insights2_pivot <- recast(insights1, opportunity_id + variable ~ Description, id.var = c("opportunity_id", "Description"))


# Selecting only one row of each Opp in SFDC dump file
# as we have seen multiple entries for single opp ID
dim(sfdcdat2)
colnames(sfdcdat2)

sfdcdat3 <- sfdcdat2[!duplicated(sfdcdat2$opportunity_id),]

# Left join with the output table in order to get opp_id from sfdc dump file
Insights2_pivot_2 <- left_join(Insights2_pivot,sfdcdat3[c("opp_id","opportunity_id")])

# Dropping OLID columns and changing the structure of the table
Insights2_pivot_2$opportunity_id <- NULL
Insights2_pivot_2 <- Insights2_pivot_2[,c(3,1,2)]
colnames(Insights2_pivot_2) <- c("Opportunity_ID","Variable","Description")

Insights2_pivot_3 <- na.omit(Insights2_pivot_2)

# Select data.frame to be sent to the output Dataset port


write.csv(Insights2_pivot_3,"Insights2_pivot____________________final___________new.csv", row.names = F)


