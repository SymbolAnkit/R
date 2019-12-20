


# packages_needed <- c("forecast","nonlinearTseries","urca","tseries","EMD","rEDM","GA","stats","e1071","rugarch")
# install_lib<-packages_needed[!packages_needed %in% installed.packages()]
# for(lib in install_lib) install.packages(lib,dependencies=TRUE)
install.packages("GA")
install.packages("EMD") 
install.packages("rEDM") 
install.packages("stats") 
install.packages("e1071") 
install.packages("forecast") 
install.packages("rugarch") 
install.packages("tseries")
install.packages("nonlinearTseries")

library(GA)
library(EMD)
library(rEDM)
library(stats)
library(e1071)
library(forecast)
library(rugarch)
library(reshape)
library(reshape2)
library(tseries)
library(nonlinearTseries)

inflow <- read.csv("104_weeks_New.csv", header = T)
colnames(inflow) <- gsub("[[:punct:]]","_" , colnames(inflow))
inflow$Prediscover_Opportunity_Creation_Date <- as.Date(inflow$Prediscover_Opportunity_Creation_Date, format = "%m/%d/%Y")
inflow$Created_date <- as.Date(inflow$Created_date, format = "%m/%d/%Y")
Mod_Industry_Vertical <- data.frame("Industry_Vertical" = c("BFS","CPG","Capital Markets","Healthcare","High Tech","Insurance","Life Sciences","Manufacturing","Retail","Services"), "New_Industry_Vertical" = c("BFS_CM","CPG_RET","BFS_CM","LS_HC","HighT","Insurance","LS_HC","MAN_SERV","CPG_RET","MAN_SERV"))
inflow2 <- merge(inflow,Mod_Industry_Vertical, by=c("Industry_Vertical"))          
inflow2$Nature_Of_work <- toupper(inflow2$Nature_Of_work)
inflow2 <- subset(inflow2, Nature_Of_work !="SUPPORT")
inflow_5 <- inflow2[,c("Nature_Of_work","TCV__USD_","New_Industry_Vertical","Prediscover_Opportunity_Creation_Date")]
colnames(inflow_5) <- c("Nature_of_Work","TCV_USD","Industry_Vertical","Prediscover_Opportunity_Creation_Date")

inflow_5$weekday <- strftime(inflow_5$Prediscover_Opportunity_Creation_Date,'%u')
inflow_5$weeknum <- strftime(inflow_5$Prediscover_Opportunity_Creation_Date,'%V')
inflow_5$Year <- strftime(inflow_5$Prediscover_Opportunity_Creation_Date,'%Y')
inflow_5$Month <- strftime(inflow_5$Prediscover_Opportunity_Creation_Date,'%m')
inflow_Yearweek <- sqldf('Select *,(Year||weeknum)WeekYear from inflow_5 ')
inflow_Yearmonth <- sqldf('Select *,(Year||Month)Yearmonth from inflow_5 ')
inflow_Yearweek_2 <- sqldf("Select distinct Weekyear, Industry_Vertical, Nature_of_Work, sum(TCV_USD) TCV 
                           from 
                           inflow_Yearweek 
                           group by 
                           Weekyear, Industry_Vertical, Nature_of_Work")
inflow_Yearweek_2$weeklogval <- log10(inflow_Yearweek_2$TCV)
inflow_Yearweek_2$weeksqrtval <- sqrt(inflow_Yearweek_2$TCV)
inflow_Yearweek_2$weekcbrtval <- inflow_Yearweek_2$TCV^(1/3)
##Discontinuous occasions
#Weekly
Disc_weekly1 <- sqldf("Select count(Industry_Vertical)over from inflow_cnt_weeks")
Disc_weekly2 <- sqldf("Select count(Industry_Vertical) from inflow_cnt_weeks where week_data < 104")
Disc_weekly_prop <- (Disc_weekly2/ Disc_weekly1)* 100

## Considering past 2 years worth of data for modelling from First week of 2017 to 52nd week of 2018

inflow_weeks <- sqldf("select * from inflow_Yearweek_2 where WeekYear between '201701' and '201953'")
inflow_weeks2 <- sqldf("select *, (Industry_Vertical||Nature_of_Work)Primary_key from inflow_weeks")
inflow_weeks3 <- inflow_weeks2[,c("WeekYear","weeklogval","Primary_key")]
inflow_weeks4 <- inflow_weeks2[,c("WeekYear","TCV","Primary_key")]
inflow_weeks5 <- inflow_weeks2[,c("WeekYear","weeksqrtval","Primary_key")]
inflow_weeks6 <- inflow_weeks2[,c("WeekYear","weekcbrtval","Primary_key")]
t1 <-cast(inflow_weeks4,WeekYear ~ Primary_key, value = "TCV")
t2 <- replace(t1, is.na(t1), 0)
inflowweek_cast <- cast(inflow_weeks6,WeekYear ~ Primary_key, value = "weekcbrtval")
inflowweek_cast_1 <- replace(inflowweek_cast,is.na(inflowweek_cast),0)

##Doing for BFS_CM and Analytics
t2_1 <- t2[,1:2] # Using data as it is
genrev_ts <- ts(t2_1$BFS_CMANALYTICS,frequency = 52,start = c(2017, 1))
ts_decompose <- stl(genrev_ts, s.window="periodic")
ts_decompose_df <- as.data.frame(ts_decompose$time.series)


inflowweek_cast_2 <- inflowweek_cast_1[,1:2] # Variance in data is high hence applying cuberoot transformation.
genrev_ts_cbrt <- ts(inflowweek_cast_2$BFS_CMANALYTICS,frequency = 52,start = c(2017, 1))
ts_decompose_cbrt <- stl(genrev_ts_cbrt, s.window="periodic")
ts_decompose_df_cbrt <- as.data.frame(ts_decompose_cbrt$time.series)

### Checking for Stationarity
###Augmented Dickey Fuller test indicates that a smaller value of p is stationary. The same is Vice-versa for KPSS test
adf_rev <- adf.test(genrev_ts_cbrt)
adf_pvalue <- adf_rev$p.value
if(adf_pvalue>0.05){ts_stationarity <- 'NonStationary'} else{ts_stationarity <- 'Stationary'}
#kpss_rev <- kpss.test(genrev_ts)
#kpss_pvalue <- (kpss_rev$p.value)
#if(kpss_pvalue<0.05){ts_stationarity <- 'NonStationary'} else{ts_stationarity <- 'Stationary'}

## Checking for nonlinearity in the time series data...
#Nonlineartest_rev <- nonlinearityTest(genrev_ts)
sdt <- surrogateTest(time.series = genrev_ts_cbrt,significance = 0.05,FUN=timeAsymmetry) 

if(max(sdt$surrogates.statistics) < sdt$data.statistic){ts_linearity <- 'NonLinear'} else{ts_linearity <- 'Linear'}

###Fitting ARIMA Post Transformation
nARIMA_Train_dataset <- floor(nrow(inflowweek_cast_2)*0.8)
nARIMA_Test_dataset <- nARIMA_Train_dataset+1
nARIMA_Train_dataset2 <- inflowweek_cast_2[1:nARIMA_Train_dataset,] 
nARIMA_Test_dataset2 <- inflowweek_cast_2[nARIMA_Test_dataset:nrow(inflowweek_cast_2),]
t2_2 <- auto.arima(nARIMA_Train_dataset2$BFS_CMANALYTICS)
t2_predict <- forecast(t2_2, h=(nrow(inflowweek_cast_2)-nARIMA_Test_dataset+1))
nARIMA_error <- nARIMA_Test_dataset2$BFS_CMANALYTICS - t2_predict$mean
nARIMA_sqerror <- nARIMA_error*nARIMA_error
nARIMA_msqerror <- mean(nARIMA_sqerror)
nARIMA_rmsqerror <- sqrt(nARIMA_msqerror)

##################
#########################Using Alternate Method for Time Series Forecasting######################
###################
rev_uts <- inflowweek_cast_2[,c(1,2)]
dat_norm <- function(x){(x-min(x))/(max(x)-min(x))}
#rev_uts$datscale <-dat_norm(rev_uts$BFS_CMANALYTICS.TCV)##Test Direct
rev_uts$datscale <-(rev_uts$BFS_CMANALYTICS)
ordered_uts <- rev_uts[order(as.Date(rev_uts[,1],format="%Y%d")),]
#Using Empirical mode decomposition technique splitting the data into IMFs and Residue
rev_uts_emd <- emd(rev_uts$datscale, boundary="wave")
series_imfs_rev_uts <- rev_uts_emd[[1]]
series_residue_rev_uts <- rev_uts_emd[[2]]
nimfs_rev_uts <- rev_uts_emd[[3]]
Imfs_residue_matrix_comb <- cbind(series_imfs_rev_uts,as.matrix(series_residue_rev_uts))
###Plotting the IMFs
par(mfrow=c(4, 1), mar=c(3,1,1,1))
plot(rev_uts$datscale, type="l")
plot(series_imfs_rev_uts[,1], type="l")
plot(series_imfs_rev_uts[,2], type="l")
plot(series_imfs_rev_uts[,3], type="l")
plot(series_imfs_rev_uts[,4], type="l")
plot(series_imfs_rev_uts[,5], type="l")
plot(series_residue_rev_uts, type="l")

trainset_nrow <- floor(nrow(Imfs_residue_matrix_comb)*0.8)
testset_nrow <- trainset_nrow+1
test_dat <- rev_uts[testset_nrow:nrow(rev_uts),]
###Training set data
trainset_list_imfs_residue <- list()
for (i in 1:ncol(Imfs_residue_matrix_comb))
{uts_trainset <- Imfs_residue_matrix_comb[1:trainset_nrow,i]
trainset_list_imfs_residue[[i]] <- uts_trainset}
### Testing data set
testset_list_imfs_residue <- list()
for (i in 1:ncol(Imfs_residue_matrix_comb))
{uts_testset <- Imfs_residue_matrix_comb[testset_nrow:nrow(Imfs_residue_matrix_comb),i]
testset_list_imfs_residue[[i]] <- uts_testset}
####Model building using ARIMA
model_build_uts <- list()
model_build_uts <- lapply(trainset_list_imfs_residue,function(x){auto.arima(x)})
model_predict_uts <- list()
for (i in 1:ncol(Imfs_residue_matrix_comb))
{uts_predict <- forecast(model_build_uts[[i]], h=(nrow(Imfs_residue_matrix_comb)-testset_nrow+1))
model_predict_uts[[i]] <- uts_predict}
model_predict_uts_df <- as.data.frame(model_predict_uts)
model_predict_uts_df$row_name <- row.names(model_predict_uts_df)
Point_Estimate_Model_Predict <- sqldf("select row_name,
                                      (`Point.Forecast`+`Point.Forecast.1`+`Point.Forecast.2`+`Point.Forecast.3`+`Point.Forecast.4`+`Point.Forecast.5`) Point_Estimate
                                      from
                                      model_predict_uts_df")
emd_ARIMA_error <- test_dat$BFS_CMANALYTICS - Point_Estimate_Model_Predict$Point_Estimate
emd_ARIMA_sqerror <- emd_ARIMA_error*emd_ARIMA_error
emd_ARIMA_msqerror <- mean(emd_ARIMA_sqerror)
emd_ARIMA_RMSE <- sqrt(emd_ARIMA_msqerror)
emd_ARIMA_cor <- data.frame(test_dat$BFS_CMANALYTICS, Point_Estimate_Model_Predict$Point_Estimate)
################################################################################
##############################USING SVR+GA for forecasting######################
dat_nrow <- nrow(Imfs_residue_matrix_comb)
dat_split <- floor(dat_nrow/2)

E_imf <- 0
for(i in 1:ncol(Imfs_residue_matrix_comb))
{
  dat_sim <- simplex(Imfs_residue_matrix_comb[,i],lib=c(1,dat_split),pred=c((dat_split+1),dat_nrow),E=c(2:10))
  E_imf[i] <-dat_sim[which.max(dat_sim$rho),"E"][1]
}

listofdfs <- list()
for(i in 1:ncol(Imfs_residue_matrix_comb))
{
  dat_embed <- embed(Imfs_residue_matrix_comb[,i],E_imf[i])
  listofdfs[[i]] <- dat_embed
}

# Coming up with the Train and test split for the data..to maintain the same time we are taking 
#the IMF with min number of records post embedding and then use the prediction horizon to come up
# with the train and test split
embed_row_count <- 0
for(i in 1:ncol(Imfs_residue_matrix_comb))
{
  embed_row_count[i] <- nrow(listofdfs[[i]])
}
min_embed_record <- min(embed_row_count)
#train_set_till<- min_embed_record - prediction_horizon
train_set_till<- floor(dim(Imfs_residue_matrix_comb)[[1]]*.8)
prediction_horizon <- floor(dim(Imfs_residue_matrix_comb)[[1]]*.2)

test_set_from <- train_set_till +1
test_set_till <- test_set_from + prediction_horizon
#Converting list of list in the listofdfs data to list of dataframes
listtodfs <- list() ###Lets check####
for (i in 1:length(listofdfs)) 
{
  listtodfs[[i]] <- as.data.frame(listofdfs[[i]])
  
}
#Modelling building and predicting with Support Vector machines and applying Genetic Algorithm for coming up with

#optimal epsilon, gamma and cost values
cost_function <- lapply(listtodfs,function(x){
  
  fitness_func <- function(b,dat)
  {
    cost_par <- b[1]
    gamma_par <- b[2]
    epsilon_par <- b[3]
    tdat <- dat
    ntdat <- nrow(dat)
    ntrdat <- floor(dim(dat)[[1]]*.8)
    trdat <- tdat[1:ntrdat,]
    EDim <- ncol(trdat)
    modelbuildSVM <- svm(trdat[,1]~., data = trdat[-1],cost=cost_par, gamma=gamma_par, epsilon=epsilon_par,kernel = "radial",type="eps-regression")
    modcolnames <- colnames(trdat[-1])
    
    newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,1]
    newrecord_embed <- embed(newrecordpred,(EDim-1))
    newrecord_embed_df <- as.data.frame(newrecord_embed)
    colnames(newrecord_embed_df) <- modcolnames
    pred_newrecord_embed_df <- predict(modelbuildSVM,newrecord_embed_df)
    trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
    ntrdat <- nrow(trdat)
    
    while(ntrdat <ntdat){
      newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,]
      newrecord_embed <- embed(newrecordpred,(EDim-1))
      newrecord_embed_df <- as.data.frame(newrecord_embed)
      colnames(newrecord_embed_df) <- modcolnames
      pred_newrecord_embed_df <- predict(modelbuildSVM,newrecord_embed_df)
      trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
      ntrdat <- nrow(trdat)
    }
    
    error <- tdat[,1]-trdat[1:nrow(tdat),1]
    error <- subset(error,error!=0)
    sqerror <- error*error
    msqerror <- mean(sqerror)
    rmsqerror <- sqrt(msqerror)
    return(-rmsqerror)
  }
  ga.svm <- ga(type = 'real-valued', lower = c(cost = 0,gamma = 0,epsilon = 0), upper = c(cost = 50,gamma = 50,epsilon = 0.01),
               popSize =max(500, floor(train_set_till/2)),maxiter = 50, names = c("cost","gamma","epsilon"),keepBest = T,
               fitness = fitness_func,x)
  
  ga_optimal <- ga.svm@solution
}
)

cost_function2  <- list()
for(i in 1: length(cost_function))
{
  cf <- cost_function[[i]][1,]
  cost_function2[[i]] <- as.matrix(cf)
}

svm_imfspredict <- list()
for(i in 1:length(listtodfs))
{
  tdat <- listtodfs[[i]]
  ntdat <- nrow(tdat)
  ntrdat <- floor(dim(tdat)[[1]]*.8)
  trdat <- tdat[1:ntrdat,]
  EDim <- ncol(trdat)
  
  modelSVM <- svm(trdat[,1]~., data = trdat[-1],cost=cost_function2[[i]][1], gamma=cost_function2[[i]][2], epsilon=cost_function2[[i]][3],kernel = "radial",type="eps-regression")
  modcolnames <- colnames(trdat[-1])
  
  newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,1]
  newrecord_embed <- embed(newrecordpred,(EDim-1))
  newrecord_embed_df <- as.data.frame(newrecord_embed)
  colnames(newrecord_embed_df) <- modcolnames
  pred_newrecord_embed_df <- predict(modelSVM,newrecord_embed_df)
  trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
  ntrdat <- nrow(trdat)
  
  while(ntrdat <ntdat){
    newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,]
    newrecord_embed <- embed(newrecordpred,(EDim-1))
    newrecord_embed_df <- as.data.frame(newrecord_embed)
    colnames(newrecord_embed_df) <- modcolnames
    pred_newrecord_embed_df <- predict(modelSVM,newrecord_embed_df)
    trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
    ntrdat <- nrow(trdat)
  }
  actual_imf <- tdat[,1]
  act_pred_imf <- trdat[1:nrow(tdat),]
  svm_imfspredict[[i]] <- data.frame(actual_imf,act_pred_imf)
  error <- (actual_imf- act_pred_imf)
  error <- subset(error,error!=0)
  sqerror <- error*error
  msqerror <- mean(sqerror)
  rmsqerror <- sqrt(msqerror)
}

row_nam <- list()
for(i in 1:length(E_imf))
{
  row_nam[[i]]<- (E_imf[i]:(nrow(svm_imfspredict[[i]])+(E_imf[i]-1)))
}
imfs_act_pred <- Map(cbind,svm_imfspredict,row_names=row_nam)
imfs_in_one <- do.call(rbind,imfs_act_pred)
sum_imfs <- aggregate(imfs_in_one[,1:2],by=list(imfs_in_one$row_names),sum)
sum_imfs$error_imfs <- (sum_imfs$actual_imf - sum_imfs$act_pred_imf)
error_imfs <- subset(sum_imfs,sum_imfs$error_imfs!=0)
testset_post_mod_from <- min(error_imfs[,1])
testset_post_mod_till <- max(error_imfs[,1])
sqerror_imfs <- error_imfs$error_imfs*error_imfs$error_imfs
msqerror_imfs <- mean(sqerror_imfs)
rmsqerror_svr_imfs <- sqrt(msqerror_imfs)
###########
###Predictions for the future

##########
svm_imfspredict_fut <- list()
for(i in 1:length(listtodfs))
{
  tdat <- listtodfs[[i]]
  ntdat <- nrow(tdat)+12 # if its monthly data 12 else 4 for quarter
  ntrdat <- nrow(tdat)
  trdat <- tdat[1:ntrdat,]
  EDim <- ncol(trdat)
  
  modelSVM <- svm(trdat[,1]~., data = trdat[-1],cost=cost_function2[[i]][1], gamma=cost_function2[[i]][2], epsilon=cost_function2[[i]][3],kernel = "radial",type="eps-regression")
  modcolnames <- colnames(trdat[-1])
  
  newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,1]
  newrecord_embed <- embed(newrecordpred,(EDim-1))
  newrecord_embed_df <- as.data.frame(newrecord_embed)
  colnames(newrecord_embed_df) <- modcolnames
  pred_newrecord_embed_df <- predict(modelSVM,newrecord_embed_df)
  trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
  ntrdat <- nrow(trdat)
  
  while(ntrdat <ntdat){
    newrecordpred <- trdat[(ntrdat-(EDim-2)):ntrdat,]
    newrecord_embed <- embed(newrecordpred,(EDim-1))
    newrecord_embed_df <- as.data.frame(newrecord_embed)
    colnames(newrecord_embed_df) <- modcolnames
    pred_newrecord_embed_df <- predict(modelSVM,newrecord_embed_df)
    trdat <- rbind(as.data.frame(trdat[,1]),pred_newrecord_embed_df)
    ntrdat <- nrow(trdat)
  }
  
  act_pred_imf <- trdat
  svm_imfspredict_fut[[i]] <- data.frame(act_pred_imf)
  
}
row_nam_fut <- list()
for(i in 1:length(E_imf))
{
  row_nam_fut[[i]]<- (E_imf[i]:(nrow(svm_imfspredict_fut[[i]])+(E_imf[i]-1)))
}
imfs_future <- Map(cbind,svm_imfspredict_fut,row_names=row_nam_fut)
imfs_in_one_future <- do.call(rbind,imfs_future)
sum_imfs_future <- aggregate(imfs_in_one_future[,1],by=list(imfs_in_one_future$row_names),sum)
###Coming up with prediction intervals...... at 95% confidence Level...
sum_imfs_future$Lower_Bound_95 <- (sum_imfs_future$x - (1.96*rmsqerror_svr_imfs))
sum_imfs_future$Upper_Bound_95 <- (sum_imfs_future$x + (1.96*rmsqerror_svr_imfs))
sum_imfs_future$BT_Point_Estimate <- sum_imfs_future$x
sum_imfs_future$BT_Lower_Bound_95 <- sum_imfs_future$Lower_Bound_95
sum_imfs_future$BT_Upper_Bound_95 <- sum_imfs_future$Upper_Bound_95
Final_prediction_AFModel <- sum_imfs_future[,c("BT_Point_Estimate","BT_Lower_Bound_95","BT_Upper_Bound_95")] 


