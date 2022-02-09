library(spdep)
library(starma)
library(tmap)
library(janitor)
library(Metrics)
library(stats)
library(forecast)


### Create the spatial weight matrix ###
# load shape of police precicts with bridges
admin_data_b <- read_sf("*/Input_Data/NY_PP/nypp_bridges.shp")
pp_b_sf <- pp_b  %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
pp_b <- admin_data_b[,c(1,3,4)]
names(pp_b)[names(pp_b) == 'POLY_ID'] <- 'Precinct'

# transpose data for input in GeoDa (row-wise are precincts, column time)
cr_pp_all_ts_y <- dcast(cr_pp_all_y, Precinct~tp, value.var = "sumall") #transpose for crime type all
cr_pp_all_ts_y$Precinct <- 1:77
cr_pp_p_ts_y <- dcast(cr_pp_p_y, Precinct~tp, value.var = "sump") #transpose for crime type p
cr_pp_p_ts_y$Precinct <- 1:77
cr_pp_v_ts_y <- dcast(cr_pp_v_y, Precinct~tp, value.var = "sumv") #transpose for crime type v
cr_pp_v_ts_y$Precinct <- 1:77

# write output file
cr_pp_poly <- merge(cr_pp_all_ts_y,pp_b, by="Precinct")
cr_pp_poly <- merge(cr_pp_p_ts_y,pp_b, by="Precinct")
cr_pp_poly <- merge(cr_pp_v_ts_y,pp_b, by="Precinct")
st_write(cr_pp_poly, "*/Outputs/Input_Data/R_Outputs/GeoDa_Xplore/cr_pp_w.shp", append=FALSE)


k2_nb <- knearneigh(pp_XY_mtx, k=2)
k2_nb <- knn2nb(k2_nb)
k2_nb <- nblag(k2_nb, 2)
k2_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
                order1=nb2mat(k2_nb[[1]], zero.policy=TRUE),
                order2=nb2mat(k2_nb[[2]], zero.policy=TRUE))

### STARMA modeling ###

# The process is repeated for each crime type:

# Normalize the data
input_data_starma <- cr_pp_sts_w_p [,c(2:78)] #paste the transpose data frame here, the date column will be dropped
colnames(input_data_starma) <- c(1:77) #rename the police precincts into numbers from 1 to 77
cr_norm <- stcenter(input_data_starma) #stcenter centers and scales the space-time series data such that its mean is 0 and its standard error 1.
timesteps <- nrow(input_data_starma)

# insert spatial weights list here
wlist <- k1_list

# 1 Identification: Using stacf and stpacf, the user should try to identify which parameters should be estimated.
I_stacf <- stacf(cr_norm, wlist) 
I_stpacf <- stpacf(cr_norm, wlist)

# 2 Estimation: Use starma to estimate the parameters.
# set AR and MA parameters to 1 for the first run
ar <- 1
ma <- 1

# AR parameters
ar <- matrix(0, 12, 2) #row -th tlag #col -th slag
ar[1,1] <- 1 #set AR parameter of spatial lag 0
ar[1,2] <- 1 #set AR parameter of spatial lag 1

# MA parameters
ma <- matrix(0, 12, 2) #row -th tlag #col -th slag
ma[3,1] <-1 #set MA parameter of spatial lag 0
ma[1,2] <- 1 #set AR parameter of spatial lag 1

# Run the Kalman filter algorithm
model <- starma(cr_norm, wlist, ar, ma, iterate=5)
model
summary(model)
D_stpacf <- stpacf(model$residuals, wlist, tlag.max = 53)

# 3 Diagnose the process. Go back if the residuals show autocorrelation
D_stacf <- stacf(model$residuals, wlist)
D_stpacf <- stpacf(model$residuals, wlist)
D_stcor_test <- stcor.test(model$residuals, wlist)

#Calculate error metrics

res_starma <- model$residuals
cr_actl <- t(cr_norm)
cr_prdct <- res_starma + cr_norm 
cr_prdct <- t(cr_prdct)
cr_actl <-  cr_actl[,c(5:timesteps)] 
cr_prdct <- cr_prdct[,c(5:timesteps)] 
acc_starma <- data.frame(matrix(NA, ncol = 3))
colnames(acc_starma)<- c("RMSE","MAE","R2")

#Function for R squared
rsq <- function (x, y) cor(x, y) ^ 2

for(i in 1:nrow(cr_prdct))  {
  temp_actl <- cr_actl [i,]
  temp_prdct <- cr_prdct [i,]
  temp_RMSE <- rmse(temp_actl, temp_prdct)
  temp_MAE <- mae(temp_actl, temp_prdct)
  temp_R2 <- rsq(temp_actl, temp_prdct)
  temp_tbl <- cbind(temp_RMSE, temp_MAE, temp_R2)
  colnames(temp_tbl)<- c("RMSE","MAE","R2")
  acc_starma <- rbind(acc_starma, temp_tbl)
  rm(temp_actl,temp_prdct,temp_RMSE,temp_R2,temp_tbl)
}
acc_starma <- acc_starma [c(2:78),]
rownames(acc_starma) <- 1:77

acc_starma_sum <- data.frame(matrix(NA, ncol = 6))
colnames(acc_starma_sum)<- c("RMSE","sd_RMSE","MAE","sd_MAE","R2","sd_R2")
acc_starma_sum$RMSE <- mean(acc_starma[,1])
acc_starma_sum$sd_RMSE <- sd(acc_starma[,1])
acc_starma_sum$MAE <- mean(acc_starma[,2])
acc_starma_sum$sd_MAE <- sd(acc_starma[,2])
acc_starma_sum$R2 <- mean(acc_starma[,3])
acc_starma_sum$sd_R2 <- sd(acc_starma[,3])

# Save model & data
STARMA_w_p <- model
AR_w_p <- ar
MA_w_p <- ma
stacf_w_p <- D_stacf
stpacf_w_p <- D_stpacf
stcor_test_w_p <- D_stcor_test
acc_starma_w_p <- acc_starma
acc_starma_sum_w_p <- acc_starma_sum

### ARIMA modeling ###

input_data_arima <- cr_pp_sts_y_v [,c(2:78)] #paste the transpose data frame here, the time column will be dropped
colnames(input_data_arima) <- c(1:77) #rename the police precincts into numbers from 1 to 77
input_data_arima<- stcenter(input_data_arima) #stcenter centers and scales the space-time series data such that its mean is 0 and its standard error 1.
timesteps <- nrow(input_data_arima)

acc_arima <- data.frame(matrix(NA, ncol = 3))# create a data frame for the calculate error metrics
colnames(acc_arima)<- c("RMSE","MAE","R2")
res_arima <- data.frame(matrix(NA, ncol = timesteps))

for(i in 1:ncol(input_data_arima)) {         #function that calculates an ARIMA model for all police precincts and then calculates the error metrics
  temp <- data.frame(input_data_arima[,i])
  colnames(temp) <- colnames(input_data_arima)[i]
  temp <- ts(temp, start = 1, end = timesteps, frequency = 1)
  temp_arima <- auto.arima(temp, max.order= 3)
  temp_res <- temp_arima$residuals
  temp_prdct <- temp_arima$residuals + temp 
  temp_actl <- as.vector(temp)
  temp_prdct <- as.vector(temp_prdct)
  temp_actl <-  temp_actl[c(4:timesteps)] 
  temp_prdct <- temp_prdct[c(4:timesteps)] 
  temp_RMSE <- rmse(temp_actl, temp_prdct)
  temp_MAE <- mae(temp_actl, temp_prdct)
  temp_R2 <- rsq(temp_actl, temp_prdct)
  temp_tbl <- cbind(temp_RMSE, temp_MAE, temp_R2)
  colnames(temp_tbl)<- c("RMSE","MAE","R2")
  acc_arima <- rbind(acc_arima, temp_tbl)
  res_arima <- rbind(res_arima, temp_res)
  rm(temp,temp_arima,temp_prdct,temp_RMSE,temp_R2,temp_tbl,temp_res)
}

# Calculate ARIMA error metrics
acc_arima <- acc_arima [c(2:78),]
rownames(acc_arima) <- 1:77
res_arima <- res_arima [c(2:78),]
rownames(res_arima) <- 1:77

acc_arima_sum <- data.frame(matrix(NA, ncol = 6))
colnames(acc_arima_sum)<- c("RMSE","sd_RMSE","MAE","sd_MAE","R2","sd_R2")
acc_arima_sum$RMSE <- mean(acc_arima[,1])
acc_arima_sum$sd_RMSE <- sd(acc_arima[,1])
acc_arima_sum$MAE <- mean(acc_arima[,2])
acc_arima_sum$sd_MAE <- sd(acc_arima[,2])
acc_arima_sum$R2 <- mean(acc_arima[,3])
acc_arima_sum$sd_R2 <- sd(acc_arima[,3])

# Save model & data
acc_arima_y_v <- acc_arima
acc_arima_sum_y_v <- acc_arima_sum
res_arima_y_v <- res_arima
view(acc_arima_sum_y_v)
