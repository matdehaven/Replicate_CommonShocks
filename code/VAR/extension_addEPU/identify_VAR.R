##
##  Extend VAR analysis from "Common Shocks..." paper
##  to 2022
##
##  Matthew DeHaven
##  2022 11 04
##
require(data.table)
require(lubridate)
require(vars)

tic <- function(){
  time_stamp <<- Sys.time()
}
toc <- function(){
  difftime(Sys.time(), time_stamp, units = "secs")
}

## The time sample used in the paper
date_range <- ymd(c("1983-01-01", "2022-12-31"))

shock_restriction_size <- 3
bm_year <- 1987 ## Black Monday
bm_month <- 10
lhb_year <- 2008 ## Lehman collapse
lhb_month <- 09
gfc_start <- ymd("2007-12-01") ## GFC Period
gfc_end <- ymd("2009-06-30")
dc_year <- 2011 ## Debt Ceiling Crisis
dc_months <- c(07,08)
cvd_year <- 2020 ## CVD-19 
cvd_month <- 03
twn_year <- 2001 ## 911 attack
twn_month <- 09 


check_signs <- function(X){
  sigma <- abs(diag(diag(X),5,5))
  A <- X %*% solve(sigma)
  
  res <- rep(FALSE,32) 
  
  ## Direct Impulse restrictions
  if(A[1,1] >  0) res[1] <- TRUE
  if(A[2,1] >  0) res[2] <- TRUE
  if(A[3,1] >  0) res[3] <- TRUE
  if(A[4,1] >  0) res[4] <- TRUE
  
  if(A[1,2] >  0) res[5] <- TRUE
  if(A[2,2] >  0) res[6] <- TRUE
  if(A[3,2] >  0) res[7] <- TRUE
  if(A[4,2] <=  0) res[8] <- TRUE
  
  if(A[1,3] <= 0) res[9] <- TRUE
  if(A[2,3] <= 0) res[10] <- TRUE
  if(A[3,3] <= 0) res[11] <- TRUE
  if(A[4,3] <= 0) res[12] <- TRUE
  
  if(A[1,4] >  0) res[13] <- TRUE
  if(A[2,4] >  0) res[14] <- TRUE
  if(A[3,4] >  0) res[15] <- TRUE
  if(A[4,4] <= 0) res[16] <- TRUE
  
  ## Between Asset Restrctions
  ## Growth Shock
  if(abs(A[1,1]) > abs(A[3,1])) res[17] <- TRUE
  if(abs(A[2,1]) > abs(A[3,1])) res[18] <- TRUE
  
  ## Monetary Policy Shock
  if(abs(A[1,2]) > abs(A[2,2])) res[19] <- TRUE
  if(abs(A[2,2]) > abs(A[3,2])) res[20] <- TRUE
  
  ## Hedging Risk Premium Shock
  if(abs(A[1,3]) < abs(A[2,3])) res[21] <- TRUE
  if(abs(A[2,3]) < abs(A[3,3])) res[22] <- TRUE
  
  ## Common Risk Premium Shock
  if(abs(A[1,4]) < abs(A[2,4])) res[23] <- TRUE
  if(abs(A[2,4]) < abs(A[3,4])) res[24] <- TRUE
  
  ## Within Asset Restrictions
  ## Two-year Yield
  if(X[1,1]**2 + X[1,2]**2 > X[1,3]**2 + X[1,4]**2) res[25] <- TRUE
  
  # ## Ten-year Yield
  if(X[3,1]**2 + X[3,2]**2 < X[3,3]**2 + X[3,4]**2) res[26] <- TRUE
  
  ## Check the Shock Restrictions for EPU
  omega <- u %*% solve(X)
  epu_shocks <- data.table(date = omega_dates, epu_shock = omega[,5])
  
  ## Restriction on Black Monday
  if(any(epu_shocks[year(date) == bm_year & month(date) == bm_month, epu_shock] > shock_restriction_size)) res[27] <- TRUE
  
  ## Restriction on Lehman Collapse (at least one day in Sep 2008, Lehman collapse)
  if(any(epu_shocks[year(date) == gfc_year & month(date) == gfc_month, epu_shock] > shock_restriction_size)) res[28] <- TRUE

  ## Restriction on sum of shocks over GFC (broadly)
  if(sum(epu_shocks[date %between% c(gfc_start, gfc_end), epu_shock]) > shock_restriction_size) res[29] <- TRUE
  
  ## Restriction on Debt-Ceiling Crisis (2011, 08)
  if(any(epu_shocks[year(date) == dc_year & month(date) %in% dc_months, epu_shock] > shock_restriction_size)) res[30] <- TRUE
  
  ## Restriction on Covid-19
  if(any(epu_shocks[year(date) == cvd_year & month(date) == cvd_month, epu_shock] > shock_restriction_size)) res[31] <- TRUE
  
  ## Restriction on 9-11 Terrorist Attacks
  if(any(epu_shocks[year(date) == twn_year & month(date) == twn_month, epu_shock] > shock_restriction_size)) res[32] <- TRUE
  
  return(res)
} 



## Read in the data
data <- fread("./data/cleaned_data/transformed_data.csv")
data[, date := ymd(date)]

## Subset to same time period and drop EPU
## Omit some NAs because we have EPU data on weekends
series <- c("yc2", "yc5", "yc10", "sp", "epu")
series_d <- c("date", series)
analysis_data <- na.omit(data[date %between% date_range, ..series_d])
X <- as.matrix(analysis_data[, ..series])

## Demean the data
X <- X - matrix(rep(colMeans(X), nrow(X)), ncol = 5, byrow = T)



## Find the BIC for lag length (now is 5! vs 1 w/out EPU)
## SC(n) is the BIC number
IC <- vars::VARselect(X)$selection
print(IC)

## Set up reduced form VAR
## type = "none" means there is no beta constant
myvar <- vars::VAR(X, type = "none", p = 1)

## Grab the variance-covariance matrix of reduced form shocks
u <- residuals(myvar)
Omega <- cov(u)

## Construct a Lower Choleskey Matrix
P <- t(chol(Omega))

fwrite(u, file = "./data/VAR_data/extension_addEPU/u.csv")
fwrite(P, file = "./data/VAR_data/extension_addEPU/P.csv")

#######
#######


## Can back out the orthoganalized shocks, omega
omega <- u %*% solve(t(P))
omega_dates <- analysis_data[-1,"date"][[1]]
data.table(date = omega_dates, omega)

## Note that cov-var of orthogonalized shocks is roughly I
cov(omega) |> round(4)

## Function to randomly rotate a matrix (P)
r_rotate <- function(X){
  ## Generate Rotation matrixes, Q
  a <- matrix(rnorm(25),5,5)
  
  ## Decompose into Qr parts
  Q <- qr.Q(qr(a))
  R <- qr.R(qr(a))
  
  ## Make sure the diagonal of r is all positive
  R2 <- diag(sign(diag(R)),5,5) %*% R
  Q2 <- Q %*% solve(diag(sign(diag(R)),5,5))
  
  ## Rotate the Choleskey matrix
  A_tilde <- X %*% t(Q2)
  
  return(A_tilde)
}

## Example: check if it matches sign restrictions
start.seed <- 13805
set.seed(start.seed) 

A_tilde <- r_rotate(P)
check_signs(A_tilde)

tic()
r_rotate(P)
toc()

## Construct a large number of rotations, then plot the orthogonalized EPU shocks
maxiter <- 10^4
rots <- vector("list", length= maxiter)
for(i in 1:maxiter){
  A_tilde <- r_rotate(P)
  
  omega <- u %*% solve(t(A_tilde))
  
  rots[[i]] <- data.table(i = i, date = omega_dates, omega)
}
shocks_rots <- rbindlist(rots)

shocks_rots_d <- shocks_rots[(V4) >= 5,.(count_n = .N), by = .(date)]
setorder(shocks_rots_d, -count_n)
shocks_rots_d |> head(20)



EPU_shocks_rots[date %between% ymd(c("2007-12-31", "2009-06-30")), .(s = sum(epu_shock)), by = .(i) ] |> summary()

EPU_shocks_counts_d |>
  ggplot(aes(
    x = date,
    y = count_n
  )) +
  geom_col(width = 5)

EPU_summ <- EPU_shocks_rots[,.(
  mean_EPU = mean(epu_shock),
  med_EPU = median(epu_shock),
  min_EPU = min(epu_shock),
  max_EPU = max(epu_shock)
), by = .(date)]

EPU_summ |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = mean_EPU, color = "mean")) +
  geom_hline(yintercept = 0 + 2*sd(EPU_summ$mean_EPU) ) +
  geom_hline(yintercept = 0 - 2*sd(EPU_summ$mean_EPU) )

setorder(EPU_summ, mean_EPU)
EPU_summ


## We need to search a large number of possible rotation matrices Q: ~ 22 Million## To find 1000 ones that match all of our sign restrictions

maxiter = 10^4 #6 ## then give update on progress
times = 1      ## To get to 22 M
successes <- list()
success_seeds <- list()

for(t  in 1:times){
  print(t)
  tic()
  for(i in 1:maxiter){
    current_seed <- start.seed + maxiter*t + i
    set.seed(current_seed) ## So we can always recreate a rotation matrix
    
    A_tilde <- r_rotate(P)
    
    signs <- check_signs(A_tilde)
    
    if(all(signs)){
      # print(paste0("Success on ", current_seed))
      successes <- c(successes, list(A_tilde))
      success_seeds <- c(success_seeds, list(current_seed))
    }
  }
  print(length(successes))
  print(toc())
}

epu_shocks <- rbindlist(lapply(1:length(successes), function(i){
  omega <- u %*% solve(t(successes[[i]]))
  
  rots[[i]] <- data.table(i = i, date = analysis_data[-1,"date"][[1]], epu_shock = omega[,5])
}))
epu_shocks
epu_shocks_m <- epu_shocks[, yyyymm := zoo::as.yearmon(date)][abs(epu_shock) >= 5,.(count_n = .N), by = .(yyyymm)]
epu_shocks_d <- epu_shocks[(epu_shock) >= 5,.(count_n = .N, avg_shock = mean(epu_shock)), by = .(date)]

epu_shocks[date == ymd("2020-03-09")] |>
  ggplot(aes(x = epu_shock)) + geom_histogram(bins = 10)

setorder(epu_shocks_m, -count_n)
setorder(epu_shocks_d, -count_n)
epu_shocks_m |> head(20)
epu_shocks_d |> head(20)


## Save Successes out to File as well
saveRDS(successes, file = "./data/VAR_data/extension_addEPU//successful_rotations.RDS")
saveRDS(success_seeds, file = "./data/VAR_data/extension_addEPU/success_seeds.RDS")

## And save out our VAR to use later
saveRDS(list(
  var = myvar,
  u = u,
  Omega = Omega,
  P = P,
  analysis_data = analysis_data
), file = "./data/VAR_data/extension_addEPU/estimated_VAR.RDS")



### Read in Julia iterations
rots <- as.matrix(fread("./data/VAR_data/extension_addEPU/rotations/test.csv"))

  

