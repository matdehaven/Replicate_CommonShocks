##
##  Replicate VAR analysis from "Common Shocks..." paper
##  Use their time sample
##
##  Matthew DeHaven
##  2022 10 29
##
require(data.table)
require(lubridate)

require(vars)

## The time sample used in the paper
date_range <- ymd(c("1983-01-01", "2017-12-31"))

check_signs <- function(X){
  res <- rep(FALSE,26)
  
  ## Direct Impulse restrictions
  if(X[1,1] >  0) res[1] <- TRUE
  if(X[2,1] >  0) res[2] <- TRUE
  if(X[3,1] >  0) res[3] <- TRUE
  if(X[4,1] >  0) res[4] <- TRUE
  
  if(X[1,2] >  0) res[5] <- TRUE
  if(X[2,2] >  0) res[6] <- TRUE
  if(X[3,2] >  0) res[7] <- TRUE
  if(X[4,2] <=  0) res[8] <- TRUE
  
  if(X[1,3] <= 0) res[9] <- TRUE
  if(X[2,3] <= 0) res[10] <- TRUE
  if(X[3,3] <= 0) res[11] <- TRUE
  if(X[4,3] <= 0) res[12] <- TRUE
  
  if(X[1,4] >  0) res[13] <- TRUE
  if(X[2,4] >  0) res[14] <- TRUE
  if(X[3,4] >  0) res[15] <- TRUE
  if(X[4,4] <= 0) res[16] <- TRUE
  
  ## Between Asset Restrctions
  ## Growth Shock
  if(abs(X[1,1]) > abs(X[3,1])) res[17] <- TRUE
  if(abs(X[2,1]) > abs(X[3,1])) res[18] <- TRUE
  
  ## Monetary Policy Shock
  if(abs(X[1,2]) > abs(X[2,2])) res[19] <- TRUE
  if(abs(X[2,2]) > abs(X[3,2])) res[20] <- TRUE
  
  ## Hedging Risk Premium Shock
  if(abs(X[1,3]) < abs(X[2,3])) res[21] <- TRUE
  if(abs(X[2,3]) < abs(X[3,3])) res[22] <- TRUE
  
  ## Common Risk Premium Shock
  if(abs(X[1,4]) < abs(X[2,4])) res[23] <- TRUE
  if(abs(X[2,4]) < abs(X[3,4])) res[24] <- TRUE
  
  ## Within Asset Restrictions
  ## Two-year Yield
  if(X[1,1]**2 + X[1,2]**2 > X[1,3]**2 + X[1,4]**2) res[25] <- TRUE
  
  ## Ten-year Yield
  if(X[3,1]**2 + X[3,2]**2 < X[3,3]**2 + X[3,4]**2) res[26] <- TRUE
  
  return(res)
} 


## Read in the data
data <- fread("./data/cleaned_data/transformed_data.csv")
data[, date := ymd(date)]

## Subset to same time period and drop EPU
## Omit some NAs because we have EPU data on weekends
series <- c("yc2", "yc5", "yc10", "sp")
X <- as.matrix(na.omit(data[date %between% date_range, ..series]))

## Demean the data
X <- X - apply(X, 2, mean) 

## Find the BIC for lag length (should be 1)
## SC(n) is the BIC number
IC <- vars::VARselect(X)$selection
print(IC)

## Set up reduced form VAR
svars::id.chol(myvar)
myvar <- vars::VAR(X, type = "const", ic = "SC")

## Grab the variance-covariance matrix of reduced form shocks
u <- residuals(myvar)
Omega <- cov(u)

## Construct a Lower Choleskey Matrix
P <- t(chol(Omega))

## Can back out the orthoganalized shocks
ws <- u %*% solve(t(P))

## Note that cov-var of orthogonalized shocks is roughly I
cov(ws) |> round(4)

## Generate Rotation matrixes, Q
a <- matrix(rnorm(16),4,4)
Q <- qr.Q(qr(a))

## Cheack that QQ' = I
Q %*% t(Q) |> round(4)

## Rotate the Choleskey matrix
R <- P %*% Q

## splitting like the appendix does
sigma <- abs(diag(diag(R),4,4))
R2 <- R %*% solve(sigma)

R2

## Back out impolied shocks
ws_R <- u %*% solve(t(R))

## Check if it matches sign restrictions
check_signs(R)

set.seed(13805)
i = 0
maxiter = 10^7
t <- rbindlist(lapply(1:maxiter, function(i){
  if(i %% 10^4 == 0) print(i)
  
  a <- matrix(rnorm(16),4,4)
  Q <- qr.Q(qr(a))
  
  ## Rotate the Choleskey matrix
  R <- P %*% t(Q)
  
  ## Check if it matches sign restrictions
  data.table(t(check_signs(R)))
}))

apply(t,2,function(x){sum(x)/length(x)})


a <- matrix(rnorm(16),4,4)
qr(a)

b <- matrix(rnorm(16),4,4)
t <- qr(b)
t(t$qr) %*% (t$qr)
qr.Q(t)
qr.R(t)
qr.Q(t) %*% t(qr.Q(t)) |> round(4)




