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
  sigma <- abs(diag(diag(X),4,4))
  A <- X %*% solve(sigma)
  
  res <- rep(FALSE,26) 
  
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
X <- X - matrix(rep(colMeans(X), nrow(X)), ncol = 4, byrow = T)



## Find the BIC for lag length (should be 1)
## SC(n) is the BIC number
IC <- vars::VARselect(X)$selection
print(IC)

## Set up reduced form VAR
## type = "none" means there is no beta constant
myvar <- vars::VAR(X, type = "none", ic = "SC")

## Grab the variance-covariance matrix of reduced form shocks
u <- residuals(myvar)
Omega <- cov(u)

## Construct a Lower Choleskey Matrix
P <- t(chol(Omega))

## Can back out the orthoganalized shocks, omega
omega <- u %*% solve(t(P))

## Note that cov-var of orthogonalized shocks is roughly I
cov(omega) |> round(4)

## Function to randomly rotate a matrix (P)
r_rotate <- function(X){
  ## Generate Rotation matrixes, Q
  a <- matrix(rnorm(16),4,4)
  
  ## Decompose into Qr parts
  Q <- qr.Q(qr(a))
  R <- qr.R(qr(a))
  
  ## Make sure the diagonal of r is all positive
  R2 <- diag(sign(diag(R)),4,4) %*% R
  Q2 <- Q %*% solve(diag(sign(diag(R)),4,4))
  
  ## Rotate the Choleskey matrix
  A_tilde <- X %*% t(Q2)
  
  return(A_tilde)
}

## Example: check if it matches sign restrictions
start.seed <- 13805
set.seed(start.seed) 

A_tilde <- r_rotate(P)

## Store in a data.table so we can keep track of which signs are restrictive
sign_results <- data.table(
  seed = start.seed ,
  t(check_signs(A_tilde))
  )

sign_results |> print()

## We need to search a large number of possible rotation matrices Q: ~ 20 Million
## To find ones that match all of our sign restrictions

maxiter = 10^6 ## then need to save to disk
times = 20     ## To get to 20 M
successes <- list()

for(t  in 1:times){
  sign_results <- data.table(seed = 0, t(rep(NA,26)))
  
  for(i in 1:maxiter){
    if(i %% 10^6 == 0) print(i/10^6) ## Print out each million
    set.seed(start.seed + i) ## So we can always recreate a rotation matrix
    
    A_tilde <- r_rotate(P)
    
    signs <- check_signs(A_tilde)
    
    sign_results <- rbind(sign_results, data.table(seed = start.seed + i, t(signs)))
    
    if(all(signs)){
      print(paste0("Success on ", i))
      successes <- c(successes, list(A_tilde))
    }
  }

  saveRDS(sign_results[-1,], file = "./output/VAR_rotations/rotations_M", t, ".RDS")
}

## Save Successes out to File as well
saveRDS(successes, file = "./output/VAR_rotations/successful_rotations.RDS")

## And save out our VAR to use later
saveRDS(list(
  var = myvar,
  u = u,
  Omega = Omega,
  P = P,
), file = "./output/VAR_rotations/reduced_form_VAR.RDS")