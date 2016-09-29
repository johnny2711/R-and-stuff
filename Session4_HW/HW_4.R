#Q2

beta = 0.5
SIGMA = matrix(data=c(1,beta,beta,1), ncol=2)
MU = c(2.0, 1.0)
N=50
out <- mvrnorm(N, mu = MU, Sigma = SIGMA)
plot(out)
abline(lm(out[,2]~out[,1]), col="red") 

y <- out[, 2]
X <- out[, 1]
X <- matrix(c(rep(1,50),X),ncol=2)
XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%y
beta = invXTX %*% XTy
beta
abline(a=beta[1,], b=beta[2,], col="blue")

#Q3

beta1 = 0.5
beta2 = 0.4
SIGMA = matrix(data=c(1,0,beta1,0,1,beta2,beta1,beta2,1), ncol=3) #covariances were assigned by assumption
N=50
out <- mvrnorm(N, mu = MU, Sigma = SIGMA)
y <- out[, 3]
X <- out[, 1:2]
X <- matrix(c(rep(1,50),X),ncol=3)
XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%y
beta = invXTX %*% XTy
beta

model <- lm(y~X) # to verify our answer, we can let R run the regression
summary(model)
model$coefficients

#Q4

# var: number of variables
# n: sample size
# These need to be defined, which is done arbitrarliy here
n <- 100
var <- 4

reg <- function(n, MU, SIGMA){
  # MU and SIGMA are randomely chosen unless specific values are assigned
  if(is.na(MU)){
    MU<-runif(var, 1, 50)
  }
  if(is.na(SIGMA)){
    coeff_sigma<- runif(var*var, 0, 1)
    SIG<- matrix(coeff_sigma, ncol = var, nrow = var)
    SIGMA<-SIG%*%t(SIG)
  }
  data <- mvrnorm(n, mu = MU, Sigma = SIGMA)
  y <- data[,1]
  X <- data[, 2:var]
  X <- matrix(c(rep(1,n),X),ncol=var)
  XT = t(X)
  XTX = XT%*%X
  invXTX = solve(XTX)
  XTy = XT%*%y
  beta = invXTX %*% XTy
  beta
}
reg(n = 50, MU = NA, SIGMA = NA)

