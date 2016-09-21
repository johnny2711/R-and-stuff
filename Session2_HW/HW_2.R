#Q1

w0 <- 100 # Initial wealth
T <- 10000  # Terminal time

# Create empty objects to fill
w <- rep(0, T+1)
x <- rep(0, T)

# Fill initial wealth
w[1] = w0

for(t in 1:T){
  p <- runif(1) #Random probability to win in each period
  u <- runif(1) #determines if gambler has won in respective period (random shock) 
  if(u > p){ #gambler loses
    x[t] <- 0
    w[t+1] <- w[t] - 1
  } else { #gambler wins
    x[t] <- 1
    w[t+1] <- w[t] + 1
  }
}
time = 0:T
plot(time, w, type='l')

#Q2

w0 <- 100 # Initial wealth
T <- 10000  # Terminal time

# Create empty objects to fill
w1 <- rep(0, T+1)
x <- rep(0, T)
u <- runif(1) #random shock now defined outside loop 
# Fill initial wealth
w1[1] = w0

for(t in 1:T){
  p <- runif(1) #Random probability to win in each period
  if(u > p){ #gambler loses
    x[t] <- 0
    w1[t+1] <- w1[t] - 1
  } else { #gambler wins
    x[t] <- 1
    w1[t+1] <- w1[t] + 1
  }
}
time = 0:T
plot(time, w1, type='l')

#Q3

maxval<-max(w, w1) 
minval<-min(w, w1)
plot(x = periods, y=w, col = "blue", type = "l", ylim= c(minval, maxval), 
     main = "Gambler's wealth path", 
     xlab = "Number of bets", ylab = "Wealth")
lines(x = periods, y = w1, col = "red", type = "l")
legend("topleft", 
       legend = c("Random p and u", "Random p, fixed u"), 
       col= c("blue", "red"), lty = c(1, 1))

#Q4

w0 <- 100 # Initial wealth
M <- 10000  # Number of loops
R <- 100 #Number of realizations
V <- c(0, R) #vector to store the M+1 wealth for each realization
p <- runif(1,0,1) #outside loop to enable comparison between realizations

for(j in 1:R){  
  
  w2<-rep(0, M+1)  
  w2[1]<-w0
  
for(t in 1:M){
  u <- runif(1,0,1)
  if(u > p){ #gambler loses
    w2[t+1] <- w2[t] - 1
  } else { #gambler wins
    w2[t+1] <- w2[t] + 1
  }
}
  V[j]<-w2[M+1]
}

average <- mean(V)
print(average)

#Q5

#same as above, but p is different for each j in R
w0 <- 100 # Initial wealth
M <- 10000  # Number of loops
R <- 100 #Number of realizations
V <- c(0, R) #vector to store the M+1 wealth for each realization

for(j in 1:R){  
  p <- runif(1,0,1) 
  w3<-rep(0, M+1)  
  w3[1]<-w0
  
  for(t in 1:M){
    u <- runif(1,0,1)
    if(u > p){ #gambler loses
      w3[t+1] <- w3[t] - 1
    } else { #gambler wins
      w3[t+1] <- w3[t] + 1
    }
  }
  V[j]<-w3[M+1]
}

average2 <- mean(V)
print(average2)


