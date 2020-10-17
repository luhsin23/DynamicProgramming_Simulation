#Data Input
setwd("~/Desktop/108-2/Dynamic Programming/HW")
dta = read.delim2("dp02.txt",sep="",header = FALSE)
# new example with 10 nodes
# dta = read.delim2("dp02_n.txt",sep="",header = FALSE)
dta0 = dta
dta0[,1] = dta[,2]
dta0[,2] = dta[,1]
dta_new = rbind(dta, dta0)


#create the t matrix, which is symmetric because the route is cyclic
#there are 10 nodes: N=10
N= max(dta$V2)
t=matrix(Inf,N,N)
for(j in 1:nrow(dta_new)){
  t[dta_new[j,1],dta_new[j,2]] = dta_new[j,3]
}


#Step 1
# 設定起始與結束的點
s = 1
e = 10

V = rep(Inf,N)
V[s] = 0
T = c(1:N)
T = T[T!=s]
T0 = T

K = list()
for(i in T){
  # print(i)
  V[i] = t[s,i]
  #initialize the K matrix: optimal path
  if(V[i]<Inf){K[[i]] = c(s,i)}
}
V_s = rep(Inf,N)


for(i in 1:(N)-1){
#Step 2: find the i star, and update the V star matrix
  for(i in T){
    if(i==T[1]){i_s = i}
    if(V[i]<=V[i_s]){
      i_s = i
      V_s[i] = V[i]
    }
  }
 
  
#Step 3: delete the i star from T, and check if T is empty now
  T = T[T!=i_s]
  # if(T == c()){break}
#Step 4:
  for(j in T){
    #update the K matrix: optimal path
    if(V[j] >  V[i_s] + t[i_s,j]){K[[j]] = c(K[[i_s]], j)}
    V[j] = min(V[j], V[i_s] + t[i_s,j])
  }
  
}


#output
cat("從",s,"到",e,"的最短距離為",V[e])
cat("所經路徑為",K[[e]])



