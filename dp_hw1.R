setwd("~/Desktop/108-2/Dynamic Programming/HW")
dta = read.delim2("dp01.txt",sep="",header = FALSE)
N = dta[1,1]
f=c(rep(Inf,N))
t=matrix(Inf,N,N)

dta0 = as.data.frame(dta)[3:nrow(dta)-1,]

for(j in 1:nrow(dta0)){
    t[dta0[j,1],dta0[j,2]] = dta0[j,3]
}

f[N] = 0
f0 = Inf
t0 = t
j0 = 0
path = c(1)
for(i in (N-1):1){
  for(j in (i+1):N){
    if(f[i]<t[i,j]+f[j]){t0[i,j]=Inf}
    f[i] = min(f[i], t[i,j]+f[j])
  }
}




path = c(1)
j0 = 0
for(i in 1:7){
  if(i< j0) next
  k = Inf
  for(j in (i+1):N){
    m = t[i,j] + f[j]
    if(m < k){
      k = m
      j0 = j
    }
  }
  path = c(path, j0)
}
#path

p = paste(path, collapse = ",")
cat("從1到",N,"的最短路徑為",f[1],",最短路徑為",p)
