#HW6 EX1
# Input values are changable here!
actualDemand = c(1,2,3,4,5,6,7,8,9,10)
orderingCostPerUnit = 10
holdingCostPerUnit = 5
price = 20
fixedCost = 5
N = 10

#Construct the path length
f = list()
for(i in 1:N){
  f[[i]] = matrix(0, max(actualDemand)+1, max(actualDemand)+1)
}

for(t in N : 1){
  for(It in 0 : max(actualDemand)){
    for(Xt in max(0, actualDemand[t]-It) :max(actualDemand)){
      f[[t]][It,Xt] = max(price * actualDemand[t] - fixedCost - holdingCostPerUnit * It - orderingCostPerUnit * Xt)
    }
  }
}

#In this special setting, one soulution would be ordering at the demand level
profit = 0
for(t in 1:N){
  profitT = price * actualDemand[t] - fixedCost - holdingCostPerUnit * 0 - orderingCostPerUnit * actualDemand[t]
  profit = profit + profitT
}
cat("profit =", profit)

