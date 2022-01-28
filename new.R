
set <- 0
for (k in 1:29){
  k1 = as.numeric(state_wise_technical_efficiencies_tpd$`2021-22`[k])
  k2 = as.numeric(state_wise_technical_efficiencies_tpd$`2020-21`[k])
  k3 = as.numeric(state_wise_technical_efficiencies_tpd$`2019-20`[k])
  k4 = as.numeric(state_wise_technical_efficiencies_tpd$`2018-19`[k])
  k5 = as.numeric(state_wise_technical_efficiencies_tpd$`2017-18`[k])
  if (k1<k2){
    if (k1<k3){
      if (k1<k4){
        if (k1<k5){
          set <- set + 1
        }
      }
    }
  }
}




set2 <- 0
for (m in 1:29){
  m1 = as.numeric(state_wise_technical_efficiencies_twc$`2021-22`[m])
  m2 = as.numeric(state_wise_technical_efficiencies_twc$`2020-21`[m])
  m3 = as.numeric(state_wise_technical_efficiencies_twc$`2019-20`[m])
  m4 = as.numeric(state_wise_technical_efficiencies_twc$`2018-19`[m])
  m5 = as.numeric(state_wise_technical_efficiencies_twc$`2017-18`[m])
  if (m1<m2){
    if (m1<m3){
      if (m1<m4){
        if (m1<m5){
          set2 <- set2 + 1
        }
      }
    }
  }
}

set3 <- 0
for (m in 1:29){
  m1 = as.numeric(state_wise_technical_efficiencies_twc$`2021-22`[m])
  m2 = as.numeric(state_wise_technical_efficiencies_twc$`2020-21`[m])
  if (m1<m2){
    set3 <- set3+1
  }
}