len <- 5
empty_list_1_twc <- rep(NA, len)
empty_list_2_twc <- rep(NA, len)

# twc
for (v in 2:6){
  p <- 0
  q <- 0
  p1 <- as.numeric(state_wise_technical_efficiencies_twc[n1,v])
  for (i in 1:(n1-1)){
    q1 <- as.numeric(state_wise_technical_efficiencies_twc[i+1,v])
    if (p1 > q1){
      p <- p+1
    }else {
      q<-q+1
    }
  }
  empty_list_1_twc[v-1] <- p
  empty_list_2_twc[v-1] <- q
}
year_column = c("2017-18",
                "2018-19",
                "2019-20",
                "2020-21",
                "2021-22") 
year_wise_mean_efficiency_tws <- data.frame(year_column,empty_list_1_twc,empty_list_2_twc)
colnames(year_wise_mean_efficiency_tws) <- c("Year","No.of states(TE(twc)<Mean)","No.of states(TE(twc)>Mean)")

#tpd

empty_list_1_tpd <- rep(NA, len)
empty_list_2_tpd <- rep(NA, len)
for (v in 2:6){
  p <- 0
  q <- 0
  p1 <- as.numeric(state_wise_technical_efficiencies_tpd[n1,v])
  for (i in 1:(n1-1)){
    q1 <- as.numeric(state_wise_technical_efficiencies_tpd[i+1,v])
    if (p1 > q1){
      p <- p+1
    }else {
      q<-q+1
    }
  }
  empty_list_1_tpd[v-1] <- p
  empty_list_2_tpd[v-1] <- q
}
year_wise_mean_efficiency_tpd <- data.frame(year_column,empty_list_1_tpd,empty_list_2_tpd)
colnames(year_wise_mean_efficiency_tpd) <- c("Year","No.of states(TE(tpd)<Mean)","No.of states(TE(tpd)>Mean)")


write.csv(state_wise_technical_efficiencies_twc,"State-wise efficiency score during 2017-18 to 2021-22 w.r.to Total Works completed.csv")
write.csv(state_wise_technical_efficiencies_tpd,"State-wise efficiency score during 2017-18 to 2021-22 w.r.to Total person days.csv")


H <- state_wise_technical_efficiencies_twc$AverageTE_Totalworkscompleted
H1 <- state_wise_technical_efficiencies_tpd$AverageTE_Totalpersondays
D <- state_wise_technical_efficiencies_twc$`State Name`
barplot(H,names.arg=D,ylab="AverageTE(twc)",las=2,cex.names = .5,col="orange",border="black")
barplot(H1,names.arg=D,ylab="AverageTE(tpd)",las=2,cex.names = .5,col="green",border="black")
h2 <- as.numeric(state_wise_technical_efficiencies_twc[29,2:6])
h3 <- as.numeric(state_wise_technical_efficiencies_tpd[29,2:6])
barplot(h2,names.arg=year_column,ylab="MeanTE of states(twc)",las=2,col="blue",border="black")
barplot(h3,names.arg=year_column,ylab="MeanTE of states(tpd)",las=2,col="red",border="black")
