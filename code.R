library(frontier)
library(psych)
library(stringr)

sfa_model_of_data <- function(reportdata){
  df <- reportdata
  df$logtpd_i <- log(df$`Total person-days`)
  df$logtwc_i <- log(df$`Total works`)
  df$logtpaw_o <- log(df$`Total persons allotted work`)
  df$logle_o <- log(df$`Labour exp. (disbursed, Rs. In Lakhs)`)
  df$logas_o <- log(df$`Amount sanctioned (Rs. In Lakhs)`)
  model_1 <- sfa(logtwc_i~logtpaw_o+logle_o+logas_o,truncNorm = TRUE,data = df)
  model_2 <- sfa(logtpd_i~logtpaw_o+logle_o+logas_o,truncNorm = TRUE,data = df)
  df$TE_twc <- frontier::efficiencies(model_1)
  df$TE_tpd <- frontier::efficiencies(model_2)
  return(df)
}
report_2017_18<-sfa_model_of_data(report_2017_18)
report_2018_19<-sfa_model_of_data(report_2018_19)
report_2019_20<-sfa_model_of_data(report_2019_20)
report_2020_21<-sfa_model_of_data(report_2020_21)
report_2021_22<-sfa_model_of_data(report_2021_22)
#Part2
state_wise_technical_efficiencies_twc <- data.frame(report_2017_18$`State Name`,
                                                    report_2017_18$TE_twc,
                                                    report_2018_19$TE_twc,
                                                    report_2019_20$TE_twc,
                                                    report_2020_21$TE_twc,
                                                    report_2021_22$TE_twc)

state_wise_technical_efficiencies_tpd <- data.frame(report_2017_18$`State Name`,
                                                    report_2017_18$TE_tpd,
                                                    report_2018_19$TE_tpd,
                                                    report_2019_20$TE_tpd,
                                                    report_2020_21$TE_tpd,
                                                    report_2021_22$TE_tpd)

colnames(state_wise_technical_efficiencies_twc) <- c("State Name", 
                                                     "2017-18",
                                                     "2018-19",
                                                     "2019-20",
                                                     "2020-21",
                                                     "2021-22")
colnames(state_wise_technical_efficiencies_tpd) <- c("State Name", 
                                                     "2017-18",
                                                     "2018-19",
                                                     "2019-20",
                                                     "2020-21",
                                                     "2021-22")
meanTE_twc <- c("Mean",
                  geometric.mean(state_wise_technical_efficiencies_twc$`2017-18`),
                  geometric.mean(state_wise_technical_efficiencies_twc$`2018-19`),
                  geometric.mean(state_wise_technical_efficiencies_twc$`2019-20`),
                  geometric.mean(state_wise_technical_efficiencies_twc$`2020-21`),
                  geometric.mean(state_wise_technical_efficiencies_twc$`2021-22`))

meanTE_tpd <- c("Mean",
            geometric.mean(state_wise_technical_efficiencies_tpd$`2017-18`),
            geometric.mean(state_wise_technical_efficiencies_tpd$`2018-19`),
            geometric.mean(state_wise_technical_efficiencies_tpd$`2019-20`),
            geometric.mean(state_wise_technical_efficiencies_tpd$`2020-21`),
            geometric.mean(state_wise_technical_efficiencies_tpd$`2021-22`))

state_wise_technical_efficiencies_twc <- rbind(state_wise_technical_efficiencies_twc,meanTE_twc)
state_wise_technical_efficiencies_tpd <- rbind(state_wise_technical_efficiencies_tpd,meanTE_tpd)
state_wise_technical_efficiencies_twc$`State Name` <- str_replace_na(state_wise_technical_efficiencies_twc$`State Name`,replacement = "Mean")
state_wise_technical_efficiencies_tpd$`State Name` <- str_replace_na(state_wise_technical_efficiencies_tpd$`State Name`,replacement = "Mean")

a1<-(report_2017_18$TE_twc+report_2018_19$TE_twc+report_2019_20$TE_twc+report_2020_21$TE_twc+report_2021_22$TE_twc)/5
a2<-(report_2017_18$TE_tpd+report_2018_19$TE_tpd+report_2019_20$TE_tpd+report_2020_21$TE_tpd+report_2021_22$TE_tpd)/5
n1 <- nrow(state_wise_technical_efficiencies_twc)
n2 <- nrow(state_wise_technical_efficiencies_tpd)
a1[29] <- mean(as.numeric(state_wise_technical_efficiencies_twc[n1,-1]))
a2[29] <- mean(as.numeric(state_wise_technical_efficiencies_tpd[n2,-1]))
state_wise_technical_efficiencies_twc$AverageTE_Totalworkscompleted <- a1
state_wise_technical_efficiencies_tpd$AverageTE_Totalpersondays <- a2


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
year_wise_mean_efficiency_twc <- data.frame(year_column,empty_list_1_twc,empty_list_2_twc)
colnames(year_wise_mean_efficiency_twc) <- c("Year","No.of states(TE(twc)<Mean)","No.of states(TE(twc)>Mean)")

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