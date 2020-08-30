library(stringr)
ana <- read.csv("./2019eve/2019ANA.csv", na.strings=c(""," ","NA"))
ana[,1] <- as.character(ana[,1])
ana[,2] <- as.character(ana[,2])
ana[,3] <- as.character(ana[,3])
ana[,7] <- as.character(ana[,7])
ana[2,1]
ana[90,1]
i <- c(1:13813)
ana[13812,]
team <- 0
trip <- 0
if(ana[90,1] == "play"){
  print("TRUE")
}
for(val in i){
  temp <- ana[val,1]
  if(!is.na(temp)){
    if(temp == "info"){
      if(ana[val,2] == "visteam" & ana[val,3] == "ANA"){
        team <- 0
      }
      if(ana[val,2] == "hometeam" & ana[val,3] == "ANA"){
        team <- 1
      }
    }
    if(temp == "play" & (str_detect(ana[val,7], "T") & !str_detect(ana[val,7], "TH") & !str_detect(ana[val,7], "INT"))){
      if(ana[val,3] == as.character(team)){
        print(ana[val,])
        trip <- trip + 1
      }
    }
  }
}
trip
#This is technically correct, but it only has the home games, which maybe makes this a lot harder.