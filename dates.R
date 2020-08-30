library(magrittr)
library(rvest)
library(xml2)
library(stringi)
library(dplyr)
library(stringr)
library(pracma)

#List of all team abbreviations
teams <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE",
           "COL","DET","HOU","KCR","LAA","LAD","MIA","MIL",
           "MIN","NYM","NYY","OAK","PHI","PIT","SDP","SEA","SFG","STL","TBR","TEX","TOR","WSN")
num <- c(1:30)

#Dataframe to store all home dates for each team
home_games <- data.frame(matrix(nrow = 81, ncol = 30))
colnames(home_games) <- teams

#Loop through each team
for(x in num){
  #Get the url for each team page
  #This is where we get the dates of each game
  url <- "https://www.baseball-reference.com/teams/"
  url <- strcat(url, teams[x])
  url <- strcat(url, "/2019-schedule-scores.shtml")
  print(url)
  team <- teams[x]
  sched_url <- read_html(url)
  
  #Get our table
  schedules <- xml_find_all(sched_url, "//table") %>% html_table
  
  schedules <- as.data.frame(schedules)
  
  #Basically 1-162
  games <- c(1:nrow(schedules))
  #Where we are in the home_games df
  t <- 1
  #Loop throgu each game the team played
  for(i in games){
    #Find games where our team was the home team
    if(schedules[i,"Gm."] != "Gm#" & schedules[i,"Var.5"] != "@"){
      #Extract the date and convert it to what we want
      date <- strsplit(schedules[i,"Date"], "\\s+")
      month <- sapply(date ,`[`, 2)
      day <- sapply(date, `[`, 3)
      #For formatting, add a leading zero to a single digit day
      if(nchar(day) == 1){
        day <- strcat("0",day)
      }
      #Convert the month name to a number
      if(month == "Mar"){
        d <- "03"
        d <- strcat(d, day)
      }
      else if(month == "Apr"){
        d <- "04"
        d <- strcat(d, day)
      }
      else if(month == "May"){
        d <- "05"
        d <- strcat(d, day)
      }
      else if(month == "Jun"){
        d <- "06"
        d <- strcat(d, day)
      }
      else if(month == "Jul"){
        d <- "07"
        d <- strcat(d, day)
      }
      else if(month == "Aug"){
        d <- "08"
        d <- strcat(d, day)
      }
      else if(month == "Sep"){
        d <- "09"
        d <- strcat(d, day)
      }
      else if(month == "Oct"){
        d <- "10"
        d <- strcat(d, day)
      }
      #Add the date to the dataframe
      home_games[t,team] <- d
      t <- t + 1
    }
  }
}