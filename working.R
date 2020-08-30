library(magrittr)
library(rvest)
library(xml2)
library(stringi)
library(dplyr)
library(stringr)
library(pracma)
library(xlsx)

teams <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE",
           "COL","DET","HOU","KCR","LAA","LAD","MIA","MIL",
           "MIN","NYM","NYY","OAK","PHI","PIT","SDP","SEA","SFG","STL","TBR","TEX","TOR","WSN")
data <- data.frame(matrix(nrow = 30, ncol = 15))
head <- c("Team","Doubles","Doubles_No_Outs","Doubles_No_Outs_Scored","Doubles_One_Out","Doubles_One_Out_Scored","Doubles_Two_Outs","Doubles_Two_Outs_Scored",
          "Triples","Triples_No_Outs","Triples_No_Outs_Scored","Triples_One_Out","Triples_One_Out_Scored","Triples_Two_Outs","Triples_Two_Outs_Scored")
colnames(data) <- head
rownames(data) <- teams
data[,"Team"] <- teams
data[is.na(data)] <- 0

teams <- c("ARI","ATL","BAL","BOS","CHN","CHA","CIN","CLE",
           "COL","DET","HOU","KCA","ANA","LAN","MIA","MIL",
           "MIN","NYN","NYA","OAK","PHI","PIT","SDN","SEA","SFN","SLN","TBA","TEX","TOR","WAS")
no_score <- data.frame(matrix(ncol = 13))
doubles <- data.frame(matrix(ncol = 13))
k <- 1
z <- 1
hou <- c(2,3,4,5,6,8,10,11,12,13,15,17,18,19,20,21,24,27,28,29,30)
for(i in 1:30){
  for(j in 1:81){
    if(!is.na(home_games[j,i])){
      url <- "https://www.baseball-reference.com/boxes/"
      url <- strcat(url,teams[i])
      url <- strcat(url,"/")
      url <- strcat(url,teams[i])
      url <- strcat(url,"2019")
      url <- strcat(url, home_games[j,i])
      if(j > 1 & strcmp(home_games[j,i],home_games[j-1,i])){
        url <- strcat(url,"2.shtml")
      }
      else if(!is.na(home_games[j+1,i]) & strcmp(home_games[j,i],home_games[j+1,i])){
        url <- strcat(url,"1.shtml")
      }
      else{
        url <- strcat(url,"0.shtml")
      }
    
      print(url)
      urlbbref <- read_html(url)
      # First table is in the markup
      table_one <- xml_find_all(urlbbref, "//table") %>% html_table
      
      # Additional tables are within the comment tags, ie <!-- tables -->
      # Which is why your xpath is missing them.
      # First get the commented nodes
      alt_tables <- xml2::xml_find_all(urlbbref,"//comment()") %>% {
        #Find only commented nodes that contain the regex for html table markup
        raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
        # Remove the comment begin and end tags
        strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                      vectorize_all = FALSE)
        # Loop through the pieces that have tables within markup and 
        # apply the same functions
        lapply(grep("<table", strip_html, value = TRUE), function(i){
          rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
            .[[1]]
        })
      }
      # Put all the data frames into a list.
      all_tables <- c(
        table_one, alt_tables
      )
      
      plays <- as.data.frame(all_tables[8])
      
      p <- c(1:nrow(plays))
      for(val in p){
        #print(plays[val,"Play.Description"])
        if(startsWith(plays[val,"Play.Description"], "Triple to")){
          cur <- plays[val,"X.Bat"]
          outs <- plays[val,"Out"]
          data[cur,"Triples"] <- data[cur,"Triples"] + 1
          scored <- 1
          if(outs == 0){
            data[cur,"Triples_No_Outs"] <- data[cur,"Triples_No_Outs"] + 1
            scored <- 0
          }
          else if(outs == 1){
            data[cur,"Triples_One_Out"] <- data[cur,"Triples_One_Out"] + 1
          }
          else{
            data[cur,"Triples_Two_Outs"] <- data[cur,"Triples_Two_Outs"] + 1
          }
          p_full <- plays[val,"Batter"]
          lst <- strsplit(plays[val,"Batter"], "\\s+")
          v1 <- sapply(lst ,`[`, 1)
          player <- sapply(lst, `[`, 2)
          if(length(lst) > 2){
            for(y in 3:length(lst)){
              player <- strcat(player, " ")
              player <- strcat(player,sapply(lst, `[`, y))
            }
          }
          p_steal <- player
          player <- strcat(player, " Scores")
          p_steal <- strcat(p_steal, " Steals Hm")
          x <- val
          while(x < nrow(plays) & !startsWith(plays[x, "Inn"], "Top of the") & !startsWith(plays[x, "Inn"], "Bottom of the")){
            if(str_detect(plays[x,"Play.Description"], strcat("pinch runs for ", p_full))){
              lst <- strsplit(plays[x,"Play.Description"], "\\s+")
              p <- 1
              while(sapply(lst, `[`, p) != "pinch"){
                p <- p + 1
              }
              p_full <- sapply(lst ,`[`, p - 2)
              p_full <- strcat(p_full, " ")
              p_full <- strcat(p_full, sapply(lst ,`[`, p - 1))
              player <- sapply(lst ,`[`, p - 1)
              p_steal <- player
              player <- strcat(player, " Scores")
              p_steal <- strcat(p_steal, " Steals Hm")
              print(player)
            }
            if(str_detect(plays[x,"Play.Description"], player) | str_detect(plays[x,"Play.Description"], p_steal)){
              #print(player)
              if(outs == 0){
                scored <- 1
                data[cur,"Triples_No_Outs_Scored"] <- data[cur,"Triples_No_Outs_Scored"] + 1
              }
              else if(outs == 1){
                data[cur,"Triples_One_Out_Scored"] <- data[cur,"Triples_One_Out_Scored"] + 1
              }
              else{
                data[cur,"Triples_Two_Outs_Scored"] <- data[cur,"Triples_Two_Outs_Scored"] + 1
              }
            }
            x <- x + 1
          }
          if(scored == 0){
            no_score[k,] = plays[val,]
            no_score[k,13] = home_games[j,i]
            k <- k + 1
          }
        }
        if(startsWith(plays[val,"Play.Description"], "Double to") || startsWith(plays[val,"Play.Description"], "Ground-rule Double") || startsWith(plays[val,"Play.Description"], "Double/Fan")){
          cur <- plays[val,"X.Bat"]
          outs <- plays[val,"Out"]
          data[cur,"Doubles"] <- data[cur,"Doubles"] + 1
          if(outs == 0){
            data[cur,"Doubles_No_Outs"] <- data[cur,"Doubles_No_Outs"] + 1
          }
          else if(outs == 1){
            data[cur,"Doubles_One_Out"] <- data[cur,"Doubles_One_Out"] + 1
          }
          else{
            data[cur,"Doubles_Two_Outs"] <- data[cur,"Doubles_Two_Outs"] + 1
          }
          if(plays[val,"X.Bat"] == "CHW"){
            doubles[z,] = plays[val,]
            doubles[z,13] = home_games[j,i]
            z <- z + 1
          }
          p_full <- plays[val,"Batter"]
          lst <- strsplit(plays[val,"Batter"], "\\s+")
          v1 <- sapply(lst ,`[`, 1)
          player <- sapply(lst, `[`, 2)
          if(length(lst) > 2){
            for(y in 3:length(lst)){
              player <- strcat(player, " ")
              player <- strcat(player,sapply(lst, `[`, y))
            }
          }
          p_steal <- player
          player <- strcat(player, " Scores")
          p_steal <- strcat(p_steal, " Steals Hm")
          x <- val
          while(x < nrow(plays) & !startsWith(plays[x, "Inn"], "Top of the") & !startsWith(plays[x, "Inn"], "Bottom of the")){
            if(str_detect(plays[x,"Play.Description"], strcat("pinch runs for ", p_full))){
              lst <- strsplit(plays[x,"Play.Description"], "\\s+")
              p <- 1
              while(sapply(lst, `[`, p) != "pinch"){
                p <- p + 1
              }
              p_full <- sapply(lst ,`[`, p - 2)
              p_full <- strcat(p_full, " ")
              p_full <- strcat(p_full, sapply(lst ,`[`, p - 1))
              player <- sapply(lst ,`[`, p - 1)
              p_steal <- player
              player <- strcat(player, " Scores")
              p_steal <- strcat(p_steal, " Steals Hm")
              print(player)
            }
            if(str_detect(plays[x,"Play.Description"], player) | str_detect(plays[x,"Play.Description"], p_steal)){
              #print(player)
              if(outs == 0){
                data[cur,"Doubles_No_Outs_Scored"] <- data[cur,"Doubles_No_Outs_Scored"] + 1
              }
              else if(outs == 1){
                data[cur,"Doubles_One_Out_Scored"] <- data[cur,"Doubles_One_Out_Scored"] + 1
              }
              else{
                data[cur,"Doubles_Two_Outs_Scored"] <- data[cur,"Doubles_Two_Outs_Scored"] + 1
              }
            }
            x <- x + 1
          }
        }
      }
    }
  }
}

colnames(no_score) <- colnames(plays)

write.xlsx(data, "c:/users/tdpot/Triples/data.xlsx")
#write.xlsx(no_score, "c:/users/tdpot/Triples/no_score.xlsx")

#urlbbref <- read_html("https://www.baseball-reference.com/boxes/OAK/OAK201903290.shtml")
urlbbref <- read_html("https://www.baseball-reference.com/boxes/SEA/SEA201906030.shtml")

# First table is in the markup
table_one <- xml_find_all(urlbbref, "//table") %>% html_table

# Additional tables are within the comment tags, ie <!-- tables -->
# Which is why your xpath is missing them.
# First get the commented nodes
alt_tables <- xml2::xml_find_all(urlbbref,"//comment()") %>% {
  #Find only commented nodes that contain the regex for html table markup
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  # Remove the comment begin and end tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Loop through the pieces that have tables within markup and 
  # apply the same functions
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
}
# Put all the data frames into a list.
all_tables <- c(
  table_one, alt_tables
)
head(all_tables[8])
plays <- as.data.frame(all_tables[8])
plays[26,"Play.Description"]
doubles_OAK <- 0
doubles_ANA <- 0
doubles_OAK_scored <- 0
doubles_ANA_scored <- 0
p <- c(1:nrow(plays))
for(val in p){
  #print(plays[val,"Play.Description"])
  if(startsWith(plays[val,"Play.Description"], "Double to") || startsWith(plays[val,"Play.Description"], "Ground-rule Double")){
    print("Double")
    if(plays[val,"X.Bat"] == "SEA"){
      doubles_OAK <- doubles_OAK + 1
    }
    else{
      doubles_ANA <- doubles_ANA + 1
    }
    p_full <- plays[val,"Batter"]
    lst <- strsplit(plays[val,"Batter"], "\\s+")
    v1 <- sapply(lst ,`[`, 1)
    player <- sapply(lst, `[`, 2)
    if(length(lst) > 2){
      for(y in 3:length(lst)){
        player <- strcat(player, " ")
        player <- strcat(player,sapply(lst, `[`, y))
      }
    }
    player <- strcat(player, " Scores")
    x <- val
    while(!startsWith(plays[x, "Inn"], "Top of the") & !startsWith(plays[x, "Inn"], "Bottom of the")){
      if(str_detect(plays[x,"Play.Description"], strcat("pinch runs for ", p_full))){
        lst <- strsplit(plays[x,"Play.Description"], "\\s+")
        p <- 1
        while(sapply(lst, `[`, p) != "pinch"){
          p <- p + 1
        }
        p_full <- sapply(lst ,`[`, p - 2)
        p_full <- strcat(p_full, " ")
        p_full <- strcat(p_full, sapply(lst ,`[`, p - 1))
        player <- sapply(lst ,`[`, p - 1)
        player <- strcat(player, " Scores")
        print(player)
      }
      if(str_detect(plays[x,"Play.Description"], player)){
        if(plays[val,"X.Bat"] == "SEA"){
          doubles_OAK_scored <- doubles_OAK_scored + 1
        }
        else{
          doubles_ANA_scored <- doubles_ANA_scored + 1
        }
      }
      x <- x + 1
    }
  }
}

doubles_OAK
doubles_ANA
doubles_OAK_scored
doubles_ANA_scored