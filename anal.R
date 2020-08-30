
library(ggplot2)
library(dplyr)
library(GGally)
library(ggrepel)
data <- read.xlsx("C:/users/tdpot/Triples/data.xlsx", "Sheet1")
data[,1] <- as.character(data[,1])
#plotmatrix(with(data, data.frame(Win_Percent, Doubles_No_Outs_Percent, Doubles_One_Out_Percent, Doubles_Two_Outs_Percent)))

fit <- lm(Win_Percent ~ Doubles_No_Outs_Percent + Doubles_One_Out_Percent + Doubles_Two_Outs_Percent + Triples_No_Outs_Percent + Triples_One_Out_Percent + Triples_Two_Outs_Percent, data = data)
summary(fit)
plot(fit)

fit2 <- lm(Win_Percent ~ Doubles_No_Outs_Percent + Doubles_Two_Outs_Percent, data = data)
summary(fit2)
plot(fit)

plot(data$Doubles_No_Outs_Percent + data$Doubles_One_Out_Percent + data$Doubles_Two_Outs_Percent, data$Win_Percent)
abline(fit)

d <- data %>%
  select(Team, Win_Percent, Doubles_No_Outs_Percent, Doubles_One_Out_Percent, Doubles_Two_Outs_Percent, Triples_No_Outs_Percent,
         Triples_One_Out_Percent, Triples_Two_Outs_Percent)


require(tidyr)
nl_east <- d %>%
  filter(Team %in% c("ATL","MIA","NYM","PHI","WSN"))
nl_east_long <- gather(nl_east, Variable,value, -Team)

ggplot(data = nl_east_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("National League East Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))

nl_cen <- d %>%
  filter(Team %in% c("CHC","CIN","MIL","PIT","STL"))
nl_cen_long <- gather(nl_cen, Variable,value, -Team)

ggplot(data = nl_cen_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("National League Central Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))

nl_west <- d %>%
  filter(Team %in% c("ARI","COL","LAD","SDP","SFG"))
nl_west_long <- gather(nl_west, Variable,value, -Team)

ggplot(data = nl_west_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("National League West Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))


al_east <- d %>%
  filter(Team %in% c("BAL","BOS","NYY","TBR","TOR"))
al_east_long <- gather(al_east, Variable,value, -Team)

ggplot(data = al_east_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("American League East Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))

al_cen <- d %>%
  filter(Team %in% c("CHW","CLE","DET","KCR","MIN"))
al_cen_long <- gather(al_cen, Variable,value, -Team)

ggplot(data = al_cen_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("American League Central Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))

al_west <- d %>%
  filter(Team %in% c("HOU","LAA","OAK","SEA","TEX"))
al_west_long <- gather(al_west, Variable,value, -Team)

ggplot(data = al_west_long, aes(x = Team, y = value, fill = Variable)) +
  geom_col(position = position_dodge()) +
  ggtitle("National League West Division") + 
  ylab("Percentage of Variable") +
  theme(plot.title = element_text(hjust = 0.5))

d_no_mean <- sum(data$Doubles_No_Outs_Scored) / sum(data$Doubles_No_Outs)
ggplot(data = d, aes(x = Team, y = Doubles_No_Outs_Percent)) + 
  geom_col() +
  geom_hline(yintercept = d_no_mean, color = "red", size = 2) + 
  ylab("Percentage of Doubles With No Outs that Scored") +
  ggtitle("Percentage of Doubles With No Outs Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 23, y =.7, label = "League Average = .604", color = "black")

d_one_mean <- sum(data$Doubles_One_Out_Scored) / sum(data$Doubles_One_Out)
ggplot(data = d, aes(x = Team, y = Doubles_One_Out_Percent)) + 
  geom_col() +
  geom_hline(yintercept = d_one_mean, color = "red", size = 2) + 
  ylab("Percentage of Doubles With One Out that Scored") +
  ggtitle("Percentage of Doubles With One Out Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 25, y =.5, label = "League Average = .410", color = "black")

d_two_mean <- sum(data$Doubles_Two_Outs_Scored) / sum(data$Doubles_Two_Outs)
ggplot(data = d, aes(x = Team, y = Doubles_Two_Outs_Percent)) + 
  geom_col() +
  geom_hline(yintercept = d_two_mean, color = "red", size = 2) + 
  ylab("Percentage of Doubles With Two Outs that Scored") +
  ggtitle("Percentage of Doubles With Two Outs Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 23, y =.25, label = "League Average = .218", color = "red")

t_no_mean <- sum(data$Triples_No_Outs_Scored) / sum(data$Triples_No_Outs)
ggplot(data = d, aes(x = Team, y = Triples_No_Outs_Percent)) + 
  geom_col() +
  geom_hline(yintercept = t_no_mean, color = "red", size = 2) + 
  ylab("Percentage of Triples With No Outs that Scored") +
  ggtitle("Percentage of Triples With No Outs Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 27, y =.9, label = "League Average = .819", color = "red")

t_one_mean <- sum(data$Triples_One_Out_Scored) / sum(data$Triples_One_Out)
ggplot(data = d, aes(x = Team, y = Triples_One_Out_Percent)) + 
  geom_col() +
  geom_hline(yintercept = t_one_mean, color = "red", size = 2) + 
  ylab("Percentage of Triples With One Out that Scored") +
  ggtitle("Percentage of Triples With One Out Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 8, y =.85, label = "League Average = .624", color = "red")

t_two_mean <- sum(data$Triples_Two_Outs_Scored) / sum(data$Triples_Two_Outs)
ggplot(data = d, aes(x = Team, y = Triples_Two_Outs_Percent)) + 
  geom_col() +
  geom_hline(yintercept = t_two_mean, color = "red", size = 2) + 
  ylab("Percentage of Triples With Two Outs that Scored") +
  ggtitle("Percentage of Triples With Two Outs Scored by Team") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(x = 23, y =.6, label = "League Average = .298", color = "red")

corData <- d %>%
  select(-Team)
ggpairs(corData)
cor(corData)

ggplot(data = d, aes(x = Doubles_No_Outs_Percent, y = Win_Percent, label = Team)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel() +
  ggtitle("Percentage of Doubles with No Outs that Scored Vs. Winning Percentage")

ggplot(data = d, aes(x = Doubles_Two_Outs_Percent, y = Win_Percent, label = Team)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label_repel() +
  ggtitle("Percentage of Doubles with Two Outs that Scored Vs. Winning Percentage")

lnData <- d %>%
  mutate(Win_Percent = log10(Win_Percent))

ggplot(data = lnData, aes(x = Doubles_Two_Outs_Percent, y = Win_Percent, label = Team)) + 
  geom_point() +
  geom_smooth(method = "auto", se = FALSE) +
  geom_label_repel() +
  ggtitle("Percentage of Doubles with Two Outs that Scored Vs. Winning Percentage")
lnData %>%
  select(-Team) %>%
  cor()



dan <- read.csv("C:/users/tdpot/Triples/savant_data.csv")
head(dan)
dan <- dan %>%
  select(plate_x, plate_z, pitch_name)

dan
dan %>%
  ggplot(aes(x = plate_x, y = plate_z)) + geom_point(aes(color = pitch_name), size = 5) +
  geom_segment(aes(x = -0.8, y = 1.5, xend = -0.8, yend = 3.3)) +
  geom_segment(aes(x = -0.8, y = 1.5, xend = 0.8, yend = 1.5)) +
  geom_segment(aes(x = 0.8, y = 1.5, xend = 0.8, yend = 3.3)) +
  geom_segment(aes(x = -0.8, y = 3.3, xend = 0.8, yend = 3.3))
  
