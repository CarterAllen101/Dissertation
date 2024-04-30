#This code is for sbs.csv data
library(dplyr)
library(ggplot2)
###########################
#Prepare shot by shot data#
###########################
#Read data set for sbs.csv and call this as data1
data1<- sbs
#Split the dataset by gender, fix typos/capitalization
womenmatchessbs <- subset(data1, Type == "women's singles")
menmatchessbs <- subset(data1, Type == "men's singles")
womenmatchessbs$`Player Gamestyle`[womenmatchessbs$`Player Gamestyle` == "Big Server / Aggressive baseliner"] <- "Big server / Aggressive baseliner"
menmatchessbs$`Player Gamestyle`[menmatchessbs$`Player Gamestyle` == "Aggressive Baseliner"] <- "Aggressive baseliner"
menmatchessbs$`Player Gamestyle`[menmatchessbs$`Player Gamestyle` == "Big Server / Aggressive Baseliner"] <- "Big server / Aggressive baseliner"
menmatchessbs$`Player Gamestyle`[menmatchessbs$`Player Gamestyle` == "Big server / aggressive baseliner"] <- "Big server / Aggressive baseliner"
womenmatchessbs$`Player Gamestyle`[womenmatchessbs$`Player Gamestyle` == "Aggressive Baseliner"] <- "Aggressive baseliner"
womenmatchessbs$`Player Gamestyle`[womenmatchessbs$`Player Gamestyle` == "Big Server / Aggressive Baseliner"] <- "Big server / Aggressive baseliner"
womenmatchessbs$`Player Gamestyle`[womenmatchessbs$`Player Gamestyle` == "Big server / aggressive baseliner"] <- "Big server / Aggressive baseliner"



#Check classifications
unique(womenmatchessbs$`Player Gamestyle`)
unique(menmatchessbs$`Player Gamestyle`)

#Unclassified players
unclassifiedwomensbs <- subset(womenmatchessbs, is.na(womenmatchessbs$`Player Gamestyle`))
unique(unclassifiedwomensbs$Player)
unclassifiedwomensbs[unclassifiedwomensbs == "T¡mea Babos"] <- "Tímea Babos"
unclassifiedmensbs <- subset(menmatchessbs, is.na(menmatchessbs$`Player Gamestyle`))
unique(unclassifiedmensbs$Player)

#Join different player names
unique(menmatchessbs$Player)
unique(womenmatchessbs$Player)
womenmatchessbs$Player[womenmatchessbs$Player == "T¡mea Babos"] <- "Tímea Babos"

#Join shot types
unique(menmatchessbs$`Shot Type`)
unique(womenmatchessbs$`Shot Type`)

menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Drop Volley"] <- "Volley"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Half Volley"] <- "Volley"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Drive Volley"] <- "Volley"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Lob Volley"] <- "Lob"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Drop"] <- "Drop Shot"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Other"] <- "Groundstroke"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Lob"] <- "Other"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Pick-Up"] <- "Other"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Smash"] <- "Other"
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Unclassified"] <- "Other"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Drop Volley"] <- "Volley"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Half Volley"] <- "Volley"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Drive Volley"] <- "Volley"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Drive V"] <- "Volley"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Lob Volley"] <- "Lob"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Drop"] <- "Drop Shot"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Other"] <- "Groundstroke"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Lob"] <- "Other"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Pick-Up"] <- "Other"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Smash"] <- "Other"
womenmatchessbs$`Shot Type`[womenmatchessbs$`Shot Type` == "Unclassified"] <- "Other"

#Remove low level tournaments and fix mislabelling
unique(womenmatchessbs$`Tournament Level`)
womenmatchessbs<-womenmatchessbs[!(womenmatchessbs$`Tournament Level`== "Return to play"),]
womenmatchessbs<-womenmatchessbs[!(womenmatchessbs$`Tournament Level`== "ITF"),]
unique(menmatchessbs$`Tournament Level`)
menmatchessbs$`Tournament Level`[menmatchessbs$`Tournament Level` == "WTA 500"] <- "ATP 500"
menmatchessbs<-menmatchessbs[!(menmatchessbs$`Tournament Level`=="ATP Challenger"),]
menmatchessbs<-menmatchessbs[!(menmatchessbs$`Tournament Level`=="Return to Play"),]
menmatchessbs<-menmatchessbs[!(menmatchessbs$`Tournament Level`=="Return to play"),]

#Fix labelling
menmatchessbs$`Shot Type`[menmatchessbs$`Shot Type` == "Drop"] <- "Drop Shot"

#########################
#Rally Length Figure 2.1#
#########################
#Calculate rally length for each point by counting the number of rows for each point index in each match. It is grouped by date to ensure each match is accounted for.
#Men
menrally_length_per_match <- menmatchessbs %>%
  group_by(Date, Player, Opponent, `Point Index`) %>%
  summarize(RallyLength = n())


#Merging this information back with the original data to get the game styles
menrally_length_with_style <- menrally_length_per_match %>%
  left_join(menmatchessbs[, c("Date", "Player", "Opponent", "Point Index", "Player Gamestyle")], 
            by = c("Date", "Player", "Opponent", "Point Index"))

#Remove duplicates
menrally_length_with_style <- menrally_length_with_style[!duplicated(menrally_length_with_style[, c("Date", "Player", "Opponent", "Point Index")]), ]

#Women
womenrally_length_per_match <- womenmatchessbs %>%
  group_by(Date, Player, Opponent, `Point Index`) %>%
  summarize(RallyLength = n())


#Merging this information back with the original data to get the game styles
womenrally_length_with_style <- womenrally_length_per_match %>%
  left_join(womenmatchessbs[, c("Date", "Player", "Opponent", "Point Index", "Player Gamestyle")], 
            by = c("Date", "Player", "Opponent", "Point Index"))

#Remove duplicates
womenrally_length_with_style <- womenrally_length_with_style[!duplicated(womenrally_length_with_style[, c("Date", "Player", "Opponent", "Point Index")]), ]

#Filter out NA game styles for men and women
menrally_length_with_style <- menrally_length_with_style %>%
  filter(!is.na(`Player Gamestyle`))

womenrally_length_with_style <- womenrally_length_with_style %>%
  filter(!is.na(`Player Gamestyle`))

#Add a gender identifier to each dataset
menrally_length_with_style$Gender <- 'Men'
womenrally_length_with_style$Gender <- 'Women'


#Combine the two datasets
combined_rally_length <- rbind(menrally_length_with_style, womenrally_length_with_style)

#Create the plot
ggplot(combined_rally_length, aes(x = `Player Gamestyle`, y = log(RallyLength), fill = Gender)) +
  geom_violin(position = position_dodge(width = 0.8)) +
  labs(title = "Boxplot and Violin Plot of Rally Lengths for Each Game Style by Gender", x = "Game Style", y = "Log Rally Length") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.1) +
  scale_fill_manual(values = c("blue", "pink")) 


################################################################
#Histogram of Shot Type by Game Style Figure 2.2 and Figure 2.3#
################################################################
#Men
filtered_menshottypedata <- menmatchessbs[!is.na(menmatchessbs$`Shot Type`), ]
filtered_menshottypedata <- filtered_menshottypedata[!is.na(filtered_menshottypedata$`Player Gamestyle`), ]

#Count the shot types
mengamestyle_shots2 <- filtered_menshottypedata %>%
  group_by(PlayerGamestyle = `Player Gamestyle`, ShotType = `Shot Type`) %>%
  summarise(Count = n()) %>%
  group_by(PlayerGamestyle) %>%
  mutate(Percentage = Count / sum(Count))


ggplot(mengamestyle_shots, aes(x = PlayerGamestyle, y = Percentage, fill = ShotType)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Block" = "#FFF500", "Drop Shot" = "#00FF75", "Groundstroke" = "#CD76EA", "Other" = "#FFB443", "Slice" = "#FF5E5E", "Volley" = "#39DBFF")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Percentage Comparison of Shot Types by Game Style for Male Players",
       x = "Player Gamestyle",
       y = "Percentage of Shots",
       fill = "Shot Type")

#Women
filtered_womenshottypedata <- womenmatchessbs[!is.na(womenmatchessbs$`Shot Type`), ]
filtered_womenshottypedata <- filtered_womenshottypedata[!is.na(filtered_womenshottypedata$`Player Gamestyle`), ]

#Count the shot types
womengamestyle_shots <- filtered_womenshottypedata %>%
  group_by(PlayerGamestyle = `Player Gamestyle`, ShotType = `Shot Type`) %>%
  summarise(Count = n())%>%
  group_by(PlayerGamestyle) %>%
  mutate(Percentage = Count / sum(Count))

ggplot(womengamestyle_shots, aes(x = PlayerGamestyle, y = Percentage, fill = ShotType)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Block" = "#FFF500", "Drop Shot" = "#00FF75", "Groundstroke" = "#CD76EA", "Other" = "#FFB443", "Slice" = "#FF5E5E", "Volley" = "#39DBFF"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Percentage Comparison of Shot Types by Game Style for Female Players",
       x = "Player Gamestyle",
       y = "Percentage of Shots",
       fill = "Shot Type")

