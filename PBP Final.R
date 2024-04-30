#This code is for pbp.csv data
library(tidyr)
library(patchwork)
library(cowplot)
library(dplyr)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(caret)

#############################
#Prepare point by point data#
#############################
#Load pbp.csv for this code.
data2 <- pbp

#Split the dataset by gender
womenmatchespbp <- subset(data2, Type == "women's singles")
menmatchespbp <- subset(data2, Type == "men's singles")
womenmatchespbp$`Player Gamestyle`[womenmatchespbp$`Player Gamestyle` == "Big server / aggressive baseliner"] <- "Big server / Aggressive baseliner"
womenmatchespbp$`Player Gamestyle`[womenmatchespbp$`Player Gamestyle` == "Aggressive Baseliner"] <- "Aggressive baseliner"
menmatchespbp$`Player Gamestyle`[menmatchespbp$`Player Gamestyle` == "Aggressive Baseliner"] <- "Aggressive baseliner"
menmatchespbp$`Player Gamestyle`[menmatchespbp$`Player Gamestyle` == "Big Server / Aggressive Baseliner"] <- "Big server / Aggressive baseliner"
menmatchespbp$`Player Gamestyle`[menmatchespbp$`Player Gamestyle` == "Big server / aggressive baseliner"] <- "Big server / Aggressive baseliner"
menmatchespbp$`Player Gamestyle`[menmatchespbp$`Player Gamestyle` == "Big Server"] <- "Big server"

#Check classifications
unique(womenmatchespbp$`Player Gamestyle`)
unique(menmatchespbp$`Player Gamestyle`)

#Remove low level tournaments
unique(womenmatchespbp$`Tournament Level`)
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "Return to play"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "Return to Play"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "UK Pro Series"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "ITF"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "UK Pro League"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "ITF 25k"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "Exhibition"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "Return to Tennis"),]
womenmatchespbp<-womenmatchespbp[!(womenmatchespbp$`Tournament Level`== "WTA 125"),]
womenmatchespbp$`Tournament Level`[womenmatchespbp$`Tournament Level` == "ATP 1000"] <- "WTA 1000"
womenmatchespbp$`Tournament Level`[womenmatchespbp$`Tournament Level` == "ATP 500"] <- "WTA 500"
womenmatchespbp$`Tournament Level`[womenmatchespbp$`Tournament Level` == "ATP 250"] <- "WTA 250"

unique(menmatchespbp$`Tournament Level`)
menmatchespbp$`Tournament Level`[menmatchespbp$`Tournament Level` == "WTA 500"] <- "ATP 500"
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="ATP Challenger"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="ITF"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="Return to play"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="UK Pro Series"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="Return to Play"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="Exhibition"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="R2P"),]
menmatchespbp<-menmatchespbp[!(menmatchespbp$`Tournament Level`=="Exibition"),]

###########################################################
#Plot for number of matches recorded per player Figure 2.4#
###########################################################
#Count the number of points recorded per player
player_counts <- table(menmatchespbp$Player)

#Convert to a data frame for ggplot
player_counts_df <- as.data.frame(player_counts)
names(player_counts_df) <- c("Player", "NumberOfMatches")

#Create the bar plot
menmatches <- ggplot(player_counts_df, aes(x = reorder(Player, -NumberOfMatches), y = NumberOfMatches)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Number of Matches Recorded ATP Player",
       x = "Player",
       y = "Number of Matches Recorded")

#Count the number of points recorded per player
player_countswomen <- table(womenmatchespbp$Player)

#Convert to a data frame for ggplot
player_counts_dfwomen <- as.data.frame(player_countswomen)
names(player_counts_dfwomen) <- c("Player", "NumberOfMatches")

#Create the bar plot
womenmatches <- ggplot(player_counts_dfwomen, aes(x = reorder(Player, -NumberOfMatches), y = NumberOfMatches)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Number of Matches Recorded WTA Player",
       x = "Player",
       y = "Number of Matches Recorded")
grid.arrange(menmatches, womenmatches, ncol = 1, nrow = 2)

#########################################
#Frequency of each game style Figure 2.5#
#########################################
#Count the frequency of each game style with unique players for men
unique_men <- menmatchespbp[!duplicated(menmatchespbp$Player), ]
men_gamestyle_counts <- table(unique_men$`Player Gamestyle`)

#Count the frequency of each game style with unique players for women
unique_women <- womenmatchespbp[!duplicated(womenmatchespbp$Player), ]
women_gamestyle_counts <- table(unique_women$`Player Gamestyle`)

#Convert to data frames for ggplot
men_gamestyle_counts_df <- as.data.frame(men_gamestyle_counts)
women_gamestyle_counts_df <- as.data.frame(women_gamestyle_counts)
new_row <- data.frame(Var1 = "Big server", Freq = NA)
women_gamestyle_counts_df <- rbind(women_gamestyle_counts_df, new_row)


#Rename columns
names(men_gamestyle_counts_df) <- c("GameStyle", "Frequency")
names(women_gamestyle_counts_df) <- c("GameStyle", "Frequency")


#Add a column for gender
men_gamestyle_counts_df$Gender <- 'Male'
women_gamestyle_counts_df$Gender <- 'Female'

#Combine the two data frames
gamestyle_counts_df <- rbind(men_gamestyle_counts_df, women_gamestyle_counts_df)

#Create bar plot
ggplot(gamestyle_counts_df, aes(x = GameStyle, y = Frequency, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Frequency, y = Frequency/2, group = Gender),
            position = position_dodge(width = 0.8), size = 3, vjust = 0.5) +
  theme_minimal() +
  coord_flip() +  #Flips the axes for better readability
  labs(title = "Frequency of Game Style by Gender (Unique Player-Game Style Combinations)",
       x = "Game Style",
       y = "Frequency") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"),
                    limits = c("Male", "Female"))

####################################################################
#Proportion of rally lengths for each game style Figure 2.6 and 2.7#
####################################################################
#Prepare the data
rallylengthdata <- menmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(long_rally_total = sum(long_rally_total, na.rm = TRUE),
            medium_rally_total = sum(medium_rally_total, na.rm = TRUE),
            short_rally_total = sum(short_rally_total, na.rm = TRUE)) %>%
  mutate(Total = long_rally_total + medium_rally_total + short_rally_total,
         Long_Rally_Prop = long_rally_total / Total,
         Medium_Rally_Prop = medium_rally_total / Total,
         Short_Rally_Prop = short_rally_total / Total) 
rallylengthdata <- rallylengthdata[,c(1,6:8)]
rallylengthdata <- melt(rallylengthdata)
rallylengthdata <- rallylengthdata %>% rename(Rally_Type = variable, Proportion = value)


pie_charts <- list()

#Create pie charts for each game style
for(game_style in unique(rallylengthdata$`Player Gamestyle`)) {
  game_style_subset <- rallylengthdata[rallylengthdata$`Player Gamestyle` == game_style, ]
  
  pie_chart <- ggplot(game_style_subset, aes(x = "", y = Proportion, fill = Rally_Type)) +
    geom_col() +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 5) +
    coord_polar(theta = "y", direction = 1) +
    theme_void() +
    labs(fill = "Rally Type", title = game_style)
  
  pie_charts[[game_style]] <- pie_chart
}

#Combine all pie charts into a single plot and create space for the legend
combined_plot <- wrap_plots(pie_charts, ncol = 3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

#Add main title to the combined plot
combined_plot <- combined_plot + 
  plot_annotation(title = "Proportion of Rally Lengths for Each Male Game Style")

#Plot 
combined_plot

#Prepare the data for women
rallylengthdata <- womenmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(short_rally_total = sum(short_rally_total, na.rm = TRUE),
            medium_rally_total = sum(medium_rally_total, na.rm = TRUE),
            long_rally_total = sum(long_rally_total, na.rm = TRUE)) %>%
  mutate(Total = short_rally_total + medium_rally_total + long_rally_total,
         long_Rally_Prop = long_rally_total / Total,
         Medium_Rally_Prop = medium_rally_total / Total,
         short_Rally_Prop = short_rally_total / Total) 
rallylengthdata <- rallylengthdata[,c(1,6:8)]
rallylengthdata <- melt(rallylengthdata)
rallylengthdata <- rallylengthdata %>% rename(Rally_Type = variable, Proportion = value)


pie_charts <- list()

#Create pie charts for each game style
for(game_style in unique(rallylengthdata$`Player Gamestyle`)) {
  game_style_subset <- rallylengthdata[rallylengthdata$`Player Gamestyle` == game_style, ]
  
  pie_chart <- ggplot(game_style_subset, aes(x = "", y = Proportion, fill = Rally_Type)) +
    geom_col() +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 5) +
    coord_polar(theta = "y", direction = 1) +
    theme_void() +
    labs(fill = "Rally Type", title = game_style)
  
  pie_charts[[game_style]] <- pie_chart
}

#Combine all pie charts into a single plot and create space for the legend
combined_plot <- wrap_plots(pie_charts, ncol = 3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

#Add main title to the combined plot
combined_plot <- combined_plot + 
  plot_annotation(title = "Proportion of Rally Lengths for Each Female Game Style")

#Plot 
combined_plot

################################
#Aces Per Serve Plot Figure 2.8#
################################
#Group by Player Gamestyle and calculate mean of aces_per_serve
mengamestyle_aces <- menmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(Average_Aces_Per_Serve = mean(aces_per_serve, na.rm = TRUE))


#Group by Player Gamestyle and calculate mean of aces_per_serve
womengamestyle_aces <- womenmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(Average_Aces_Per_Serve = mean(aces_per_serve, na.rm = TRUE))


#Add a gender identifier to each dataset
mengamestyle_aces$Gender <- 'Men'
womengamestyle_aces$Gender <- 'Women'

#Combine the two datasets
combined_gamestyle_aces <- rbind(mengamestyle_aces, womengamestyle_aces)
#Women do not have big servers, set as 0
new_row <- data.frame(`Player Gamestyle` = "Big server", 
                      Average_Aces_Per_Serve = NA, 
                      Gender = "Women")

#Column names match exactly with those in combined_gamestyle_aces
names(new_row) <- names(combined_gamestyle_aces)

#Add the new row to the combined_gamestyle_aces dataframe
combined_gamestyle_aces <- rbind(combined_gamestyle_aces, new_row)


#Create the bar plot with value labels
ggplot(combined_gamestyle_aces, aes(x = `Player Gamestyle`, y = Average_Aces_Per_Serve, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.3) + 
  geom_text(aes(label = round(Average_Aces_Per_Serve, 2)), 
            position = position_dodge(width = 0.7), vjust = -0.25, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Men" = "blue", "Women" = "pink")) +
  ylim(0, 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Aces Per Serve by Player Gamestyle and Gender",
       x = "Player Gamestyle",
       y = "Average Aces Per Serve")

######################################################
#Average Percentage of Effective 1st Serve Figure 2.9#
######################################################
#Group by Player Gamestyle and calculate mean of aces_per_serve
mengamestyle_s1 <- menmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(Average_S1 = mean(effective_s1, na.rm = TRUE))


#Group by Player Gamestyle and calculate mean of aces_per_serve
womengamestyle_s1 <- womenmatchespbp %>%
  group_by(`Player Gamestyle`) %>%
  summarise(Average_S1 = mean(effective_s1, na.rm = TRUE))


#Add a gender identifier to each dataset
mengamestyle_s1$Gender <- 'Men'
womengamestyle_s1$Gender <- 'Women'

#Combine the two datasets
combined_gamestyle_s1 <- rbind(mengamestyle_s1, womengamestyle_s1)
#Women do not have big servers, set as 0
new_rows1 <- data.frame(`Player Gamestyle` = "Big server", 
                        Average_S1 = NA, 
                        Gender = "Women")

#Column names match exactly with those in combined_gamestyle_aces
names(new_rows1) <- names(combined_gamestyle_s1)

#Add the new row to the combined_gamestyle_aces dataframe
combined_gamestyle_s1 <- rbind(combined_gamestyle_s1, new_rows1)


#Create the bar plot with value labels
ggplot(combined_gamestyle_s1, aes(x = `Player Gamestyle`, y = Average_S1, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.3) + 
  geom_text(aes(label = round(Average_S1, 2)), 
            position = position_dodge(width = 0.7), vjust = -0.25, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Men" = "blue", "Women" = "pink")) +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Percentage of Effective First Serve by Player Gamestyle and Gender",
       x = "Player Gamestyle",
       y = "Average Aces Per Serve")
################################EDA Done################################

################################MENS################################
#####
#PCA#
#####
#We only use the percentages after attacking errors total for the PCA.
#Find the index of the column "attacking_errors_total"
index_of_attacking_errors_total <- which(names(menmatchespbp) == "attacking_errors_total")

#Selecting columns after "attacking_errors_total" for PCA
columns_after <- names(menmatchespbp)[(index_of_attacking_errors_total + 1):ncol(menmatchespbp)]

#Filtering out rows with any NA values in the columns used for PCA
menmatchespbpclean <- na.omit(menmatchespbp[,c("Player", "Player Gamestyle", columns_after)])

#Summary Stats data set
menmatchesstats <- na.omit(menmatchespbpclean[, columns_after])

#Extracting the game styles corresponding to the cleaned data
game_styles_clean <- menmatchespbpclean$`Player Gamestyle`
name_clean <- menmatchespbpclean$Player

#Perform PCA on the cleaned data without the game styles
pca_result <- prcomp(menmatchespbpclean[, 3:ncol(menmatchespbpclean)], 
                     center = TRUE, scale. = TRUE)
#Print a summary of the PCA results
summary(pca_result)

#pca_result$x is the score for each player
#Join the scores with game style and player name
pca_scores <- cbind(as.data.frame(pca_result$x), Gamestyle = game_styles_clean, Player = name_clean)

#Get the variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

#Calculate the cumulative variance explained
cumulative_variance <- cumsum(variance_explained)

#Create a data frame for plotting
scree_data <- data.frame(
  PC = 1:length(variance_explained),
  Variance = variance_explained,
  CumulativeVariance = cumulative_variance
)

#Generate the scree plot
ggplot(scree_data, aes(x = PC)) +
  geom_bar(aes(y = Variance, fill = "Individual Variance"), stat = "identity") +
  geom_line(aes(y = CumulativeVariance, color = "Cumulative Variance"), linewidth = 1) +
  geom_point(aes(y = CumulativeVariance, color = "Cumulative Variance"), size = 2) +
  scale_fill_manual(name = "Legend", values = c("Individual Variance" = "steelblue")) +
  scale_color_manual(name = "", values = c("Cumulative Variance" = "darkred")) +
  scale_y_continuous(labels = scales::percent, sec.axis = sec_axis(~ ., name = "Cumulative Variance")) +
  labs(x = "Principal Component", y = "Variance Explained (%)") +
  geom_hline(yintercept=0.70, linetype="dashed", color = "red") +
  theme_minimal() +
  ggtitle("Scree Plot for ATP Data") +
  theme(legend.position = "bottom")


#Biplot
#We will use only the first two principal components for the biplot with zones
autoplot(pca_result, data = pca_scores, 
         colour = 'Gamestyle', frame = TRUE, 
         loadings = F, loadings.label = F, 
         loadings.colour = 'black', loadings.label.size = 3) +
  theme_minimal() +
  ggtitle("Biplot on PC1 and PC2 for ATP Data")

#Kaiser Rule
#Square the standard deviations to get the eigenvalues
eigenvalues <- pca_result$sdev^2

#Print the eigenvalues
print(eigenvalues)
#Based on Kaiser Criterion, drop PCs with EV<1. Take first 7 PC.

#Modelling
###############
#Random Forest#
###############
#Take first 10 PCs, use the 70% cut off rule
pca_scoresrf <- pca_scores[,c(1:10, 27, 28)]
#Split the data into test and train
set.seed(123)  
index <- createDataPartition(pca_scoresrf$Gamestyle, p=0.8, list=FALSE)
trainData <- pca_scoresrf[index, -which(names(pca_scoresrf) == "Player")]
testData <- pca_scoresrf[-index, -which(names(pca_scoresrf) == "Player")]

#Ensure Gamestyle is a factor
trainData$Gamestyle <- as.factor(trainData$Gamestyle)
testData$Gamestyle <- as.factor(testData$Gamestyle)

#Check the distribution of classes in the 'Gamestyle' column
class_distribution <- table(pca_scoresrf$Gamestyle)

#Calculate the relative frequency of each class to check for imbalance for Table 3.2
class_distribution_relative <- prop.table(class_distribution)
print(class_distribution_relative)
##############################BASE#########################################
#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelOptimized <- randomForest(
  Gamestyle ~ ., 
  data = trainData, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsOptimized <- predict(rfModelOptimized, testData)

#Get confusion matrix from caret package, can also observe other metrics here
confMatrixOptimized <- confusionMatrix(predictionsOptimized, testData$Gamestyle)

#Print the optimized model's confusion matrix
print(confMatrixOptimized)

#Convert confusion matrix to a data frame for ggplot
confMatPost <- as.data.frame(confMatrixOptimized$table)

row_sums_post <- confMatPost %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_post <- confMatPost %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatPost <- left_join(confMatPost, row_sums_post, by = "Reference")
confMatPost <- left_join(confMatPost, col_sums_post, by = "Prediction")

#Plot the confusion matrix with row and column sums for Figure 5.1a
plotBase <- ggplot(data = confMatPost, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_post, aes(x = Reference, y = 6, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_post, aes(x = 6, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotBase)

#Extract the error rate for Base Model
error_rate_post <- rfModelOptimized$err.rate
error_df_post <- data.frame(Trees = seq_len(nrow(error_rate_post)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate for Figure 3.2
ggplot(error_df_post, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for ATP Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart for Figure 3.4
varImpPlot(rfModelOptimized, main = "Variable Importance for ATP Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelOptimized, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.2a
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) +  #Scale density to appear as proportion
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilites of each game style for each player. Do this to whole dataset.
predictedProbsall <- predict(rfModelOptimized, pca_scoresrf, type = "prob")
predictedProbsall_df <- as.data.frame(predictedProbsall)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsall_df$Player <- pca_scoresrf$Player
predictedProbsall_df$Player[predictedProbsall_df$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
pca_scoresrf$Player[pca_scoresrf$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
#Calculate the mean of each game style by player
player_means <- predictedProbsall_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check. Need the row sum to be 1 as it is a probabilitiy
row_sums <- rowSums(player_means[-1])
row_sums

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropy <- player_means %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropy, aes(x = Entropy)) +  
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Entropy Scores for ATP Data", x = "Entropy Score", y = "Density") +  
  theme_minimal()   

#Cross Entropy
#Obtain the true classes of each player by matching from player_means with pca results. Match function will return index from pca_scoresrf data.
true_classes <- pca_scoresrf$Gamestyle[match(player_means$Player, pca_scoresrf$Player)]
true_classes

cross_entropy_scores <- c()

#Loop over each player
for (i in 1:nrow(player_means)) {
  true_class_index <- which(colnames(player_means) == true_classes[i])
  true_prob <- player_means[i, true_class_index]
  cross_entropy_scores[i] <- -log(true_prob)
}

cross_entropy_scores <- as.numeric(cross_entropy_scores)
player_cross_entropy <- data.frame(Player = player_means$Player, CrossEntropy = cross_entropy_scores)
#Plot cross entropy scores
ggplot(player_cross_entropy, aes(x = CrossEntropy, y = ..scaled..)) + 
  geom_density(fill = "#008080", alpha = 0.5) + 
  labs(title = "Density Plot of Cross-Entropy Scores for ATP Data", x = "Cross-Entropy Score", y = "Density") + 
  theme_minimal()   

##################SMOTE#################################################
library(recipes)
library(themis)
rec <- recipe(Gamestyle ~ ., data = trainData) %>%
  step_smote(Gamestyle)  #Apply SMOTE to fix imbalance
trainDataSmote <- prep(rec) %>%
  juice()
table(trainDataSmote$Gamestyle)

#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelSMOTE <- randomForest(
  Gamestyle ~ ., 
  data = trainDataSmote, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsSMOTE <- predict(rfModelSMOTE, testData)

#Get the confusion matrix from caret package, can also observe other metrics here
confMatrixSMOTE <- confusionMatrix(predictionsSMOTE, testData$Gamestyle)

#Print the SMOTE model's confusion matrix
print(confMatrixSMOTE)

#Convert confusion matrix to a data frame for ggplot
confMatSMOTE <- as.data.frame(confMatrixSMOTE$table)

row_sums_SMOTE <- confMatSMOTE %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_SMOTE <- confMatSMOTE %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatSMOTE <- left_join(confMatSMOTE, row_sums_SMOTE, by = "Reference")
confMatSMOTE <- left_join(confMatSMOTE, col_sums_SMOTE, by = "Prediction")

#Plot the SMOTE confusion matrix with row and column sums for Figure 5.1b
plotSMOTE <- ggplot(data = confMatSMOTE, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_SMOTE, aes(x = Reference, y = 6, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_SMOTE, aes(x = 6, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotSMOTE)

#Extract the error rate for SMOTE model
error_rate_SMOTE <- rfModelSMOTE$err.rate
error_df_SMOTE <- data.frame(Trees = seq_len(nrow(error_rate_SMOTE)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate, same result, at 800 trees, it stabilizes
ggplot(error_df_SMOTE, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for ATP Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart, very similar with base model. PC2 and 3 are important.
varImpPlot(rfModelSMOTE, main = "Variable Importance for ATP Model")

#Predict on the test data. Type = "vote" to extracts the votes of all trees
votes <- predict(rfModelSMOTE, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.2b
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) +  #Scale density to appear as proportion
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilites of each game style for each player. Do this to whole dataset.
predictedProbsSMOTE <- predict(rfModelSMOTE, pca_scoresrf, type = "prob")
predictedProbsSMOTE_df <- as.data.frame(predictedProbsSMOTE)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsSMOTE_df$Player <- pca_scoresrf$Player
predictedProbsSMOTE_df$Player[predictedProbsSMOTE_df$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
pca_scoresrf$Player[pca_scoresrf$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
#Calculate the mean of each column (game style) by player
player_meansSMOTE <- predictedProbsSMOTE_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check
row_sumsSMOTE <- rowSums(player_meansSMOTE[-1])
row_sumsSMOTE

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropySMOTE <- player_meansSMOTE %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entopy scores
ggplot(player_entropySMOTE, aes(x = Entropy)) +  
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot of Entropy Scores for ATP Data", x = "Entropy Score", y = "Density") +  
  theme_minimal()   

#Cross Entropy
#Obtain the true classes of each player by matching from player_meansSMOTE with pca results. Match function will return index from pca_scoresrf data.
true_classes <- pca_scoresrf$Gamestyle[match(player_meansSMOTE$Player, pca_scoresrf$Player)]
true_classes

cross_entropy_scoresSMOTE <- c()

#Loop over each player
for (i in 1:nrow(player_meansSMOTE)) {
  true_class_indexSMOTE <- which(colnames(player_meansSMOTE) == true_classes[i])
  true_probSMOTE <- player_meansSMOTE[i, true_class_indexSMOTE]
  cross_entropy_scoresSMOTE[i] <- -log(true_probSMOTE)
}

cross_entropy_scoresSMOTE <- as.numeric(cross_entropy_scoresSMOTE)
player_cross_entropySMOTE <- data.frame(Player = player_meansSMOTE$Player, CrossEntropy = cross_entropy_scoresSMOTE)

#Plot cross entropy scores
ggplot(player_cross_entropySMOTE, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "#008080", alpha = 0.5) +  
  labs(title = "Density Plot of Cross-Entropy Scores for ATP Data", x = "Cross-Entropy Score", y = "Density") + 
  theme_minimal()   

#################RUS########################
#Function from caret
data_balanced <- downSample(trainData[,1:10], trainData$Gamestyle, yname = "Gamestyle")
table(data_balanced$Gamestyle)
#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelbalanced <- randomForest(
  Gamestyle ~ ., 
  data = data_balanced, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsbalanced <- predict(rfModelbalanced, testData)


#Get confusion matrix from caret package, can also observe other metrics here
confMatrixbalanced <- confusionMatrix(predictionsbalanced, testData$Gamestyle)

#Print the RUS model's confusion matrix
print(confMatrixbalanced)

#Convert confusion matrix to a data frame for ggplot
confMatbalanced <- as.data.frame(confMatrixbalanced$table)

row_sums_balanced <- confMatbalanced %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_balanced <- confMatbalanced %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatbalanced <- left_join(confMatbalanced, row_sums_balanced, by = "Reference")
confMatbalanced <- left_join(confMatbalanced, col_sums_balanced, by = "Prediction")

#Plot the confusion matrix with row and column sums for Figure 5.1c
plotbalanced <- ggplot(data = confMatbalanced, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_balanced, aes(x = Reference, y = 6, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_balanced, aes(x = 6, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotbalanced)

#Extract the error rate for RUS model
error_rate_balanced <- rfModelbalanced$err.rate
error_df_balanced <- data.frame(Trees = seq_len(nrow(error_rate_balanced)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate, same result. Stabilises at 800
ggplot(error_df_balanced, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for ATP Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart, similar result with PC2 and 3.
varImpPlot(rfModelbalanced, main = "Variable Importance for ATP Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelbalanced, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.2c
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) + 
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilites of each game style for each player. Do this to whole dataset.
predictedProbsbalanced <- predict(rfModelbalanced, pca_scoresrf, type = "prob")
predictedProbsbalanced_df <- as.data.frame(predictedProbsbalanced)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsbalanced_df$Player <- pca_scoresrf$Player
predictedProbsbalanced_df$Player[predictedProbsbalanced_df$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
pca_scoresrf$Player[pca_scoresrf$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
#Calculate the mean of each column (game style) by player
player_meansbalanced <- predictedProbsbalanced_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check. Need the row sum to be 1 as it is a probabilitiy
row_sumsbalanced <- rowSums(player_meansbalanced[-1])
row_sumsbalanced

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)  
player_entropybalanced <- player_meansbalanced %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropybalanced, aes(x = Entropy)) +  
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Entropy Scores for ATP Data", x = "Entropy Score", y = "Density") +   
  theme_minimal()   

#Cross Entropy
#Obtain the true classes of each player by matching from player_meansbalanced with pca results. Match function will return index from pca_scoresrf data.
true_classes <- pca_scoresrf$Gamestyle[match(player_meansbalanced$Player, pca_scoresrf$Player)]
true_classes

cross_entropy_scoresbalanced <- c()

#Loop over each player
for (i in 1:nrow(player_meansbalanced)) {
  true_class_indexbalanced <- which(colnames(player_meansbalanced) == true_classes[i])
  true_probbalanced <- player_meansbalanced[i, true_class_indexbalanced]
  cross_entropy_scoresbalanced[i] <- -log(true_probbalanced)
}

cross_entropy_scoresbalanced <- as.numeric(cross_entropy_scoresbalanced)
player_cross_entropybalanced <- data.frame(Player = player_meansbalanced$Player, CrossEntropy = cross_entropy_scoresbalanced)
#Plot cross entropy scores
ggplot(player_cross_entropybalanced, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "#008080", alpha = 0.5) +
  labs(title = "Density Plot of Cross-Entropy Scores for ATP Data", x = "Cross-Entropy Score", y = "Density") +   
  theme_minimal()   

#################ROS#######################
#Function from caret
data_balancedup <- upSample(trainData[,1:10], trainData$Gamestyle, yname = "Gamestyle")
table(data_balancedup$Gamestyle)
#Train the Random Forest using mtry = 3
set.seed(123)
rfModelbalancedup <- randomForest(
  Gamestyle ~ ., 
  data = data_balancedup, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsbalancedup <- predict(rfModelbalancedup, testData)
#Get confusion matrix from caret package, can also observe other metrics here
confMatrixbalancedup <- confusionMatrix(predictionsbalancedup, testData$Gamestyle)

#Print the ROS model's confusion matrix
print(confMatrixbalancedup)

#Convert confusion matrix to a data frame for ggplot
confMatbalancedup <- as.data.frame(confMatrixbalancedup$table)

row_sums_balancedup <- confMatbalancedup %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_balancedup <- confMatbalancedup %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatbalancedup <- left_join(confMatbalancedup, row_sums_balancedup, by = "Reference")
confMatbalancedup <- left_join(confMatbalancedup, col_sums_balancedup, by = "Prediction")

#Plot the confusion matrix with row and column sums for Figure 5.1d
plotbalancedup <- ggplot(data = confMatbalancedup, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_balancedup, aes(x = Reference, y = 6, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_balancedup, aes(x = 6, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotbalancedup)

#Extract the error rate for post-tuning model
error_rate_balancedup <- rfModelbalancedup$err.rate
error_df_balancedup <- data.frame(Trees = seq_len(nrow(error_rate_balancedup)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate. Same result, 800 stabilizes.
ggplot(error_df_balancedup, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for ATP Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart. Same result with PC2 and PC3.
varImpPlot(rfModelbalancedup, main = "Variable Importance for ATP Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelbalancedup, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.2d
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) + 
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilities of each game style for each player. Do this to the whole dataset.
predictedProbsbalancedup <- predict(rfModelbalancedup, pca_scoresrf, type = "prob")
predictedProbsbalancedup_df <- as.data.frame(predictedProbsbalancedup)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsbalancedup_df$Player <- pca_scoresrf$Player
predictedProbsbalancedup_df$Player[predictedProbsbalancedup_df$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
pca_scoresrf$Player[pca_scoresrf$Player == "Carlos Alcaraz Garifa"] <- "Carlos Alcaraz"
#Calculate the mean of each column (game style) by player
player_meansbalancedup <- predictedProbsbalancedup_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check
row_sumsbalancedup <- rowSums(player_meansbalancedup[-1])
row_sumsbalancedup

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropybalancedup <- player_meansbalancedup %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropybalancedup, aes(x = Entropy)) +  
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot of Entropy Scores for ATP Data", x = "Entropy Score", y = "Density") +
  theme_minimal()   

#Cross Entropy
#Obtain the true classes of each player by matching from player_means with pca results. Match function will return index from pca_scoresrf data.
true_classes <- pca_scoresrf$Gamestyle[match(player_meansbalancedup$Player, pca_scoresrf$Player)]
true_classes

cross_entropy_scoresbalancedup <- c()

#Loop over each player
for (i in 1:nrow(player_meansbalancedup)) {
  true_class_indexbalancedup <- which(colnames(player_meansbalancedup) == true_classes[i])
  true_probbalancedup <- player_meansbalancedup[i, true_class_indexbalancedup]
  cross_entropy_scoresbalancedup[i] <- -log(true_probbalancedup)
}

cross_entropy_scoresbalancedup <- as.numeric(cross_entropy_scoresbalancedup)
player_cross_entropybalancedup <- data.frame(Player = player_meansbalancedup$Player, CrossEntropy = cross_entropy_scoresbalancedup)

#Plot cross entropy scores
ggplot(player_cross_entropybalancedup, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "#008080", alpha = 0.5) +  
  labs(title = "Density Plot of Cross-Entropy Scores for ATP Data", x = "Cross-Entropy Score", y = "Density") +
  theme_minimal()   
###################################################################################

#Entropy joined for Figure 5.3a and b
entropyjoined <- data.frame(Base = player_entropy_ordered$Entropy, BalancedDown = player_entropy_orderedbalanced$Entropy, SMOTE = player_entropy_orderedSMOTE$Entropy, BalancedUp = player_entropy_orderedbalancedup$Entropy)
#Change into long format
entropyjoinedlong <- melt(entropyjoined)
entropyjoinedlong <- entropyjoinedlong %>% rename(Model = variable, EntropyScore = value)
ggplot(entropyjoinedlong, aes(x = EntropyScore, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Entropy Scores by Model",
       x = "Entropy Score",
       y = "Density") +
  theme_minimal()

entropyjoinedlong$Model <- factor(entropyjoinedlong$Model, levels = c("Base", "SMOTE", "BalancedDown", "BalancedUp"))
#Boxplot
ggplot(entropyjoinedlong, aes(x = Model, y = EntropyScore, fill = Model)) +
  geom_boxplot() +
  labs(title = "Boxplot of Entropy Scores by Model",
       x = "Model",
       y = "Entropy Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#Cross entropy joined for Figure 5.3c and d
crossentropyjoined <- data.frame(Base = player_cross_entropy$CrossEntropy, BalancedDown = player_cross_entropybalanced$CrossEntropy, SMOTE = player_cross_entropySMOTE$CrossEntropy, BalancedUp = player_cross_entropybalancedup$CrossEntropy)
#Change into long format
crossentropyjoinedlong <- melt(crossentropyjoined)
crossentropyjoinedlong <- crossentropyjoinedlong %>% rename(Model = variable, CrossEntropyScore = value)
ggplot(crossentropyjoinedlong, aes(x = CrossEntropyScore, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Cross Entropy Scores by Model",
       x = "Entropy Score",
       y = "Density") +
  xlim(0,2)+
  theme_minimal()

crossentropyjoinedlong$Model <- factor(crossentropyjoinedlong$Model, levels = c("Base", "SMOTE", "BalancedDown", "BalancedUp"))
#Boxplot
ggplot(crossentropyjoinedlong, aes(x = Model, y = CrossEntropyScore, fill = Model)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cross Entropy Scores by Model",
       x = "Model",
       y = "Entropy Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

########################################WOMENS########################################
#####
#PCA#
#####
#We only use the percentages after attacking errors total for the PCA
#Finding the index of the column "attacking_errors_total"
index_of_attacking_errors_total <- which(names(womenmatchespbp) == "attacking_errors_total")

#Selecting columns after "attacking_errors_total" for PCA
columns_after <- names(womenmatchespbp)[(index_of_attacking_errors_total + 1):ncol(womenmatchespbp)]

#Filtering out rows with any NA values in the columns used for PCA and storing the result in a new data frame
womenmatchespbpclean <- na.omit(womenmatchespbp[,c("Player", "Player Gamestyle", columns_after)])

#Summary Stats data set
womenmatchesstats <- na.omit(womenmatchespbpclean[, columns_after])

#Extracting the game styles corresponding to the cleaned data
game_styles_clean <- womenmatchespbpclean$`Player Gamestyle`
name_clean <- womenmatchespbpclean$Player

#Perform PCA on the cleaned data without the game styles
pca_result <- prcomp(womenmatchespbpclean[, 3:ncol(womenmatchespbpclean)], 
                     center = TRUE, scale. = TRUE)
#Print a summary of the PCA results
summary(pca_result)

#pca_result$x is the score for each player. 
#Join the scores with game style and player name
pca_scores <- cbind(as.data.frame(pca_result$x), Gamestyle = game_styles_clean, Player = name_clean)

#Get the variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

#Calculate the cumulative variance explained
cumulative_variance <- cumsum(variance_explained)

#Create a data frame for plotting
scree_data <- data.frame(
  PC = 1:length(variance_explained),
  Variance = variance_explained,
  CumulativeVariance = cumulative_variance
)

#Generate the scree plot
ggplot(scree_data, aes(x = PC)) +
  geom_bar(aes(y = Variance, fill = "Individual Variance"), stat = "identity") +
  geom_line(aes(y = CumulativeVariance, color = "Cumulative Variance"), linewidth = 1) +
  geom_point(aes(y = CumulativeVariance, color = "Cumulative Variance"), size = 2) +
  scale_fill_manual(name = "Legend", values = c("Individual Variance" = "steelblue")) +
  scale_color_manual(name = "", values = c("Cumulative Variance" = "darkred")) +
  scale_y_continuous(labels = scales::percent, sec.axis = sec_axis(~ ., name = "Cumulative Variance")) +
  labs(x = "Principal Component", y = "Variance Explained (%)") +
  geom_hline(yintercept=0.70, linetype="dashed", color = "red") +
  theme_minimal() +
  ggtitle("Scree Plot for WTA Data") +
  theme(legend.position = "bottom")


#Biplot
#We will use only the first two principal components for the biplot with zones
autoplot(pca_result, data = pca_scores, 
         colour = 'Gamestyle', frame = TRUE,
         loadings = F, loadings.label = F, 
         loadings.colour = 'black', loadings.label.size = 3) +
  theme_minimal() +
  ggtitle("Biplot on PC1 and PC2 for WTA Data")

#Kaiser Rule
#Extract the standard deviations of the principal components
standard_deviations <- pca_result$sdev

#Square the standard deviations to get the eigenvalues
eigenvalues <- standard_deviations^2

#Print the eigenvalues
print(eigenvalues)

#Based on Kaiser Criterion, drop PCs with EV<1. Take first 7 PC.

###############
#Random Forest#
###############
#Take first 10 PCs, use the 70% cut off rule
pca_scoreswomenrf <- pca_scores[,c(1:10, 27, 28)]

#Split the data into test and train
set.seed(123)  
index <- createDataPartition(pca_scoreswomenrf$Gamestyle, p=0.8, list=FALSE)
trainData <- pca_scoreswomenrf[index, -which(names(pca_scoreswomenrf) == "Player")]  
testData <- pca_scoreswomenrf[-index, -which(names(pca_scoreswomenrf)== "Player")]

#Ensure Gamestyle is a factor
trainData$Gamestyle <- as.factor(trainData$Gamestyle)
testData$Gamestyle <- as.factor(testData$Gamestyle)

#Check the distribution of classes in the 'Gamestyle' column
class_distribution <- table(pca_scoreswomenrf$Gamestyle)

#Calculate the relative frequency of each class to check for imbalance
class_distribution_relative <- prop.table(class_distribution)
print(class_distribution_relative)


###########################BASE########################################
#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelOptimized <- randomForest(
  Gamestyle ~ ., 
  data = trainData, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)

#Make predictions on the test set
predictionsOptimized <- predict(rfModelOptimized, testData)

#Get confusion matrix from caret package, can also observe other metrics here
confMatrixOptimized <- confusionMatrix(predictionsOptimized, testData$Gamestyle)

#Print the optimized model's confusion matrix
print(confMatrixOptimized)

#Convert confusion matrix to a data frame for ggplot
confMatPost <- as.data.frame(confMatrixOptimized$table)

row_sums_post <- confMatPost %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_post <- confMatPost %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatPost <- left_join(confMatPost, row_sums_post, by = "Reference")
confMatPost <- left_join(confMatPost, col_sums_post, by = "Prediction")

#Plot confusion matrix with row and column sums for Figure 5.4a
plotPost <- ggplot(data = confMatPost, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_post, aes(x = Reference, y = 5, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_post, aes(x = 5, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotPost)

#Extract the error rate for post-tuning model
error_rate_post <- rfModelOptimized$err.rate
error_df_post <- data.frame(Trees = seq_len(nrow(error_rate_post)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate
ggplot(error_df_post, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for WTA Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart
varImpPlot(rfModelOptimized, main = "Variable Importance for WTA Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelOptimized, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct PRobability and Gamestyle for Figure 5.5a
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) + 
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilities of each game style for each player. Do this to whole dataset
predictedProbsall <- predict(rfModelOptimized, pca_scoreswomenrf, type = "prob")
predictedProbsall_df <- as.data.frame(predictedProbsall)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsall_df$Player <- pca_scoreswomenrf$Player
#Calculate the mean of each game style by player
player_means <- predictedProbsall_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check. Need the row sum to be 1 as it is a probability
row_sums <- rowSums(player_means[-1])
row_sums

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropy <- player_means %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropy, aes(x = Entropy, ..scaled..)) +  
  geom_density(fill = "red", alpha = 0.5) +  
  labs(title = "Density Plot of Entropy Scores for WTA Data", x = "Entropy Score", y = "Density") +  
  theme_minimal()  

#Cross Entropy
#Obtain the true classes of each player my matching from player_means with pca_scoreswomenrf
true_classes <- pca_scoreswomenrf$Gamestyle[match(player_means$Player, pca_scoreswomenrf$Player)]
true_classes

cross_entropy_scores <- c()

#Loop over each player
for (i in 1:nrow(player_means)) {
  true_class_index <- which(colnames(player_means) == true_classes[i])
  true_prob <- player_means[i, true_class_index]
  cross_entropy_scores[i] <- -log(true_prob)
}

cross_entropy_scores <- as.numeric(cross_entropy_scores)
player_cross_entropy <- data.frame(Player = player_means$Player, CrossEntropy = cross_entropy_scores)
#Plot cross entropy scores
ggplot(player_cross_entropy, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot of Cross-Entropy Scores for WTA Data", x = "Cross-Entropy Score", y = "Density") + 
  theme_minimal()
##################SMOTE#################################################
library(recipes)
library(themis)
rec <- recipe(Gamestyle ~ ., data = trainData) %>%
  step_smote(Gamestyle)  #Apply SMOTE to fix imbalance
trainDataSmote <- prep(rec) %>%
  juice()
table(trainDataSmote$Gamestyle)

#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelSMOTE <- randomForest(
  Gamestyle ~ ., 
  data = trainDataSmote, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsSMOTE <- predict(rfModelSMOTE, testData)


#Get the confusion matrix from caret package, can also observe other metrics here
confMatrixSMOTE <- confusionMatrix(predictionsSMOTE, testData$Gamestyle)

#Print the SMOTE model's confusion matrix
print(confMatrixSMOTE)

#Convert confusion matrix to a data frame for ggplot
confMatSMOTE <- as.data.frame(confMatrixSMOTE$table)

row_sums_SMOTE <- confMatSMOTE %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_SMOTE <- confMatSMOTE %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatSMOTE <- left_join(confMatSMOTE, row_sums_SMOTE, by = "Reference")
confMatSMOTE <- left_join(confMatSMOTE, col_sums_SMOTE, by = "Prediction")

#Plot the SMOTE confusion matrix with row and column sums for Figure 5.4b
plotSMOTE <- ggplot(data = confMatSMOTE, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_SMOTE, aes(x = Reference, y = 5, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_SMOTE, aes(x = 5, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotSMOTE)

#Extract the error rate for SMOTE model
error_rate_SMOTE <- rfModelSMOTE$err.rate
error_df_SMOTE <- data.frame(Trees = seq_len(nrow(error_rate_SMOTE)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate. Stabilizes around 800.
ggplot(error_df_SMOTE, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for WTA Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16)  
  )

#Get variable importance dot chart, very similar with base model. PC2 and 3 are important
varImpPlot(rfModelSMOTE, main = "Variable Importance for WTA Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelSMOTE, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Game style for Figure 5.5b
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) +
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
#Obtain predicted probabilities of each game style for each player. Do this to whole dataset.
predictedProbsSMOTE <- predict(rfModelSMOTE, pca_scoreswomenrf, type = "prob")
predictedProbsSMOTE_df <- as.data.frame(predictedProbsSMOTE)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsSMOTE_df$Player <- pca_scoreswomenrf$Player
#Calculate the mean of each column (game style) by player
player_meansSMOTE <- predictedProbsSMOTE_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check
row_sumsSMOTE <- rowSums(player_meansSMOTE[-1])
row_sumsSMOTE

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropySMOTE <- player_meansSMOTE %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropySMOTE, aes(x = Entropy)) +  
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot of Entropy Scores for WTA Data", x = "Entropy Score", y = "Density") +
  theme_minimal()

#Cross Entropy
#Obtain true classes of each player
true_classes <- pca_scoreswomenrf$Gamestyle[match(player_meansSMOTE$Player, pca_scoreswomenrf$Player)]
true_classes

cross_entropy_scoresSMOTE <- c()

#Loop over each player
for (i in 1:nrow(player_meansSMOTE)) {
  true_class_indexSMOTE <- which(colnames(player_meansSMOTE) == true_classes[i])
  true_probSMOTE <- player_meansSMOTE[i, true_class_indexSMOTE]
  cross_entropy_scoresSMOTE[i] <- -log(true_probSMOTE)
}


cross_entropy_scoresSMOTE <- as.numeric(cross_entropy_scoresSMOTE)
player_cross_entropySMOTE <- data.frame(Player = player_meansSMOTE$Player, CrossEntropy = cross_entropy_scoresSMOTE)
#Plot cross entropy scores
ggplot(player_cross_entropySMOTE, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "red", alpha = 0.5) + 
  labs(title = "Density Plot of Cross-Entropy Scores for WTA Data", x = "Cross-Entropy Score", y = "Density") +
  theme_minimal() 

#################RUS########################
#Function from caret
data_balanced <- downSample(trainData[,1:10], trainData$Gamestyle, yname = "Gamestyle")
table(data_balanced$Gamestyle)
#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelbalanced <- randomForest(
  Gamestyle ~ ., 
  data = data_balanced, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsbalanced <- predict(rfModelbalanced, testData)


#Get confusion matrix from caret package, can also observe other metrics here
confMatrixbalanced <- confusionMatrix(predictionsbalanced, testData$Gamestyle)

#Print the RUS model's confusion matrix
print(confMatrixbalanced)

#Convert confusion matrix to a data frame for ggplot
confMatbalanced <- as.data.frame(confMatrixbalanced$table)

row_sums_balanced <- confMatbalanced %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_balanced <- confMatbalanced %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatbalanced <- left_join(confMatbalanced, row_sums_balanced, by = "Reference")
confMatbalanced <- left_join(confMatbalanced, col_sums_balanced, by = "Prediction")

#Plot the confusion matrix with row and column sums for Figure 5.4c
plotbalanced <- ggplot(data = confMatbalanced, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_balanced, aes(x = Reference, y = 5, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_balanced, aes(x = 5, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(plotbalanced)

#Extract the error rate for RUS model
error_rate_balanced <- rfModelbalanced$err.rate
error_df_balanced <- data.frame(Trees = seq_len(nrow(error_rate_balanced)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate, same result. Stabilisted at 800
ggplot(error_df_balanced, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for WTA Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart, similar result with PC2 and 3.
varImpPlot(rfModelbalanced, main = "Variable Importance for WTA Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelbalanced, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.5c
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) +
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 


#Entropy calculation
predictedProbsbalanced <- predict(rfModelbalanced, pca_scoreswomenrf, type = "prob")
predictedProbsbalanced_df <- as.data.frame(predictedProbsbalanced)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsbalanced_df$Player <- pca_scoreswomenrf$Player
#Calculate the mean of each column (game style) by player
player_meansbalanced <- predictedProbsbalanced_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check
row_sumsbalanced <- rowSums(player_meansbalanced[-1])
row_sumsbalanced

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropybalanced <- player_meansbalanced %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropybalanced, aes(x = Entropy)) +  
  geom_density(fill = "red", alpha = 0.5) + 
  labs(title = "Density Plot of Entropy Scores for WTA Data", x = "Entropy Score", y = "Density") + 
  theme_minimal()

#Cross Entropy
#Obtain true classes for each player
true_classes <- pca_scoreswomenrf$Gamestyle[match(player_meansbalanced$Player, pca_scoreswomenrf$Player)]
true_classes

cross_entropy_scoresbalanced <- c()

#Loop over each player
for (i in 1:nrow(player_meansbalanced)) {
  true_class_indexbalanced <- which(colnames(player_meansbalanced) == true_classes[i])
  true_probbalanced <- player_meansbalanced[i, true_class_indexbalanced]
  cross_entropy_scoresbalanced[i] <- -log(true_probbalanced)
}

cross_entropy_scoresbalanced <- as.numeric(cross_entropy_scoresbalanced)
player_cross_entropybalanced <- data.frame(Player = player_meansbalanced$Player, CrossEntropy = cross_entropy_scoresbalanced)
#Plot cross entropy scores
ggplot(player_cross_entropybalanced, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "red", alpha = 0.5) +  
  labs(title = "Density Plot of Cross-Entropy Scores for WTA Data", x = "Cross-Entropy Score", y = "Density") +
  theme_minimal() 

#################ROS#######################
#Function from caret
data_balancedup <- upSample(trainData[,1:10], trainData$Gamestyle, yname = "Gamestyle")
table(data_balancedup$Gamestyle)
#Train the Random Forest model using mtry = 3
set.seed(123)
rfModelbalancedup <- randomForest(
  Gamestyle ~ ., 
  data = data_balancedup, 
  mtry = 3, 
  ntree = 800,
  importance = TRUE
)
#Make predictions on the test set
predictionsbalancedup <- predict(rfModelbalancedup, testData)


#Get confusion matrix from caret package, can also observe other metrics here
confMatrixbalancedup <- confusionMatrix(predictionsbalancedup, testData$Gamestyle)

#Print the ROS model's confusion matrix
print(confMatrixbalancedup)

#Convert confusion matrix to a data frame for ggplot
confMatbalancedup <- as.data.frame(confMatrixbalancedup$table)

row_sums_balancedup <- confMatbalancedup %>% 
  group_by(Reference) %>%
  summarise(RowSum = sum(Freq))

col_sums_balancedup <- confMatbalancedup %>% 
  group_by(Prediction) %>%
  summarise(ColSum = sum(Freq))

confMatbalancedup <- left_join(confMatbalancedup, row_sums_balancedup, by = "Reference")
confMatbalancedup <- left_join(confMatbalancedup, col_sums_balancedup, by = "Prediction")

#Plot the confusion matrix with row and column sums for Figure 5.4d
pbalancedup <- ggplot(data = confMatbalancedup, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  geom_text(data = row_sums_balancedup, aes(x = Reference, y = 5, label = RowSum), vjust = 1) +
  geom_text(data = col_sums_balancedup, aes(x = 5, y = Prediction, label = ColSum), hjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
  theme(axis.text.y  = element_text(angle = 45, hjust = 1, face="bold"))

print(pbalancedup)

#Extract the error rate for ROS model
error_rate_balancedup <- rfModelbalancedup$err.rate
error_df_balancedup <- data.frame(Trees = seq_len(nrow(error_rate_balancedup)), OOB_Error_Rate = error_rate_post[, "OOB"], Model = "Random Forest")

#Plot the error rate. Same result, 800 stabilizes
ggplot(error_df_balancedup, aes(x = Trees, y = OOB_Error_Rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Random Forest OOB Error Rate for WTA Data", x = "Number of Trees", y = "OOB Error Rate") +
  scale_color_manual(values = "Black") +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20), 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )

#Get variable importance dot chart. Same result with PC2 and PC3.
varImpPlot(rfModelbalancedup, main = "Variable Importance for WTA Model")

#Predict on the test data. Type = "vote" extracts the votes of all trees
votes <- predict(rfModelbalancedup, newdata = testData, type = "vote")
votes_df <- as.data.frame(votes)
votes_df$Gamestyle <- testData$Gamestyle
#Create a new column to extract the probability assigned to the correct game style of each player
votes_df$CorrectProbability <- apply(votes_df, 1, function(row) row[as.character(row["Gamestyle"])])

#Compile and plot Correct Probability and Gamestyle for Figure 5.5d
final_df <- data.frame(CorrectProbability = votes_df$CorrectProbability, Gamestyle = votes_df$Gamestyle)
final_df$CorrectProbability <- as.numeric(as.character(final_df$CorrectProbability))
#Empirical Distribution
ggplot(final_df, aes(x = CorrectProbability, fill = Gamestyle)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 0.01)) + 
  labs(title = "Empirical Distribution of Correct Probability by Game Style",
       x = "Correct Probability",
       y = "Density") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() 

#Entropy calculation
predictedProbsbalancedup <- predict(rfModelbalancedup, pca_scoreswomenrf, type = "prob")
predictedProbsbalancedup_df <- as.data.frame(predictedProbsbalancedup)
#Add the player names as a new column in the predicted probabilities data frame
predictedProbsbalancedup_df$Player <- pca_scoreswomenrf$Player
#Calculate the mean of each column (game style) by player
player_meansbalancedup <- predictedProbsbalancedup_df %>%
  group_by(Player) %>%
  summarise(across(everything(), mean, na.rm = TRUE))   

#Sanity check
row_sumsbalancedup <- rowSums(player_meansbalancedup[-1])
row_sumsbalancedup

#Function to calculate entropy for a vector of probabilities
calculate_entropy <- function(probs) {
  probs <- probs[probs > 0]
  entropy <- -sum(probs * log2(probs))
  return(entropy)
}

#Apply the entropy function to each row (excluding the Player column)
player_entropybalancedup <- player_meansbalancedup %>%
  rowwise() %>%
  mutate(Entropy = calculate_entropy(across(-Player))) %>%
  select(Player, Entropy)

#Plot entropy scores
ggplot(player_entropybalancedup, aes(x = Entropy)) +  
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot of Entropy Scores for WTA Data", x = "Entropy Score", y = "Density") + 
  theme_minimal() 

#Cross Entropy
#Obtain true classes by matching from player_meansbalancedup and pca_scoreswomenrf
true_classes <- pca_scoreswomenrf$Gamestyle[match(player_meansbalancedup$Player, pca_scoreswomenrf$Player)]
true_classes

cross_entropy_scoresbalancedup <- c()

#Loop over each player
for (i in 1:nrow(player_meansbalancedup)) {
  true_class_indexbalancedup <- which(colnames(player_meansbalancedup) == true_classes[i])
  true_probbalancedup <- player_meansbalancedup[i, true_class_indexbalancedup]
  cross_entropy_scoresbalancedup[i] <- -log(true_probbalancedup)
}

#Ensure cross_entropy_scores is a numeric vector
cross_entropy_scoresbalancedup <- as.numeric(cross_entropy_scoresbalancedup)
player_cross_entropybalancedup <- data.frame(Player = player_meansbalancedup$Player, CrossEntropy = cross_entropy_scoresbalancedup)
#Plot cross entropy scores
ggplot(player_cross_entropybalancedup, aes(x = CrossEntropy, ..scaled..)) + 
  geom_density(fill = "#008080", alpha = 0.5) + 
  labs(title = "Density Plot of Cross-Entropy Scores for WTA Data", x = "Cross-Entropy Score", y = "Density") +  
  theme_minimal()  

###################################################################################
#Entropy joined for Figure 5.6 and b
entropyjoined <- data.frame(Base = player_entropy_ordered$Entropy, BalancedDown = player_entropy_orderedbalanced$Entropy, SMOTE = player_entropy_orderedSMOTE$Entropy, BalancedUp = player_entropy_orderedbalancedup$Entropy)

#Change into long format
entropyjoinedlong <- melt(entropyjoined)
entropyjoinedlong <- entropyjoinedlong %>% rename(Model = variable, EntropyScore = value)
ggplot(entropyjoinedlong, aes(x = EntropyScore, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Entropy Scores by Model",
       x = "Entropy Score",
       y = "Density") +
  theme_minimal()

entropyjoinedlong$Model <- factor(entropyjoinedlong$Model, levels = c("Base", "SMOTE", "BalancedDown", "BalancedUp"))

#Boxplot
ggplot(entropyjoinedlong, aes(x = Model, y = EntropyScore, fill = Model)) +
  geom_boxplot() +
  labs(title = "Boxplot of Entropy Scores by Model",
       x = "Model",
       y = "Entropy Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#Cross entropy joined for Figure 5.6c and d
crossentropyjoined <- data.frame(Base = player_cross_entropy$CrossEntropy, BalancedDown = player_cross_entropybalanced$CrossEntropy, SMOTE = player_cross_entropySMOTE$CrossEntropy, BalancedUp = player_cross_entropybalancedup$CrossEntropy)

#Gather the data to long format
crossentropyjoinedlong <- melt(crossentropyjoined)
crossentropyjoinedlong <- crossentropyjoinedlong %>% rename(Model = variable, CrossEntropyScore = value)
ggplot(crossentropyjoinedlong, aes(x = CrossEntropyScore, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Cross Entropy Scores by Model",
       x = "Entropy Score",
       y = "Density") +
  theme_minimal()


crossentropyjoinedlong$Model <- factor(crossentropyjoinedlong$Model, levels = c("Base", "SMOTE", "BalancedDown", "BalancedUp"))

#Boxplot
ggplot(crossentropyjoinedlong, aes(x = Model, y = CrossEntropyScore, fill = Model)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cross Entropy Scores by Model",
       x = "Model",
       y = "Entropy Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")
